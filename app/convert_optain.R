####################  Convert OPTAIN ###########################################
# Project: Clustering Pareto solutions/Multi-objective visualisation
# creates a .csv to be used in the Correlation and PCA
# comment: new variables for the pca require adapted functions.R and ui.R
# each row one Pareto-optimal solution
# 1.- 4. = objectives to be maximised
# 5 - end = variables considered in clustering (=all_var provided separately)
# used files: pareto_genomes.txt, hru.con, measure_location.csv
# author: cordula.wittekind@ufz.de
################################################################################

mode <- Sys.getenv("MY_MODE", unset = "default") #communicate with server.R
source("functions.R")

#### 1. Creating a small dataset with the variables for clustering and correlation ####
if(mode == "fast"){
  suppressPackageStartupMessages({
  library(foreign)
  library(dplyr)
  library(tidyr)
  })
  
  fit = read.table("../data/pareto_fitness.txt", header = FALSE, stringsAsFactors = FALSE, sep = deli("../data/pareto_fitness.txt"))
  yolo = readRDS("../input/object_names.RDS")
  names(fit) = yolo
  fit$id = seq_len(nrow(fit)) #ids are optima
  
  con = read.dbf("../data/hru.dbf")
  hru = readRDS("../input/hru_in_optima.RDS")
  nopt = ncol(hru)-1
  #pull unique meas
  
  hru %>%pivot_longer(cols = -id, names_to = "optims", values_to = "measure") %>%
    group_by(id)%>%filter(!is.na(measure)) %>%select(-id, -optims) %>%distinct(measure) %>% pull() %>% unique() -> meas
  
  if(length(meas)==0){
    writeLines("could not determine decision space elements. You might want to try to reupload your genome and lookup table.","../output/error_convert.txt")
    return()
  }
  
  missing_in_hru <- setdiff(con$id, hru$id)
  missing_in_con <- setdiff(hru$id, con$id)
  
  if (length(missing_in_hru) > 2) {#to exclude NA, 0 etc
    writeLines(paste("The following ids from the shapefile are not in the genome:", paste(missing_in_hru, collapse = ", ")),"../output/error_convert.txt")
    return()
  }
  if (length(missing_in_con) > 2) {
    writeLines(paste("The following ids from the genome are not in the shapefile:", paste(missing_in_con, collapse = ", ")),"../output/error_convert.txt")
    return()
  }
  
  hru_donde <- con %>% select(id,area)%>% inner_join(hru, by = "id")%>%mutate(obj_id = "id") # Pareto front in columns
  
  
  arre = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
  colnames(arre) = meas  
  rownames(arre) = paste0("V", 1:nopt)
  
  for (op in paste0("V", 1:nopt)) {
    #how much area was covered by individual measures 
    opti = hru_donde %>% select(c(all_of(op), area))
    
    for (m in meas) {
      if (m %in% opti[[op]]) {
        #check if land use is part of optimum (pond sometimes is not in Schwarzer Schoeps)
        
        arre[op, m] = opti %>% filter(.data[[op]] == m) %>%
          mutate(tot = sum(area)) %>% distinct(tot) %>% pull()
        
      }
      else{
        arre[op, m] = 0
      }
    }
    print(paste0("caculated area share of measures across Optimum ",op,"..."),
          quote = FALSE)
    
  }
  
  ## share in implemented catchment area
  share_con = apply(arre, 2, function(x) (x/max(x))*100) %>% as.data.frame() %>%
    rename_with(~paste0(., "_share_con"), all_of(meas)) %>% mutate(id = row_number())
  
  test_clu = fit %>% 
    left_join(share_con, by = "id") 
  
  write.csv(test_clu, "../input/cluster_params.csv",  row.names = FALSE, fileEncoding = "UTF8")  
  
  writeLines(meas, "../output/meas_fast.txt") # this is the best way to communicate "back"

}else{
#### 2. SWAT+/CoMOLA workflow - produce hru_in_optima.RDS from genome and 
#       measure_location, produce a more exhaustive set of cluster variables ####

  print(paste0("loading required packages..."), quote=F)
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(geosphere)
    library(spdep)
    library(Matrix)
    library(rlang)
  })
  
  
land_u = c("hedge", "buffer","edgefilter","grasshedge", "shrubhedge", "grassbuffer", "shrubbuffer", "grassslope","grassland","grassrchrg", "terrace", "floodres","swale", "rip_forest", "afforest", "afforestation", "contr")

## check, assign and write priorities, hardcodes what has been used in CoMOLA
nswrm_priorities <- function(lu) {
  prio_groups <- list(
    structural = c("pond", "constr_wetland", "wetland"), # structural elements (1st prio)
    land_use = land_u, #land use (2nd prio)
    management = c("constill","lowtillcc", "lowtill", "droughtplt","mintill", "notill", "intercrop", "covcrop", "rotation", "rot") # management (3rd prio)
  )
  
  prio <- data.frame(nswrm = character(), priority = integer(), mngmt = integer(), stringsAsFactors = FALSE)
  priority <- 1
  
  for (group_name in names(prio_groups)) {
    group_measures <- prio_groups[[group_name]]
    matching_indices <- which(lu %in% group_measures)
    matching_measures <- rev(lu[matching_indices]) #rev req because of COMOLA order
    
    if (length(matching_measures) > 0) {
      new_rows <- data.frame(
        nswrm = matching_measures,
        priority = seq(priority, length.out = length(matching_measures)),
        mngmt = as.integer(group_name == "management")
      )
      prio <- rbind(prio, new_rows)
      priority <- max(prio$priority) + 1
    }
  }
  
  # write.csv(prio, file = "../input/nswrm_priorities.csv", row.names = FALSE)
  
  return(prio)
}

## Genomes
  gen = read.table("../data/pareto_genomes.txt", header = FALSE, stringsAsFactors = FALSE, sep = deli("../data/pareto_genomes.txt"), encoding = "UTF-8")
  fit = read.table("../data/pareto_fitness.txt", header = FALSE, stringsAsFactors = FALSE, sep = deli("../data/pareto_fitness.txt"))
  #get number of optima
  nopt = nrow(fit)
  
  # if(nopt != ncol(gen)){gen = as.data.frame(t(gen))}#fixed in upload, more stable (row AND column check)
  
  print("check: read pareto_fitness.txt and pareto_genomes.txt...",quote=F)

  gen <- gen %>%
    mutate(id = seq_len(nrow(gen)), .before = V1) #id is number of AEP

# genome_hru matches AEP with hrus, several hrus for each AEP (hru = obj_id)
  genome_hru <- read.csv("../data/measure_location.csv")
  
  print("check: read measure_location.csv...", quote = FALSE)
  
  #count the number of extra columns required
 mc =  genome_hru %>%
    mutate(num_count = str_count(obj_id, ",") + 1) %>%
    summarize(max_numbers = max(num_count)) %>%
    pull(max_numbers)
  
#Separate values in obj_id/every hru its own column
  genome_hru_separate <- genome_hru %>%
     separate(obj_id, paste0("hru_sep_", 1:mc), sep = ',', remove = FALSE)#hru = obj_id in separate columns

  gen_act_hru <- genome_hru_separate %>%
     left_join(gen, by = "id") #id = individual AEPs

# Pivot 
  gen_act <- gen_act_hru %>%
   pivot_longer(cols = paste0("hru_sep_", 1:mc), names_to = "code", values_to = "name_new") %>% #name_new = hru separated
   relocate(name_new, .after = obj_id)%>%drop_na(name_new)

# Eliminate space before some "name_new"
  gen_act$name_new <- str_remove(gen_act$name_new, " ")
  
# Pull priorities
  meloc <- read.csv("../data/measure_location.csv") 
  mesrs = unique(meloc$nswrm) #only these have to be allocated
  prios <- nswrm_priorities(mesrs)

  gen_act_prio= gen_act %>% 
    left_join(prios, by = c("nswrm" = "nswrm")) %>%
    relocate(priority, .after = nswrm)%>%arrange(priority)
  
  print("check: assigned priorities...",quote=F)


#### Land use Cover of HRUs ####
 ## 1. create a dataframe defining all land uses of hru across pareto optima
 # empty hru dataframe
  
  print(paste0("Allocating to ", length(unique(gen_act_prio$name_new))," different hrus."))
  
  hru = data.frame(id=unique(gen_act_prio$name_new)) #name_new is hru, id switches from AEP to hru here!
  hru[paste0("V",1:nopt)]=NA
  
  # reduce the df size for efficiency ===> create main df to work with, activation, measure, priority, involved hru
  gen_check = gen_act_prio %>% select(-c(id,name,obj_id))

  print("calculating: land use allocation in optima under priorities...",quote=F)
  
# loop through optima
for(op in paste0("V", 1:nopt)){ 
  
  # only consider activated hrus
  all_act = gen_check %>%select(c(all_of(op),name_new, nswrm,priority))%>%filter(.data[[op]]==2)%>%group_by(name_new)
  
  # without conflicting use 
  no_confl = all_act%>%filter(n()==1)%>%ungroup()
  
  if(nrow(no_confl) > 0){
    
    hru <- hru %>%
      left_join(no_confl %>% select(name_new, nswrm), by = c("id" = "name_new")) %>%
      mutate(!!op := ifelse(!is.na(nswrm), nswrm, !!sym(op))) %>% 
      select(-nswrm)
  }
  
  if(nrow(all_act)!=nrow(no_confl)){
  # With conflicting use (no ungroup and regroup needed)
  confl_use <- all_act %>% filter(n() > 1) %>% #confl_use is then distinct solution
    filter(priority == min(priority)) %>% 
    distinct(name_new, nswrm)
 
  if(nrow(confl_use) > 0) {
    hru <- hru %>%
      left_join(confl_use%>% select(name_new, nswrm), by = c("id" = "name_new")) %>%
      mutate(!!op := ifelse(!is.na(nswrm), nswrm, !!sym(op))) %>%#updates only those with conflicting use
      select(-nswrm)
  }}
  print(paste0("check: calculated land use allocation in optima ",op,"..."),quote=F)
  
}
  ## hru represents the connection between the optima and plot
  hru <- hru%>%mutate(id = as.integer(id))
  hru = hru %>% mutate(across(starts_with("V"),as.character))
  
  hru = hru %>% filter(!rowSums(is.na(across(starts_with("V")))) == ncol(across(starts_with("V"))))#remove hrus that are never activated
  saveRDS(hru,file= "../input/hru_in_optima.RDS")
  print(paste0("check: made hru land use available for future plotting..."),quote=F)
  
  ## Moran's, share in total and activated area and linE
  con = read.table("../data/hru.con",header=T,skip=1)
  
  # could also use id as is the same as obj_id in con
  hru_donde <- con %>% select(id,area,lat,lon)%>% inner_join(hru, by = "id")%>%mutate(obj_id = "id") # Pareto front in columns
   
  # empty measures dataframe
  meas = unique(gen_act_prio$nswrm)
  
  ## Local Moran's i
    mit_i = hru_donde %>% select(obj_id, lat, lon)
  # Calculate pairwise distances using the Haversine formula
  
   dist_matrix <- distm(mit_i[, c('lon', 'lat')], fun = distHaversine)
   # Create spatial weights matrix using inverse distances
   inv_dist_matrix <- 1 / dist_matrix
   diag(inv_dist_matrix) <- 0  # Set the diagonal to zero to avoid infinity
   
   # distance threshold for balancing computational efficiency and small scale information
     if(as.numeric(object.size(inv_dist_matrix)) / (1024^2) > 70){
      dist_thresh <- quantile(dist_matrix, probs = 0.65)
      
      inv_dist_matrix[dist_matrix > dist_thresh] <- 0  #everything further than threshold not considered
      }
   
    # total_elements <- length(inv_dist_matrix)
    # non_zero_count <- sum(inv_dist_matrix != 0)
    # pnz <- non_zero_count / total_elements #proportion on zero
    
    #work with sparse matrix if over 100Mb or high share of zeroes
    # if(as.numeric(object.size(inv_dist_matrix)) / (1024^2) > 100 || pnz < -0.1){
      inv_dist_matrix <- Matrix::Matrix(inv_dist_matrix, sparse = TRUE)
      # }
    
  # Convert to a listw object for spatial analysis
  print(paste0("calculating spatial weights object, this may take a while..."),quote=F)
      
   weights_listw <- mat2listw(inv_dist_matrix, style = "B",zero.policy = TRUE)
   print("check: produced spatial weights object...",quote=F)
  # this weight object is used to calculate spatial autocorrelation across different measures, using the area they cover as input value

  # empty dataframe
    mesur = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
    colnames(mesur) = meas  #replace with <meas>_moran below
    rownames(mesur) = paste0("V", 1:nopt)
  
    #also needed for calculation of area share
    hru_copy = hru_donde %>% select(paste0("V", 1:nopt))
    print("calculating: Moran's I...",quote=F)
    
  # Moran's per measure/land use (setting all others to 0 and taking the area)
    for (op in paste0("V", 1:nopt)) {
      #   #how much area was covered by individual measures (hedge and linear stuff of course very little)
      opti = hru_donde %>% select(c(all_of(op), area))
      
      for (m in meas) {
        if (m %in% opti[[op]]) {
          #check if land use is part of optimum (pond sometimes is not in Schwarzer Schoeps)
          
          moran_area =  opti %>% mutate(mor = ifelse(.data[[op]] == m, area, 0)) %>%
            replace(is.na(.), 0)
          
          mor_clean <- na.omit(moran_area$mor) #can be used as NAs are random
          local_mr <- localmoran(mor_clean, weights_listw, zero.policy = TRUE)
          vr <- local_mr[!is.na(local_mr[, "Ii"]), ]
          mesur[op, m] <- median(vr[, "Ii"])
          

        }else{mesur[op, m] = 0}
      }
      print(paste0("check: calculated Moran's I for Optimum ", op,"..."),quote=F)
    }
  # change col names
  colnames(mesur) = paste(colnames(mesur),"moran",sep="_")
  mesur = mesur %>%mutate(id = row_number())

  ## linE - number of management versus number of structural measures in each optimum
  mngmt_obj= prios%>%filter(mngmt ==1)%>%select(nswrm)%>%pull() #management measures
  strct_obj = prios%>%filter(mngmt ==0)%>%select(nswrm)%>%pull()#structural measures
  
  lin = as.data.frame(array(NA, dim = c(nopt,1)))
  names(lin) = "linE"
  rownames(lin) = paste0("V", 1:nopt)
  
  for(op in paste0("V", 1:nopt)){
    opti = hru_donde %>% select(all_of(op))%>%group_by(.data[[op]])%>%mutate(count=n())
    
    mngmt = opti%>%filter(.data[[op]] %in% mngmt_obj)%>%distinct()%>%ungroup()%>%select(count)%>%sum()
    
    strc = opti %>% filter(.data[[op]] %in% strct_obj)%>%distinct()%>%ungroup()%>%select(count)%>%sum()

    lin[op,]= ifelse(mngmt != 0 && strc !=0,(strc/mngmt)*100,0) #just a nicer value
    print(paste0("caculated linE for Optimum ",op,"..."),quote=F)
    }
  
  lin = lin %>%mutate(id = row_number())

  ### fraction of water from individual measures that goes directly into channel (channel_frac)
  # read rout_unit.con (ensure in this ugly way that those w/ missing col names look the same)
  ru_names = readLines("../data/rout_unit.con", n=2 )[2]
  ru_names = unlist(strsplit(ru_names,"\t"))
  ru_names = unlist(strsplit(ru_names," "))
  
  ru_names = ru_names[ru_names != ""] 
  if(!("frac_1" %in% ru_names)){
    ru <- read.table("../data/rout_unit.con", header = FALSE, fill = TRUE,
                     stringsAsFactors = FALSE, skip = 2)
    ru = ru[,1:length(ru_names)]
    colnames(ru) <- ru_names  
    ru = ru %>% select(c(id, obj_typ, area, frac)) %>% filter(id != "aqu") %>% filter(obj_typ != "")
    colnames(ru) = c("obj_id", "obj_typ_1", "area", "frac_1")#path dependent name
    
    ru$obj_id = as.numeric(ru$obj_id)
    ru$frac_1 = as.numeric(ru$frac_1)
    
  }else{
  ru = read.table("../data/rout_unit.con",header = T, fill = TRUE,
                   stringsAsFactors = FALSE, skip = 1)
  ru = ru %>% select(c(obj_id,obj_typ_1,area,frac_1))}
  
  channel_frac = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
  colnames(channel_frac) = meas  
  rownames(channel_frac) = paste0("V", 1:nopt)
  
  for (op in paste0("V", 1:nopt)) {
    opti = hru_donde %>% select(c(id, all_of(op), area))
    
    for (m in meas) {
      if (m %in% opti[[op]]) {
        #check if land use is part of optimum 
        hru_ac = opti %>% filter(.data[[op]] == m) %>% select(id)
        
        if (nrow(hru_ac) != 0){
          #ru$name contains hrus called "rtuxxx", or use obj_id --> match with activated hrus in optimum
          ru_ac = ru[which(ru$obj_id %in% hru_ac$id), ] 
          
          if (nrow(ru_ac) == 0 | !any(ru_ac$obj_typ_1 == "sdc")) {#check if none routs to channel
            channel_frac[op, m] = 0
          } else{
            #total activated area
            tot_ac = opti %>% filter(.data[[op]] == m) %>%
              mutate(tot = sum(area)) %>% distinct(tot) %>% pull()
            
            #individual share in activated area (considering all including those that are not along channel)
            cf = opti %>% filter(.data[[op]] == m) %>% mutate(cf = area/tot_ac)
            
            #ru$obj_typ contains the type of receiving object --> subset to those where sdc (=channel)
            ru_ch = ru_ac[which(ru_ac$obj_typ_1 == "sdc"), ]
            
            #subset to channel adjacent
            cf = cf[which(cf$id %in% ru_ch$obj_id), ]
            
            #merge, multiply area share and fraction and pull value
            channel_frac[op, m] = ru_ch %>% left_join(cf, by =c("obj_id"="id")) %>%
              mutate(cf_a = cf * area.y) %>% summarise(median(cf_a, na.rm = T)) %>% pull()
          } 
        } else{
          channel_frac[op, m] = 0
        }
        
      }
      else{
        channel_frac[op, m] = 0
      }
    }
    print(paste0("caculated fraction of water routing into channel ", op, "..."),
          quote = F)
    
  }
  colnames(channel_frac) = paste(colnames(channel_frac),"channel_frac",sep="_")
  channel_frac = channel_frac %>%mutate(id = row_number())
  
  
  ## Area per measure, required for calculating share below
  # empty dataframe
   arre = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
   colnames(arre) = meas  
   rownames(arre) = paste0("V", 1:nopt)
  
  for (op in paste0("V", 1:nopt)) {
    #how much area was covered by individual measures (hedge and linear stuff of course very little)
    opti = hru_donde %>% select(c(all_of(op), area))
    
    for (m in meas) {
      if (m %in% opti[[op]]) {
        #check if land use is part of optimum (pond sometimes is not in Schwarzer Schoeps)
        
        arre[op, m] = opti %>% filter(.data[[op]] == m) %>%
          mutate(tot = sum(area)) %>% distinct(tot) %>% pull()
        
      }
      else{
        arre[op, m] = 0
      }
    }
    print(paste0("caculated area share of measures across Optimum ",op,"..."),
          quote = FALSE)
    
  }
   
   ## share in implemented catchment area
   share_con = apply(arre,2,function(x) (x/max(x))*100)%>%as.data.frame()%>%
     rename_with(~paste0(., "_share_con"), all_of(meas)) %>% mutate(id = row_number())
   
   ## land use share in considered/implemented catchment area
   if(any(meas %in% land_u)){
     lu = meas[which(meas %in% land_u)]
   
     # new column with combined area share
     lu_share = arre %>%
       mutate(lu_share = (rowSums(across(all_of(lu)))/rowSums(across(all_of(meas))))*100)%>%
       mutate(lu_share = ifelse(is.nan(lu_share), 0, lu_share))%>%mutate(id=row_number())%>%select(id,lu_share)
     
     share_con = share_con %>%left_join(lu_share, by = "id") #we chuck it here, 
     #otherwise too much testing needed
   }

  
  ## merge with pareto fitness, # the first row is optimum V1
  yolo = readRDS("../input/object_names.RDS")
  names(fit) = yolo
  fit$id = seq_len(nrow(fit))
  print("check: assigned names to pareto_finess.txt...",quote = FALSE)
  
  test_clu = fit %>% 
    left_join(lin, by = "id") %>%
    left_join(share_con, by = "id") %>%
    left_join(mesur, by = "id") %>%
    left_join(channel_frac, by = "id") %>%
    select(-id) %>% replace(is.na(.), 0) %>% select_if( ~ !all(. == 0))

  #scaling to between 0 and 1
  test_clu[, 5:ncol(test_clu)] <- lapply(test_clu[, 5:ncol(test_clu)], function(x) {
    if(is.numeric(x)) {
      (x - min(x)) / (max(x) - min(x))
    } else {
      x
    }
  })
  
  write.csv(test_clu, "../input/var_corr_par.csv",  row.names = FALSE, fileEncoding = "UTF8")  
  print("check: printed output ---> /input/var_corr_par...",quote = FALSE)

  all_var = colnames(test_clu)[5:ncol(test_clu)]  #assuming four variables here
  saveRDS(all_var,file = "../input/all_var.RDS") #required for PCA
  print("check: provided variable names ---> /input/all_var...", quote = FALSE)
  }