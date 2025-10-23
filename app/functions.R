######################## FUNCTIONS #################################
# comments: foo1 function loaded straight into global.R
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################

#### Correlation Analysis Functions ####
## correlation plot
plt_corr = function(corr,labelcol = "black",labelorder='alphabet',meth = 'square', tpe = 'lower'){
corma = as.matrix(corr)
plt = corrplot(corma, method = meth, order =labelorder, tl.col = labelcol, type=tpe, diag=FALSE, tl.cex=1.6, cl.cex = 1.6)
return(plt)
}

## correlation table
find_high_corr <- function(cor_matrix, threshold = 0.75, tab = T,strike = NULL) {
  var_names <- colnames(cor_matrix)
  
  # empty dataframe
  high_cor_pairs <- data.frame(
    variable1 = character(),
    variable2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(nrow(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > abs(threshold)) {
        high_cor_pairs <- rbind(high_cor_pairs, data.frame(
          variable1 = var_names[i],
          variable2 = var_names[j],
          Correlation = cor_matrix[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  higg = high_cor_pairs %>% 
    arrange(desc(abs(Correlation)))%>%mutate(Correlation = round(Correlation,2))
  
  if(!is.null(strike)){
    higg[which(higg$variable1 %in% strike,arr.ind = T),] = sapply(higg[which(higg$variable1 %in% strike,arr.ind = T),], strike_through)
    higg[which(higg$variable2 %in% strike,arr.ind = T),] = sapply(higg[which(higg$variable2 %in% strike,arr.ind = T),], strike_through)
    
  }
  yolo = unique(c(higg$variable1,higg$variable2)) #used to fill drop down menu
  
  if(tab==T){return(higg)}else(return(yolo))
}


#### Write Config Functions ####

## pca and correlation update
write_corr = function(vars,
                      measures = mes,
                      cor_analysis = F,
                      pca_content = all_var,
                      pca = T,inipath="../input/config.ini") {
  
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  
  config <- read.ini(inipath)
 
  if (cor_analysis && pca) {
    stop("cannot write both PCA and Correlation ini at the same time, set pca = T OR cor_analysis = T, not both")
  }
  if (cor_analysis) {
    varmes = NULL
    
    if ("moran" %in% vars) {
      varmes = append(varmes, paste(mes, "moran", sep = "_"))
    }
    if ("linE" %in% vars) {
      varmes = append(varmes, "linE") #linE is not calculated for each measure
    }
    if ("share_con" %in% vars) {
      varmes = append(varmes, paste(mes, "share_con", sep = "_"))
    }
    if ("lu_share" %in% vars) {
      varmes = append(varmes, "lu_share") #lu_share is not calculated for each measure
    }
    if ("channel_frac" %in% vars) {
      varmes = append(varmes, paste(mes, "channel_frac", sep = "_"))
    }
    
    config[[1]]$col_correlation_matrix = paste(varmes, collapse = ", ")
    
    write.ini(config, inipath)
  } 
  if (pca) {
    config[[1]]$columns = paste(pca_content, collapse = ", ")
    write.ini(config, inipath)
    
  }
  
}
## check if some variables have been removed by convert_optain
check_align = function(inipath="../input/config.ini",var_path="../input/var_corr_par.csv"){
  
  if (!file.exists(inipath) | !file.exists(var_path)) {
    return(NULL)  
  } 
  
  config <- read.ini(inipath)
  
  written <- config[[1]]$col_correlation_matrix
  written <- strsplit(written, ", ")[[1]]
  
  var_corr = read.csv(var_path)
  var_corr = colnames(var_corr)[5:ncol(var_corr)]
  
  che = setdiff(written,var_corr)
  if(length(che)==0){return(NULL)}else{
    written = setdiff(written, che)
    config[[1]]$col_correlation_matrix = paste(written, collapse = ", ")
    write.ini(config, inipath)
  }
}

##
write_pca_ini <- function(var1 = "", var2 = "", var3 = "", var4 = "",
                          var1_lab= "", var2_lab = "", var3_lab = "", var4_lab = "",inipath="../input/config.ini") {
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  config <- read.ini(inipath)
  
  config[[5]]$var_1 <- ifelse(var1 == "off", "", var1)
  config[[5]]$var_2 <- ifelse(var2 == "off", "", var2)
  config[[5]]$var_3 <- ifelse(var3 == "off", "", var3)
  config[[5]]$var_4 <- ifelse(var4 == "off", "", var4)
  
  off_count <- sum(c(var1, var2, var3, var4) == "off")
  config[[5]]$num_variables_to_plot <- 4 - off_count
  config[[5]]$var_1_label <- ifelse(var1_lab == "off", "", var1_lab)
  config[[5]]$var_2_label <- ifelse(var2_lab == "off", "", var2_lab)
  config[[5]]$var_3_label <- ifelse(var3_lab == "off", "", var3_lab)
  config[[5]]$var_4_label <- ifelse(var4_lab == "off", "", var4_lab)
  write.ini(config, inipath) 
  
}

## write only units
write_uns <- function(var1_lab= "", var2_lab = "", var3_lab = "", var4_lab = "",inipath="../input/config.ini") {
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  config <- read.ini(inipath)
  
  config[[5]]$var_1_label <- ifelse(var1_lab == "off", "", var1_lab)
  config[[5]]$var_2_label <- ifelse(var2_lab == "off", "", var2_lab)
  config[[5]]$var_3_label <- ifelse(var3_lab == "off", "", var3_lab)
  config[[5]]$var_4_label <- ifelse(var4_lab == "off", "", var4_lab)
  write.ini(config, inipath) 
  
}

##
write_quali_ini = function(var1 = "", var2 = "", var3 = "", var4 = "",inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
   config <- read.ini(inipath)
  
    conf_clust = NULL
    if (var1 != "off") {
      conf_clust = append(conf_clust, var1)
    }
    if (var2 != "off") {
      conf_clust = append(conf_clust, var2)
    }
    if (var3 != "off") {
      conf_clust = append(conf_clust, var3)
    }
    if (var4 != "off") {
      conf_clust = append(conf_clust, var4)
    }
    
    config$Qualitative_Clustering$qualitative_clustering_columns = paste(conf_clust, collapse = ", ")
    write.ini(config, inipath)

}

##
write_cluster<- function(min_cluster=0,max_cluster=0,fixed_cluster_boolean="true",fixed_clusters=7,
                         inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)  
  }
  config <- read.ini(inipath)
  
  config[[2]]$fixed_clusters_boolean = fixed_cluster_boolean
  config[[2]]$fixed_clusters = fixed_clusters
  
  config[[2]]$min_clusters = min_cluster
  config[[2]]$max_clusters = max_cluster
  
  write.ini(config, inipath)
}

##
write_outl <- function(handle_outliers_boolean="false",deviations_min=3,deviations_max=3,
                        count_min=3,count_max=3,outlier_to_cluster_ratio=0.5,inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)  
  }
  
  config <- read.ini(inipath)
  
  config[[3]]$handle_outliers_boolean = handle_outliers_boolean
  config[[3]]$deviations_min = deviations_min
  config[[3]]$deviations_max = deviations_max
  config[[3]]$count_min = count_min
  config[[3]]$count_max = count_max
  config[[3]]$outlier_to_cluster_ratio = outlier_to_cluster_ratio
  
  write.ini(config, inipath)
  
}

## 
write_pcanum = function(pcamin,pcamax,inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)
  }
  config <- read.ini(inipath)
  
  config[[4]]$min_components = pcamin
  config[[4]]$max_components = pcamax
  write.ini(config, inipath)
  
  
}

#### Read Config Functions ####

## config for pca on startup
read_pca = function(inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)
  }
  config <- read.ini(inipath)
  
  if(!is.null(config[[1]]$columns)){pca_col_incl <- unlist(strsplit(config[[1]]$columns,", "))
  pca_col <- pca_col_incl[order(pca_col_incl)]}else{pca_col <- NULL}
  
  return(pca_col)
}

## 
read_config_plt = function(obj=T,axis=F,inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)
  }
  
  config <- read.ini(inipath)
  
  if(obj){
  var1 = config[[5]]$var_1
  var2 = config[[5]]$var_2
  var3 = config[[5]]$var_3
  var4 = config[[5]]$var_4}else if(axis){
    var1 = config[[5]]$var_1_label
    var2 = config[[5]]$var_2_label
    var3 = config[[5]]$var_3_label
    var4 = config[[5]]$var_4_label
  }else{stop("either obj or axis has to be TRUE")}
  
  return(c(var1,var2,var3,var4))
  
}


#### Table/Output Formatting Functions ####

## count number of decimals
num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}

## formatting with strike through
strike_through <- function(x) {
  sprintf('<span style="text-decoration: line-through;">%s</span>', x)
}

## extract objective ranges
get_obj_range = function(filepath = "../data/pareto_fitness.txt",colnames=paste0("objective", seq(1, 4))){
  stopifnot(file.exists(filepath))
  
  pf = read.table(filepath,sep=deli(filepath))
  colnames(pf) = colnames
  
  range_df <- data.frame(objective = character(), min = numeric(), max = numeric(), stringsAsFactors = FALSE)
  
  for (col_name in colnames) {
    min_val <- min(pf[[col_name]], na.rm = TRUE)
    max_val <- max(pf[[col_name]], na.rm = TRUE)
    range_df <- rbind(range_df, data.frame(objective = col_name, min = min_val, max = max_val, stringsAsFactors = FALSE))
  }
  
  if(file.exists("../data/pareto_fitness_original.txt")){
    
    pf = read.table("../data/pareto_fitness_original.txt",sep=deli("../data/pareto_fitness_original.txt"))
    colnames(pf) = colnames
    
    range_df2 <- data.frame(objective = character(), min = numeric(), max = numeric(), stringsAsFactors = FALSE)
    
    for (col_name in colnames) {
      min_val <- min(pf[[col_name]], na.rm = TRUE)
      max_val <- max(pf[[col_name]], na.rm = TRUE)
      range_df2 <- rbind(range_df2, data.frame(objective = col_name, min = min_val, max = max_val, stringsAsFactors = FALSE))
    }
    range_df$objective = NULL
    range_df = cbind(range_df2, range_df)
    colnames(range_df) = c("objective","min_original","max_original","min_ParetoPickR","max_ParetoPickR")
  }
  
  return(range_df)##
}

## split labels
word_splitter <- function(words, segment_length = 6) {
  sapply(words, function(word) {
    segments <- strsplit(word, "")[[1]]  # Split the word into characters
    n <- length(segments)                  # Get the number of characters
    formatted_word <- character(0)         # Initialize an empty character vector
    
    for (i in seq(1, n, by = segment_length)) {
      segment <- paste(segments[i:min(i + segment_length - 1, n)], collapse = "")
      formatted_word <- c(formatted_word, segment)
    }
    
    return(paste(formatted_word, collapse = "-\n"))
  })
}

## add percentage difference to status quo in brackets
add_perc_stq = function(df, stq) {
  df = as.matrix(df)
  stq = as.matrix(stq)
  df_new = df
  
  for (j in seq_len(ncol(df))) {
    for (i in seq_len(nrow(df))) {
      if (stq[j] != 0) {
        pd = (df[i, j] - stq[j]) / stq[j] * 100
        
        direction_sign <-  if (pd > 0) "+" else "-"
        
        dumb_bracket <- paste0(" (", direction_sign)
        
      } else{ pd = NA } #if status quo is 0
       
      
      main_value <- abs(df[i, j])
      if (main_value < 1) {
        rounded_value <- round(main_value, 4)
      } else if (main_value < 10) {
        rounded_value <- round(main_value, 2)
      } else {
        rounded_value <- round(main_value, 0)
      }
      
      if (is.na(pd)) {
        df_new[i, j] <- paste0("<span title='no percentage change can be provided because the status quo is zero for this objective'>",as.character(rounded_value), "*","</span>")
      } else if (pd == 0) {
        df_new[i, j] <- as.character(rounded_value)
      } else{
        df_new[i, j] <- paste0(as.character(rounded_value), dumb_bracket, round(abs(pd), 2), "%)")
      }
      
    }
  }
  return(df_new)
  
}


# add_perc <- function(df1, df2) {
#   df1 = as.matrix(df1)
#   df2 = as.matrix(df2)
#   
#   df1_new <- df1
#   
#   for (j in seq_len(ncol(df1))) {
#     col_range <- max(df2[, j]) - min(df2[, j])
#     mixed_signs <- (max(df2[, j]) > 0 & min(df2[, j]) < 0)
#     
#     for (i in seq_len(nrow(df1))) {
#       if (col_range != 0) {
#         pd = (df1[i, j] - df2[i, j]) / col_range * 100
#       } else {
#         pd = NA  # no change
#       }
#       
#       # conditional rounding
#       main_value <- abs(df1[i, j])
#       if (main_value < 1) {
#         rounded_value <- round(main_value, 4)
#       } else if (main_value < 10) {
#         rounded_value <- round(main_value, 2)
#       } else {
#         rounded_value <- round(main_value, 0)
#       }
#       
#       if (!is.na(pd) & !is.nan(pd) & !is.infinite(pd) & round(pd, 2) != 0) {
#         direction_sign <- if (pd > 0) "+" else "-"
#         dumb_bracket <- paste0(" (", direction_sign)
#         
#         if (mixed_signs) {
#           if (df1[i, j] > 0 && df2[i, j] < 0) {
#             value <- paste0("-", rounded_value)
#           } else if (df1[i, j] < 0 && df2[i, j] > 0) {
#             value <- paste0("-", rounded_value)
#           } else {
#             value <- if (df1[i, j] > 0) paste0("+", rounded_value) else paste0("-", rounded_value)
#           }
#         } else {
#           value <- as.character(rounded_value)
#         }
#         
#         df1_new[i, j] <- paste0(value, dumb_bracket, round(abs(pd), 2), "%)")
#       } else {
#         if (mixed_signs) {
#           if (df1[i, j] > 0 && df2[i, j] < 0) {
#             df1_new[i, j] <- paste0("-", rounded_value)
#           } else if (df1[i, j] < 0 && df2[i, j] > 0) {
#             df1_new[i, j] <- paste0("-", rounded_value)
#           } else {
#             df1_new[i, j] <- if (df1[i, j] > 0) paste0("+", rounded_value) else paste0("-", rounded_value)
#           }
#         } else {
#           df1_new[i, j] <- as.character(rounded_value)
#         }
#       }
#     }
#   }
#   
#   return(df1_new)
# }


#### Python Caller ####

run_python_script <- function(path_script="",pca_status) {
  pca_status("")
  p <- processx::process$new(
    # "python",  #old - when running with .py files
    # c(path_script),
    path_script,
    stdout = "|", stderr = NULL
  )
  
  while (p$is_alive()) {
    new_output <- p$read_output_lines()
    # new_output <- c(new_output, p$read_error_lines())
    if (length(new_output) > 0) {
      pca_status(paste(pca_status(), paste(new_output, collapse = "\n"), sep = "\n"))
    }
    Sys.sleep(0.1)
  }
  
  final_output <- p$read_all_output_lines()
  # final_output <- c(final_output, p$read_all_error_lines())
  if (length(final_output) > 0) {
    pca_status(paste(pca_status(), paste(final_output, collapse = "\n"), sep = "\n"))
  }
}

#### Plotting the optima ####
## get linear elements requiring a buffer
pull_buffer = function(){
  if(file.exists("../input/buffers.RDS")){
    return(readRDS("../input/buffers.RDS"))
  }else{return(NULL)}
}

# pull_buffer = function(prios= "../input/nswrm_priorities.csv"){
#   if (!file.exists(prios)) {
#     return(NULL)  
#   }
#   priodf = read.csv(prios)
#   strct_obj = priodf%>%filter(mngmt ==0)%>%select(nswrm)%>%pull()#structural measures
#   return(strct_obj)
# }

## get map extent
plt_latlon = function(conpath){
  conny = read.table(conpath,skip = 1,header = T)
  lon_map = mean(conny$lon)
  lat_map = mean(conny$lat)
  return(c(lat_map,lon_map))
}

## color assigned based on highest frequency, not distinguishing buffers, would require two freq columns
color_meas_most <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  fr <- table(x)
  tied <- names(fr)[fr == max(fr)]
  sample(tied, 1)
}

## plot frequency
plt_freq = function(data, lo, la, buffers, remaining, dispal = pal, 
                    mes = mes, legend = TRUE, basemap = basemap ) {
  
  data = left_join(data, remaining, by = c("id"))%>%st_make_valid() #only those with highest priority
  data = data %>%subset(!st_is_empty(geometry))
  
  m =  leaflet(data=data) %>%
    setView(lng = lo, lat = la, zoom = 12) 
  
  if(!basemap){ #show basemap if anonymise NOT selected
    m = m %>%
      addProviderTiles(providers$CartoDB.Positron)#poviders$Esri.NatGeoWorldMap, $Stadia.StamenToner, $OpenTopoMap
  }
  
  #buffer first otherwise small elements not selectable
  if(!is.null(buffers)){
    buffered_data <- buffers %>%filter(id %in% remaining$id)
    buffered_data <- buffered_data %>%
      inner_join(remaining %>% filter(freq > 0), by = "id") %>% #previously NA filtered beforehand
      st_make_valid() 
    
    m = m %>% addPolygons(
      data = buffered_data,
      fillColor = NA,
      color = ~ dispal(measure),
      weight = 1,
      dashArray = "3",
      fillOpacity = ~ freq,
      highlightOptions = highlightOptions(
        weight = 2,
        bringToFront = F
      ),
      label = ~ paste0(measure," frequency: ", round(freq,2))
    )
  }
  
  
  
  m = m %>%
    addPolygons(
      fillColor = ~ dispal(measure),
      fillOpacity = ~ freq,
      color = "lightgrey",
      weight = 1,
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      label = ~ paste0(measure," frequency: ", round(freq,2))
    ) 
  
  
  color_swatches <- lapply(mes, function(mess) {
    base_color <- dispal(mess)
    sapply(c(0.15, 0.5, 1), function(opacity) {
      rgb(t(col2rgb(base_color) / 255), alpha = opacity, maxColorValue = 1)
    })
  })
  
  
  if(legend){
  custom_legend <- HTML(
    paste0(
      "<div style='background: rgba(255, 255, 255, 0.8); padding: 4px; border-radius: 2px; font-size: 12px; line-height: 1;'>", # Compact line height
      "<strong>Measure<br>Frequency</strong><br><br>",
      paste(
        sapply(seq_along(mes), function(i) {
          paste0(
            "<div style='margin-bottom: 1px;'><strong style='margin: 0; padding: 0;'>", mes[i], "</strong></div>", # Tight margin for subheadings
            paste0(
              "<div style='margin-left: 4px; margin-bottom: 0;'>",
              paste(
                sprintf(
                  "<span style='display: inline-block; width: 11px; height: 11px; background-color: %s;'></span> %s",
                  color_swatches[[i]], c("Low", "Medium", "High")
                ),
                collapse = "<br>"
              ),
              "</div>"
            )
          )
        }),
        collapse = "<br>"
      ),
      "</div>"
    )
  )}else{custom_legend <- HTML(
    paste0(
      "<div style='background: rgba(255, 255, 255, 0.8); padding: 4px; border-radius: 2px; font-size: 12px; line-height: 1;'>",
      "<strong>Measure<br>Frequency</strong><br><br>",
      paste(
        mapply(function(measure, colors) {
          paste0(
            "<div style='margin-bottom: 1px;'><strong style='margin: 0; padding: 0;'>", measure, "</strong></div>",
            "<div style='margin-left: 4px; margin-bottom: 0;'>",
            paste(
              mapply(function(color, label) {
                # Parse color if it's in rgba format
                if(grepl("^rgba", color)) {
                  col_values <- as.numeric(strsplit(gsub("rgba\\(|\\)", "", color), ",")[[1]])
                  png_data <- png::writePNG(array(col_values/255, dim=c(1,1,4)))
                } else {
                  # If it's a named color or hex code
                  col_rgb <- col2rgb(color, alpha=TRUE)
                  png_data <- png::writePNG(array(col_rgb/255, dim=c(1,1,4)))
                }
                base64_color <- base64enc::base64encode(png_data)
                sprintf(
                  "<img src='data:image/png;base64,%s' style='width: 11px; height: 11px; vertical-align: middle;'> %s",
                  base64_color, label
                )
              }, colors, c("Low", "Medium", "High"), SIMPLIFY = FALSE),
              collapse = "<br>"
            ),
            "</div>"
          )
        }, mes, color_swatches, SIMPLIFY = FALSE),
        collapse = "<br>"
      ),
      "</div>"
    )
  )
  }
  
  m <- m %>%
    addControl(custom_legend,
      position = "bottomright"
    )
  
  if (legend) {#remove fullscreen option on png
    m = m %>%
      addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
  }
  
 
  return(m) 
}

## pull cm clean
pull_shp_new = function(layername = "hru", hru_in_opt_path="../input/hru_in_optima.RDS"){
  if(file.exists(paste0("../data/",layername,".shp")) && file.exists(hru_in_opt_path)){
    hio= readRDS(hru_in_opt_path)
    
    hru = hio %>% filter(rowSums(!is.na(select(., starts_with("V")))) > 0) #only those hru that are used
    
    cm = read_sf(dsn = "../data/", layer = layername) #adapt path
    if(class(cm)[1]=="sfg" || class(cm$geometry)[2]=="sfc"){
      sfc_mixed <- st_as_sfc(cm, crs = 4326)
      
      sfc_mixed <- st_cast(sfc_mixed, "POLYGON")#if there are other elements like points
      
      cm <- st_sf(cm, geometry = sfc_mixed)

    }
    cm = cm %>% filter(id %in% hru$id) %>% select(id, name, geometry) %>%st_make_valid()
    
    cm_utm <-  st_transform(cm, crs = 32633) # UTM zone 33N
    cuffy <-st_buffer(cm_utm,0.0)
    cm = st_transform(cuffy, crs = st_crs(cm))
    
    cm = cm %>%select(id,geometry)%>%st_transform(., 4326)
    
    return(cm)}else{return(NULL)}
  
}

## fill cm with optima
fit_optims = function(cm,hru_in_opt_path="../input/hru_in_optima.RDS",optims){
  if(file.exists(hru_in_opt_path)){
    hio = readRDS(hru_in_opt_path)
    hio = hio %>% rename_with( ~ str_remove(., "^V"), starts_with("V"))
    
    hio = hio %>% select(optims[["optimum"]], id)#works because character, subset to only optima remaining after clustering
    
    cm = cm %>% filter(id %in% hio$id)%>%st_make_valid()#remove all hrus that are never activated in this pareto front
    
    # cm_utm <-  st_transform(cm, crs = 32633) # UTM zone 33N
    # cuffy <-st_buffer(cm_utm,0.0)
    # cm = st_transform(cuffy, crs = st_crs(cm))
   
    cm = cm %>%select(id,geometry)
    
    cm = left_join(cm, hio, by = c("id")) %>% st_transform(., 4326)
    
    return(cm)}else{return(NULL)}
  
}

## merge hrus with optima
plt_sel = function(opti_sel, shp){
  new_names <- paste0("Optimum_",opti_sel)
  plt_sel = shp %>% dplyr::select(id, geometry, all_of(opti_sel)) %>%  rename_with(.fn = ~ new_names, .cols = all_of(opti_sel))
  plt_sel = st_make_valid(plt_sel) # too slow annoyingly
  return(plt_sel)
}

## plot boxplots
plt_boxpl_clus = function(dat, sel, all_obs,mima){
  clus <- dat %>%
    pivot_longer(
      cols = 2:5
    )
  
  plts=list()

  colli = c( "#FFC61E", "#009ADE","#AF58BA", "#F28522")
  labs = length((unique(clus$optimum)))
  
  for(i in 1:length(all_obs)){
    
    coll = colli[i]
    
    mini = mima[i,2]
    maxi = mima[i,3]
    labspo =median(clus[clus$name == all_obs[i], ]$value)
    if(nrow(clus[clus$name == all_obs[i],])==1){
      
      p = ggplot(clus[clus$name == all_obs[i], ])+
        geom_point(color=coll, aes(x = name, y = value),size=4) +
        ylim(mini, maxi)+ 
        theme_bw() + theme(
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.y = element_text(size=14),
          axis.text.x = element_text(size = 22),
          axis.title = element_blank(),
          legend.position = "none"
        )+
        annotate("text",x=1, y=labspo, label =labs ,size=6)
    }else{
    p =ggplot(clus[clus$name == all_obs[i], ], aes(x = name, y = value)) +
      geom_boxplot(fill = coll) +
      ylim(mini, maxi)+ 
      theme_bw() + theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size = 22),
        axis.title = element_blank(),
        legend.position = "none"
      )+
      annotate("text",x=1, y=labspo, label =labs )
    }
    plts[[i]] = p
  }
  return(plts)
}


## plot leaflet w/ specific column
plt_lf <- function(data, lo, la, buff_els, col_sel, buffers, dispal = pal, basemap = basemap, fullscreen = T) {
  data = data %>%subset(!st_is_empty(geometry))
  m <- vector("list", length = length(col_sel))
  
  for (i in seq_along(col_sel)) {
    col = col_sel[i]
    
    p  = leaflet(data = data) %>%
      setView(lng = lo, lat = la, zoom = 12)
    
    if(!basemap){ #show basemap if anonymise NOT selected
      p = p %>%
        addProviderTiles(providers$CartoDB.Positron)#poviders$Esri.NatGeoWorldMap, $Stadia.StamenToner, $OpenTopoMap
    }
    
    if(!is.null(buffers)){
      relevant_data <- data[data[[col]] %in% buff_els, ]
      
      buffered_data <- buffers %>%filter(id %in% relevant_data$id)%>%
        # rename(!!col := measure)%>%
        st_make_valid()
      
      p = p%>%
        addPolygons(
          data = buffered_data,
          fillColor = NA,
          color = ~ dispal(relevant_data[[col]]),
          weight = 1,
          dashArray = "3",
          fillOpacity = 0.2,
          highlightOptions = highlightOptions(
            color = ~ dispal(relevant_data[[col]]),
            weight = 2,
            bringToFront = TRUE
          )
        ) 
    }
    
   p = p %>%
      addPolygons(
        fillColor = ~ dispal(data[[col]]),
        fillOpacity = 0.8,
        color = "lightgrey",
        weight = 1,
        popup = ~ paste0("Value: ", data[[col]]),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = ~ data[[col]]
      ) %>%
      addControl(
        html = paste(col, "</b>"),
        position = "topright",
        className = "map-title"
      ) 
    

   p = p %>% 
      addLegend("bottomright", pal = dispal, values = data[[col]], na.label = "no change")
    
    if(fullscreen){
      p = p %>% addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
    }
    
    m[[i]] = p
  }
  return(m)
}


## legend plot
# plt_leg = function(mes){
#   dispal = colorFactor("Spectral", domain = mes, na.color = "lightgrey")
#   
#   leaflet() %>% addLegend( pal = dispal, title = "measures", values = mes, opacity = 1)
# }


## scatterplot in AHP, comparing two objectives
plt_scat2 = function(dat, x, y){
   ggplot(dat, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(color="grey50",size=1.1)+
    geom_smooth(method = "loess", se = FALSE,colour="darkblue")  +
    theme_bw() + theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "lightgray", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16)) +  #5%range around both ends
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.05)), labels = function(x) {rem_min(x)}) +
    scale_y_continuous(labels = function(y) {rem_min(y)})
}

#### Plotting the exploration tab

## plot (return of prep_diff_bar)
# plot_diff_bar= function(pct,obj_choices=NULL){
#   pl2 <- pct %>% rownames_to_column(var = "objective")  %>%mutate(objective=factor(objective)) %>%
#     mutate(objective=forcats::fct_relevel(objective,obj_choices))%>%
#     pivot_longer(-objective)%>%
#     
#     ggplot(aes(x = name, y = value, fill = objective)) +
#     geom_bar(position = "dodge",
#              stat = "identity",
#              alpha = 0.75) + labs(x = 'Objective', y = 'percentage change (%)') +
#     theme_bw() +
#     theme_minimal() +
#     scale_fill_manual(values = c( "#FFC61E", "#009ADE","#AF58BA", "#F28522", "#FF1F5B")) +
#     geom_text(aes(label = str_wrap(objective,width=8)),size=4,
#               colour = "black",
#               position = position_dodge(width = 1), vjust = -0.5) +
#     theme(
#       plot.title =  element_blank(),
#       axis.text.y = element_text(size = 15),
#       axis.text.x = element_text(size = 15),
#       axis.title.y = element_text(size =18),
#       axis.title.x = element_blank(),
#       legend.position = "none"
#     ) +
#     theme(plot.background = element_rect(fill = NA, color = NA))+scale_y_continuous(limits=c(-10,10))
#   
#   return(pl2)
# }


## parallel axis plot
plot_parline = function(datt,sizz=rep(.5, length(unique(datt$id))),colols=rep("grey50", length(unique(datt$id))),sq=NULL){
 
   
  pl1 <- ggplot(datt, aes(x = name, y = value,group=id,size=id, color=id)) +   # group = id is important!
    
      annotate("rect", xmin=1, xmax=4, ymin=0,    ymax=0.3333333, alpha=0.1, fill="#dc3545") +
      annotate("rect", xmin=1, xmax=4, ymin=0.3333333, ymax=0.6666667,  alpha=0.15, fill="#fd7e14") +
      annotate("rect", xmin=1, xmax=4, ymin=0.6666667, ymax=1,    alpha=0.2, fill="#28a745") +
    
    annotate("text", x = 4.04, y = 0.1666666, label = "worst",  angle=90, size = 10) + # Adjust hjust for alignment
    annotate("text", x = 4.04, y = 0.5, label = "medium",  angle=90, size = 10) +
    annotate("text", x = 4.04, y = 0.8333333, label = "best",  angle=90, size = 10) +
    
    geom_line(
      aes(group = id),
      alpha = 0.5,
      lineend = 'round',
      linejoin = 'round'
    ) + theme_bw()+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_blank()
    )+
    scale_y_continuous(limits = c(0,1)) +
    scale_x_discrete(expand = expansion(mult =  c(-0.05, 0.035)),labels = function(x) str_wrap(x, width = 8)) + 
    labs(x = "Factors", y = "Scaled Values") +
    scale_size_manual(values = sizz) +
    scale_color_manual(values = colols)+
    coord_cartesian(clip = "off") #prevent labels to be cut off
  
  if("#FF5666" %in% colols){ #double the trouble, triple the fun
   ids= which(colols != "grey50") 
  pl1 = pl1 + geom_line(data=datt[which(datt$id %in% ids),],aes(x = name, y = value),color = "#FF5666", size=1)
  }
  
  if(!is.null(sq)){
    sq$id = rep("1000",nrow(sq))
   pl1 = pl1 + geom_line(data=sq, aes(x=name,y=value),color="cyan",size=1)

  }
  
  return(pl1)
  
}

## share_con plot in Analysis tab

plt_share_con = function(dat){
  
  
  # pull number of measures = number of subplots
  # nsp = clus_all %>% select(ends_with("share_con"))%>%length()
  
  mesrs = dat%>%select(Cluster,ends_with("share_con")) %>%rename_with(~ sub("_share_con$", "", .),.cols = ends_with("share_con"))
  
  # pull number of selected clusters (max = 4)
  # nc = length(unique(clus_all$Cluster))
  
  # find a nice/clear way to plot this
  grp = mesrs %>% mutate(Cluster = as.factor(Cluster))%>%
    pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value")
  grp$Value = grp$Value*100
  p = ggplot(grp, aes(x = Variable, y = Value, fill = Cluster)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    scale_y_continuous(expand = c(0.02, 0.02),limits=c(0,105))+
    theme_bw() +
    theme_minimal() + 
    theme(
      plot.title =  element_blank(),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 28),
      axis.title = element_blank(),
      legend.position = "none"
    )+geom_text(data = grp, aes(x = Variable, y = 100, label = Cluster), 
                position = position_dodge(width = 0.75), vjust = 0,size=8)
  
  return(p)
}

## base theme
.base_theme_cache <- NULL
.get_base_theme <- function() {
  if (is.null(.base_theme_cache)) {
    .base_theme_cache <<- theme_bw() + theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "lightgray", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 16)
    )
  }
  return(.base_theme_cache)
}

## scatter plot in play around
plt_sc = function(dat, ranges, col=rep("grey",nrow(dat)), 
                  size=rep(2.8, nrow(dat)), sq=NULL, coefo = NULL){
 
  vars <- colnames(dat)
  num_vars <- length(vars)
  
  bt = .get_base_theme()

  sel_col <- "#FF5666" %in% col
  sel_ids <- if (sel_col) which(col == "#FF5666") else integer(0)
  sel_data <- if (sel_col) dat[sel_ids, , drop = FALSE] else NULL
  
  #get stable limits
  ranges_matrix <- as.matrix(ranges[vars, 2:3])
  
  
  plots <- list()
  plot_index <- 1
  
  for (i in 1:(num_vars - 1)) {
    for (j in (i + 1):num_vars) {
      
      xcol = vars[i]
      ycol = vars[j]
      
      x_min <- ranges_matrix[xcol, 1]
      x_max <- ranges_matrix[xcol, 2]
      y_min <- ranges_matrix[ycol, 1]
      y_max <- ranges_matrix[ycol, 2]
       
      # p <- ggplot(dat, aes_string(x = vars[i], y = vars[j])) +
     
     # regression line
     pname = paste0(xcol, "_", ycol)
     coef_dat = coefo[[pname]]
     
     p <- ggplot(dat, aes(x = !!sym(xcol), y = !!sym(ycol)))+
       geom_point(size = size, color = col) + bt + coord_cartesian(clip = "off") +  #prevent labels to be cut off
       geom_abline(
       intercept = coef_dat$intercept,
       slope = coef_dat$slope,
       color = "blue",
       size = 0.4,
       alpha = 0.7
     )+annotate("text", x = Inf, y = -Inf,#add R²
                label = paste("R² =", coef_dat$r_val),
                hjust = 1.1, vjust = -0.5, size = 4)+    #correct for negative scale aesthetics
       scale_x_continuous(limits = c(x_min, x_max),labels = rem_min, expand = expansion(mult =  c(0.015, 0.015))) +
       scale_y_continuous(limits = c(y_min, y_max),labels = rem_min, expand = c(0.15, 0))
     
     
    
     if (sel_col) {
       p <- p + geom_point(data = sel_data,aes(x = .data[[xcol]], y = .data[[ycol]]),color = "#FF5666", size = 2.8)
     }
     
     if(!is.null(sq)){
       p = p+ geom_point(data=sq,aes(x = !!sym(xcol), y = !!sym(ycol)), color="cyan", size=2.8)
     }
      
      
     plots[[plot_index]] <- p
     plot_index <- plot_index + 1
    }
  }

return(plots)

}

## scatter plot in analysis tab and AHP tab

plt_sc_optima <- function(dat, x_var, y_var, col_var, size_var, high_point = NULL, full_front = NULL, sq_path ="../data/sq_fitness.txt",
                          extra_dat = NULL, #highlight optima in AHP tab
                          an_tab = FALSE,
                          plt_extra=F, #potentially redundant tbf
                          sel_tab = NULL, #highlight table selection Analysis tab and point selection in Visualisation tab
                          add_whole = F, #add the whole pareto front Analysis tab
                          status_q = FALSE,
                          rev = FALSE,
                          unit = FALSE,
                          ahp_man = FALSE #adapt label in AHP for manual selection
) {
  
  if(is.null(full_front)){return(NULL)}
  
  if(file.exists("../input/units.RDS")){units = readRDS("../input/units.RDS")}else{units = rep("-",ncol(dat))}
  
  
  if(unit){
    current_obj_order = c(x_var, y_var,
                          col_var, size_var)
    
    original_order = names(dat)#=objectives()
    # 
    reorder_current = match(original_order, current_obj_order)
    reorder_original = match(current_obj_order,original_order)
    # 
    units[which(is.na(units) | units %in% c(" ", "","unitless","no unit"))] <- "-"
    # 
    colnames(dat) = paste(original_order, " [",units,"]", sep = "") #needs old order
    
    new_order = paste(original_order, " [",units,"]", sep = "")[reorder_original]
    
    x_var =new_order[1]
    y_var =new_order[2]
    col_var=new_order[3]
    size_var =new_order[4]
    
  }
  
  
  #fit() to establish range limits
  whole <- full_front
  colnames(whole) = colnames(dat)[1:4]
  # swiss_extra = whole #for controlling that all data is within range limits
  
  xma = yma = NULL
  
  if(an_tab){
    #separate for label (so nothing else has to change)
    dat2 = dat
    dat = dat[,1:4]
  }
  
  #all extra data prepared first
  # all_extra_data = NULL
  aed = list() #all extra data

  if (!is.null(extra_dat) && plt_extra) {
    names(extra_dat) = names(dat)
    # swiss_extra <- rbind(swiss_extra, extra_dat)
    
    extra_dat$set <- "cluster solutions"
    # all_extra_data <- rbind(all_extra_data,extra_dat)
    aed[[length(aed) + 1]] = extra_dat
  }
  
  if (!is.null(high_point) && nrow(high_point)!=0) {
    names(high_point) = names(dat)
    # swiss_extra <- rbind(swiss_extra, high_point)
    
    if(ahp_man){
      high_point$set = "Manual Selection"
    }else{high_point$set = "AHP - best option"}
    
    # all_extra_data <- rbind(all_extra_data,high_point)
    aed[[length(aed) + 1]] = high_point
    
  }
  
  if (!is.null(sel_tab) && nrow(sel_tab) > 0) {
    names(sel_tab) = names(dat)
    # swiss_extra <- rbind(swiss_extra, sel_tab)
    
    sel_tab$set <- "Selection"
    
    # if(!is.null(all_extra_data)){all_extra_data <- rbind(all_extra_data,sel_tab)}else{all_extra_data = sel_tab}
    aed[[length(aed) + 1]] = sel_tab
      
     }
  
  if (status_q) {
    st_q <- read.table(sq_path, header = FALSE, stringsAsFactors = FALSE, sep = deli(sq_path),colClasses = rep("numeric",4))
    names(st_q) <- names(dat)
    # swiss_extra <- rbind(swiss_extra, st_q)
    
    st_q$set <- "Status Quo"
    # all_extra_data <- rbind(all_extra_data,st_q)
    aed[[length(aed) + 1]] = st_q
    
  }
  
  if (length(aed) > 0) {
    swiss_extra <- bind_rows(list(whole, aed)) %>% select(-set)
  } else{
    swiss_extra = whole
  }
  
  #plot with main data
  p = ggplot(dat, aes(x = !!sym(x_var), y = !!sym(y_var),
                      fill = !!sym(col_var), size = !!sym(size_var)), alpha = 0.5) +
    #essential scales added first to prevent warnings
    viridis::scale_fill_viridis(alpha = 0.8, name = col_var, labels = function(x) abs(as.numeric(x)), limits=range(swiss_extra[[col_var]], na.rm = TRUE)) +
    scale_size(range = c(1, 10), limits=range(swiss_extra[[size_var]], na.rm = TRUE), name = size_var, labels = function(x) abs(as.numeric(x))) +
    scale_x_continuous(limits= range(swiss_extra[[x_var]], na.rm = TRUE), labels = function(x) {rem_min(x)}) +
    scale_y_continuous(limits= range(swiss_extra[[y_var]], na.rm = TRUE), labels = function(x) {rem_min(x)}) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.position = "right",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))
  
  #shape and color scales only if extra data exists
  # if (!is.null(all_extra_data)) {
  if(length(aed)>0){
    p = p + 
      scale_shape_manual(labels = function(x) gsub("-", "", x),
                         values = c("cluster solutions" = 21, "AHP - best option" = 22, "Manual Selection" = 22, "Selection" = 21, "Status Quo" = 21), name="") +
      scale_color_manual(labels = function(x) gsub("-", "", x),
                         values = c("cluster solutions" = "cyan", "AHP - best option" = "#FF4D4D", "Manual Selection" = "#FF4D4D", "Selection" = "black", "Status Quo" = "#FF00FF"), name="") +
      guides(color = guide_legend(override.aes = list(size = 5)),
             shape = guide_legend(override.aes = list(size = 5)))
  }
  
  #the optional whole dataset 
  if(add_whole){
    p = p + geom_point(data=whole, aes(x=!!sym(x_var), y = !!sym(y_var), size = !!sym(size_var)), fill="grey50", alpha=0.1)
  }
  
  #main data points
  p = p + geom_point(shape = 21, stroke = 0.5)
  
  #cluster number labels if needed
  if(an_tab && "cluster number" %in% colnames(dat2)){
    p = p + geom_text(data = dat2, aes(x = !!sym(x_var)+(0.03*diff(range(!!sym(x_var)))), y = !!sym(y_var), label = `cluster number`),
                      position = position_dodge(width = 0.85), hjust = 0, size=6)
  }
  
  #extra data points
  # if (!is.null(all_extra_data)) {
  if(length(aed)>0){
    all_extra_data = do.call(rbind, aed)
    p = p + geom_point(data = all_extra_data, aes(x = !!sym(x_var), y = !!sym(y_var), shape = set, color = set, size = !!sym(size_var), fill = !!sym(col_var)),
                       stroke = 1.8, show.legend = TRUE, alpha=0.7)
  }
  
  
  if(rev){
    p = p + scale_y_reverse(labels = function(y) {rem_min(y)}, limits= rev(range(swiss_extra[[y_var]], na.rm = TRUE))) +
      scale_x_reverse(labels = function(x) {rem_min(x)}, limits= rev(range(swiss_extra[[x_var]], na.rm = TRUE)))
  }
  
  return(p)
}

## scatter plot in Analysis tab for comparing decision and objective space 

pcs_vs_var <- function(dat, x_var, y_var, col_var, size_var,flip=F, sel_tab=NULL){
  
  p = ggplot(dat, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[col_var]], size = .data[[size_var]])) +
    geom_point(shape = 21, stroke = 0.5 ) +
    viridis::scale_fill_viridis(alpha = 0.8, name = col_var,labels = function(x) abs(as.numeric(x))) +  
    scale_size(range = c(1, 10), name = size_var,labels = function(x) abs(as.numeric(x))) +     
    theme_bw() + 
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          legend.position = "right", 
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(labels = function(x) {rem_min(x)})+scale_x_continuous(labels = function(x) {rem_min(x)})
  
  
  if (!is.null(sel_tab)) {
    
    sel_tab$set <- "Selection"
    p=  p+
      geom_point(data = sel_tab, aes(x = .data[[x_var]], y = .data[[y_var]], shape = set, color = set, size = .data[[size_var]]), 
                 stroke = 1.8, show.legend = TRUE, alpha=0.7)+
      scale_shape_manual(labels = function(x) gsub("-", "", x),
                         values = c("Selection" =21),name="") + 
      scale_color_manual(labels = function(x) gsub("-", "", x),
                         values = c( "Selection" = "black"),name="")
  }
  
  if(flip){
    p = p+coord_flip()}
  
  return(p)
}

#### AHP Functions ####

## consistency index for AHP
consistency_index <- function(m) {
  eig <- eigen(m)$values
  lambda_max <- Re(eig[which.max(Re(eig))])
  n <- nrow(m)
  return((lambda_max - n) / (n - 1))
}

## find main inconsistencies
check_inconsistencies <- function(comparison_matrix, weights) {
  n <- nrow(comparison_matrix)
  inconsistencies <- c() 
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        if (i != j && j != k && i != k) {
          if (comparison_matrix[i, j] > 1 &&
              comparison_matrix[j, k] > 1 &&
              comparison_matrix[i, k] <= 1) {
            
            inconsistencies <- c(
              inconsistencies,
              rownames(comparison_matrix)[i],
              rownames(comparison_matrix)[j],
              rownames(comparison_matrix)[k]
            )
          }
        }
      }
    }
  }
  
  return(unique(inconsistencies))
}



#### Rescaling and matching Functions ####
## return the original value and the position of scaled value in the original dataset
scaled_abs_match = function(minval_s=c(0,0,0,0),
                            maxval_s=c(1,1,1,1),
                            scal_tab=NULL,abs_tab=NULL,
                            allobs=NULL,smll = TRUE,at=F,mes_slider =F, mes_df = NULL){ 
  

  #merge with selection under measure sliders
  if(mes_slider &&!is.na(mes_slider) && identical(names(abs_tab),names(mes_df))){
    abs_tab = abs_tab %>% mutate(.temp_idx = row_number())
    scal_tab = scal_tab %>% mutate(.temp_idx = row_number())
    
    abs_tab = semi_join(abs_tab, mes_df, by = allobs)
    
    scal_tab = scal_tab %>% filter(.temp_idx %in% abs_tab$.temp_idx)
    
    abs_tab = abs_tab %>% select(-.temp_idx)
    scal_tab = scal_tab %>% select(-.temp_idx)
  }
  

  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  if(nrow(scal_tab)>0){
    
  # locate values in scaled dataframe 
  for(i in seq_along(allobs)){ #this does not have to run for those where we only want abs_tab/output(ch)
    
    idx_max <- which.min(abs(scal_tab[[allobs[i]]] - maxval_s[i]))
    df["max", allobs[i]] = abs_tab[idx_max, allobs[i]]  
    
    idx_min <- which.min(abs(minval_s[i] - scal_tab[[allobs[i]]]))
    df["min", allobs[i]] = abs_tab[idx_min, allobs[i]]
  }
  
  # consider interactions between objectives (some are not attainable anymore)
  # reduced dataframe of absolute values within all objective ranges
  ch = abs_tab
  
  for(k in seq_along(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<=valma & .data[[allobs[k]]]>=valmi)

  }}else{ch = scal_tab}
  
  cw = as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(cw) = allobs
  
  if(nrow(ch)!=0){
  # retain only min and max
  
  for (l in seq_along(allobs)) {
    if(length(ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1 ||
       length(ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1) {
      cw["max", allobs[l]] = NA
      cw["min", allobs[l]] = NA
    }else{
      
      cw["max", allobs[l]] = ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
      cw["min", allobs[l]] = ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
    }
    
  }
    
  
  if(at){rownames(cw) = c("best","worst")}}else{ch=ch[0, , drop = FALSE]}
  
  
  #when smll is set to false the table with all absolute values is returned
  if(smll){return(cw)}else{return(ch)} 
  
  }


## similar to ch in scaled_abs_match, matching input scaled data with a scaled dataframe
match_scaled = function(minval_s=c(0,0,0,0),
                        maxval_s=c(1,1,1,1),
                        scal_tab = NULL,
                        abs_tab = NULL, #needed for matching with measure sliders
                        mes_slider =F, mes_df = NULL,
                        allobs){
  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  
  # if measure slider has been touched, we need to recreate f_scaled()
  # from fit() after first subsetting it
  if(mes_slider &&!is.na(mes_slider) && identical(names(abs_tab),names(mes_df))){
    abs_tab = abs_tab %>% mutate(.temp_idx = row_number())
    scal_tab = scal_tab %>% mutate(.temp_idx = row_number())
    
    abs_tab = semi_join(abs_tab, mes_df, by = allobs)
    
    scal_tab = scal_tab %>% filter(.temp_idx %in% abs_tab$.temp_idx)
    
    scal_tab = scal_tab %>% select(-.temp_idx)
    }
  
  
  if(nrow(scal_tab)>0){
    # locate values in scaled data frame 
  for(i in seq_along(allobs)){
    
    sca_max <- scal_tab[which.min(abs(scal_tab[[allobs[i]]]-maxval_s[i])),]
    df["max",allobs[i]] = scal_tab[rownames(sca_max),allobs[i]]  
    
    sca_min <- scal_tab[which.min(abs(minval_s[i]-scal_tab[[allobs[i]]])),]
    df["min",allobs[i]] = scal_tab[rownames(sca_min),allobs[i]]
  }
  
  # surely this should be easier 
  ch = scal_tab
  
  for(k in seq_along(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<=valma & .data[[allobs[k]]]>=valmi)
  }
  }else{ch = scal_tab}
  
  if(dim(ch)[1]==0){return(ch[0, , drop = FALSE])}else{return(ch)}
  
}

## subset dataframe based on (slider) selection
match_abs <- function(minval, maxval, abs_tab, ranger = NULL, mes_slider = F, mes_df = NULL) {
  n_cols <- ncol(abs_tab)

  if(!is.null(ranger)){#undo the scaling which was done for the slider visibility
    
    indices <- which(names(abs_tab) %in% ranger)
    maxval[indices] = maxval[indices] / 1000
    minval[indices] = minval[indices] / 1000
  }
  
  #consider measure slider
  allobs = names(abs_tab) #naja
  if(mes_slider && !is.na(mes_slider) && identical(names(abs_tab),names(mes_df))){
    abs_tab = semi_join(abs_tab, mes_df, by = allobs)
  }
  
  
  filter_conditions <- lapply(seq_len(n_cols), function(i) {
    abs_tab[[i]] >= minval[i] & abs_tab[[i]] <= maxval[i]
  })
  
  combined_filter <- Reduce(`&`, filter_conditions)
  
  abs_filter <- abs_tab %>% filter(combined_filter)
  return(abs_filter)

}

## rescale
rescale_column <- function(column, min_val, max_val) {
  if (min_val == max_val) {
    return(rep(NA, length(column)))  
  }
  rescale(column, to = c(0, 1), from = c(min_val, max_val))
}

## ahp score function
which.ahp <- function(df, weights) {
  if (length(weights) != ncol(df)) {
    stop("Length of weights must match the number of columns in df")
  }
  score <- rowSums(sweep(1 - df, 2, weights, `*`))
  which.min(score)
}

## calculate appropriate step value for sliders, option for laggy sliders
# cal_step = function(ra, n = 100){ # only used for tiny ranges
#   rs = ra/n  #raw step
# 
#   mag = 10^floor(log10(abs(rs)))
#   rn = rs/mag
# 
#   if(rn <= 1){
#     nice = 1
#   }else if(rn <= 2){
#     nice = 2
#   }else if(rn <= 5){
#     nice = 5
#   }else{
#     nice = 10
#   }
# 
#   nice = nice*mag
#   nice = pmax(nice, ra/1000)
#   nice = pmin(nice, ra/5)
# 
#   return(nice)
# }


## pull highest range for nice plot
pull_high_range <- function(df, num_order=F) {
  abs_ranges <- sapply(df, function(col) {
    max(col, na.rm = TRUE) - min(col, na.rm = TRUE)
  })
  
  res <- data.frame(
    col = names(df),
    order = rank(-abs_ranges, ties.method = "first")  # rank with highest absolute range first
  )
  

  if(num_order) {
    return(order(res$order)) #order of objectives, needed to match unit labels
  } else{
    return(res[order(res$order), ]$col) #reordered objectives
  }

}

## scale fit() - function
# scale_data <- function(df, target_min = 10, target_max = 100) {
#   target_range <- target_max - target_min
#   
# df %>% mutate(across(everything(), ~ {
#     col_min <- min(.)
#     col_max <- max(.)
#     col_range <- col_max - col_min
#     
#     scale_factor <- 10 ^ round(log10(target_range / col_range))
#     
#     . * scale_factor
#   }))
# 
# }
# 
# get_scaler <- function(df, target_min = 10, target_max = 100) {
#   target_range <- target_max - target_min
#   
#   scale_facs <- sapply(df, function(col) {
#     col_min <- min(col, na.rm = TRUE)
#     col_max <- max(col, na.rm = TRUE)
#     col_range <- col_max - col_min
#     
#     10 ^ round(log10(target_range / col_range))
#   })
#   
#   return(scale_facs)
# }


#### Other Functions ####

get_mima <- function(df) {
  mins <- vapply(df, min, numeric(1), na.rm = TRUE)
  maxs <- vapply(df, max, numeric(1), na.rm = TRUE)
  
  data.frame(
    Variable = names(mins),
    worst = mins,
    best = maxs,
    stringsAsFactors = FALSE
  )
}

## check sliders and adapt var_corr_par accordingly
check_sliders <- function(input_vals, default_vals, ranger = NULL) {  #input_vals as list made from input$ranx
  touched <- sapply(1:4, function(s) {
    !all(input_vals[[s]] == default_vals[[s]])
  })
  
  
  if (any(touched)) {
    #check which var_corr_par are available, if previously touched take fresh one, store it, change it and safe it under new name
    if (file.exists("../input/var_corr_par_bu.csv")) {
      whole = read.csv("../input/var_corr_par_bu.csv", check.names = F)
    } else{#if never been reduced we read in original and create back up
      whole = read.csv("../input/var_corr_par.csv", check.names = F)
      write.csv(whole, file = "../input/var_corr_par_bu.csv", row.names = F) #now its changed a backup is needed
    }
    
    trs = whole
    
    if (!is.null(ranger)) {#basically what match_abs is doing too plus more columns
      indics <- which(names(trs) %in% ranger)
    }else{indics = NULL}
    
    for (k in which(touched)) {
      valma = input_vals[[k]][2]
      valmi = input_vals[[k]][1]
      
      if (k %in% indics) {
       valma = valma / 1000
       valmi = valmi / 1000
      }
      trs =  trs %>% filter(trs[[k]] <= valma & trs[[k]] >= valmi)
    }
    
   
    
    write.csv(trs,file="../input/var_corr_par.csv",row.names = F)
    
  } else{return(NULL)}
  
}

## remove minus (required for nicer plotting)
rem_min <- function(x) {
  gsub("-", "", scales::number(x))
}

## used with rem_min in plots
all_neg <- function(column) {
  all(column < 0)
}

## default max number of pc
get_num_pca <- function(pc_path = "../input/pca_content.RDS") {
  if(!file.exists(pc_path)){return(NULL)}
  pcc <- readRDS(pc_path)
  return(length(pcc))
}


## display selected pca settings
pca_settings = function(input){
  settings <- paste0("<ul>",
                     "<li><strong>",input$element1,"</strong> is shown on the x-axis","</li>",
                     "<li>", "The x-axis label is: \"<strong>",input$axisx,"</strong>\"</li>",
                     "<li><strong>", input$element2,"</strong> is shown on the y-axis", "</li>",
                     "<li>", "The y-axis label is: \"<strong>",input$axisy,"</strong>\"</li>",
                     "<li>", "The colour hue is defined by <strong>", input$element3, "</strong></li>",
                     "<li>", "The colour label is: \"<strong>",input$colour,"</strong>\"</li>",
                     "<li>", "The size of the data points is defined by: <strong>", input$element4, "</strong></li>",
                     "<li>", "The size label is: \"<strong>",input$size,"</strong>\"</li>",
                     "<li>", "A range of <strong>",input$pca_min,"</strong> to <strong>",input$pca_max,"</strong> principal components is tested.","</li>","</ul>"
  )
  
  # conditional settings
  if (input$clusyn == "Yes" &
      input$outlyn =="No") {
    #only cluster
    clus <- paste0(
      "<ul>",
      "<li>",
      "A range of <strong>",
      input$clus_min,
      "</strong> to <strong>",
      input$clus_max,
      "</strong> clusters is tested.",
      "</li>",
      "</ul>"
    )
    settings <- paste(settings, clus, collapse= "<br>")
  }else if (input$clusyn == "No" & input$outlyn == "No"){
    clus <- paste0(
      "<ul>", "<li>","Using a fixed number of <strong>",input$clus_fix,"</strong> of clusters","</li>",
      "<li> Outliers are not considered </li></ul>")
    settings <- paste(settings, clus, collapse= "<br>")
  } else if (input$clusyn == "Yes" & input$outlyn == "Yes") {
    #both
    clus <- paste0(
      "<ul>",
      "<li>",
      "A range of <strong>",
      input$clus_min,
      "</strong> to ",
      input$clus_max,
      "</strong> clusters is tested.",
      "</li>",
      "</ul>"
    )
    outly <- paste0(
      "<ul>",
      "<li>","Outliers are tested.","</li>",
      "<li>",
      "A range of <strong>",
      input$count_min,
      "</strong> to <strong>",
      input$count_max,
      "</strong> extreme variables is tested for removing clusters.",
      "</li>",
      "<li>",
      "The standard deviations tested range from <strong>",
      input$sd_min,
      "</strong> to <strong>",
      input$sd_max,
      "</strong></li>",
      "<li>","The tested ratio of number of ouliers to cluster size is: <strong> ",input$outlier_ratio, "</strong></li>",
      "</ul>"
    )
    settings <- paste(settings, clus, outly, collapse = "<br> ")
  } else if (input$clusyn == "No" & input$outlyn == "Yes") {
    outly <- paste0(
      "<ul>",
      "<li>","Outliers are tested.","</li>",
      "<li>","Using a fixed <strong>",input$clus_fix,"</strong> of clusters","</li>",
      "<li>",
      "A range of <strong>",
      input$count_min,
      "</strong> to <strong>",
      input$count_max,
      "</strong> extreme variables is tested for removing clusters.",
      "</li>",
      "<li>",
      "The standard deviations tested range from <strong>",
      input$sd_min,
      "</strong> to <strong>",
      input$sd_max,
      "</strong></li>",
      "<li>","The tested ratio of number of ouliers to cluster size is: <strong> ",input$outlier_ratio, "</strong></li>",
      "</ul>"
    )
    settings <- paste(settings, outly,collapse = "<br>")}
  
  return(settings)
}

deli = function(path){

which_del <- readLines(path, n = 1)

if (grepl(',', which_del)) {
  deliM = ','
} else if (grepl(' ', which_del)) {
  deliM = ' '
} else {
  deliM = ';' #this will break
}

return(deliM)

}
