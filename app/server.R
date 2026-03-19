######################## SERVER ####################################
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
server <- function(input, output, session) {

  ## flag observers
  pareto_da <- reactiveVal(if(file.exists(pareto_path)) 1 else NULL) #observer for pareto_path availability Data Prep --> Vis
  clus_out <- reactiveVal(if (list.files(path = output_dir, pattern = "clusters_representativesolutions.*\\.csv$", full.names = TRUE) %>% length() > 0) 1 else NULL) #observer for cluster output availability
  axiselected = reactiveVal(if(file.exists("../input/units.RDS")) readRDS("../input/units.RDS") else NULL) #axis labels
  clusp_da = reactiveVal(if(file.exists("../input/cluster_params.csv") || file.exists("../input/var_corr_par.csv")) 1 else NULL) 
  hru_da =  reactiveVal(if(file.exists("../input/hru_in_optima.RDS")) 1 else NULL) 
  pg_da = reactiveVal(if(file.exists("../data/pareto_genomes.txt")) 1 else NULL) 
  shp_da = reactiveVal(if(file.exists("../data/hru.shp")) 1 else NULL)
  sq_da = reactiveVal(if(file.exists("../data/sq_fitness.txt")) 1 else NULL)
  
  ## reactive values
  objectives <- reactiveVal(character()) #objective names
  
  file_data3 <- reactiveVal(NULL)
  file_data6 <- reactiveVal(NULL)
  file_hrucon = reactiveVal(NULL)
  file_lookup_loc = reactiveVal(NULL)
  file_hru_activ = reactiveVal(NULL)
  file_hru_con = reactiveVal(NULL)
  file_shapefile_cd = reactiveVal(NULL)
  file_cluster_params = reactiveVal(NULL)
  ##
  shapefile <- reactiveVal(NULL)
  
  sf = reactiveVal()
  par_fiti <- reactiveVal(NULL)#handling pareto_fitness
  sq_file <- reactiveVal(NULL)#handling sq_fitness
  fit <- reactiveVal(NULL) #absolute value dataframe
  f_scaled <- reactiveVal(NULL) #scaled value dataframe
  rng_plt <- reactiveVal(NULL) #getting the highest range across dataframe
  rng_plt_axes <- reactiveVal(NULL) #getting matching axis labels for highest range
  pca_remove <- reactiveVal(NULL) #variables removed from pca
  
  pca_rv = reactiveValues(input_file = NULL, #all variables for cluster function
                          min_components = NULL, #pca min tested
                          max_components = NULL, #pca max tested
                          var_1 = NULL, var_2= NULL, var_3= NULL, var_4= NULL,
                          num_variables_to_plot= NULL,
                          var_1_label= NULL, var_2_label= NULL, var_3_label= NULL, var_4_label= NULL,
                          handle_outliers_boolean = F,
                          deviations_min = NULL,
                          deviations_step = 0.2,
                          deviations_max = NULL,
                          count_min = NULL,
                          count_max = NULL,
                          outlier_to_cluster_ratio = NULL,
                          fixed_cluster_boolean = T,
                          fixed_clusters = 15,
                          
                          min_clusters = NULL,
                          max_clusters = NULL)
  stq <- reactiveVal(NULL) #status quo
  
  #data prep
  run_prep_possible = reactiveValues(files_avail = FALSE) #allow prep script to run when all required files available
  script_output <- reactiveVal("") # data prep R output
  cscript_output <- reactiveVal("") # cluster prep R output
  
  dp_done = reactiveVal(FALSE) # checking if data prep R output is done
  cvp_done = reactiveVal(FALSE) # checking if cluster var prep is done
  sel_tay = reactiveVal(NULL)
  
  all_choices = reactiveVal()
  ahp_choices = reactiveVal()
  isElementVisible = reactiveVal(FALSE)
  clickpoint_button <- reactiveVal(FALSE) #control click point button visibility
  
  #play around
  rv <-reactiveValues(sizes= NULL,colls = NULL) #color for parallel axes
  er <- reactiveVal(NULL) #position
  best_option = reactiveVal(NULL)
  bo_pass = reactiveVal()
  fit1 = reactiveVal()

  whole_ahp = reactiveVal(NULL)
  sols_ahp = reactiveVal(NULL)
 
  buffers = reactiveVal(NULL)
  cmf = reactiveVal(NULL)
  slider_mes_ini <- reactiveVal(FALSE)
  opti_mima = reactiveVal(NULL)
  memima_ini = reactiveVal(NULL)
  mes_touched = reactiveVal(FALSE)
  reset_move = reactiveVal(FALSE)
 
  scatter_regr = reactiveVal(NULL)
  clus_path = reactiveVal(NULL)
  #control/limit range of objectives, works in combination with slider input$ran1 etc.
  default_vals = reactiveVal(list(ran1 = c(0,100),
                                  ran2 = c(0,100),
                                  ran3 = c(0,100),
                                  ran4 = c(0,100)))
  map_plotted <- reactiveVal(FALSE)
  mahp_plotted <- reactiveVal(FALSE)#once clicked ahp measure map renders on best_option() changes
  settings_text <- reactiveVal("") #printing pca settings
  update_settings <- function() {
    settings <- pca_settings(input)
    settings_text(settings)
  }
  play_running <- reactiveVal(NULL)#spinner in visualisation/play around tab
  write_corr_rv = reactiveValues(columns = NULL, col_correlation_matrix = NULL, input_file = NULL)
  pca_ini <- read_pca()
  pca_table <- reactiveVal(pca_ini)
  pca_in <- reactiveValues(data = read_pca()) #this only reads config$columns, NULL if opening for the first time
  lalo <- reactiveVal()
  #empty pca table
  output$pca_incl <- renderTable({pca_table()}, rownames = T, colnames = F)
  
  # pca_status <- reactiveVal("")
  pca_spin <- reactiveVal(NULL)#spinner in cluster tab
  max_pca <- reactiveVal()# required for max pc field
  pca_available <- reactiveValues(button1_clicked = FALSE, button2_clicked = FALSE, button3_clicked = FALSE)
  #results table
  check_files<- reactiveVal(NULL)
  df_antab = reactiveVal(NULL)
  sols <- reactiveVal()
  sols2 <- reactiveVal() #for boxplot
  sols3 <- reactiveVal() #for objectives vs. cluster variables
  #figure in analysis rendering
  is_rendering <- reactiveVal(FALSE)
  #catchment shapes
  cm <- reactiveVal()
 
  needs_buffer <- reactiveVal()
  hru_matcher <- reactiveVal() #which optima have which measure on which hru
  hru_ever <- reactiveVal() #long version of hru_matcher id-measure-optims
  mt <- reactiveVal()
  aep_100 <- reactiveVal() #hru-nswrm-distinct aep name
  aep_100_con <- reactiveVal()
  msrs = reactiveVal(NULL)
  sel_opti <- reactiveVal()
  fan_tab <- reactiveVal() #matching nswrm order
  
  prio <- reactiveVal(NULL)
  #ahp
  previous_vals = reactiveValues(
    x_var = NULL,
    y_var = NULL,
    color_var = NULL,
    size_var = NULL
  )
  ahp_combo = reactiveVal(character(0))
  sids =   reactiveVal(c("c1_c2", "c1_c3", "c1_c4","c2_c3","c2_c4", "c3_c4"))
  slider_ahp <- reactiveValues(c1_c2 = "Equal",
                               c1_c3 = "Equal",
                               c1_c4 = "Equal",
                               c2_c3= "Equal",
                               c2_c4= "Equal",
                               c3_c4= "Equal")
  
  coma = reactiveVal()
  range_controlled = reactiveVal(NULL)
  calculate_weights = reactiveVal()
  initial_update_done = reactiveValues(initial = FALSE)
  card_shown <- reactiveValues(ahp_card1 = FALSE, ahp_card2 = FALSE, ahp_card3 = FALSE, ahp_card4 = FALSE, ahp_card5 = FALSE, ahp_card6 = FALSE)
  sliders <- reactiveValues()
  boo <- reactiveVal() #best option optimum
  
  mahp = reactiveVal(NULL) #measure vals
  mahp_touched = reactiveVal(FALSE)
  
  ahpmt = reactiveVal()
  ahpima_ini = reactiveVal(NULL)
  mahp_ini <- reactiveVal(FALSE)
  man_weigh <- reactiveVal(NULL)
  pass_to_manual <- reactiveVal()
  dfx <- reactiveVal() #switch between cluster and whole datasets
  default_running <- reactiveVal(NULL)#spinner in configure tab
  one_on <- reactiveValues(vari="") #to turn off single cards
  meas_running <- reactiveVal(FALSE) #spinner in ahp tab
  fit_row <- reactiveVal()  
  fit_sorted <- reactiveVal()
  
  ### Startup ####
  
  #pull status quo
  observe({
    sq_da()
    if (!is.null(sq_da())) {
      req(objectives())
      st_q = read.table('../data/sq_fitness.txt', header = FALSE, stringsAsFactors = FALSE, sep = deli('../data/sq_fitness.txt'))
      names(st_q) = objectives()
      stq(st_q)
    }})
  
  
  ## ggplot melt and change plotting order
  pp <- reactive({
    req(f_scaled(),objectives())
    f_scaled()%>% pivot_longer(.,cols=-id)%>%mutate(name=factor(name,levels=objectives()),id=as.factor(id))
  })

  observe({
    req(pp())
    req(length(unique(pp()$id))>0)
    rv$sizes= rep(0.5, length(unique(pp()$id)))
    rv$colls = rep("grey50", length(unique(pp()$id)))
  })
  

  ### Data Prep ####
  
  # limit input size of objective names
  shinyjs::runjs("$('#short1').attr('maxlength', 19)")
  shinyjs::runjs("$('#short2').attr('maxlength', 19)")
  shinyjs::runjs("$('#short3').attr('maxlength', 19)")
  shinyjs::runjs("$('#short4').attr('maxlength', 19)")
  
 
  observeEvent(input$tabs == "data_prep",{
   

    #buffer selection
    observe({
      
        req(msrs())
        buffs = msrs()
        updateSelectInput(session,inputId = "buffies", choices = buffs)
        
        if(file.exists("../input/buffers.RDS")){
          sel_buff = readRDS("../input/buffers.RDS")
          updateSelectInput(session,inputId = "buffies", choices = buffs,selected=sel_buff)
          
        }
      
      
    })
    
    
    observe({if(file.exists("../input/object_names.RDS")){
      
      short = readRDS("../input/object_names.RDS")
      objectives(short)
      
      updateTextInput(session,"short1", value = objectives()[1] )
      updateTextInput(session,"short2", value = objectives()[2] )
      updateTextInput(session,"short3", value = objectives()[3] )
      updateTextInput(session,"short4", value = objectives()[4] )
      
      shinyjs::disable("short1")
      shinyjs::disable("short2")
      shinyjs::disable("short3")
      shinyjs::disable("short4")
    }})
    
    
    observe({
      req(input$short1, input$short2, input$short3, input$short4,rng_plt())
      short <<- c(input$short1, input$short2, input$short3, input$short4)
      objectives(short)
      
      updateSelectInput(session, "x_var3",   choices = short, selected = rng_plt()[1])
      updateSelectInput(session, "y_var3",   choices = short, selected = rng_plt()[2])
      updateSelectInput(session, "col_var3", choices = short, selected = rng_plt()[3])
      updateSelectInput(session, "size_var3",choices = short, selected = rng_plt()[4])
      
      num_criteria = length(objectives())
      k=1
      for (i in 1:(num_criteria - 1)) {
        for (j in (i + 1):num_criteria) {
          new_label <- paste0(objectives()[j]," vs. ",objectives()[i])
          updateActionButton(session, paste0("ahp_card",k), label = new_label)
          k = k+1}}
      
    })
    
    
  }) ###end of things that happen when data prep tab is opened
  
    
  observe({
    hru_da()
    shp_da()
    if(!is.null(shp_da()) && !is.null(hru_da())){
      output$map_plot_da = renderUI({HTML(
        paste0('<p style="color: blue;"> You can now use the decision space/map plotting of the tool.</p>'))})
      
    }else{output$map_plot_da = renderUI({HTML(
      ""
    )})}
  })
  
  ## make fit() based on user input
    observeEvent(input$par_fit, {     
      req(input$par_fit)
      file <- input$par_fit
      if (is.null(file)) {return(NULL)}
      par_fiti(list(path = file$datapath, name = file$name))
      save_path_par_fiti <- file.path(save_dir, "pareto_fitness.txt")
      file.copy(par_fiti()$path, save_path_par_fiti, overwrite = TRUE) #copy pareto_fitness.txt
      pareto_da(1)
      })
    
  
  #make status quo based on user input
    observeEvent(input$sq_in, {
      req(input$sq_in)
      file <- input$sq_in
      if(is.null(file)){return(NULL)}
      sq_file(list(path=file$datapath, name = file$name))#name here superfluous
      save_path_sq <- file.path(save_dir, "sq_fitness.txt")
      file.copy(sq_file()$path,save_path_sq,overwrite = TRUE) #copy sq_fitness.txt
      sq_da(1)
    })
    
  
    ##get new objective names, make fit() and objectives() and f_scaled
    observeEvent(input$save_par_fiti, {
      
      short_temp <- c(trimws(input$short1), trimws(input$short2), 
                      trimws(input$short3), trimws(input$short4))
      
      # Check for empty values
      if(any(short_temp == "" | is.na(short_temp))) {
        showNotification("Please provide all names or '-' ", type = "error", duration = 5)
        return()  #don't do the rest
      }
      
      if(length(unique(short_temp)) < length(short_temp)){
        showNotification("Objective names have to be unique! Please change.", type = "error", duration = 5)
        return() #don't do the rest
      }
      #only if above passes
      short <<- short_temp
      
      objectives(short)
      saveRDS(short, file = "../input/object_names.RDS")
      
      updateTextInput(session,"short1", value = objectives()[1] )
      updateTextInput(session,"short2", value = objectives()[2] )
      updateTextInput(session,"short3", value = objectives()[3] )
      updateTextInput(session,"short4", value = objectives()[4] )
      
      shinyjs::disable("short1")
      shinyjs::disable("short2")
      shinyjs::disable("short3")
      shinyjs::disable("short4")
      
      
      updateTextInput(session,"col1", value = objectives()[1] )
      updateTextInput(session,"col2", value = objectives()[2] )
      updateTextInput(session,"col3", value = objectives()[3] )
      updateTextInput(session,"col4", value = objectives()[4] )
      
      write_labels(pca_rv = pca_rv,var1=input$short1,var2=input$short2,var3=input$short3,var4=input$short4,
                   var1_lab="",var2_lab="",var3_lab="",var4_lab="")#save label for use in same session, not stable as previously
     
      if(file.exists(pareto_path)){
        
      data = read.table(pareto_path, header=F,stringsAsFactors=FALSE,sep = deli(pareto_path))
      names(data) = objectives()
      fit(data)
      fit1(fit() %>% rownames_to_column("optimum"))
      yo = fit() %>% mutate(across(everything(), ~ scales::rescale(.)))%>%mutate(id = row_number())
      f_scaled(yo)}
      pareto_da(1) #trigger for pareto front availability, used in other tabs to show/hide content
      
      
      output$obj_conf <- renderTable({
        if(file.exists(pareto_path)){
        rng = get_obj_range(colnames = short)
        bng = rng
        
        for(i in 1:4){
          for(j in 2:3){
            bng[i,j] = formatC(rng[i,j],digits= unlist(lapply(rng[,2],num.decimals))[i],drop0trailing = T,format="f")#same decimal for min and max
          }
        }
        bng
        }else{data.frame(objective = character(0),
                                     min = numeric(0),
                                     max = logical(0),
                                     stringsAsFactors = FALSE)}  
      },rownames = T)
      
      
    })
    
  
    
    ## get unit input 
    observeEvent(input$save_unit,{
      axiselected(c(input$unit1,input$unit2,input$unit3, input$unit4))
      saveRDS(axiselected(), file="../input/units.RDS")
      updateTextInput(session, "axisx",  value  = axiselected()[1])
      updateTextInput(session, "axisy", value = axiselected()[2])
      updateTextInput(session, "colour", value = axiselected()[3])
      updateTextInput(session, "size", value = axiselected()[4])
      
      updateTextInput(session, "unit1",  value  = axiselected()[1])
      updateTextInput(session, "unit2", value = axiselected()[2])
      updateTextInput(session, "unit3", value = axiselected()[3])
      updateTextInput(session, "unit4", value = axiselected()[4])
      
      write_uns_converted(rv = pca_rv, var1_lab= input$unit1, var2_lab = input$unit2, var3_lab = input$unit3, var4_lab = input$unit4)
      
      
    })
    
    
    
    
    output$obj_conf <- renderTable({
      req(fit(),objectives()) #fit() is proxy for file connection
      rng = get_obj_range(colnames = objectives())
      bng = rng
      
      for(i in 1:4){
        for(j in 2:3){
          bng[i,j] = formatC(rng[i,j],digits= unlist(lapply(rng[,2],num.decimals))[i],drop0trailing = T,format="f")#same decimal for min and max
        }
      }
      bng
      
    } ,rownames = T) 
   
    
    # text if visualisation would work
    observe({
      sq_da()
      if (file.exists("../input/object_names.RDS") && file.exists(pareto_path) && file.exists("../data/sq_fitness.txt")) {
        output$can_visualise = renderText({
          "At this point you can use the Visualisation and AHP tab. If you haven't done so already, you can supply the genome, shapefile and/or cluster information below."
        })
        
      } else if (file.exists("../input/object_names.RDS") &&
                 file.exists(pareto_path) &&
                 !file.exists("../data/sq_fitness.txt")) {
        output$can_visualise = renderText({ "At this point you can use the Visualisation and AHP tab.
          If you'd like to also plot the status quo, please provide it above.
          If you haven't done so already, you can supply the genome, shapefile and/or cluster information below."})
      } 
    })
    
    ## Automated SWAT+/CoMOLA workflow
    ##measure_location - copied straight away
    observeEvent(input$file3, { file <- input$file3
    if (is.null(file)) {return(NULL)}
    file_data3(list(path = file$datapath, name = file$name))
    save_filename3 <- file_data3()$name
    save_path3 <- file.path(save_dir, save_filename3)
    file.copy(file_data3()$path, save_path3, overwrite = TRUE)
    })
    
    ##rout_unit - copied straight away
    observeEvent(input$file6, { file <- input$file6
    if (is.null(file)) {return(NULL)}
    file_data6(list(path = file$datapath, name = file$name))
    save_filename6 <- file_data6()$name
    save_path6 <- file.path(save_dir, save_filename6)#rout_unit.con
    file.copy(file_data6()$path, save_path6, overwrite = TRUE)
    })
     
    ##hru.con
    observeEvent(input$hrucon, { file <- input$hrucon
    if (is.null(file)) {return(NULL)}
    file_hrucon(list(path = file$datapath, name = file$name))
    save_hruname <- file_hrucon()$name
    save_pathhrucon <- file.path(save_dir, save_hruname)
    file.copy(file_hrucon()$path, save_pathhrucon, overwrite = TRUE)})

    #SWAT+/CoMOLA
    
    observeEvent(input$files_avail, ignoreInit = TRUE,{
      required_files <- c("../data/pareto_genomes.txt","../data/hru.con",
                          "../data/measure_location.csv",
                          "../data/hru.shp","../data/hru.shx", "../data/hru.dbf", "../data/hru.prj",
                          "../data/rout_unit.con",
                          "../data/pareto_fitness.txt")

      checkFiles <- sapply(required_files, function(file) file.exists(file))

      if (all(checkFiles)) {
        if(file.exists("../input/object_names.RDS")){
          run_prep_possible$files_avail = T
          output$fileStatusMessage <- renderText({HTML("All Files found.")})
        }else{
          output$fileStatusMessage <- renderText({HTML(
            "All files found. <br>Please provide the names of the objectives represented in the Pareto front.
             The names and the order in which they are given have
             to align with what is provided in the first four columns of pareto_fitness.txt"
          )
          })
        }
        }else{
        missing_files = required_files[!checkFiles]
        output$fileStatusMessage <- renderText({HTML(paste("The following file(s) are missing:<br/>",
            paste(sub('../data/', '', missing_files), collapse = "<br/> ")
          ))
        })
        }

})


    
    #cannot run if already exists, has to be deleted manually
    
    observe({
        if (!file.exists("../input/var_corr_par.csv")) {
      shinyjs::show(id = "runprep_show")
      } 
      
    })
    observe({if (run_prep_possible$files_avail) {  shinyjs::enable("runprep")} else{  shinyjs::disable("runprep")
    } })
  
    
    #### File Upload - Decision Space ####
    observe({
      pg_da()
      shinyjs::toggleState("measure_loc", condition = file.exists("../data/pareto_genomes.txt"))
    })
    
   
    
    observeEvent(input$measure_loc, { 
      
    file <- input$measure_loc
    if (is.null(file))return(file_lookup_loc(NULL))#lookup table
    file_lookup_loc(list(path = file$datapath, name = file$name))

    ##create hru_in_optima.RDS
    #not copied, just used
    acti = read.table("../data/pareto_genomes.txt",sep = deli("../data/pareto_genomes.txt"))
    # lookup table
    lookup_ext = sub(".*\\.", "", file_lookup_loc()$name)
    save_path_lookup_loc <- file.path(save_dir, paste0("lookup_table.", lookup_ext))
    file.copy(file_lookup_loc()$path, save_path_lookup_loc, overwrite = TRUE)
    
    if (lookup_ext == "csv") {
      lookup_table = read.csv(save_path_lookup_loc, stringsAsFactors = FALSE, header = F)%>%
        rename(code = V1, measure = V2)
    } else if (lookup_ext == "txt") {
      lookup_table = read.table(save_path_lookup_loc, stringsAsFactors = FALSE) %>%
        select(V1, V3) %>%
        rename(code = V1, measure = V3)
    }
    
    code_happening = sort(unique(unlist(acti)))
    not_ac = setdiff(lookup_table$code, code_happening) #not used but in lookup (ignored if alone)
    ac_nm = setdiff(code_happening, lookup_table$code) #activated but not in lookup (worse)
    
    if(length(ac_nm)>0){
      if(length(not_ac) == 0){
      output$genome_lu_fu <- renderUI({HTML(paste0('<p style="color: red;">
                                                    Please check your lookup table!<br/>
                                                    The meaning of <b>', ac_nm, '</b> has not been provided.</p>'))})
      }else{
      output$genome_lu_fu <- renderUI({HTML(paste0('<p style="color: red;">
                                                    Please check your lookup table!<br/>
                                                    The meaning of the code <b>', ac_nm, '</b> has not been provided. You can also delete the line for <b>',
                                                    lu[which(lu$code == not_ac),]$measure, '</b> as it is never activated in your genome     </p>'))})                                             
      }
      
      file.remove(save_path_lookup_loc)  #clean up bad file
      file_lookup_loc(NULL) 
      
      return()
    }
    output$genome_lu_fu = renderUI({HTML('<p style="color: blue;"> All codes used in the genome are provided in the lookup table. </p>')})
    
    genome_filled <- acti %>%
      mutate(across(starts_with("V"), 
                    ~ lookup_table$measure[match(., lookup_table$code)])) %>%
      mutate(id = row_number())
    
    saveRDS(genome_filled, file = "../input/hru_in_optima.RDS")
    hru_da(1)
    #remove lookup table
    file.remove(save_path_lookup_loc); rm(genome_filled)

    }, ignoreInit = TRUE)
    
    
    observeEvent(input$hru_activ, {
      file <- input$hru_activ
      if (is.null(file))return(file_hru_activ(NULL))
      
      file_hru_activ(list(path = file$datapath, name = file$name))
      
      save_path_hru_activ <- file.path(save_dir, "pareto_genomes.txt")
      file.copy(file_hru_activ()$path, save_path_hru_activ, overwrite = TRUE)
      # shinyjs::refresh()
      
      # check if align
      if (!is.null(fit())) {
        gen = read.table(save_path_hru_activ,sep = deli(save_path_hru_activ))
        if (ncol(gen) != nrow(fit())) {
          if(nrow(gen) == nrow(fit())){#catch when the genome is reversed
            gen = as.data.frame(t(gen))
            write.table(gen,save_path_hru_activ, row.names = FALSE, col.names = F)}else{
          
          output$gen_fit_fu <- renderUI({
            HTML('<p style="color: red;">
               Please upload again!<br/>
              The genome does not match the Pareto fitness file you supplied, the columns in the genome have to align with the rows in the fitness file.
          <br/> </p>')})
          file.remove(save_path_hru_activ)  #clean up bad file
          file_hru_activ(NULL) 
          return()
        }}
      }
      output$gen_fit_fu = renderUI({HTML('<p style="color: blue;"> The genome matches the Pareto fitness file you supplied. </p>')})
      
      pg_da(1)
    }, ignoreInit = TRUE)
  
    #### File upload - Clustering (standard workflow) #####
    observeEvent(input$cluster_params, { file <- input$cluster_params
    if (is.null(file)) {return(NULL)}
    file_cluster_params(list(path = file$datapath, name = file$name))
    path_cluster_params = file.path(input_dir, "cluster_params.csv")
    file.copy(file_cluster_params()$path, path_cluster_params, overwrite = T)
    clusp_da(1)
    # shinyjs::refresh()
    }, ignoreInit = TRUE)
    
    #### File Upload - Mapping ####
    observeEvent(input$shapefile_cd, { file <- input$shapefile_cd
    if (is.null(file)) {return(NULL)}
    file_shapefile_cd(list(path = file$datapath, name = file$name))
    shp_req = c(".shp",".shx", ".dbf", ".prj")
    shapefile <- input$shapefile_cd
    shapefile_names <- shapefile$name
    shapefile_paths <- shapefile$datapath
    missing_shapefile_components <- shp_req[!sapply(shp_req, function(ext) any(grepl(paste0(ext, "$"), shapefile_names)))]
    
    # copy shapefile components if none are missing
    if (length(missing_shapefile_components) == 0) {
      lapply(seq_along(shapefile_paths), function(i) {
        
        which_comp = sub(".*\\.","", shapefile_names[i])
        hru_named = paste0("hru.",which_comp)
        
        save_path <- file.path(save_dir,hru_named)
        if (!file.exists(save_path)) {
          file.copy(shapefile_paths[i], save_path, overwrite = TRUE)
        }
      })
    }
    
    #write hru_con from shapefile
    shp <- st_read(file.path(save_dir, "hru.shp"), quiet = TRUE) %>% st_transform(crs = 4326)
    
    bbox <- st_bbox(shp)
    mean_lon <- mean(c(bbox["xmin"], bbox["xmax"]))
    mean_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
    latlon = c(mean_lat, mean_lon)
    write.table(latlon,file = "../input/hru.con", row.names = FALSE, col.names = F)
    
    shp_da(1)
    # shinyjs::refresh()
    }, ignoreInit = TRUE)
    
    #### Automated workflow Cluster Var Prep ####
    cvp <- NULL 
    coutput_handled <- reactiveVal(FALSE)
    
    observeEvent(input$runaclust, {
      
      aclu_files <- c(
        "Pareto Genome"        = "../data/pareto_genomes.txt",
        "Blubb"         = "../input/hru_in_optima.RDS",
        "HRU shapefile (shp)"         = "../data/hru.shp",
        "HRU shapefile (shx)"   = "../data/hru.shx",
        "HRU shapefile (dbf)"   = "../data/hru.dbf",
        "HRU shapefile (prj)"   = "../data/hru.prj",
        "Pareto fitness"        = "../data/pareto_fitness.txt",
        "Object names"          = "../input/object_names.RDS"
      )

       checkFiles2 <- sapply(aclu_files, function(file) file.exists(file))

       missing_files <- names(checkFiles2)[!checkFiles2]
       hru_present   <- isTRUE(checkFiles2["HRU shapefile (dbf)"])
       
       text_added <- ""
       
       # Only read DBF once, and only if needed
       if (hru_present) {
         con <- foreign::read.dbf("../data/hru.dbf")
         
         if (!all(c("area", "id") %in% names(con))) {
           text_added <- " please reupload a shapefile with an area and an id column matching it to individual decision space elements."
         }
       }
       
       # remove hru_in_optima from missing list = internal check 
       missing_no_blubb <- setdiff(missing_files, "Blubb")
       
       output$automated_clustering <- renderText({
         
         #case 1 - files missing
         if (length(missing_files) > 0) {
           
           # only hru_in_optima missing
           if (identical(missing_files, "Blubb")) {
             return(HTML("Please upload the genome and the lookup table again!"))
           }
           
           #one missing (not hru_in_optima)
           if (length(missing_no_blubb) == 1) {
             return(HTML(paste("Only one file missing:", missing_no_blubb)))
           }
           
           #multiple missing
           return(HTML(paste(
             "Currently missing:<br/>",
             paste(missing_no_blubb, collapse = "<br/> "),
             text_added
           )))
         }
         
         #case 2 - multiple missing
         if (nzchar(text_added)) {
           return(HTML(paste("All files found but", text_added)))
         }
         
         shinyjs::enable("runaclust2")
         
         HTML("All files found.")
         
       })
    })
       
    observeEvent(input$runaclust2,{
      
    
      cvp_done(FALSE)
      coutput_handled(FALSE)
      cscript_output(character())
      
      # Start the process
      cvp <<- process$new(
        "Rscript",
        c("convert_optain.R"),
        stdout = "|",
        stderr = "|",
        env = c(current = Sys.getenv(), MY_MODE = "fast")#communicate w/ covert_optain.R
      )
      
    })
    
    autoInvalidate <- reactiveTimer(50)
    
    observe({
      autoInvalidate()
      
      # check if the process is running
      if (!is.null(cvp) && cvp$is_alive()) {
        new_output <- isolate(cvp$read_output_lines())
        
        if (length(new_output) > 0) {
          current_output <- cscript_output()
          updated_output <- unique(c(current_output, new_output))
          
          if (length(updated_output) > 10) {
            updated_output <- tail(updated_output, 10)
          }
          
          cscript_output(updated_output)
          coutput_handled(TRUE) 
        }
        
      } else if (!is.null(cvp)) {
        final_output <- cvp$read_output_lines()
        
        if (length(final_output) > 0 ) {
          current_output <- cscript_output()
          updated_output <- c(current_output, final_output)
          
          if (length(updated_output) > 10) {
            updated_output <- tail(updated_output, 10)
          }
          
          cscript_output(updated_output)
        }
        
        cvp_done(TRUE)

        cvp <<- NULL # clear
        coutput_handled(TRUE) 
      }
    })
    
    observe({
      cvp_done()

      if(isTRUE(cvp_done())){
        
        if(file.exists("../output/error_convert.txt")){
          error_msg = readLines("../output/error_convert.txt")
          output$what_clp = renderUI({HTML(error_msg)})
            
            rm("../output/error_convert.txt")
            
          
        }else if (file.exists("../input/cluster_params.csv")){
          clusp_da(1)
          
        cl_mes = readLines("../output/meas_fast.txt")
        output$what_clp = renderUI({(
          HTML(paste0("The cluster variable preparation was successful.
                      In all optima, it calculated the share of area (share_con) used for the following variables: ","<br/>", paste(cl_mes, collapse = "<br/>"))))
        })
        }
        
       
      }
    })
    
    
    # Render UI output
    output$aclustout <- renderUI({
      if (!cvp_done() && !file.exists("../input/cluster_params.csv")) {
      verbatimTextOutput("cluster_automated")}
      
    })
    
    # Render process output
    output$cluster_automated <- renderText({
      paste(cscript_output(), collapse = "\n")
    })
    
    #### Automated workflow Data Prep ####
    
    optain <- NULL 
    output_handled <- reactiveVal(FALSE)
    
    observeEvent(input$runprep, {
      dp_done(FALSE)
      script_output(character()) # Clear old output
      
      # Start the process
      optain <<- process$new(
        "Rscript",
        c("convert_optain.R"),
        stdout = "|",
        stderr = NULL
      )
    })
    
    autoInvalidate <- reactiveTimer(100)
    
    observe({
      autoInvalidate()
      
      # Check if the process is running
      if (!is.null(optain) && optain$is_alive()) {
        new_output <- isolate(optain$read_output_lines())
        
        if (length(new_output) > 0) {
          current_output <- script_output()
          updated_output <- unique(c(current_output, new_output))
          
          if (length(updated_output) > 10) {
            updated_output <- tail(updated_output, 10)
          }
          
          script_output(updated_output)
          output_handled(TRUE) 
        }
        
      } else if (!is.null(optain)) {
        final_output <- optain$read_output_lines()
        
        if (length(final_output) > 0 && !output_handled()) {
          current_output <- script_output()
          updated_output <- c(current_output, final_output)
          
          if (length(updated_output) > 10) {
            updated_output <- tail(updated_output, 10)
          }
          
          script_output(updated_output)
        }
        
        dp_done(TRUE)
        hru_da(if(file.exists("../input/hru_in_optima.RDS")) 1 else NULL) 
        clusp_da(if(file.exists("../input/var_corr_par.RDS")) 1 else NULL)
        
        optain <<- NULL # clear
        output_handled(TRUE) 
      }
    })
    
    
    # Render UI output
    output$scriptdp <- renderUI({
      if (dp_done() && file.exists("../input/var_corr_par.csv")) {
        tags$strong("The data preparation was successful. You can now continue with the Correlation and Principal Component Analysis. You will not need this tab again.")
      } else {
        verbatimTextOutput("rscriptcmd")
      }
    })
    
    # Render process output
    output$rscriptcmd <- renderText({
      paste(script_output(), collapse = "\n")
    })
    

    #### Hard Reset ####
    observe({ 
      pareto_da()
      pg_da()
      if(length(list.files(c(save_dir,output_dir), full.names = TRUE))==0){ #do not show reset option if there haven't been files uploaded
        shinyjs::hide(id="reset")
      }else{
        shinyjs::show(id="reset")
        output$reset_prompt <- renderText({
          HTML(paste("<p style='color: red;'> If you would like to restart the app if it crashes or behaves inconsistently, you can hard reset it here. Clicking this button
                   deletes all files you provided. The contents of the Output folder are also deleted, please move or copy those files you would like to keep. For all changes to take effect please restart the app after each Hard Reset. Please proceed with caution!</p>"))
        })
        
       
        
        observeEvent(input$reset_btn, {
          pareto_da(NULL)
          
          updateTextInput(session,"short1", value = "" )
          updateTextInput(session,"short2", value = "" )
          updateTextInput(session,"short3", value = "" )
          updateTextInput(session,"short4", value = "" )
          
          #enable objective names again
          shinyjs::enable("short1")
          shinyjs::enable("short2")
          shinyjs::enable("short3")
          shinyjs::enable("short4")
          
          updateTextInput(session,"unit1", value = "")
          updateTextInput(session,"unit2", value = "")
          updateTextInput(session,"unit3", value = "")
          updateTextInput(session,"unit4", value = "")
          
          if (dir.exists(save_dir) && dir.exists(input_dir)) {
            
            
            
            files1 <- list.files(save_dir, full.names = TRUE)
            files2 <- list.files(input_dir, full.names = TRUE)
            files3 <- list.files(output_dir, full.names = TRUE)
            files4 <- list.files(pattern = "\\.html$")
            files5 <- list.files(pattern = "\\.Rhistory$")
            
            sapply(files1, file.remove)
            sapply(files2, file.remove)
            sapply(files3, file.remove)
            sapply(files4, file.remove)
            sapply(files5, file.remove)
            
            remaining_files <- unlist(lapply(c(save_dir,input_dir,output_dir), function(dir) {
              list.files(path = dir, full.names = TRUE)
            }))
            
            if (length(remaining_files) == 0) {
              status <- "All files have been deleted."
              
            } else {
              status <- "Some files could not be deleted."
            }
          } else {
            status <- "Directory does not exist."
          }
          
          # Update the status text output
          output$reset_status <- renderText(status)
          
          shinyjs::refresh()
        })
        
      } })
    #### Buffer selection ####

  
    observeEvent(input$save_buff,{
      saveRDS(input$buffies,file = "../input/buffers.RDS")
    })  
    
 
    ### Play Around Tab ####
  if (!file.exists("../data/sq_fitness.txt")){
    shinyjs::disable("plt_sq")
    shinyjs::hide("status_quo_title")
  }else{
    shinyjs::show("status_quo_title") 
    shinyjs::enable("plt_sq")}
  
  #remove tab content if fitness not available
  observe({
    pareto_da()
    # if (!file.exists(pareto_path)) {
    if(is.null(pareto_da())){
      output$config_needs_var = renderText({"Please provide pareto_fitness.txt, the objective names and a .csv with variables to cluster on in the Data Preparation tab before proceeding here! If you're using the SWAT+/CoMOLA workflow, please click Run Prep in the Data Preparation tab."})
      output$uploaded_pareto <- renderText({"To be able to proceed, please provide pareto_fitness.txt as well as the objective names in the previous tab."})
      shinyjs::hide("main_analysis")
      shinyjs::hide("all_ahp")
      shinyjs::hide("ahp_analysis")
      shinyjs::hide("config_all")
      # shinyjs::hide("play_sidebar")
      shinyjs::hide("tab_play1")
      shinyjs::hide("tab_play2")
      shinyjs::hide("play_sidebar")
      shinyjs::hide("plt_opti")
      output$analysis_needs_var = renderText({"The correlation analysis and the clustering have to run first before their results can be analysed."})
      
    }else{
      
      #show stuff in vis and ahp tab
      shinyjs::show("play_sidebar")
      shinyjs::show("all_ahp")
      shinyjs::show("ahp_analysis")
      shinyjs::show("random_ahp2")
      shinyjs::show("show_status_quo")
      shinyjs::show("unit_add3")
                    
      output$uploaded_pareto <- NULL
     
      ## make or pull fit()
      
      req(objectives())
      
      data <- read.table(pareto_path, header = FALSE, stringsAsFactors = FALSE,sep = deli(pareto_path))
      new_col_data <- objectives()
      colnames(data) = new_col_data
      fit(data)
      fit1(fit() %>% rownames_to_column("optimum"))
      yo = fit() %>% mutate(across(everything(), ~ scales::rescale(.)))%>%mutate(id = row_number())
      f_scaled(yo)
      
      yo2 <- pull_high_range(fit())
      rng_plt(yo2)
      
      yo2 <- pull_high_range(fit(),num_order=T)
      rng_plt_axes(yo2)
      
      # output$uploaded_pareto <- renderText({"All Files found.
      #                                        You can now examine the Pareto front.
      #                                        How does it change when the objective ranges are modified?"})
      
      
      ## adapt sliders in ahp and configure tab
      if(!(initial_update_done$initial)){ #making sure this only runs once
        min_max <-data.frame(t(sapply(data, function(x) range(x, na.rm = TRUE))))
        names(min_max) =c("min","max")
        range_value = NULL
        
        new_defaults <- default_vals()
        
        for (i in 1:4) {
          var_name <- paste0("steps", i)
          
          #step_val also fails on small distances
          if (pmin(abs(min_max$min[i]),abs(min_max$max[i])) <= 1 || abs(min_max$max[i]-min_max$min[i]) <= 1) {              
            
            min_max$max[i] = min_max$max[i] * 1000
            min_max$min[i] = min_max$min[i] * 1000
            
            range_value = append(range_value,(rownames(min_max[i, ])))
            
          }
          
          if (abs(min_max$min[i]) > 100) {
            min_max$max[i] = ceiling(min_max$max[i])
            min_max$min[i] = floor(min_max$min[i])
          }
          
          step_val = 1 #otherwise keyboard arrows do not work properly
          # ranger not particularly flexible, another option instead of /1000:
          
          # range_abs =  abs(min_max$max[i]- min_max$min[i])
          #   
          # if(range_abs > 10){
          #   step_val = 1 #
          # }else{
          #   step_val = cal_step(ra = range_abs, n = 100)
          #   }
          
          range_controlled(range_value)
          
          updateSliderInput(session, paste0("obj",i,"_ahp"), value = c(min_max$min[i],min_max$max[i]),min =min_max$min[i],max = min_max$max[i],step=step_val)
          updateSliderInput(session, paste0("ran",i), value = c(min_max$min[i],min_max$max[i]),min =min_max$min[i],max = min_max$max[i],step=step_val)
          
          
          new_defaults[[paste0("ran",i)]] <- c(min_max$min[i], min_max$max[i]) 
          
        }
        default_vals(new_defaults)
        initial_update_done$initial = TRUE
      }
      
    }
  })
  ##check if names of objectives have to be supplied or already present
  observeEvent(input$tabs=="play_around",{ 
    ## make or pull objectives()
    map_plotted(FALSE)
    
  
    if(!file.exists("../input/object_names.RDS")) {
      
      shinyjs::hide(id = "tab_play1")#do not show visualisation tab content 
      shinyjs::hide(id = "tab_play2")
      
      
      }else {
       
        short = readRDS("../input/object_names.RDS")
        objectives(short)
        
        updateSelectInput(session, "x_var3",   choices = short, selected = rng_plt()[1])
        updateSelectInput(session, "y_var3",   choices = short, selected = rng_plt()[2])
        updateSelectInput(session, "col_var3", choices = short, selected = rng_plt()[3])
        updateSelectInput(session, "size_var3",choices = short, selected = rng_plt()[4])
        
        num_criteria = length(objectives())
        k=1
        ahp_combo(character(0))
        for (i in 1:(num_criteria - 1)) {
          for (j in (i + 1):num_criteria) {
            new_label <- paste0(objectives()[j]," vs. ",objectives()[i])
            updateActionButton(session, paste0("ahp_card",k), label = new_label)
            ahp_combo(c(ahp_combo(), new_label))  #identifier for ahp scatter plots
            k = k+1}}
        
      }
    
 
    
    ## update slider labels based on objectives
    observe({
      req(objectives())
      obj <- objectives()
      for(i in 1:4){
        updateSliderInput(session, paste0("obj",i), label = obj[i])
        updateSliderInput(session, paste0("obj",i,"_ahp"), label = obj[i])
        updateSliderInput(session, paste0("ran",i), label = obj[i])
      }
      updateCheckboxGroupInput(session, "sel_neg", choices = objectives(), selected = NULL)
      
    })
    
    observe({ #create mt() for measure sliders
      req(aep_100(), hru_ever())
      
      fk = aep_100() %>% inner_join(hru_ever(),by = c("hru" = "id", "nswrm" = "measure"))%>% select(-hru)
    
      mt(fk %>%
           group_by(nswrm, optims) %>%
           summarize(distinct_aep = n_distinct(name), .groups = "drop") %>%
           pivot_wider(names_from = optims, values_from = distinct_aep, values_fill = 0) %>%
           column_to_rownames("nswrm") %>%  
           t() %>%  
           as.data.frame())
      
      })
    
    observe({
      req(mt())
      memima_ini(rbind(
        min = apply(mt(), 2, min, na.rm = TRUE),
        max = apply(mt(), 2, max, na.rm = TRUE)
      ))
      })
    
    #measure slider
    output$mes_sliders <- renderUI({
      req(mt())
      numeric_cols <- sapply(mt(), is.numeric)
      sliders <- lapply(names(mt())[numeric_cols], function(col) {
        sliderInput(inputId = paste0("slider_", col),
                    label = col,
                    min = min(mt()[[col]]),
                    max = max(mt()[[col]]),
                    step = 1,
                    value = c(min(mt()[[col]]), max(mt()[[col]]))
        )
      })
      
      
      slider_mes_ini(TRUE)
      
      sliders
    })
 
    
    observe({
      sq_da()
      if (is.null(sq_da())) {
        shinyjs::disable("add_sq_f")
      } else{
        shinyjs::enable("add_sq_f")
      }
    })

  observe({
    req(fit())
      req(slider_mes_ini(),mt())
    req(length(names(mt())[sapply(mt(), is.numeric)]) > 0)
    
    numeric_cols <- names(mt())[sapply(mt(), is.numeric)]
    values <- setNames(
      lapply(numeric_cols, function(col) input[[paste0("slider_", col)]]),
      numeric_cols
    )
    
    if (any(sapply(values, is.null))) return()

    names(values) = numeric_cols #slider current setting
    
    df_values <- as.data.frame(values)

    df_first_values <- as.data.frame(memima_ini())
    rownames(df_first_values) <- NULL
    
    if (!identical(df_values, df_first_values)) {#otherwise sometimes run too soon
      isolate(mes_touched(TRUE))
      

      ff <- mt() %>%
        rownames_to_column("optimum") %>%
        reduce(numeric_cols, function(df, col) {
          df %>% filter(.data[[col]] >= values[[col]][1] & .data[[col]] <= values[[col]][2])
        }, .init = .)
      
      mt_optis = ff$optimum #optima
      opti_mima(fit1()%>%filter(optimum %in% mt_optis) %>% select(-optimum))
      
    }else{opti_mima(FALSE)} 
   
  })
  

    })#end of play tab observer

  
  observe({
   axiselected()
    
    if(!is.null(axiselected())){
      updateTextInput(session, "unit1",  value  = axiselected()[1])
      updateTextInput(session, "unit2", value = axiselected()[2])
      updateTextInput(session, "unit3", value = axiselected()[3])
      updateTextInput(session, "unit4", value = axiselected()[4])
    }else{
      if(!is.null(objectives())){
        
        write_labels(pca_rv = pca_rv, var1 = objectives()[1], var2 = objectives()[2], 
                     var3 = objectives()[3], var4 = objectives()[4],
                     var1_lab= "", var2_lab = "", var3_lab = "",
                     var4_lab = "")
        
      }else{
        
        write_labels(pca_rv = pca_rv, var1 = "", var2 = "", 
                     var3 = "", var4 = "",
                     var1_lab= "", var2_lab = "", var3_lab = "",
                     var4_lab = "")
      }
    }
  })
  

  # cache slider observations
  # cache slider observations
  input_ranges_valid <- reactive({
    all(
      !is.null(input$obj1), !is.null(input$obj2), 
      !is.null(input$obj3), !is.null(input$obj4),
      length(input$obj1) == 2, length(input$obj2) == 2,
      length(input$obj3) == 2, length(input$obj4) == 2
    )
  })
  
  #extract min/max vals
  input_ranges <- reactive({
    req(input_ranges_valid())
    
    list(
      minvals = c(input$obj1[1], input$obj2[1], input$obj3[1], input$obj4[1]),
      maxvals = c(input$obj1[2], input$obj2[2], input$obj3[2], input$obj4[2])
    )
  })
  
  #filtered_data and scaled_filtered_data(), currently rather verbose setup
  scaled_filtered_data <- reactive({ 
    req(input_ranges_valid())
    req(f_scaled())
    
    ranges <- input_ranges()
    
    match_scaled(minval_s = ranges$minvals, maxval_s = ranges$maxvals,
                 scal_tab=f_scaled(),allobs=objectives(), abs_tab = fit(),  
                 mes_slider = mes_touched(), mes_df = opti_mima())
  })
  
  filtered_data <- reactive({
    req(scaled_filtered_data())
    req(f_scaled())

    ranges <- input_ranges()
 
    scaled_abs_match(minval_s = ranges$minvals,#to be merged with above function, subset with row indices
                     maxval_s = ranges$maxvals,
                     abs_tab = fit(),scal_tab = f_scaled(),
                     allobs = objectives(),smll=F, mes_slider = mes_touched(), 
                     mes_df =opti_mima())
  })  
  
  observe({
    req(scaled_filtered_data(),filtered_data())
    sk = scaled_filtered_data();fd = filtered_data()
    if(nrow(sk) == nrow(fd)){#indication if something's wrong in subset
    output$opt_count <- renderUI({
      tagList("The remaining number of optima is: ",tags$b(nrow(sk)))
      })
    }
    
  })
  
  
  object_names_exists <- reactive({
    file.exists("../input/object_names.RDS")
  }) %>% bindCache("object_names_file_check")
  

  observe({
    req(filtered_data(), object_names_exists())
    
    df <- filtered_data()
    
    is_empty <- nrow(df) == 0
    
    output$ensure_sel <- renderText({
      if (is_empty) {
        "None of the optima fall within the specified ranges. Please select different data ranges!"
      } else {""}
    })
  })
 

   observe({
     req(opti_mima())
     if(nrow(opti_mima())== 0){
       shinyjs::show("mes_empty")
       output$mes_empty =  renderText({paste("None of the optima fall within the specified ranges. Please select different measure ranges!")
    }) }else{shinyjs::hide("mes_empty")}
   })
  
    ## show rest of tab if all required data available
    observe({
      
      test_fit = fit()
      test_objectives = objectives()

    if (!is.null(test_fit) && !is.null(test_objectives)) {
      shinyjs::show("tab_play1")
      shinyjs::show("tab_play2")
      shinyjs::show("scatter")
     }
    })
    ## pareto plot on top
    
    observe({
      if(any(fit()[[input$x_var3]]<=0) && 
         any(fit()[[input$y_var3]]<=0)){shinyjs::show("rev_plot")}else{shinyjs::hide("rev_plot")}
    })
    
  
    first_pareto_fun = function(){
      req(filtered_data(),input$y_var3)
      #match scaled input with unscaled fit() to create dat
      dat=filtered_data()
      if(nrow(dat)==0){return(NULL)}
      if(!is.null(sel_tay()) && nrow(merge(sel_tay(),dat))==0){sel_tay(NULL)} #remove selection when not in sliders
      
      #run plt_sc_optima with sq
      return(plt_sc_optima(dat=dat, x_var = input$x_var3,
                           y_var = input$y_var3,
                           col_var = input$col_var3,
                           size_var = input$size_var3, 
                           full_front = fit(),
                           status_q = input$add_sq_f,an_tab=T, rev = input$rev_box,
                           sel_tab = sel_tay(),unit=input$unit_add1))
      
    }
    
    
    ##TOP Pareto plot
    output$first_pareto <- renderPlot({ 
      axiselected()
      first_pareto_fun() 
      })
  
    observeEvent(input$clickpoint, { #first pareto
      req(filtered_data(), input$x_var3) #non-scaled
      
      clickpoint_button(TRUE)
      dat <- filtered_data()#always filters from the set currently selected
      nearest <- nearPoints(dat, input$clickpoint, xvar = input$x_var3, yvar = input$y_var3, maxpoints = 1)
      if(nrow(nearest) > 0) {
        id <-  as.numeric(rownames(nearest)[1])
        update_selection(id) #pass to line plot
        yo <- dat[id, , drop = FALSE]
        sel_tay(yo) #point in pareto plot and link for changes after
        }
      
    })
  
   
    output$clickpoint_map <- renderUI({
      if(clickpoint_button()){
        if(is.null(shp_da()) || is.null(hru_da())){ 
          return(NULL)
        }else{
          actionButton("map_sel", "Plot decision space of selected optimum")
        } 
      }
    })
    # observe({ if(clickpoint_button()){shinyjs::show("download_play_id")}})

    observeEvent(input$map_sel,{
      req(sel_tay(),objectives())
      shinyjs::show("download_play_id")
      play_running(TRUE) #for spinner
      
      map_plotted(TRUE)
      eve = fit()%>% rownames_to_column("optimum")
      
        # measure plot prep 
          if (file.exists("../input/hru_in_optima.RDS")) {
            
            req(cm())
            cmf(fit_optims(cm=cm(),optims=eve,hru_in_opt_path = "../input/hru_in_optima.RDS"))
           
          }
        
        if(file.exists("../input/hru.con")){lalo(plt_latlon(conpath = "../input/hru.con"))}
         needs_buffer(pull_buffer())
      
         output$plt_play_measure = renderUI({ uiOutput("actual_plt_play_measure")#map
         })#slightly verbose set up, all for square map/css styling to work
         
         output$actual_plt_play_measure <- renderUI({req(map_plotted())
           single_meas_fun2()})
         })
    
    
    single_meas_fun2 = function(fs = T) {#fs - full screen, set to true in app, false in download
      req( cmf(), sel_tay(), fit1(), msrs())
      
      cols = objectives()
      values = sel_tay()
      
      mv <- fit1() %>%  filter(if_all(all_of(cols), ~ . %in% values))%>%slice(1) #slice needed for duplicate optima, should not happen in clean optimisation
      
      hru_one = plt_sel(shp = cmf(), opti_sel = mv$optimum)
      
      col_sel = names(hru_one)[grep("Optim", names(hru_one))]
      man_col = c("#66C2A5" ,"#4db818","#663e13", "#F7A600", "#03597F" ,"#83D0F5","#FFEF2C","#a84632","#b82aa5","#246643")
      man_col = man_col[1:length(msrs())]
      pal = colorFactor(palette = man_col, domain = msrs(), na.color = "lightgrey")
      
      m1 = plt_lf( data = hru_one, dispal = pal,la = lalo()[1],lo =lalo()[2],buff_els = needs_buffer(),
                   col_sel = col_sel, buffers=buffers(), basemap = input$anomap, fullscreen = fs)
      return(m1)
      play_running(FALSE) #for spinner
    }

    output$download_pm <- downloadHandler(

        filename = function(){
          curt = format(Sys.time(), "_%Y%m%d")
          paste0(input$meas_play_savename,curt, ".png")
        },

        content = function(file) {
          shinyjs::show("spinner_download_play")
          measmap <- single_meas_fun2(fs = F)[[1]]
          saveWidget(measmap, "temp.html", selfcontained = FALSE)
          webshot2::webshot("temp.html", file = file, cliprect = "viewport",vwidth = 900,
                           vheight = 900)
          shinyjs::hide("spinner_download_play")
          file.remove("temp.html")
          unlink("temp_files", recursive = TRUE)
          }
    )
    
    
    shp_single_meas = function(shp=T){
      req(cmf(), sel_tay(), fit1())
      
      cols = objectives()
      values = sel_tay()
      mv <- fit1() %>%  filter(if_all(all_of(cols), ~ . %in% values))
      
      # make sf files
      hru_one = plt_sel(shp = cmf(), opti_sel = mv$optimum)
      
      if (shp) {
        data = hru_one %>% subset(!st_is_empty(geometry))
      } else{
        data = names(hru_one)[grep("Optim", names(hru_one))] #col_sel
      }
      return(data)
      
    }
    
    output$download_shp <- downloadHandler(
      
      filename = function(){
        curt = format(Sys.time(), "_%Y%m%d")
        paste0(shp_single_meas(shp=F),curt, ".zip")
      },
      
      content = function(file) {
        shinyjs::show("spinner_download_shp")
        data <- shp_single_meas()
        out_name <- shp_single_meas(shp=F)
        sf::st_write(data,paste0(out_name,".shp"), driver = "ESRI Shapefile")
        zip::zip( paste0(out_name,".zip"), c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                                              paste0(out_name,".dbf"), paste0(out_name,".prj")))
        
        file.rename(paste0(out_name,".zip"), file) #deletes zip from wd
        file.remove(c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                       paste0(out_name,".dbf"), paste0(out_name,".prj")))
        shinyjs::hide("spinner_download_shp")
        
      }
    )
    
      
    output$spinner_play <- renderUI({
      if(isTRUE(play_running())) {
        return(NULL)  
      } else if(isFALSE(play_running())) {
        return("Process finished!") 
      } else {
        return(NULL) 
      }
    })
    
   
    output$download_fp_plot <- downloadHandler(
      filename = function() {
        curt = format(Sys.time(), "_%Y%m%d")
        ext <- tolower(input$dl_fp_format) #pull the selected file extension
        paste0(input$fp_plot_savename, curt, ".", ext)
      },
      content = function(file) {
        format <- if (is.null(input$dl_fp_format)) "png" else input$dl_fp_format
        if (format == "png") {
          png(file, width = 1200, height = 800)
          print(first_pareto_fun())
          dev.off()
        } else if (format == "svg") {
          svglite(file, width = 12, height = 8)
          print(first_pareto_fun())
          dev.off()
        }
      }
      
    )
    
    ## line plot
    parplot_fun = function(){ #clickline
      req(scaled_filtered_data())
      sk= scaled_filtered_data()
      
      if(is.null(sk)){return(NULL)}else{
        ko= sk%>% mutate(id = factor(row_number()))%>%pivot_longer(.,cols=-id)%>%
          mutate(name=factor(name))%>%mutate(name=forcats::fct_relevel(name,objectives()))
       
        if(input$plt_sq) {
          req(stq())
          
          #rescale single (extra) point
          min_fit <- apply(fit(), 2, min)
          max_fit <- apply(fit(), 2, max)
          
          stq_sk <- as.data.frame(mapply(function(col_name, column) {
            rescale_column(column, min_fit[col_name], max_fit[col_name])
          }, objectives(), stq(), SIMPLIFY = FALSE))
          
          colnames(stq_sk) = objectives()#otherwise spaces do not work because mapply adds dots
          
          stq_ko <- pivot_longer(stq_sk,cols = everything(),names_to = "name",values_to = "value")
          stq_ko <- stq_ko %>% mutate(name=forcats::fct_relevel(name,objectives()))
          
          return(plot_parline(datt = ko,colols = rv$colls,   sizz = rv$sizes, sq = stq_ko))
          
        }else{
          
          return(plot_parline(datt = ko,colols = rv$colls, sizz = rv$sizes, sq= NULL))
        }
        
      }}
    
    observe({
      list(fit(), f_scaled())
      
      if(exists("cached_min_max")) cached_min_max <<- NULL
    })
    
    output$linePlot <- renderPlot({ parplot_fun() })
    
  # observe({if(!is.null(input$clickline)){shinyjs::show("save_click_line")}})
  observeEvent(input$clickline, {updateCheckboxInput(session, "save_click_line", value = FALSE) })
  
 
  ## pull values from parallel axis line when clicked
  observeEvent(input$clickline,
               {
                 req(filtered_data())
                 clickpoint_button(TRUE)
                 cl_line_x=round(input$clickline$x)#x
                 cl_line_val=input$clickline$y #val
                 
                 sc = scaled_filtered_data() %>% mutate(id = row_number())
                 #find closest value
                 closest_id <- which.min(abs(sc[[cl_line_x]] - cl_line_val))
                 update_selection(closest_id) #make line plot
                 sel_tay(filtered_data()[closest_id,]) 
                 
               },
               ignoreNULL = TRUE)
 
  
  observeEvent({
    #update selected line when subset changes, sel_tay() works automatically
    list(input$obj1, input$obj2, input$obj3, input$obj4, opti_mima())
  }, {
    if (!is.null(sel_tay()) && nrow(filtered_data()) >0) {
      #we ignore when sel_tay is kicked out
      
      st = sel_tay()
      fml = filtered_data() #non-scaled
      id = st %>% left_join(fml %>% rownames_to_column("id"), by = names(st)) %>% pull(id) %>% as.numeric()
      update_selection(id)
      
    } else{reset_selection()} #remove selected line
  }, ignoreNULL = TRUE)
  
  
  output$click_info <- renderTable({click_table_data()}, include.rownames = F)
  
  pull_opt_number = function(){
    req(fit1())#doesn't need to check sel_tay()
    ft1 = fit1()
    st = sel_tay()
    
    st %>% left_join(ft1, by = names(st)) %>% slice(1) %>% 
      pull(optimum) %>%as.numeric()#slice needed for duplicate optima values
  }
  
  
  click_table_data <- reactive({
    req(sel_tay())
    
    m_opt = pull_opt_number()
    
    colnms <- objectives()
    new_colnms <- if(!is.null(axiselected())){
      mapply(function(col, unit) {
        if (unit != "") paste(col, " (", unit, ")", sep = "") else col
      }, col = colnms, unit = axiselected(), SIMPLIFY = TRUE)
    } else colnms
    
    lclick <- cbind(m_opt, as.data.frame(sel_tay()[1, , drop = FALSE]))
    colnames(lclick) <- c("optimum", new_colnms)
    
    lclick %>%
      mutate(across(where(is.numeric), ~ gsub("-", "", as.character(case_when(
        abs(.) < 1 ~ round(., 4),
        abs(.) < 10 ~ round(., 2),
        TRUE ~ round(., 0)
      )))))
    
  
  })
  
 
  reset_selection <- function(){
   # sel_tay(NULL)
    n_points = length(unique(pp()$id))
    rv$sizes <- rep(0.5, n_points)
    rv$colls <- rep("grey50", n_points)
    clickpoint_button(FALSE)
  }
  
  update_selection <- function(id){
   
    rom <- id #length(rv) has to align with subset and NOT with fit()

    n_points = length(unique(pp()$id))
    #update color
    rv$sizes <- rep(0.5, n_points)
    rv$colls[] <- rep("grey50", n_points)
    rv$sizes[rom] <- 1.3
    rv$colls[rom] <- "#FF5666"

   
  }
  
  observeEvent(input$save_click_line,{
    
    if(input$save_click_line){
      req(m_opt)
      if(file.exists(paste0(output_dir,"selected_optima.csv"))){
        
      write.table(m_opt, file = paste0(output_dir,"selected_optima.csv"), sep = ",",
                  append = TRUE, col.names = FALSE, row.names = FALSE)
      
    }else{write.csv(m_opt,file=paste0(output_dir,"selected_optima.csv"),row.names = F)
    
    }}
  })
  
  
  
  output$download_line_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      ext <- tolower(input$dl_lineplot_format) #pull the selected file extension
      paste0(input$line_plot_savename, curt, ".", ext)
    },
    content = function(file) {
      plot <- parplot_fun()
      format <- if (is.null(input$dl_lineplot_format)) "png" else input$dl_lineplot_format
      
      if (format == "png") {
        png(file, width = 1200, height = 800)
        print(plot)
        dev.off()
      } else if (format == "svg") {
        svglite(file, width = 15, height = 8)
        print(plot)
        dev.off()
      }
    }
  )
  

  ## column names for scaled and absolute tables with status quo (below here)
    observe({
      req(objectives())

      if(!is.null(axiselected())){
        new_colnms <- mapply(function(col, unit) {
          if (unit != "") {
            paste(col, " (", unit, ")", sep = "")
          } else {col}
        }, col = objectives(), unit = axiselected(), SIMPLIFY = TRUE)}else{new_colnms = objectives()}
     
      colname_unit <<- new_colnms
    })
    
    ## scaled table 
    output$sliders <- renderTable({
      req(fit(),colname_unit)
      
      slid = data.frame(
        col1 = c(input$obj1[2],input$obj1[1]),
        col2 = c(input$obj2[2],input$obj2[1]),
        col3 = c(input$obj3[2],input$obj3[1]),
        col4 = c(input$obj4[2],input$obj4[1]),
        row.names = c("best","worst")
      )
      colnames(slid) = colname_unit
      
      slid}, include.rownames=TRUE)
  
  ## absolute table with status quo difference
    output$sliders_abs <- renderTable({
      req(f_scaled(), objectives(), stq(), colname_unit)
      dn <- scaled_abs_match(
        minval_s = c(input$obj1[1], input$obj2[1], input$obj3[1], input$obj4[1]),
        maxval_s = c(input$obj1[2], input$obj2[2], input$obj3[2], input$obj4[2]),
        scal_tab = f_scaled(),
        abs_tab = fit(),
        allobs = objectives(),
        at = T,
        mes_slider = mes_touched(),
        mes_df = opti_mima()
      )

      dn2 = if(nrow(dn) == 0){ #empty measure sliders
        dn[1,] = "-"
        rownames(dn)[1] = "not applicable"
        dn
      } else if(any(is.na(dn))){
        dn[is.na(dn)] = "-"
        dn
      } else {
        add_perc_stq(df = dn, stq = stq())
      }
      
      colnames(dn2) = colname_unit
      
      dn2
    }, include.rownames = TRUE, sanitize.text.function = function(x) x)
  
  
  mima_fun = function(ff){
    df = as.data.frame(t(get_mima(ff))[-1,])
    df = df[nrow(df):1,]
    df[] = lapply(df, function(x) as.numeric(as.character(x)))
    
    return(df)
  }
  

  output$whole_range <- renderTable({
    req(objectives(), colname_unit)
    cols = objectives()
   

   df = mima_fun(fit())

   df = df %>%
     mutate(across(where(is.numeric), 
                   ~if_else(abs(.) < 1, round(., digits = 4), ifelse(abs(.) < 10,round(., digits = 2), round(., digits = 0))))) 
  
   df = df %>%mutate(across(all_of(cols), 
                   ~ { has_positive <- any(. > 0)
                       has_negative <- any(. < 0)
                     
                     if (has_positive && has_negative) {
                       ifelse(. > 0, paste0("+", abs(.)), ifelse(. < 0, paste0("-", abs(.)), abs(.)))
                     } else { abs(.) 
                     }}))
   df = df %>%
     mutate(across(where(is.numeric), ~ as.character(.)))
   
   
   colnames(df) = colname_unit
   df
  }, rownames = T)
  
  
  #measure sliders, make hru_matcher/hru_ever and aep_100
  observe({
    hru_da()
    two_basis_meas = c( "../input/hru_in_optima.RDS", #hru_matcher()/hru_ever()
                       "../data/measure_location.csv")#aep_100()
    
    ## #SWAT+/CoMOLA case, both files exist
    if(all(file.exists(two_basis_meas))){
      hru= readRDS("../input/hru_in_optima.RDS")
     
      #for matching
      colnames(hru) = gsub("^V", "", colnames(hru))
      hru_matcher(as_tibble(hru))
      
      #aep for table
      genome_hru <- read.csv('../data/measure_location.csv')#connection aep, hru
      
      #msrs() used for plotting
      msrs(unique(genome_hru$nswrm))#all measures available
      mc=  genome_hru %>%# number of extra columns required
        mutate(num_count = str_count(obj_id, ",") + 1) %>%
        summarize(max_numbers = max(num_count)) %>%
        pull(max_numbers)
      
      aep_100 <- genome_hru %>%
        separate(obj_id, paste0("hru_sep_", 1:mc), sep = ',', remove = FALSE)%>%
        pivot_longer(cols = paste0("hru_sep_", 1:mc), names_to = "code", values_to = "hru") %>% #name_new = hru separated
        relocate(hru, .after = obj_id)%>%drop_na(hru)%>%select(name,nswrm, hru)#hru = obj_id in separate columns
      
      aep_100$hru <- as.numeric(str_remove(aep_100$hru, " ") )#name = AEP, hru = hru
      
      hru_ever(hru_matcher() %>%pivot_longer(cols = -id, names_to = "optims", values_to = "measure") %>%
                 group_by(id)%>%filter(!is.na(measure)))
      hru_everact = hru_ever()
      
      aep_100_con(aep_100 %>% filter(hru %in% unique(hru_everact$id)))
      aep_100(aep_100)
    }else if(file.exists("../input/hru_in_optima.RDS") && !file.exists("../data/measure_location.csv")){
      ## standard case, measure_location.csv doesn't exist
      hru= readRDS("../input/hru_in_optima.RDS")#not efficient but parallel to OPTAIN (we could pull from gen directly)
      colnames(hru) = gsub("^V", "", colnames(hru))
      hru_matcher(as_tibble(hru))
      hru_ever(hru_matcher() %>%pivot_longer(cols = -id, names_to = "optims", values_to = "measure") %>%
                 group_by(id)%>%filter(!is.na(measure)))
      
      aep_100(hru_matcher() %>% #pulls the number of measures out of hru_in_optima, every hru is its own aep
        pivot_longer(cols =!"id",
                     names_to = "variable", 
                     values_to = "nswrm") %>%
        filter(!is.na(nswrm)) %>%
        select(hru = id, nswrm) %>%
        mutate(name = paste(nswrm, hru, sep = "_")) %>%
        select(name, nswrm, hru)%>%distinct())
      aep_100_con(aep_100()) #path dependency for OPTAIN (not ideal)
      
      msrs(unique(aep_100()$nswrm))#pull unique measures for plotting (previoulsy done with measure_location.csv)
      
    }
  })
  
  #hide stuff for measures and map plotting
  observe({ #hide map title too
    if(is.null(cm())){
      shinyjs::hide("freq_title")
      
    } else{
      shinyjs::show("freq_title")
      
    }
  })
  
  #"measures" title is part of conditional
  output$hru_available <- reactive({!is.null(hru_ever())})
  outputOptions(output,"hru_available",suspendWhenHidden = FALSE)
  
  observe({ #hide measure slider title too
    if(is.null(hru_ever())){
      shinyjs::hide("measure_title_vis")
      shinyjs::hide("number_mes_tab")
      
    } else{
      shinyjs::show("measure_title_vis")
      shinyjs::show("number_mes_tab")
      
    }
  })
  
  
  observe({
    shp_da()
    hru_da()
    map_files = c(
      # "../input/hru.con", #only for lalo()
      "../data/hru.shp",#for cm() and cmf()
      "../data/hru.shx",#for cm() and cmf()
      "../data/hru.dbf",#for cm() and cmf()
      "../data/hru.prj",#for cm() and cmf()
      "../input/hru_in_optima.RDS" #for cm() and cmf()
    )
    #frequency plot/map plot
    if (all(file.exists(map_files))) {
      shinyjs::show("freq_map_play")
      shinyjs::show("download_freq_id")
      needs_buffer(pull_buffer()) #needs buffers.RDS, NULL if not exists
      
      #catchment forever
      cm(pull_shp_new())
      
      #buffers forever
      buff_els = needs_buffer()

      if(!is.null(buff_els)){
        hru_ever_buffer = hru_ever() %>% filter(measure %in% buff_els) %>% distinct(id)#ids with small elements
        bc = cm() %>% filter(id %in% hru_ever_buffer$id)%>%st_make_valid()
        relda_utm = st_transform(bc, crs = 32633) # UTM zone 33N
        buffy <-st_buffer(relda_utm, dist = 60)
        buffers(st_transform(buffy, crs = st_crs(bc))) #all buffers ever required
      }else{buffers(NULL)}

      if(file.exists("../input/hru.con")){lalo(plt_latlon(conpath = "../input/hru.con"))}
      
      
      output$freq_map_play = renderUI({ play_freq()  })
      
    }else{shinyjs::hide("freq_map_play")
      shinyjs::hide("download_freq_id")}})
 
  play_freq = function(leg = TRUE){ #excessive function
    req(cmf(), filtered_data(),hru_matcher())
    
    dat = filtered_data()
    
    if(nrow(dat)== 0 || ncol(dat)== 0){return(NULL)}else{
      optima <-unique( match(do.call(paste, dat), do.call(paste, fit())))
      hru_subset_freq = hru_matcher()[,c("id",as.character(optima))]     #subset to only those optima in selection
      
      hru_freq = hru_subset_freq

      opt_cols_cont <- hru_freq[, -which(names(hru_freq) == "id")]#only opt columns
      
      hru_freq$freq <- apply(opt_cols_cont, 1, function(row) {
        counts <- table(row, useNA = "no")
        if (length(counts) == 0) return(NA) #not activated (anymore)
        max(counts) / length(row)
      })
      # hru_freq$freq = rowSums(!is.na(hru_freq[ , -which(names(hru_freq) == "id")])) / (ncol(hru_freq) - 1)
      # hru_share = hru_freq%>%left_join(hru_100(),by="id") %>%select(id,measure,freq)
      opt_cols <- names(opt_cols_cont)
      
      hru_share = hru_freq
      hru_share$measure = apply(hru_share[opt_cols], 1, color_meas_most) 
      hru_share = hru_share %>% select(id, measure, freq)

      #make unique measures outside
      mes <<- unique(hru_ever()$measure)
      
      #make palette outside and pass to it
      man_col = c("#66C2A5", "#4db818", "#663e13", "#F7A600", "#03597F", "#83D0F5",  "#FFEF2C",   "#a84632",  "#b82aa5",  "#246643" )
      man_col = man_col[1:length(unique(mes))]
      pal <<- colorFactor(palette = man_col, domain = unique(mes), na.color = "lightgrey")
      
      m = plt_freq(data = cmf(),lo=lalo()[2], la=lalo()[1], buffers=buffers(), remaining=hru_share, dispal=pal, mes = mes, legend = leg, basemap = input$anomap)
      
      return(m)}
  }
  
  
  output$freq_map_play = renderUI({ play_freq()  })
  
  output$download_freq <- downloadHandler(
    
    filename = function(){
      curt = format(Sys.time(), "_%Y%m%d")
      paste0(input$freq_plot_savename,curt, ".png")
    },
    
    
    content = function(file) {
      shinyjs::show("spinner_download_play2")
      
      freqmap <- play_freq(leg=FALSE)#exports global pal and mes
      
      saveWidget(freqmap, "temp.html", selfcontained = FALSE)
      webshot2::webshot("temp.html", file = file, cliprect = "viewport",vwidth = 900,
                       vheight = 900)
      shinyjs::hide("spinner_download_play2")
      file.remove("temp.html")
      unlink("temp_files", recursive = TRUE)
      }
  )
  
  

 freq_shaper = function(){ 
   req(cmf(), filtered_data(),hru_matcher())
   
   dat = filtered_data()
   
   if(nrow(dat)== 0 || ncol(dat)== 0){return(NULL)}else{
     optima <-unique( match(do.call(paste, dat), do.call(paste, fit())))
     hru_subset_freq = hru_matcher()[,c("id",as.character(optima))]     #subset to only those optima in selection
     
     hru_freq = hru_subset_freq
     hru_freq$freq = (rowSums(!is.na(hru_freq[ , -which(names(hru_freq) == "id")])) / (ncol(hru_freq) - 1))*100
     hru_freq$all_optima = rowSums(!is.na(hru_freq[ , -which(names(hru_freq) %in% c("id","freq"))]))
     # hru_share = hru_freq%>%left_join(hru_100(),by="id") %>%select(id, measure, freq, all_optima)
     
     opt_cols <- setdiff(names(hru_freq), c("id", "freq", "all_optima")) #only opt colums
     
     hru_share = hru_freq
     hru_share$measure = apply(hru_share[opt_cols], 1, color_meas_most) 
     hru_share = hru_share %>% select(id, measure, freq, all_optima)
     
     #make unique measures outside
     mes <<- unique(hru_ever()$measure)
     
     #make palette outside and pass to it
     man_col = c("#66C2A5", "#4db818", "#663e13", "#F7A600", "#03597F", "#83D0F5",  "#FFEF2C",   "#a84632",  "#b82aa5",  "#246643" )
     man_col = man_col[1:length(unique(mes))]
     pal <<- colorFactor(palette = man_col, domain = unique(mes), na.color = "lightgrey")

     m = left_join(cmf(), hru_share, by = c("id"))%>%select(id, geometry, measure, freq, all_optima)%>%st_make_valid() 
     m = m %>%subset(!st_is_empty(geometry))
     
     #divide into different dataframes
     m = split(m, m$measure)
     return(m)
     }
 }
  
  output$download_shp_freq <- downloadHandler(

    filename = function(){
      curt = format(Sys.time(), "_%Y%m%d")
      paste0("frequency_selection",curt, ".zip")
    },

    content = function(file) {
      shinyjs::show("spinner_download_shp2")
      data <- freq_shaper()

      temp_dir <- tempfile()
      dir.create(temp_dir)
      
      # Step 2: Create a subfolder for all shapefiles
      shp_dir <- file.path(temp_dir, "shapes")
      dir.create(shp_dir)
      
      for (mea in names(data)) {
        out_name <- paste0("frequency_selection_", mea)
        
        sf::st_write(data[[mea]],dsn = file.path(shp_dir, paste0(out_name, ".shp")),delete_layer = T, driver = "ESRI Shapefile")
       
      }
        
      zip::zip(
        zipfile = file,
        files = list.files(shp_dir, full.names = TRUE),
        mode = "cherry-pick"
      )      
   
      shinyjs::hide("spinner_download_shp2")

    }
  )
  

  
     output$aep_tab_one <- renderTable({
       req(hru_ever(),aep_100(),fan_tab(),sel_tay())
       
       cols = objectives()
       values = sel_tay()
       if(nrow(sel_tay())>0){
         
       
       fit = fit() %>% rownames_to_column("optimum")
       mv <- fit %>%  filter(if_all(all_of(cols), ~ . %in% values))
       one_opti = gsub("V","",mv$optimum)

       hru_one_act = hru_ever() %>%filter(optims == one_opti)
       
       # aep_one = aep_100() %>% filter(hru %in% unique(hru_one_act$id))
       # aep_one = aep_one %>%select(-hru) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
       aep_one = aep_100() %>% inner_join(hru_one_act, by = c("hru" = "id", "nswrm" = "measure"))
       aep_one = aep_one %>%select(-c(hru,optims)) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
       allmes = unique(aep_100()$nswrm)
       missmes = setdiff(allmes,aep_one$nswrm)
       
       if(length(missmes)>=1){
         
         missing_rows <- data.frame(
           nswrm = missmes,
           nom = 0,
           stringsAsFactors = FALSE
         )
         
         aep_one <- rbind(aep_one, missing_rows)
       }
       tab = aep_one[match(fan_tab(),aep_one$nswrm),]#order like table above
       tab = as.data.frame(t(tab))
       names(tab) = tab[1,]
       tab=tab[-1,]
       
       }else{
       nmes = length(fan_tab())
       nix = rep("-",nmes)
       tab <- as.data.frame(t(nix), row.names = NULL)
       colnames(tab) <- fan_tab()
        }

       tab
       }, align = "c")
     
     output$selectionmade <- reactive({ #conditional showing of second table (especially title cannot be removed otherwise)
       !is.null(sel_tay()) 
     })
     outputOptions(output, "selectionmade", suspendWhenHidden = FALSE)
     
     observe({ #fill scatter_regr() for stable regression line in scatter plot
       req(fit())
       dat = fit() #fit() is named
       vars = objectives()
       nvars = length(vars)
       
       clist = list()
       for(i in 1:(nvars-1)){
         for(j in (i+1):nvars){
           obj_par = paste(vars[i],vars[j],sep="_")
           lm_fit = lm(dat[[vars[[j]]]] ~ dat[[vars[i]]])
           
           clist[[obj_par]] = list(intercept = coef(lm_fit)[1],
                                   slope = coef(lm_fit)[2],
                                   x_var = vars[i],
                                   y_var = vars[j],
                                   r_val = round(summary(lm_fit)$r.squared,2)
                                   )
         }
       }
         scatter_regr(clist)
     })

    output$aep_tab_full <-renderTable({
      req(aep_100_con(),hru_ever(),filtered_data())
      if(nrow(filtered_data())>= 1){

        optima <-match(do.call(paste, filtered_data()), do.call(paste, fit()))
        hru_spec_act = hru_ever() %>%filter(optims %in% optima)
        
        # aep_sel = aep_100() %>% filter(hru %in% unique(hru_spec_act$id))

        aep_100_con2 =aep_100_con() %>%select(-hru) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
        # aep_sel =aep_sel %>%select(-hru) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
       
        aep_sel = aep_100() %>% inner_join(hru_spec_act, by = c("hru" = "id", "nswrm" = "measure")) #matched by both id and measure otherwise kept non-activated/competing
        aep_sel = aep_sel %>%select(-c(hru,optims)) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
        
       tab= aep_100_con2 %>% left_join(aep_sel, by = "nswrm")%>%replace(is.na(.), 0)%>%
          mutate(implemented = paste0("<span title='slider selection: max number of measures implemented in individual optima'>",nom.y,"</span>"," / ",
                                      "<span title='total number of measures available in the catchment'>",nom.x,"</span>")) %>%select(nswrm,implemented)
        fan_tab(tab$nswrm)
        tab = as.data.frame(t(tab))
        names(tab) = tab[1,]
        tab=tab[-1,]
        tab}
    }, align = "c", sanitize.text.function = function(x) x)
    

  ## scatter plot
   
    scat_fun = function() {
      scat_abs = filtered_data()
      scatter_regr_val = scatter_regr()
      erv = sel_tay()
      ft = fit()
      stq_v = stq()
      
      req(scat_abs, scatter_regr_val)
      
      if (nrow(scat_abs) == 0 || ncol(scat_abs) == 0)
        return(NULL)
      else{
        n_rows <- nrow(scat_abs)
        col <-  rep_len("grey", n_rows)
        sizz <- rep_len(2.8, n_rows)
        
        if (!is.null(erv)) {
          erv_vec <- as.vector(unlist(erv))
          rom <- which(apply(scat_abs, 1, function(x) all(x == erv_vec)))
          col[rom] = "#FF5666"
          sizz[rom] = 3
        } 
          
      
        mima = get_mima(ft)#fit()
        
        sq_d = if (input$plt_sq) stq_v else NULL
        
        plot_scatter = plt_sc(
          dat = scat_abs,
          ranges = mima,
          col = col,
          sq = sq_d,
          size = sizz,
          coefo = scatter_regr_val
        )
        
        combo_scatter <- wrap_plots(plot_scatter, ncol = 2)
        
        return(combo_scatter)
      }
      
    }

    output$scatter_plot <- renderPlot({ scat_fun()})
  
    
  output$download_scat_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      ext <- tolower(input$dl_scat_format) #pull the selected file extension
      paste0(input$scat_plot_savename,curt,  ".", ext)
      
    },
    content = function(file) {
      plot <- scat_fun()
      
      format <- if (is.null(input$dl_scat_format)) "png" else input$dl_scat_format
      
      if (format == "png") {
        png(file, width = 1200, height = 800)
        print(plot)
        dev.off()
      } else if (format == "svg") {
        svglite(file, width = 12, height = 8)
        print(plot)
        dev.off()
      }
    }
  )
  

  ### Configure ####
  
  observe({
    clusp_da()
    if(!is.null(clusp_da())){
      shinyjs::show("config_all")
      output$config_needs_var = NULL
      optain = "../input/var_corr_par.csv"
      not_optain = "../input/cluster_params.csv"
      
      if(file.exists(optain)){
        clus_path(optain)
      }else if(file.exists(not_optain)){
        clus_path(not_optain)
      }
    }else{
      shinyjs::hide("config_all")
      output$config_needs_var = renderText({"Please provide a .csv with cluster parameters (eg descriptors of the decision space) in the Data Preparation tab or Click Run Prep (SWAT+/CoMOLA workflow) before proceeding here!"})

      }
  })
 
  
  observe({#if back up exists, the original needs replacing
    req(clus_path())
    if (file.exists(sub("\\.csv$", "_bu.csv",clus_path())
    )) { 
      file.remove(clus_path())
      file.rename(sub("\\.csv$", "_bu.csv",clus_path()), clus_path())
      
    }})
  
  
  observe({
    req(objectives())
    if(!is.null(clus_path())){
      if(clus_path() == "../input/var_corr_par.csv"){
        updateCheckboxGroupInput(#OPTAIN
          session, "selements",
          choiceNames = c("share_con (*)", "Moran's I (*)","channel_frac (*)","linE","lu_share"),
                                             choiceValues=c("share_con","moran","channel_frac","linE","lu_share"),
                                             selected = c("share_con","moran","channel_frac","linE","lu_share"))
        
        
      }else{
        
        choicestuff = check_cvp(objs=objectives())
        
        updateCheckboxGroupInput(
          session, "selements",
          choiceNames = choicestuff,
          choiceValues = choicestuff,
          selected = choicestuff
          
        )
      }
    }
  })
  
      output$next_step <- renderUI({
        if (input$show_tabs == "show") {
          actionButton("go_to_tabs", "Go to Tabs")
        } else {
          actionButton("run_defaults", "Run with Defaults")
        }
      })
      
      observe({
        req(fit(), input$ran1,initial_update_done$initial)
        df = match_abs(
                      minval = c(input$ran1[1], input$ran2[1], input$ran3[1], input$ran4[1]),
                      maxval = c(input$ran1[2], input$ran2[2], input$ran3[2], input$ran4[2]),
                      abs_tab = fit(),
                      ranger = range_controlled()
                      )
        if (nrow(df) == 0 || ncol(df) == 0) {
            output$check_range <- renderText({
            paste("None of the points fulfill these criteria. Please select different data ranges!")
          })
        }else{output$check_range <- renderText({paste("")})}
      })
      
      #add behaviour for buttons
      observeEvent(input$go_to_tabs, {
        shinydashboard::updateTabItems(session, "tabs", "correlation_analysis")
      })
      
      ##default correlation/cluster run
      
      observeEvent(input$run_defaults, {
        req(clus_path())
        req(rng_plt())
        req(msrs())
        if(is.null(corr_file_check())){
        
        output$spinner_progress <- renderText({ "Clustering is running, please wait..." })
        
        default_running(TRUE) #for spinner
        req(input$selements)
        all_var <<- readRDS("../input/all_var.RDS")
        
        da <- !file.exists("../input/cluster_params.csv")
        write_corr_converted(rv = write_corr_rv,mes = msrs(), vars = input$selements,cor_analysis = T, pca = F, isOptain = da)

        if(da){check_align_converted(var_path=clus_path(), rv = write_corr_rv)}#run a short check if all var_corr_par are in ini (sometimes they don't pass convert_optain) 
        
        check_sliders(input_vals=list(input$ran1,input$ran2,input$ran3,input$ran4), #rewrite clus_path() if sliders have moved
                      default_vals= default_vals(),ranger = range_controlled(),clus_p = clus_path())
        
        ## run correlation
        correlation_converted(var_path = clus_path(), considered = write_corr_rv$col_correlation_matrix)
        
        corr <<- read.csv("../output/correlation_matrix.csv", row.names = 1) #global because of re-rendering of plot
        high_corr = find_high_corr(corr,threshold=0.7, tab=T, strike=NULL) 
        
        #### >>> end correlation - start PCA
        
        pca_content = all_var[!(all_var %in% unique(high_corr$variable1))]

        if(file.exists("../input/units.RDS")){axiselected(readRDS("../input/units.RDS"))}else{axiselected(c("-","-","-","-"))}
        axis_high_range <- axiselected()[rng_plt_axes()]#reorder axis labels
        
        #prep pca
        write_corr_converted(rv = write_corr_rv,pca_content = pca_content,pca=T, cor_analysis = F)

        write_pca_converted(pca_rv = pca_rv, 
                            pcamin=length(pca_content),pcamax=length(pca_content), #capture old function write_pcanum
                            var1=rng_plt()[1],var2=rng_plt()[2],var3=rng_plt()[3],var4=rng_plt()[4], #formerly write_pca_ini
                            var1_lab=paste0(rng_plt()[1]," [",axis_high_range[1],"]"),
                            var2_lab=paste0(rng_plt()[2]," [",axis_high_range[2],"]"),
                            var3_lab=paste0(rng_plt()[3]," [",axis_high_range[3],"]"),
                            var4_lab=paste0(rng_plt()[4]," [",axis_high_range[4],"]"),
                            handle_outliers_boolean = F,
                            fixed_cluster_boolean=T,fixed_clusters=15)
       
        ##run clustering
        
        clusterr = its_cluster_time(rv = pca_rv, corr_rv = write_corr_rv, var_path = clus_path(), ct = "kmeans")
        clus_out(1)
        
        ## window with plots/Modaldialog
        
        output$download_scat_clus = downloadHandler(
          filename = function(){
            curt = format(Sys.time(), "_%Y%m%d")
            paste0("scatter_cluster", curt, ".png")
          },
          content = function(file){
            ggsave(file, plot = clusterr$plots$p1, width = 12, height = 8, dpi = 600)
          }
        )
        
        output$download_violin = downloadHandler(
          filename = function(){
            curt = format(Sys.time(), "_%Y%m%d")
            paste0("violin_cluster", curt, ".png")
          },
          content = function(file){
            renderPlot({grid::grid.draw(clusterr$plots$p2)}, height = 600)
            ggsave(file, plot = clusterr$plots$p2, width = 12, height = 8, dpi = 600)
          }
        )
        
        
        showModal(modalDialog(
          title = "Representative Solutions",
          fluidRow(
            column(12, renderPlot({clusterr$plots$p1}, height = 600),
                   downloadButton("download_scat_clus", "Download Plot")),
            column(12, renderPlot({
              grid::grid.draw(clusterr$plots$p2)}, height = 600),
              downloadButton("download_violin", "Download Plot"))
          ),
          tags$h4("Percentile Distribution of Solutions within Clusters"),
          tags$br(),
          tableOutput("cluster_table"),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        output$cluster_table = renderTable({
          clusterr$table
        }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = T, colnames = T)
        
        #### ===> end of Modal dialog
        
        
        
        
        default_running(FALSE) # cue for text output below
        
        }else{
            output$corr_notthere_config <- renderText({corr_file_check()}) #default not run when there are files missing
                  }
        
      })
     
      output$spinner_output <- renderUI({
        if(isTRUE(default_running())) {
          return(NULL)  
        } else if(isFALSE(default_running())) {
          return("Process finished!") 
        } else {
          return(NULL) 
        }
      })
   
      
  ### Correlation Analysis ####
      corr_file_check = function(){
        required_files <- c(
          "../data/pareto_genomes.txt",
          # "../input/hru.con", #implied if the shapefile is available
          # "../data/measure_location.csv",
          "../data/hru.shp",
          "../data/hru.shx",
          "../data/hru.dbf",
          "../data/hru.prj",
          "../input/var_corr_par.csv",
          "../data/pareto_fitness.txt",
          "../input/object_names.RDS",
          "../input/all_var.RDS" #written in dataprep for OPTAIN, automated in non-OPTAIN
        )
        
        checkFiles <- sapply(required_files, function(file) file.exists(file))
        
        if (all(checkFiles) || file.exists("../input/cluster_params.csv")) {
          shinyjs::show(id = "corr_content")
          shinyjs::show(id = "corr_sidebar")
          shinyjs::hide(id = "corr_notthere")
          return(NULL)
        }
        
          shinyjs::hide(id = "corr_content")
          shinyjs::hide(id = "corr_sidebar")
          shinyjs::show(id = "corr_notthere")
          
          # neednames = ""
          # whatsmissing = ""
          
          missing_files = required_files[!checkFiles]
          
          object_names_missing <- "../input/object_names.RDS" %in% missing_files
          all_var_missing <- "../input/all_var.RDS" %in% missing_files
          var_corr_par_missing <- "../input/var_corr_par.csv" %in% missing_files
          
          special_files <- c("../input/object_names.RDS", "../input/all_var.RDS", "../input/var_corr_par.csv")
          regular_missing <- setdiff(missing_files, special_files)
          
          messages <- c()
          
          if (length(regular_missing) > 0) {
            messages <- c(messages, 
                          "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>",
                          paste(sub('../data/', '', regular_missing), collapse = "<br/>"))
          }
          
          action_needed <- c()
          
          if (object_names_missing) {
            action_needed <- c(action_needed, "define the objective names")
          }
          
          if (all_var_missing || var_corr_par_missing) {
            action_needed <- c(action_needed, "(re)run the Data Preparation or provide .csv file for clustering")
          }
          
          if (length(action_needed) > 0) {
            if (length(regular_missing) > 0) {
              messages <- c(messages, "<br/>")
            }
            messages <- c(messages, paste("Please", paste(action_needed, collapse = " and "), "in the Data Prep tab."))
          }
          
          return(HTML(paste(messages, collapse = "")))
      }
        
      
      observeEvent(input$tabs == "correlation_analysis", {
        output$corr_notthere <- renderText({corr_file_check()})
      }) 
      
      
      ## actual CORRELATION tab
      observeEvent(input$tabs == "correlation_analysis",{ 
    
        ## pull corr from file
        if(file.exists("../output/correlation_matrix.csv")){
          
          corr <<- read.csv("../output/correlation_matrix.csv", row.names = 1) #global because of re-rendering of plot
          nvar = ncol(corr)
          ## correlation plot (does not change for different thresholds)
          output$corrplot <- renderPlot({plt_corr(corr, nvar = nvar)})
        }else{ shinyjs::hide("show_conf")}
        
        } )
      
      output$download_corr_plot <- downloadHandler(
        filename = function() {
          curt = format(Sys.time(), "_%Y%m%d")
          
          paste(input$corr_plot_savename,curt, ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1200, height=800)
          plot <- plt_corr(corr)
          print(plot)
          dev.off()
          
        }
      )
      
        ## make new corr
      observeEvent(input$run_corr,{
          shinyjs::show("show_conf") #show confirm selection button once correlation has run
        
          req(input$selements,objectives())
          req(msrs())
          all_var <<- readRDS("../input/all_var.RDS")
          clp <<- clus_path() != "../input/cluster_params.csv"
          #input_path is set here so not needed in next step
          write_corr_converted(rv = write_corr_rv,mes = msrs(), vars = input$selements,cor_analysis = T, pca = F, isOptain = clp)
         
          if(clp){check_align_converted(var_path=clus_path(), rv = write_corr_rv)}
          
          check_sliders(input_vals=list(input$ran1,input$ran2,input$ran3,input$ran4), 
                        default_vals= default_vals(),ranger = range_controlled())   
          
        
          correlation_converted(var_path = clus_path(), considered = write_corr_rv$col_correlation_matrix)
          
          corr <<- read.csv("../output/correlation_matrix.csv", row.names = 1) #global because of re-rendering of plot
          output$corrplot <- renderPlot({plt_corr(corr)})
          
          
  ## events tied to a change in threshold, however also tied to change in selected variables, therefore also observe run
  observeEvent(input$thresh,{
    
    # reprint highest correlation table marking removed 
    output$corrtable <- renderDT({
      req(corr)
      
      find_high_corr(corr,threshold=input$thresh, tab=T, strike=NULL) }) #tab = T means this returns the full table, =F is for pulling variables
   
  })
   # subset of those with selected threshold
    observe({updateSelectInput(session, "excl",choices = find_high_corr(corr,threshold=input$thresh, tab=F))})
  })
  
  ## on clicking confirm selection the config ini is updated
  observeEvent(input$confirm_selection,{
    pca_remove(input$excl)
    shinydashboard::updateTabItems(session, "tabs", "pca")
    
   
    output$corrtable <- renderDT({
      datatable(find_high_corr(corr,threshold = input$thresh,tab = T,strike = input$excl),escape = FALSE)}) #tab = T means this returns the full table, =F is for pulling variables
    
    if (is.null(pca_remove())) {
      pca_content = all_var
    } else{
      pca_content = all_var[!(all_var %in% pca_remove())]
    }
    
    saveRDS(pca_content,file = "../input/pca_content.RDS") #required for PCA
    
    max_pca(get_num_pca()) #set max number of pca here (requires pca_content to exist)
    updateNumericInput(session, "pca_max", value = max_pca(), max=max_pca()) #requires pca_content to exist

    pca_in$data = pca_content
    
    write_corr_converted(rv = write_corr_rv, pca_content = pca_in$data, cor_analysis = F, pca = T)#this is also called into the pca tab on startup

    nonoval = paste(pca_remove(), collapse = ", ")
    
  # display confirmed selection in the Correlation Analysis tab
   if(is.null(pca_remove())){
     conf_text = HTML(paste0("All variables will be considered in the Clustering.","<br>"," If you change your mind please select variables above"))
   }else{conf_text =HTML(paste0("Removed variables: ","<b>", nonoval,"</b>")) }
   output$confirmed_selection <- renderText({conf_text})
  
   
  ### PC Analysis ####
  # table with variables INCLUDED in PCA (renewed every time confirm selection is clicked in correlation tab)
    pca_table(pca_in$data)

  })
  
  # reactive values to store selected choices
  selections <- reactiveValues(
    element1 = NULL,
    element2 = NULL,
    element3 = NULL,
    element4 = NULL
  )
 
  observeEvent(input$tabs == "pca",{ 
    
    if(!file.exists("../input/pca_content.RDS") || !any(file.exists(list.files(path = output_dir, pattern = "correlation.*\\.csv$", full.names = TRUE)))
    ){shinyjs::hide("everything_else_clustering")
      shinyjs::hide("everything_cluster_sidebar")
      shinyjs::hide("everything_cluster_mainpanel")
     output$no_cluster <- renderText({HTML("Please run the correlation analysis first before proceeding with the clustering!")})
    }else{shinyjs::hide("no_cluster")
      shinyjs::show("everything_cluster_sidebar") #this is needed as previously turned off and somehow that sticks
      shinyjs::show("everything_cluster_mainpanel")
      shinyjs::show("everything_else_clustering")
    }
    
    if(!file.exists("../input/object_names.RDS")) {
      choices = "Please set the objective names in the Data Preparation Tab"
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
   
    max_pca(get_num_pca())
    updateNumericInput(session, "pca_max", value = max_pca(), max=max_pca()) #requires pca_content to exist
      
    preselected = read_rv_plt(obj = T, axis = F, rv = pca_rv)
    choices = c("off", choices)
    all_choices(choices)

    isolate({if(file.exists("../input/units.RDS")){axiselected(readRDS("../input/units.RDS"))}})
    
    #update other plots including "off"
    updateTextInput(session, "axisx",  value  = axiselected()[1])
    updateTextInput(session, "axisy", value = axiselected()[2])
    updateTextInput(session, "colour", value = axiselected()[3])
    updateTextInput(session, "size", value = axiselected()[4])
    
    updateSelectInput(session, "element1", choices = choices, selected = preselected[1])
    updateSelectInput(session, "element2", choices = choices, selected = preselected[2])
    updateSelectInput(session, "element3", choices = choices, selected = preselected[3])
    updateSelectInput(session, "element4", choices = choices, selected = preselected[4])
    
      })
  
  observe({
    req(all_choices())
    
    #current selections
    selected1 <- input$element1
    selected2 <- input$element2
    selected3 <- input$element3
    selected4 <- input$element4
    
    #available choices for each dropdown
    choices1 <- setdiff(all_choices(), c(selected2, selected3, selected4))
    choices2 <- setdiff(all_choices(), c(selected1, selected3, selected4))
    choices3 <- setdiff(all_choices(), c(selected1, selected2, selected4))
    choices4 <- setdiff(all_choices(), c(selected1, selected2, selected3))
    
    #update the choices for each dropdown
    updateSelectInput(session, "element1", choices = choices1, selected = selected1)
    updateSelectInput(session, "element2", choices = choices2, selected = selected2)
    updateSelectInput(session, "element3", choices = choices3, selected = selected3)
    updateSelectInput(session, "element4", choices = choices4, selected = selected4)
  })

 
    observeEvent(input$confirm_axis,{ 
    
    isolate({axiselected(c(input$axisx,input$axisy,input$colour, input$size))})

    updateTextInput(session, "axisx",  value  = axiselected()[1])
    updateTextInput(session, "axisy", value = axiselected()[2])
    updateTextInput(session, "colour", value = axiselected()[3])
    updateTextInput(session, "size", value = axiselected()[4])
    
    empty_count2 <- sum(input$axisx == "", input$axisy == "", input$colour == "", input$size == "")
    if (empty_count2 == 0){
      pca_available$button1_clicked = TRUE
      
  
      write_labels(pca_rv = pca_rv, var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4,
                    var1_lab=input$axisx,var2_lab=input$axisy,var3_lab=input$colour,var4_lab=input$size)
    }

    output$axis_text <- renderText({
      
      if (empty_count2 >= 1) {
        pca_available$button1_clicked = FALSE
        
        "Please make selections for all four elements."
      } else {
        HTML(paste("X Axis: ", ifelse(input$element1 == "", "No selection", input$axisx),
                   "<br/>Y Axis: ", ifelse(input$element2 == "", "No selection", input$axisy),
                   "<br/>Colour: ", ifelse(input$element3 == "", "No selection", input$colour),
                   "<br/>Size: ", ifelse(input$element4 == "", "No selection", input$size)))
      }
    })
    update_settings()
 
  })
  observeEvent(input$set_choices,{
    pca_available$button2_clicked = TRUE
    
    empty_count <- sum(input$element1 == "off", input$element2 == "off", input$element3 == "off", input$element4 == "off")
    if (empty_count < 2){
      write_labels(pca_rv = pca_rv,var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4,
                    var1_lab=input$axisx,var2_lab=input$axisy,var3_lab=input$colour,var4_lab=input$size)
      # write_quali_ini(var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4)
    }
  
  output$selected_elements <- renderText({
    
    if (empty_count >= 1) {
      "Please make selections for all four elements - the analysis currently does not support less than four objectives."
    } else {
      HTML(paste("X Axis: ", ifelse(input$element1 == "off", "No selection", input$element1),
            "<br/>Y Axis: ", ifelse(input$element2 == "off", "No selection", input$element2),
            "<br/>Colour: ", ifelse(input$element3 == "off", "No selection", input$element3),
            "<br/>Size: ", ifelse(input$element4 == "off", "No selection", input$element4)))
    }
  })
  update_settings()
  })
  
  ## confirm that axis is picked and label is written
  observe({
    if (pca_available$button1_clicked && pca_available$button2_clicked && pca_available$button3_clicked) {
      shinyjs::enable("runPCA")
      output$pca_available <- renderText("")  # Clear the notification
    } else {
      shinyjs::disable("runPCA")
      output$pca_available <- renderText({
        missing_buttons <- c()
        if (!pca_available$button1_clicked) missing_buttons <- c(missing_buttons, "Confirm Choice")
        if (!pca_available$button2_clicked) missing_buttons <- c(missing_buttons, "Confirm Axis Labels")
        if (!pca_available$button3_clicked) missing_buttons <- c(missing_buttons, "Confirm Number of PCs tested")
        
        paste("Please, ", paste(missing_buttons, collapse = " and ")," first!")
      })
    }
  })
  observeEvent(input$runPCA,{
    pca_spin(TRUE) #spinner
    
    pca_content <<- readRDS("../input/pca_content.RDS")
    
    output$pca_mess <- renderUI({
      HTML("
        <p>If all data was provided in the right format, the PCA outputs will open in separate windows - you can discard or save them as necessary.<br>
        The Silhouette Score is also provided -  you can use it to compare the cluster quality of multiple runs and you should generally aim for values of > 0.5</p>
      ")
      })
    
    isElementVisible(TRUE)
    
    ## prepare correlation rv (write_corr_rv)
    write_corr_converted(rv = write_corr_rv,pca_content = pca_content,pca=T, cor_analysis = F)# columns
    
    #rewrite var_corr_par if sliders have moved (user coming straight to this tab w/o using correlation)
    check_sliders(input_vals=list(input$ran1,input$ran2,input$ran3,input$ran4), 
                  default_vals= default_vals(),ranger = range_controlled())
    
    

    # command to run the clustering
    if(input$pcamethod=="k-means") {
      clusterr = its_cluster_time(rv = pca_rv,corr_rv = write_corr_rv,var_path = clus_path(),ct = "kmeans")
      
    } else{
      clusterr = its_cluster_time(rv = pca_rv,corr_rv = write_corr_rv,var_path = clus_path(),ct = "kmedoid")
    }
    
    clus_out(1)
    
    # output$cluster_happening <- renderPrint({ cat(clusterr$text)})
    output$cluster_happening = renderText({clusterr$text})
    output$cluster_table = renderTable({  clusterr$table }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = T, colnames = T)
    
    ## window with plots/Modaldialog
    
    output$download_scat_clus = downloadHandler(
      filename = function(){
        curt = format(Sys.time(), "_%Y%m%d")
        paste0("scatter_cluster", curt, ".png")
      },
      content = function(file){
        ggsave(file, plot = clusterr$plots$p1, width = 12, height = 8, dpi = 600)
      }
    )
    
    output$download_violin = downloadHandler(
      filename = function(){
        curt = format(Sys.time(), "_%Y%m%d")
        paste0("violin_cluster", curt, ".png")
      },
      content = function(file){
        renderPlot({grid::grid.draw(clusterr$plots$p2)}, height = 600)
        ggsave(file, plot = clusterr$plots$p2, width = 12, height = 8, dpi = 600)
      }
    )
    if(clusterr$cluster_success){
      
    showModal(modalDialog(
      title = "Representative Solutions",
      fluidRow(
        column(12, renderPlot({clusterr$plots$p1}, height = 600),
               downloadButton("download_scat_clus", "Download Plot")),
        column(12, renderPlot({
          grid::grid.draw(clusterr$plots$p2)}, height = 600),
          downloadButton("download_violin", "Download Plot"))
        ),
      tags$h4("Percentile Distribution of Solutions within Clusters"),
      tags$br(),
      tableOutput("cluster_table"),
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    
    }
    #### ===> end of Modal dialog
    
   
    pca_spin(FALSE) #spinner
    })
  
  output$cluster_spin <- renderUI({
    if(isTRUE(pca_spin())) {
      return(NULL) 
    } else if(isFALSE(pca_spin())) {
      return("Process finished!")  
    }else{
      return(NULL)
    }
  })
  

  
  ## cluster specs
  observeEvent(input$write_clust, {
    fixbool = ifelse(input$clusyn == "No", TRUE, FALSE)
    pca_rv$fixed_cluster_boolean = fixbool
    
    if (input$clusyn == "No") {
      #old write_cluster()
      pca_rv$fixed_clusters = input$clus_fix
      
      pca_rv$min_clusters = 0
      pca_rv$max_clusters = 0
      
      } else{

        pca_rv$min_clusters = input$clus_min
        pca_rv$max_clusters = input$clus_max
      }
    update_settings()
  })
  ## outlier specs
  
  # align cluster number
  observe({
    minch = input$count_min
    maxch = input$count_max
    
    if(minch > maxch){
      updateNumericInput(session, "count_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "count_min", value = maxch)
    }
  })
  
  # align sd
  observe({
    minch = input$sd_min
    maxch = input$sd_max
    
    if(minch > maxch){
      updateNumericInput(session, "sd_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "sd_min", value = maxch)
    }
  })
  
  ## align number of pca
  observe({
    minch = input$pca_min
    maxch = input$pca_max
    
    if(minch > maxch){
      updateNumericInput(session, "pca_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "pca_min", value = maxch)
    }
  })
  
  
  observeEvent(input$write_outl, {
    outlbool = ifelse(input$outlyn == "No",F,T)
    if(input$outlyn == "Yes"){
      write_outl_converted(pca_rv = pca_rv, handle_outliers_boolean=outlbool,deviations_min=input$sd_min,deviations_max=input$sd_max,
                 count_min=input$count_min,count_max=input$count_max,outlier_to_cluster_ratio=input$outlier_ratio )
    }else{
      write_outl_converted(pca_rv = pca_rv, handle_outliers_boolean=outlbool)#bool is turning on all others, if false all others are ignored/default value works
    }
    update_settings()
  })
  
  ## reactive value to output for use in conditionalPanel
  output$isElementVisible <- reactive({
    isElementVisible()
  })
  
  ## conditionalPanel to work with reactive output
  outputOptions(output, "isElementVisible", suspendWhenHidden = FALSE)
  
  ## pca min/max specs
  observeEvent(input$pcaminmax,{
    pca_available$button3_clicked = TRUE
    # write_pcanum(pcamin=input$pca_min,pcamax=input$pca_max)
    pca_rv$min_components = input$pca_min
    pca_rv$max_components = input$pca_max
    update_settings()
  })
  output$pca_settings_summary <- renderUI({HTML(settings_text())})
  
  ### Analysis Panel ####
  
  observe({
    clus_out()
    if(is.null(clus_out())){
      output$analysis_needs_var <- renderUI({HTML(
        '<p style="color: red;"> The correlation analysis and the clustering have to be performed before their results can be analysed.</p>'
      )})
      shinyjs::hide("main_analysis")
      shinyjs::hide("plt_opti")
      shinyjs::runjs("toggleSidebar(false);")  #hide sidebar
      
      shinyjs::hide("show_extra_dat") #AHP hide option to show clusters
      shinyjs::hide("random_ahp") #AHP hide option to show clusters
      updateCheckboxInput(session, "show_extra_dat", value = FALSE)#turn it off (has default TRUE)
      
     
    }else{
      
      shinyjs::hide("analysis_needs_var")
      shinyjs::runjs("toggleSidebar(true);")  #show sidebar
      shinyjs::show("main_analysis")
      shinyjs::show("plt_opti")
      
      shinyjs::show("show_extra_dat") #AHP hide option to show clusters
      shinyjs::show("random_ahp") #AHP hide option to show clusters
      updateCheckboxInput(session, "show_extra_dat", value = TRUE)#turn it ON (has default TRUE)
      output$analysis_needs_var <- NULL
  }
  })
  
  
  
  
  observe({
    axiselected()
    if (is.null(axiselected())){shinyjs::disable("unit_add2")}else{shinyjs::enable("unit_add2")} 
    
  })
  
  observeEvent(input$tabs == "analysis", { #this could be combined with the ahp tab for analysis
    
    if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
    
    if (is.null(sq_da())){shinyjs::disable("add_sq")}else{shinyjs::enable("add_sq")} 
    
    
    #update Analysis tab plot without "off"
    updateSelectInput(session, "x_var2",   choices = choices, selected = rng_plt()[1])
    updateSelectInput(session, "y_var2",   choices = choices, selected = rng_plt()[2])
    updateSelectInput(session, "col_var2", choices = choices, selected = rng_plt()[3])
    updateSelectInput(session, "size_var2",choices = choices, selected = rng_plt()[4])
    
    updateSelectInput(session, "x_var_pcs_vs", choices = choices, selected = rng_plt()[1])
    
    observe({
      req(cm())
      hru_da()
      shp_da()
      if(file.exists("../data/hru.shp") && file.exists("../input/hru_in_optima.RDS")) {
        cmf(fit_optims(cm = cm(), optims = sols(), hru_in_opt_path = "../input/hru_in_optima.RDS"))
      }
    })
    
    observe({
      if(file.exists("../data/hru.shp")) {
        needs_buffer(pull_buffer())
      }
    })
    observe({
      if(all(fit()[[input$x_var2]]<=0) && 
         all(fit()[[input$y_var2]]<=0)){shinyjs::show("rev_plot2")}else{shinyjs::hide("rev_plot2")}
    })
    
   
      if(!file.exists("../input/object_names.RDS")) {
        shinyjs::hide(id = "analysis_random")
        shinyjs::hide(id= "meas_low")
        } 
 
      if(!file.exists("../data/hru.shp")){
        output$meas_low <- renderText({
          "no shapefile found, if you'd like to produce maps, please provide it in the data preparation tab."
        })}else{shinyjs::hide("meas_low")}
      
      observe({

        all_files <- list.files(output_dir, pattern = "clusters_representativesolutions.*\\.csv", full.names = TRUE)
        
        if(length(all_files)>1){
           file_info <- file.info(all_files)
           matching_files <-  all_files[which.max(file_info$mtime)]
        }else if(length(all_files)== 1){
          matching_files <- all_files
        }else{matching_files = NULL}
         
          check_files(matching_files)
        })
      
      observe({ #check for cluster quality (especially helpful in default runs)
        #find largest cluster and calculate its ratio in whole set
        req(sols())
        
        crat = round((max(sols()[["cluster size"]])/sum(sols()[["cluster size"]]))*100,2)
        
        wc = sols()%>%select(`cluster size`,`cluster number`)%>%
          filter(`cluster size` == max(`cluster size`)) %>% pull(`cluster number`)
        
        #calculate number of 1 - point clusters 
        n1clu = sols()%>%dplyr::filter(`cluster size`==1)%>%nrow()
        
        #calculate share of these small cluster in cluster number (make dynamic as we might change that)
        n1clu=round((n1clu/length(unique(sols2()$Cluster)))*100,2)
        
        if(n1clu > 30){
          output$check_default <- renderText({ paste0("There is a high share (",n1clu,"%) of clusters with only one optimum, you might want to 
                    rerun the clustering with different settings.") })
        }else if(crat>30){
          #if clause with OR if fulfilled, else NULL
          output$check_default <- renderText({paste0("A high share of points (",crat,"%) has been assigned to a single 
                                                       cluster (cluster number ",wc,"), you might want to rerun the clustering with different settings.")})}
      })

    observeEvent(check_files(),{
        if(!is.null(check_files())) {
          req(objectives())
          all_py_out <- file.info(check_files())

          current_py_out <- rownames(all_py_out)[which.max(all_py_out$mtime)]
          
          # careful because read.csv defaults to next closest binary double for 10 significant digits
          # happens in Cherio with -21992843.9, fixed with tolerance but not great
          sols_data = read.csv(current_py_out,check.names = F)
        
          new_col_sol = c("optimum", objectives(),"cluster size","cluster number","outlier")
          
          sols(sols_data %>% rownames_to_column("optimum") %>%
                 group_by(Cluster)%>%mutate(cluster_size = n())%>%ungroup()%>%
                 dplyr::filter(!is.na(Representative_Solution)& Representative_Solution != "") %>% 
                 select(1:5,cluster_size, Cluster) %>%
                 mutate(outlier = case_when(
                   Cluster == "outlier" | cluster_size == 1 ~ "outlier",  # Condition for "inactive" or value == 6
                   TRUE ~ "" 
                 ))%>% rename_with(~new_col_sol,everything()))
          

          sols2(sols_data %>% rownames_to_column("optimum") %>%  #for boxplot the whole thing is needed
                  rename_with(~new_col_sol[1:5],1:5))
          
          sols3(sols_data %>% 
                  dplyr::filter(!is.na(Representative_Solution)& Representative_Solution != "") %>%
                  select(-Representative_Solution, -Cluster))
          
          pcs_vs_vars = sols_data %>% #pull cluster variables 
            dplyr::filter(!is.na(Representative_Solution)& Representative_Solution != "") %>%
            select(-Representative_Solution, -Cluster,-objectives())%>%colnames()
          
          updateSelectInput(session, "y_var_pcs_vs", choices = pcs_vs_vars, selected = pcs_vs_vars[1])#drop down for cluster vs. objectives plot
          updateSelectInput(session, "col_var_pcs_vs", choices = pcs_vs_vars, selected = pcs_vs_vars[2])#drop down for cluster vs. objectives plot
          updateSelectInput(session, "size_var_pcs_vs", choices = pcs_vs_vars, selected = pcs_vs_vars[4])#drop down for cluster vs. objectives plot
          

        }else{
          sols(data.frame(Message = "something went wrong - has the PCA run properly?"))
          # shinyjs::hide(id="plt_opti")
        }
      
      
        output$antab <- renderDT({
          req(sols(),objectives())

          df <- sols() %>%
            mutate(across(where(is.numeric), 
                          ~if_else(abs(.) < 1, round(., digits = 4), ifelse(abs(.) < 10,round(., digits = 2), round(., digits = 0))))) %>%
            mutate(across(where(is.numeric), ~as.numeric(gsub("-", "", as.character(.)))))

          df <- as.data.frame(df)
          colnames(df) <- names(sols())
          
          df_antab(df %>% select(`cluster number`, `cluster size`,outlier,objectives(), optimum))

          datatable(df_antab(),
                    selection = list(mode = "multiple", target = 'row', max = 12),
                    rownames = FALSE,
                    
                    options = list(dom = "t",
                                   pageLength = 10000, 
                                   order = list(list(0, 'asc')), #sort according to first column cluster number
                                   responsive = TRUE,  #slightly responsive column width
                                   columnDefs = list(
                                     list(targets = 2, className = "border-column"),#add vertical lines with html$style
                                     list(targets = 6, className = "border-column"),
                                     list(targets = 0:2, width = '50px'), #adjust column width
                                     list(targets = "_all", className = "dt-right"))))
          
        })
      })
    
    output$save_antabcsv <- downloadHandler(
      filename = function(){
        curt = format(Sys.time(), "_%Y%m%d")
        paste0(input$cluster_antab_name, curt, ".csv")
      },
      content = function(file){
        write.csv(df_antab(),file,row.names = F)
      }
    )
     
    ##three functions for output$par_plot_optima, depending on which checkbox is ticked
    
    #1 default
    clus_res_plt = function(){
      req(objectives(),sols(), input$x_var2)
      
      if(is.null(check_files())) { #sol is only useful if clustering has run
        return(sols(data.frame(Message = 
                                 'something went wrong - has the PCA run properly? 
                                  You can check the output folder for files with names containing "cluster" or
                                 "representative solutions" or both ')))
      }else{
        req(objectives(),sols())
        sol<<-sols()[,c(objectives(),"cluster number")]

        if(!is.null(input$antab_rows_selected)){
          
          selected_row <- input$antab_rows_selected
          selected_data <- sols()[selected_row,objectives()]  
          
        }else{selected_data <- NULL}
        
      return(plt_sc_optima(
          dat = sol,
          x_var = input$x_var2,
          y_var = input$y_var2,
          col_var = input$col_var2,
          size_var = input$size_var2,
          full_front = fit(),
          sel_tab = selected_data,
          add_whole = input$add_whole,
          an_tab = T,
          status_q = input$add_sq,
          rev = input$rev_box2,
          unit=input$unit_add2
        ))
      }
    }
    #2 objectives vs. decision space
    clus_vs_var = function(){
      req(objectives(),sols3(),input$x_var_pcs_vs,input$y_var_pcs_vs)
      
      if(is.null(check_files())) { #sol is only useful if clustering done
        return(sols(data.frame(Message = 
                                 'something went wrong - has the PCA run properly? 
                                  You can check the output folder for files with names containing "cluster" or
                                 "representative solutions" or both ')))
      }else{
        req(objectives(),sols())
        sol<<-sols()[,c(objectives(),"cluster number")]
        
        if(!is.null(input$antab_rows_selected)){
          
          selected_row <- input$antab_rows_selected
          selected_data <- sols3()[selected_row,]
          
        }else{selected_data <- NULL}
        
        
      return(pcs_vs_var(dat = sols3(),x_var = input$x_var_pcs_vs, y_var =input$y_var_pcs_vs, 
                        col_var=input$col_var_pcs_vs, size_var=input$size_var_pcs_vs, 
                        flip =input$flip,
                        sel_tab = selected_data
      ))

    }
    }
    #3 within-cluster
    clus_dis_plt = function(){
      req(objectives(),sols(), sols2(), fit())
      
      if(!is.null(input$antab_rows_selected)){
        
        mima = get_mima(fit())
        
        selected_row <- tail(input$antab_rows_selected,n=1) #take only row that was selected last
        selected_data <- sols()[selected_row,]   #sols not sols2, this is only one point
        
        clus_one <- sols2()[sols2()$optimum == selected_data$optimum,]
        
        clus_all <- sols2()[sols2()$Cluster == clus_one$Cluster,]
       
        return(
          grid.arrange(grobs=plt_boxpl_clus(dat=clus_all, all_obs=objectives(),mima=mima), ncol = 4, width=c(1,1,1,1)))
        
        
      }else{selected_data <- NULL} 
    }
    
    #4 - share_con per measure
    clus_share_con_plt = function(){
      req(objectives(),sols(), sols2(), fit())
      
      if(!is.null(input$antab_rows_selected)){
        
        mima = get_mima(fit())
        
        selected_row <- input$antab_rows_selected #limit to four optima
        selected_data <- sols()[selected_row,]  
        
        clus_one <- sols2()[sols2()$optimum %in% selected_data$optimum,]
        
        clus_all <<- sols2()[sols2()$Cluster %in% clus_one$Cluster,]
        
        return(plt_share_con(dat = clus_all))
        
      }else{selected_data <- NULL} 
    }
    
    # turn off share_con if not provided
    observe({
      req(clus_path())
      
      shinyjs::toggle("show_share_con", condition = has_share_con(clus_path()))
    })
    
    #switch between plots/functions
    observeEvent(input$show_pareto,{ #default
      if (input$show_pareto) {
        output$par_plot_optima <- renderPlot({
          axiselected()
          clus_res_plt()})
        updateCheckboxInput(session, "show_share_con", value = FALSE)
        updateCheckboxInput(session, "show_boxplot", value = FALSE)
        updateCheckboxInput(session, "show_pca_vs_var", value = FALSE)
        
      }
    })
    
    
    observeEvent(input$show_boxplot,{
      if (input$show_boxplot) {
        output$par_plot_optima <- renderPlot({clus_dis_plt()})
        updateCheckboxInput(session, "show_share_con", value = FALSE)
        updateCheckboxInput(session, "show_pca_vs_var", value = FALSE)
        updateCheckboxInput(session, "show_pareto", value = FALSE)
        } 
    })
      
    observeEvent(input$show_share_con,{
      if (input$show_share_con) {
        output$par_plot_optima <- renderPlot({clus_share_con_plt()})
        updateCheckboxInput(session, "show_boxplot", value = FALSE)
        updateCheckboxInput(session, "show_pca_vs_var", value = FALSE)
        updateCheckboxInput(session, "show_pareto", value = FALSE)
      } 
    })
      
    observeEvent(input$show_pca_vs_var,{
      if(input$show_pca_vs_var){
      output$par_plot_optima <- renderPlot({clus_vs_var()})#calling pcs_vs_var()
      updateCheckboxInput(session, "show_boxplot", value = FALSE)
      updateCheckboxInput(session, "show_share_con", value = FALSE)
      updateCheckboxInput(session, "show_pareto", value = FALSE)
      
    }})
    
    fun_fun <- reactive({
      if (isTruthy(input$show_pca_vs_var)) {
        clus_vs_var()
      } else if (isTruthy(input$show_share_con)) {
        clus_share_con_plt()
      } else if (isTruthy(input$show_boxplot)) {
        clus_dis_plt()
      } else {
        clus_res_plt() # default plot
      }
    })
    
    output$download_clus_plot <- downloadHandler(
      filename = function() {
        curt = format(Sys.time(), "_%Y%m%d")
        ext <- tolower(input$dl_clur_format) #pull the selected file extension
        paste0(input$par_plot_savename,curt, ".", ext)
      },
      content = function(file) {
        format <- if (is.null(input$dl_clur_format)) "png" else input$dl_clur_format
        
        if(input$show_boxplot){#grid.arrange needs to be handled differently
          pl = clus_dis_plt()
          if(format == "png"){
            ggsave(
              filename = file,
              plot = pl,
              device = "png",
              width = 12,
              height = 8
            )} else if (format == "svg"){
              ggsave(
                filename = file,
                plot = pl,
                device = "svg",
                width = 12,
                height = 8
              )
            }
          
        }else{plot <- fun_fun()

        if (format == "png") {
          png(file, width = 1200, height = 800)
          print(plot)
          dev.off()
        } else if (format == "svg") {
          svglite(file, width = 12, height = 8)
          print(plot)
          dev.off()
        }
      }}
    )
    

    output$tabtext = renderText({HTML("You can select up to 12 optima and compare the implementation of measures in the catchment.")})
    
   
    if(file.exists("../input/hru.con")){lalo(plt_latlon(conpath = "../input/hru.con"))}
   
  })
  
  comp_fun = function(){

    req(sols(),cmf(), msrs()) 
    selected_row <- isolate(input$antab_rows_selected)
    
    selected_data <- sols()[selected_row,]
    
   
    hru_sel <- plt_sel(shp=cmf(),opti_sel = selected_data$optimum)

    col_sel = names(hru_sel)[grep("Optim",names(hru_sel))]  #variable length of columns selected
    
    nplots = length(col_sel)#+1

    man_col = c("#66C2A5" ,"#4db818","#663e13", "#F7A600", "#03597F" ,"#83D0F5","#FFEF2C","#a84632","#b82aa5","#246643")
    man_col = man_col[1:length(msrs())]
    pal = colorFactor(palette = man_col, domain = msrs(), na.color = "lightgrey")
    m1 = plt_lf(data=hru_sel, col_sel = col_sel ,dispal=pal,
                la = lalo()[1],lo =lalo()[2], buff_els=needs_buffer(), buffers=buffers(), basemap = input$anomap, fullscreen = T)
    
    m = m1
    
    sync(m,sync = list(1:nplots),sync.cursor = F) #list(2:nplots) when cm_clean() used
  }#}
  
  observeEvent(input$plt_opti,{
    selected_row <- isolate(input$antab_rows_selected)
    shinyjs::show("ca_shp")
    if (is.null(selected_row)) {
      
      shinyjs::show(id = "no_row")
      output$no_row = renderText({paste("No row selected")})
    
    } else {
      shinyjs::hide(id = "no_row")
      is_rendering(TRUE) 
      output$comp_map <- renderUI({comp_fun()})
      
      output$plot_ready <- renderText({
        is_rendering(FALSE)  # Set rendering to FALSE after the plot is rendered
      })
      }
     
     
  })
  
  observe({
    shinyjs::toggle("plot_spinner", condition = is_rendering())
  })
  
  shp_ca <- function(shp=T){
    req(sols(),cmf()) 
    selected_row <- isolate(input$antab_rows_selected)
    selected_data <- sols()[selected_row,]
    
    hru_sel <- plt_sel(shp=cmf(),opti_sel = selected_data$optimum)
    
    
    if (shp){
      data = hru_sel %>% subset(!st_is_empty(geometry))
    }else{
      data = names(hru_sel)[grep("Optim", names(hru_sel))] #col_sel
      data = paste0("Optima_",paste0(gsub("Optimum","",data),collapse = ""))
    }
    return(data)
    
  }
  
  output$ca_shp_download <- downloadHandler(
    
    filename = function(){
      curt = format(Sys.time(), "_%Y%m%d")
      paste0(shp_ca(shp=F),curt, ".zip")
    },
    
    content = function(file) {
      shinyjs::show("ca_shp_spin")
      data <- shp_ca()
      out_name <- shp_ca(shp=F)
      sf::st_write(data,paste0(out_name,".shp"), driver = "ESRI Shapefile")
      zip::zip( paste0(out_name,".zip"), c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                                            paste0(out_name,".dbf"), paste0(out_name,".prj")))
      
      file.rename(paste0(out_name,".zip"), file) #deletes zip from wd
      file.remove(c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                     paste0(out_name,".dbf"), paste0(out_name,".prj")))
      shinyjs::hide("ca_shp_spin")
      
    }
  )
 observe({
   sols()
   if(is.null(sols())) {#turn off all cluster-related options
     shinyjs::disable("best_cluster")
     shinyjs::disable("show_extra_dat")
   } else{
     shinyjs::enable("best_cluster")
     shinyjs::enable("show_extra_dat")
   }
 })
  
  ### AHP ####
  observeEvent(input$tabs == "ahp",{
    if(!file.exists("../data/pareto_fitness.txt")){ #check if fit() has been created yet
      output$nothing_ran_ahp <- renderText({HTML("please provide the pareto_fitness.txt and the objective names in the Data Preparation tab.")})
    }else{ shinyjs::hide("nothing_ran_ahp")
      shinyjs::runjs("toggleSidebar(false);")  # Hide sidebar
    }      
    if (is.null(sq_da())){shinyjs::disable("show_status_quo")}else{shinyjs::enable("show_status_quo")} 
      
      if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
      
      ids_to_hide <- c( "pareto_weighted", "random_ahp2", "random_ahp", "ahp_analysis", "ahp_weights","sel_wgt")
      
      lapply(ids_to_hide, shinyjs::hide)
    
      } else{choices = readRDS("../input/object_names.RDS")}
    
  
    ahp_choices(choices)
   
    updateSelectInput(session,inputId = "x_var", choices = choices,selected = rng_plt()[1])
    updateSelectInput(session,inputId = "y_var", choices = choices, selected = rng_plt()[2])
    updateSelectInput(session,inputId = "col_var", choices = choices, selected = rng_plt()[3])
    updateSelectInput(session,inputId = "size_var", choices = choices, selected = rng_plt()[4])
    
    })
  
  observe({ #hide measure slider title too
    hru_ever()
    hru_da()
    if(is.null(hru_ever())){
      shinyjs::hide("measure_title_ahp")
      shinyjs::hide("measure_table_title")
      
      
    } else{
      shinyjs::show("measure_title_ahp")
      shinyjs::show("measure_table_title")
      
    }
  })
  
  
  
  observe({ #create ahpmt() for measure sliders
    req(aep_100(), hru_ever())
    # ks = hru_ever() %>% select(-measure)
   
    # fk = aep_100()  %>% left_join(ks, by =c("hru"="id")) %>% select(-hru)
  
    fk = aep_100() %>% inner_join(hru_ever(),by = c("hru" = "id", "nswrm" = "measure"))%>% select(-hru)
 
    ahpmt(fk %>% ## all optima and the number of implemented measures
            group_by(nswrm, optims) %>%
            summarize(distinct_aep = n_distinct(name), .groups = "drop") %>%
            pivot_wider(names_from = optims, values_from = distinct_aep, values_fill = 0) %>%
            column_to_rownames("nswrm") %>%  
            t() %>%  
            as.data.frame())
  
  })
  

  observe({
    req(ahpmt())
    ahpima_ini(rbind(
      min = apply(ahpmt(), 2, min, na.rm = TRUE),
      max = apply(ahpmt(), 2, max, na.rm = TRUE)
    ))
  })
  
  #measure slider
   output$ahpmes_sliders <- renderUI({
    req(ahpmt())
    numeric_cols <- sapply(ahpmt(), is.numeric)
    sliders <- lapply(names(ahpmt())[numeric_cols], function(col) {
      sliderInput(inputId = paste0("mahp_", col),
                  label = col,
                  min = min(ahpmt()[[col]]),
                  max = max(ahpmt()[[col]]),
                  step = 1,
                  value = c(min(ahpmt()[[col]]), max(ahpmt()[[col]]))
      )
    })
    mahp_ini(TRUE)
    
    
    sliders
  })
  
  
  observe({
    req(mahp_ini(),ahpmt(), fit())
    
    numeric_cols <- names(ahpmt())[sapply(ahpmt(), is.numeric)]
    values <- setNames(
      lapply(numeric_cols, function(col) input[[paste0("mahp_", col)]]),
      numeric_cols
    )
    
    if (any(sapply(values, is.null))) return()
    
    names(values) = numeric_cols #slider current setting
    
    df_values <- as.data.frame(values)
    
    df_first_values <- as.data.frame(ahpima_ini())
    rownames(df_first_values) <- NULL
    
    if (!identical(df_values, df_first_values)) {#otherwise sometimes run too soon
      isolate(mahp_touched(TRUE))
      
      
      ff <- ahpmt() %>%
        rownames_to_column("optimum") %>%
        reduce(numeric_cols, function(df, col) {
          df %>% filter(.data[[col]] >= values[[col]][1] & .data[[col]] <= values[[col]][2])
        }, .init = .)
      
      mt_optis = ff$optimum #optima
      mahp(fit() %>% rownames_to_column("optimum")%>%filter(optimum %in% mt_optis) %>% select(-optimum))
      
    }else{mahp(NULL)}
    
  })
  
  
  observe({
    req(mahp())
    if(nrow(mahp())== 0){
      shinyjs::show("ahpmes_empty")
      
      output$ahpmes_empty =  renderText({paste("None of the optima fall within the specified ranges. Please select different measure ranges!")
      }) }else{shinyjs::hide("ahpmes_empty")
        shinyjs::enable("save_ahp")
      }
  })

  observe({
    #best_option() is set to cluster in other table
    
    req(sols(), best_option(), dfx())
    
    if (!is.null(input$best_cluster) && input$best_cluster) {
      shinyjs::show("ahp_cluster_div")
      bor = sols() %>% filter(if_all(objectives(), ~ . %in% best_option())) %>% as.data.frame()
      
      if (
          (!is.null(mahp()) && nrow(mahp()) == 0) || 
          nrow(bor) == 0) {
        output$ahp_cluster_num <- renderText({paste0("none of the clusters fall within your selection!")})
          shinyjs::disable("save_ahp")
        
      }else{
        output$ahp_cluster_num <- renderText({paste("cluster number: ", 
                bor$`cluster number`,"; the representative optima is ", bor$optimum,sep = "")})
        
      }
    }else{shinyjs::hide("ahp_cluster_div")}
  })
  
  observe({
    req(objectives())
    n_criteria <- length(objectives())
    
    comparison_matrix <- matrix(1, nrow = n_criteria, ncol = n_criteria,dimnames = list(objectives(),objectives()))
    
    for (i in 1:(n_criteria - 1)) {
      for (j in (i + 1):n_criteria) {
        slider_id <- paste0("c", i, "_c", j)
        
        value = input[[slider_id]]
        if(is.null(value) || value=="Equal"){
          comparison_value =1
          comparison_matrix[j, i] <- comparison_value
          comparison_matrix[i, j] <- 1 / comparison_value
        }else{ 
        parts <- strsplit(value, " - ")[[1]]
        
        if (length(parts) == 2) {
            # first part is numeric
          if (grepl("^\\d+$", parts[1])) {
            comparison_value <- as.numeric(parts[1])
            comparison_matrix[i, j] <- comparison_value
            comparison_matrix[j, i] <- 1 / comparison_value
            
          } else {
            # first part is objective
            comparison_value <- as.numeric(parts[2])
            comparison_matrix[j, i] <- comparison_value
            comparison_matrix[i, j] <- 1 / comparison_value
          }
        }}
      }
    }
      coma(comparison_matrix)
      
      normalized_matrix <- comparison_matrix / colSums(comparison_matrix)

      weights <- rowMeans(normalized_matrix)

      weights <- weights/sum(weights)
      
      pass_to_manual(weights) #weights for passing and changing
      
      calculate_weights(weights) #weights for direct use
  })
  
  #debouncing to save on processing time when several sliders are moved or one slider moved with keyboard
  ahp_combined_debounced <- debounce(
    reactive({
      req(input$obj1_ahp, input$obj2_ahp, input$obj3_ahp, input$obj4_ahp)
      
      list(
        obj1 = input$obj1_ahp,
        obj2 = input$obj2_ahp,
        obj3 = input$obj3_ahp,
        obj4 = input$obj4_ahp,
        mahp_data = mahp(),  
        mahp_touched = mahp_touched()
      )
    }), 
    600
  )
  
  #main datasets for this tab: sols_ahp() and whole_ahp()
  observeEvent(ahp_combined_debounced(), {
    req(fit())
    inputs <- ahp_combined_debounced()
    
    whole_ahp(match_abs(#always produced
      minval = c(inputs$obj1[1], inputs$obj2[1], inputs$obj3[1], inputs$obj4[1]),
      maxval = c(inputs$obj1[2], inputs$obj2[2], inputs$obj3[2], inputs$obj4[2]),
      abs_tab = fit(), 
      ranger = range_controlled(), 
      mes_slider = inputs$mahp_touched, 
      mes_df = inputs$mahp_data
    ))
    
    if(!is.null(sols())){#only produced when clustering has run
      df1 = subset(sols(), select = -c(optimum, `cluster number`, `cluster size`, outlier)) #best option out of optima
      
      sols_ahp(match_abs(
        minval = c(inputs$obj1[1], inputs$obj2[1], inputs$obj3[1], inputs$obj4[1]),
        maxval = c(inputs$obj1[2], inputs$obj2[2], inputs$obj3[2], inputs$obj4[2]),
        abs_tab = df1, 
        ranger = range_controlled(), 
        mes_slider = inputs$mahp_touched, 
        mes_df = inputs$mahp_data
      ))
    } 
 
  })
  
  observe({ #switch between datasets
    
    req( whole_ahp())
    
    if(!is.null(sols_ahp()) && !is.null(input$best_cluster) && input$best_cluster){dfx(sols_ahp())}else{
      dfx(whole_ahp()) #default
    }
    
  })
  
  observe({
    req(dfx())
    sk = dfx()
   
      output$ahp_count <- renderUI({
        tagList("The remaining number of optima is: ",tags$b(nrow(sk)))
      })
    
    
  })
  
  # observe({#main datasets for this tab: sols_ahp() and whole_ahp()
  #   req(sols(), fit(), input$obj1_ahp,input$obj2_ahp,input$obj3_ahp,input$obj4_ahp )
  #   
  #   df1 = subset(sols(),select= -c(optimum,`cluster number`,`cluster size`,outlier )) #best option out of optima
  #   
  #   sols_ahp(match_abs(minval=c(input$obj1_ahp[1],input$obj2_ahp[1], input$obj3_ahp[1], input$obj4_ahp[1]),
  #                  maxval=c(input$obj1_ahp[2],input$obj2_ahp[2], input$obj3_ahp[2], input$obj4_ahp[2]),
  #                  abs_tab = df1, ranger = range_controlled(), mes_slider = mahp_touched(), mes_df = mahp()))
  #   
  # 
  #   whole_ahp(match_abs(minval=c(input$obj1_ahp[1],input$obj2_ahp[1], input$obj3_ahp[1], input$obj4_ahp[1]),
  #                  maxval=c(input$obj1_ahp[2],input$obj2_ahp[2], input$obj3_ahp[2], input$obj4_ahp[2]),
  #                  abs_tab = fit(), ranger = range_controlled(), mes_slider = mahp_touched(), mes_df = mahp()))
  #  
  # })
  
 
  
  output$weights_output <- renderTable({
                                       req(calculate_weights())
                                       wgt=(t(calculate_weights()))
                                       wgt
                                       }, colnames = T)
  
  observe({req(dfx())
    if (nrow(dfx()) == 0 || ncol(dfx()) == 0) {shinyjs::disable("save_ahp")
  
          }else{shinyjs::enable("save_ahp")}
    })
  
  
  output$best_option_output <- renderTable({
    req(objectives(), calculate_weights(), dfx(), best_option())
    
    if (!all(names(calculate_weights()) %in% colnames(dfx()))) {paste("Dataframe columns do not match criteria names.")}
    
    if (nrow(dfx()) == 0 || ncol(dfx()) == 0) {
      bo = as.data.frame(array("-", dim = c(1, length(objectives(
      ))))) #to prevent error when tab is touched first
      colnames(bo) = objectives()
    } else{
      bo = best_option() %>% mutate(across(where(is.numeric),  ~ if_else(
        abs(.) < 1,
        round(., digits = 4),
        ifelse(abs(.) < 10, round(., digits = 2), round(., digits = 0))
      ))) %>%
        mutate(across(where(is.numeric), ~ gsub("-", "", as.character(.))))
    }
    bo
  }, colnames = T)
      
    
    observe({ #control best_option
      req(calculate_weights(), dfx())
      weights <- calculate_weights()
      
      min_fit <- apply(dfx(), 2, min)
      max_fit <- apply(dfx(), 2, max)
      
      #scale to 0 and 1 not anchoring with original
      df_sc <- as.data.frame(mapply(function(col_name, column) {
        rescale_column(column, min_fit[col_name], max_fit[col_name])
      }, colnames(dfx()), dfx(), SIMPLIFY = FALSE))
      
      #final score based on df within 0 and 1
      best_option_index <- which.ahp(df_sc, weights)
      
      best_option(dfx()[best_option_index, ]) #for direct use
      
      bo_pass(dfx()[best_option_index, ]) #for passing to manual
      
    })
    
  
    
    output$aep_ahp <- renderTable({
      req(fit())
      req(hru_ever(), aep_100_con())
      req(best_option(),ahpmt(), dfx())
      
      
      # slider vs. whole front
      if(nrow(dfx())>= 1){ 
        #optima pulled from whole dataset = whole_ahp() and not dfx() which can be cluster solutions (otherwise 2nd number cluster instead of slider selection)
        optima <-unique(match(do.call(paste, whole_ahp()), do.call(paste, fit())))#position/rowname/optimum in fit(), not super stable
        hru_spec_act = hru_ever()%>%filter(optims %in% optima)
        #whole front, number of all aep
        aep_100_con2 =aep_100_con() %>%select(-hru) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))

        #selection aligned with sliders
        aep_sel = aep_100() %>% inner_join(hru_spec_act, by = c("hru" = "id", "nswrm" = "measure")) #matched by both id and measure otherwise kept non-activated/competing
        aep_sel = aep_sel %>%select(-c(hru,optims)) %>% group_by(nswrm) %>%summarise(nom = n_distinct(name))
        
      # selected point
        cols = objectives()
        values = best_option()
        
        default_empty_ahp <- function(){#then we don't recreate this all the time
          data.frame(nom = rep("-",nrow(aep_sel)),
                     nswrm = aep_sel$nswrm,
                     stringsAsFactors = F)
        }
        
         if(nrow(best_option()) == 0){
           aep_one = default_empty_ahp()
        }else{
          fit = fit() %>% rownames_to_column("optimum")
          tol_rel <- 1e-6
          #awkward but needed through rounded csv reading, needs across(all_of()), not if_all(all_of())
          mv <- fit %>% mutate(match = rowSums(across(all_of(cols),
                                      ~abs(.-values[[cur_column()]])/abs(values[[cur_column()]])<tol_rel))==length(cols)) %>%
            filter(match) %>% select(-match)
          
          if(nrow(mv) == 0){
            aep_one = default_empty_ahp()
            
          }else{
            mv$optimum <- as.character(mv$optimum)
            aep_one_fin <- ahpmt()[mv$optimum,,drop=F]
            aep_one = tibble( nom = as.numeric(aep_one_fin),
                              nswrm = colnames(aep_one_fin))
            
            
            allmes = unique(aep_100_con()$nswrm)
            missmes = setdiff(allmes,aep_one$nswrm)
            
            if(length(missmes)>=1){
              
              missing_rows <- tibble(
                nom = rep(0, length(missmes)),
                nswrm = missmes
              )
              
              aep_one <- bind_rows(aep_one, missing_rows)
            }
          }
          
        }

        #all together
        tab= aep_one %>% left_join(aep_100_con2, by = "nswrm") %>% left_join(aep_sel, by = "nswrm")%>%replace(is.na(.), 0)%>%
          mutate(implemented = paste0(nom.x," / ",nom, " / " , nom.y)) %>%select(nswrm,implemented)
        fan_tab(tab$nswrm)
        tab = as.data.frame(t(tab))
        names(tab) = tab[1,]
        tab=tab[-1,]
        tab}
    }, align = "c")
    
   
    ## ahp up down table
    
    observe({
      req(bo_pass(), dfx())
      
      fit_sorted(dfx())
      
      #find current position of bo and go from there (less scrolling)
      fit_row(match(do.call(paste, bo_pass()), do.call(paste, dfx()))) #since main pass subsets too this should exist 
    })
    
    
    manual_ahp_fun = function(){
      req( fit_sorted(), fit_row())
      
      row_data <- fit_sorted()[fit_row(), , drop = FALSE]
      #align rounding with other table
      row_data <- row_data %>% 
        mutate(across(everything(), ~ sapply(. , function(x) {
          main_value <- abs(x)  
          
          if (main_value < 1) {
            round(x, 4)  
          } else if (main_value < 10) {
            round(x, 2)  
          } else {
            round(x, 0)  
          }
        })))
      
      
      row_display <- row_data
      
      for (col in colnames(dfx())) {
        row_display[[col]] <- paste0(
          actionButton(paste0("up_", col), "⬆", 
                       onclick = paste0("Shiny.setInputValue('up_col', '", col, "', {priority: 'event'})")),
          " ", row_data[[col]], " ",
          actionButton(paste0("down_", col), "⬇", 
                       onclick = paste0("Shiny.setInputValue('down_col', '", col, "', {priority: 'event'})"))
        )
      }
      # best_option(isolate(fit_sorted()[fit_row(), , drop = FALSE])) #pass back
      
      return(row_display)
    }
    
    update_selected_row <- function(col, direction) {
      req(dfx(), fit_row())
      
      sorted_data <- dfx()[order(dfx()[[col]]), ] 
      fit_sorted(sorted_data) 
     
      current_index <- which(sorted_data[[col]] == sorted_data[fit_row(), col])[1]
      fit_row(current_index)
      if (is.na(current_index)) return()
      
      if (direction == "up") {
        if (current_index < nrow(sorted_data)) fit_row(current_index + 1)
      } else {
        if (current_index > 1) fit_row(current_index - 1)
      }
      best_option(isolate(fit_sorted()[fit_row(), , drop = FALSE])) #pass back
      
    }
    
    
    observeEvent(input$up_col, {
      update_selected_row(input$up_col, "up")
    })
    
    observeEvent(input$down_col, {
      update_selected_row(input$down_col, "down")
    })
    
    observe({
      if(input$make_manual_ahp){
        
        shinyjs::hide("weighted_approach")
        shinyjs::show("manual_ahp")
    
    output$manual_ahp_tab <- renderTable({
         manual_ahp_fun()
        }, sanitize.text.function = identity, rownames = FALSE)
        
       
      
      }else{
        shinyjs::show("weighted_approach")
        
        shinyjs::hide("manual_ahp")
       }
    
    })
    
    # switch to manual weights
    observe({
      req(pass_to_manual())
      man_weigh(as.data.frame(t(pass_to_manual()), row.names = NULL))#pull current weight
    })
    
    
    # manual weights
    observe({
      if(input$yes_weight){
        shinyjs::show("manual_weight")
        shinyjs::hide("sel_wgt")
        req(man_weigh(), objectives())
        
        output$manual_weight = renderDT({
          datatable(man_weigh(), editable = TRUE,
                    options = list( searching = FALSE,   
                      paging = FALSE, info = FALSE, autoWidth = F, dom = 't'),rownames = NULL)%>%
            formatRound(columns = objectives(), digits = 2)
        })
        
      }else{shinyjs::hide("manual_weight")
        shinyjs::show("sel_wgt")}
        
      })
        
        observeEvent(input$manual_weight_cell_edit, {
          info <- input$manual_weight_cell_edit
          j <- info$col
          v <- info$value
          
          v <- as.numeric(v)
        
          new_data <- man_weigh()
          
          new_data[1, j+1] <- v
          
          man_weigh(new_data)
         
        })
        
        #pass manual weights back into main process
        observeEvent(input$check_sum , {
          new_data = man_weigh()
          sum_values <- sum(as.numeric(new_data[1, ]), na.rm = TRUE)
          
          if (sum_values != 1) {
            scaling_factor <- 1 / sum_values
            new_data[1, ] <- round(new_data[1, ] * scaling_factor, 2)
          }
          man_weigh(new_data)
          nn = as.numeric(new_data[1,])
          names(nn) = colnames(new_data)
          
          calculate_weights(nn)
          
        })
        

    
    # observe({#moved elsewhere
    #   req(dfx(),objectives())
    #   if(!is.null(best_option())) {shinyjs::show("save_ahp")}})
    
    observe({
      req(best_option(), fit(), objectives())
      updateCheckboxInput(session, "save_ahp", value = FALSE) 
      bp <-best_option()
      
      bp <<- fit()%>% rownames_to_column("optimum") %>% filter(if_all(objectives(), ~ . %in% bp))
      })
    
      
    
    observeEvent(input$save_ahp,{
      
      if(input$save_ahp){
        
        if(file.exists(paste0(output_dir,"selected_optima.csv"))){
          
          write.table(bp, file = paste0(output_dir,"selected_optima.csv"), sep = ",",
                      append = TRUE, col.names = FALSE, row.names = FALSE)

        }else{
        write.csv(bp,file=paste0(output_dir,"selected_optima.csv"),row.names = F)

        }}
    })
  
    
    
    weight_plt_fun = function(){
      req(best_option())
      req(whole_ahp())
      req(input$x_var, input$y_var, input$col_var, input$size_var)
      
     if(!is.null(sols())){ sol<<-sols()[,objectives()]}else{sol = NULL}
      bo = best_option()
      df3 = whole_ahp()
      
      if(nrow(df3)==0){bo = NULL}
      
      return(plt_sc_optima(dat=df3,x_var=input$x_var,y_var=input$y_var,
                           col_var=input$col_var,size_var=input$size_var,high_point=bo, extra_dat = sol, full_front = fit(),
                           plt_extra = input$show_extra_dat, status_q = input$show_status_quo,an_tab = F,rev = input$rev_box3,
                           unit=input$unit_add3, ahp_man = input$make_manual_ahp
      ))
    }
    
 
    observe({
      req(whole_ahp(), best_option())
      if(nrow(whole_ahp())==1){best_option(whole_ahp())} #ugly fix
    })
    
   #show reverse option when needed
  observe({
    observe({
      if(all(fit()[[input$x_var]]<=0) && 
         all(fit()[[input$y_var]]<=0)){shinyjs::show("rev_plot3")}else{shinyjs::hide("rev_plot3")}
    })
    
    
  output$weights_plot <- renderPlot({  
    axiselected()
    weight_plt_fun() })
  
  output$download_weights_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      ext <- tolower(input$dl_weight_format) 
      paste0(input$weights_plot_savename,curt, ".", ext)
    },
    content = function(file) {
      
      plot <- weight_plt_fun()
      format <- if (is.null(input$dl_weight_format)) "png" else input$dl_weight_format
      
      if (format == "png") {
        png(file, width = 1500, height = 1000)
        print(plot)
        dev.off()
      } else if (format == "svg") {
        svglite(file, width = 15, height = 10)
        print(plot)
        dev.off()
      }   })
  
  })
  
  observe({
    
    req(coma(), calculate_weights())
    ## consistency checks
    ci = consistency_index(coma())

    cr = ci/0.89 #value found online, determined through random matrices
  
    #table stays empty without inconsistencies
    inconsistency_check = function(tab) {
      req(coma(), objectives(), cr)
      slider_ids = c(slider_ahp[["c1_c2"]], slider_ahp[["c1_c3"]], slider_ahp[["c1_c4"]], slider_ahp[["c2_c3"]], slider_ahp[["c2_c4"]], slider_ahp[["c3_c4"]])
     
      se = sum(slider_ids == "Equal") #if majority on equal, large preferences amplify mathematical inconsistency
      
      if (se > 3) {
        inconsistencies = paste("")
        if (tab == T) {
          inconsistencies = character(0)
        }
      } else if (cr <= 0.15) {
        inconsistencies = paste("No major inconsistencies, the inconsistency ratio is:",
                                round(cr, 3))
        if (tab == T) {
          inconsistencies = character(0)
        }
      } else{
        if (tab == T) {
          inconsistencies = check_inconsistencies(coma(), weights = calculate_weights())
        } else if (tab == F &  is.null(check_inconsistencies(coma(), weights = calculate_weights()))) {
          inconsistencies = paste("Potential inconsistencies, the inconsistency ratio is:",
                                  round(cr, 3))
        } else{
          inconsistencies = paste0(
            "The inconsistency ratio is: ",
            round(cr, 3),
            ". Please change your priorities for the following objectives:"
          )
        }
      }
      
      return(inconsistencies)
    }
    
    output$consistency_check = renderText({inconsistency_check(tab=F)})
    
    output$which_inconsistency = renderText({inconsistency_check(tab=T)})
    
  })
  
  observe({ #remove plot button
    shp_da()
    hru_da()
    if(is.null(shp_da()) || is.null(hru_da())) {
      shinyjs::hide("plt_bo")
    } else {
      shinyjs::show("plt_bo")
    }
  })
  
  observeEvent(input$plt_bo,{ #first click
    meas_running(TRUE) 

    req(best_option(),fit(),objectives(), fit1())
 
    mahp_plotted(TRUE)
    

  bo = best_option() 
  cols = objectives()
  bo <- fit1() %>% filter(if_all(all_of(cols), ~ . %in% bo))
   
    ##shps for maps
    if (file.exists("../input/hru_in_optima.RDS")) {
   
      req(cm())
      cmf(fit_optims(cm=cm(),hru_in_opt_path = "../input/hru_in_optima.RDS",optims=bo))
    }
    boo(bo$optimum) #for single_meas_fun
    
    if(file.exists("../input/hru.con")){lalo(plt_latlon(conpath = "../input/hru.con"))}
     needs_buffer(pull_buffer())

     output$plt_bo_measure = renderUI({single_meas_fun()})
    
     shinyjs::show("download_ahp_id") #show download button

  })
  
  
  observe({ #reactively replot every time best_option() changes
    hru_da()
    shp_da()
    req(mahp_plotted())
    req(best_option(),fit(),objectives(), fit1())
    
    
    meas_running(TRUE) 

    bo = best_option() 
    cols = objectives()
    bo <- fit1() %>% filter(if_all(all_of(cols), ~ . %in% bo))
    
    ##shps for maps
    if (file.exists("../input/hru_in_optima.RDS")) {
      
      req(cm())
      cmf(fit_optims(cm=cm(),hru_in_opt_path = "../input/hru_in_optima.RDS",optims=bo))
    }
    boo(bo$optimum) #for single_meas_fun
    
    if(file.exists("../input/hru.con")){lalo(plt_latlon(conpath = "../input/hru.con"))}
    needs_buffer(pull_buffer())
    
    
    output$plt_bo_measure = renderUI({single_meas_fun()})
    
    # shinyjs::show("download_ahp_id") #show download button
    
  })
  
  single_meas_fun = function(fs = T){
    req(boo(),cmf(),msrs())
      
    hru_one = plt_sel(shp=cmf(),opti_sel = boo())
    col_sel = names(hru_one)[grep("Optim",names(hru_one))] 
    
    man_col = c("#66C2A5" ,"#4db818","#663e13", "#F7A600", "#03597F" ,"#83D0F5","#FFEF2C","#a84632","#b82aa5","#246643")
    man_col = man_col[1:length(msrs())]
    pal = colorFactor(palette = man_col, domain = msrs(), na.color = "lightgrey")
    
    
    m1 = plt_lf(data=hru_one, dispal = pal,la = lalo()[1],lo =lalo()[2],
                buff_els=needs_buffer(),col_sel=col_sel,buffers=buffers(), basemap = input$anomap, fullscreen = fs)
    return(m1)
    meas_running(FALSE)
    
  }
  
  output$spinner_meas <- renderUI({if(isTRUE(meas_running())){return(NULL)}else{return("")}})
  

  output$download_am = downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      shinyjs::toggle("ahp_spinner", condition = is_rendering())
      paste(input$meas_ahp_savename, curt, ".png", sep = "")
    },
    content = function(file) {
      shinyjs::show("spinner_download_ahp")  
      mp =single_meas_fun(fs = F)[[1]]
      saveWidget(mp, "temp.html", selfcontained = FALSE)
      webshot2::webshot("temp.html", file = file, cliprect = "viewport",vwidth = 900,
                       vheight = 900)
      shinyjs::hide("spinner_download_ahp")  
      file.remove("temp.html")
      unlink("temp_files", recursive = TRUE)
      }
  )
  
  shp_ahp = function(shp=T){
    req(boo(),cmf())
    
    hru_one = plt_sel(shp=cmf(),opti_sel = boo())
    
    
    if (shp) {
      data = hru_one %>% subset(!st_is_empty(geometry))
    } else{
      data = names(hru_one)[grep("Optim", names(hru_one))] #col_sel
    }
    return(data)
    
  }
  
  output$ahp_shp_download <- downloadHandler(
    
    filename = function(){
      curt = format(Sys.time(), "_%Y%m%d")
      paste0(shp_ahp(shp=F),curt, ".zip")
    },
    
    content = function(file) {
      shinyjs::show("ahp_shp_spin")
      data <- shp_ahp()
      out_name <- shp_ahp(shp=F)
      sf::st_write(data,paste0(out_name,".shp"), driver = "ESRI Shapefile")
      zip::zip( paste0(out_name,".zip"), c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                                            paste0(out_name,".dbf"), paste0(out_name,".prj")))
      
      file.rename(paste0(out_name,".zip"), file) #deletes zip from wd
      file.remove(c( paste0(out_name,".shp"), paste0(out_name,".shx"),
                     paste0(out_name,".dbf"), paste0(out_name,".prj")))
      shinyjs::hide("ahp_shp_spin")
      
    }
  )
 
  
  ## AHP sliders
  # output$sliders_ui <- renderUI({
  #   req(objectives())
  #   sliders <- list()
  #   num_criteria <- length(objectives())
  #   
  #   for (i in 1:(num_criteria - 1)) {
  #     for (j in (i + 1):num_criteria) {
  #       slider_id <- paste0("c", i, "_c", j)
  #       sliders[[slider_id]] <- sliderTextInput(
  #         inputId = slider_id,
  #         label =paste0(objectives()[j]," vs. ",objectives()[i]), 
  #         choices = c(paste0(objectives()[j]," - ",9:2),"Equal",paste0(2:9," - ",objectives()[i])),
  #         
  #         selected = "Equal",
  #         grid = TRUE,
  #         hide_min_max = FALSE,
  #         animate = FALSE,
  #         width = "100%", 
  #         force_edges = T
  #         
  #       )
  #     }
  #   }
  #   
  #   do.call(tagList, sliders)
  # })

  
  
  # scatter function
  create_plot <- function(cn) { #cn - card number
    req(fit(),ahp_combo())
    
    combi = unlist(strsplit(ahp_combo()[cn],split = " vs. "))
    
    x <- fit()[,combi[1]]
    y <- fit()[,combi[2]]
    
    plt_scat2(dat= fit(), x= combi[1], y=combi[2])
    
  }
  
  # R2 
  create_r2tab <- function(cn){
    req(fit(),ahp_combo())
    
    combi = unlist(strsplit(ahp_combo()[cn],split = " vs. "))
    x <- fit()[,combi[1]]
    y <- fit()[,combi[2]]
    ## linear model
    model <- lm(y ~ x)
    metrics_df <- data.frame(
      Metric = c("R<sup>2</sup>", "Pearson's r"),  #HTML for R²
      Value = c(round(summary(model)$r.squared, 3), round(cor(x, y), 3))
    )
    
    metrics_df
  }
  
  # weight sliders
  ahp_slider_maker = function(){
    req(objectives())
    num_criteria = length(objectives())
  for (i in 1:(num_criteria - 1)) {
    for (j in (i + 1):num_criteria) {
      slider_id <- paste0("c", i, "_c", j) #aligns with sid()
      sliders[[slider_id]] <- sliderTextInput(
        inputId = slider_id,
        label = "",
        choices = c(paste0(objectives()[j]," - ",9:2),"Equal",paste0(2:9," - ",objectives()[i])),
        
        selected = "Equal",
        grid = TRUE,
        hide_min_max = FALSE,
        animate = FALSE,
        width = "110%",
        force_edges = T
        
      )
    }
  }
  }
  
  
  #make sliders
  observe({
    req(objectives())
    ahp_slider_maker()})
 
  #fun for putting all three, slider, table with r2 and scatter plot in one
  create_card <- function(title, sliid, plot, table, session, slider_val, sliders) {
    
    updateSliderTextInput(session, inputId = sliid, selected = slider_val)
    box(
      title = h1(title, style = "text-align: center; font-size: 140%; margin-top: -15px;"),
      width = 12,
      status = "primary",
      tagList( div(
        style = "display: flex; justify-content: center; align-items: center;",
        div(
          style = "margin-right: 20px;", 
          plotOutput(plot, width = "266px", height = "200px") 
        ),
        div(
          style = "margin-left: 20px;",  
          tableOutput(table)  
        )
      ),
      sliders[[sliid]]
    )
    )
  }
  

  for (k in 1:6) {
    local({
      i <- k
      ahp_card <- paste0("ahp_card", i)
      plt <- paste0("plot", i)
      tabl <- paste0("table", i)
      cardui <- paste0("card", i, "_ui")

      
      observeEvent(input$show_all_cards, {
        if (input$show_all_cards) {
          one_on$vari = NULL
          # show all cards
          for (j in 1:6) {
            card_id <- paste0("card", j, "_ui")
            shinyjs::show(card_id)
            runjs(paste0('document.getElementById("ahp_card', j, '").style.backgroundColor = "#0487bf";'))
          }
        } else {
          one_on$vari = NULL
          for (j in 1:6) {
            card_id <- paste0("card", j, "_ui")
            shinyjs::hide(card_id)
            runjs(paste0('document.getElementById("ahp_card', j, '").style.backgroundColor = "#f4f4f4";'))
          }
        }
      })
      
      observeEvent(input[[ahp_card]], {
        if (!input$show_all_cards) {
          for (j in 1:6) {
            current_cardui <- paste0("card", j, "_ui")
            if (j != i) {
              shinyjs::hide(current_cardui)
              runjs(paste0('document.getElementById("ahp_card', j, '").style.backgroundColor = "#f4f4f4";'))
            } else {
              
              if(!is.null(one_on$vari) &&  one_on$vari == current_cardui){shinyjs::hide(current_cardui)
                runjs(paste0('document.getElementById("ahp_card', j, '").style.backgroundColor = "#f4f4f4";'))
              }else{
                runjs(paste0('document.getElementById("ahp_card', j, '").style.backgroundColor = "#0487bf";'))
                
                shinyjs::show(current_cardui)
                one_on$vari = current_cardui
                }
             } 
          }
        }
      })
      #
      output[[plt]] <- renderPlot({ create_plot(i) })
      output[[tabl]] <- renderTable({ create_r2tab(i) }, rownames = FALSE, colnames = FALSE, sanitize.text.function = function(x) x)
      
      output[[cardui]] <- renderUI({
        create_card(ahp_combo()[i], sliid = sids()[i], plt, tabl, session, slider_val = isolate({slider_ahp[[sids()[i]]]}), sliders)
      })
    })
  }
  
  
  observe({
    lapply(sids(), function(sliid) {
      observeEvent(input[[sliid]], {
        slider_ahp[[sliid]] <- isolate({input[[sliid]]})
      })
    })

  })
  

  

}