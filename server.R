
#Ds_init <- matrix(1:2, nrow = 1)
#Define server
server <- function(input, output, session){
  
  #Save inputs buttons
  observeEvent(input$save_inputs, {
    #Construct Model Inputs file
    model_inputs <- data.frame(dx = input$dx,
                               dt = input$dt,
                               dt_Q = input$dt_Q,
                               type = input$transport_type,
                               bank_erosion = input$bank_erosion,
                               dt_output = input$dt_output,
                               meandering = input$meandering,
                               input_type = "reach",
                               omegac_star = input$omega_c,
                               b = input$b,
                               MC = 0,
                               max_threads = 0)
    
    write.table(model_inputs, file.path(input$file_path, "Model Inputs.txt"),
                sep = "\t", row.names = FALSE, quote = FALSE)
  })
  
    
  #Channel network inputs
  observeEvent(input$save_network, {
    write.table(matrix(unlist(input$link$data), nrow = 2, byrow = TRUE),
                file.path(input$file_path, "Input link.txt"), sep = " ",
                col.names = FALSE, row.names = FALSE)
    
    slope <- hot_to_r(input$slope)
    length <- hot_to_r(input$length)
    link <- matrix(unlist(input$link$data), nrow = 2, byrow = TRUE)
    
    ds_z <- 10 #bed elevation of "ghost" node at DS 
    
    z <- rep(ds_z, input$n_reaches + 1)
    z[input$n_reaches] <- ds_z + slope[input$n_reaches, 1] * length[input$n_reaches, 1]
    for (i in (input$n_reaches - 1):1){
      index <- which(i == link, arr.ind = TRUE)[2]
      z[i] <- z[index] + slope[i, 1] * length[i, 1]
    }
    
    write.table(z, file.path(input$file_path, "Input z.txt"),
                         sep = "\n", col.names = FALSE, row.names = FALSE)
    
    write.table(length, file.path(input$file_path, "Input length.txt"),
                sep = "\n", col.names = FALSE, row.names = FALSE)
    
    write.table(hot_to_r(input$bed_cohesive), file.path(input$file_path, "Input bed cohesive.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
  })
    
  #Channel geometry inputs
  observeEvent(input$save_geom, {
    write.table(hot_to_r(input$width), file.path(input$file_path, "Input width.txt"),
                sep = "\n", col.names = FALSE, row.names = FALSE)
    write.table(hot_to_r(input$bank_geom), file.path(input$file_path, "Input RB geometry.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
    write.table(hot_to_r(input$bank_geom), file.path(input$file_path, "Input LB geometry.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
    write.table(hot_to_r(input$fp_geom), file.path(input$file_path, "Input fp geometry.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
    write.table(hot_to_r(input$n_val), file.path(input$file_path, "Input n values.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
    
  })
  
  #Bank soil inputs
  observeEvent(input$save_soil, {
    write.table(hot_to_r(input$bank_soil), file.path(input$file_path, "Input bank prop.txt"),
                sep = " ", col.names = FALSE, row.names = FALSE)
  })
  
  #Meandering and knickpoint outputs
  observeEvent(input$save_meander, {
    if (input$meandering == 1){
      write.table(hot_to_r(input$meander_inputs), file.path(input$file_path, "Input meandering.txt"),
                  sep = " ", col.names = FALSE, row.names = FALSE)
    }
    
    if (input$n_knicks > 0){
      write.table(hot_to_r(input$knickpoints), file.path(input$file_path, "Input knickpoint.txt"),
                  sep = " ", col.names = FALSE, row.names = FALSE)
    }
  })
  
  output$link <- renderRHandsontable({
    if (input$n_reaches == 1){
      link <- matrix(c(2, 2), nrow = 2)
    }else{
      link <- matrix(c(input$n_reaches + 1, 1:(input$n_reaches - 1), 
                       rep(1 + input$n_reaches, input$n_reaches)), nrow = 2, byrow = TRUE)
    }
    rhandsontable(data = link,
                  colHeaders = 1:input$n_reaches)
  })
  
  observeEvent(input$plot_network, {
    
    output$network_plot <- renderPlot({
      plot_network(input$n_reaches, matrix(unlist(input$link$data), nrow = 2, byrow = TRUE))
      
    }, width = 300, height = 300)
  })
  
  output$slope <- renderRHandsontable({
    rhandsontable(data = data.frame(slope = rep(0, input$n_reaches)),
                  colHeaders = "Initial Slope [m/m]")
  })
  
  output$length <- renderRHandsontable({
    rhandsontable(data = data.frame(L = rep(0, input$n_reaches)),
                  colHeaders = "Reach Lengths [m]")
  })
  
  output$bed_cohesive <- renderRHandsontable({
    rhandsontable(data = data.frame(depth = rep(50, input$n_reaches),
                                    tau_c = rep(99, input$n_reaches)),
                  colHeaders = c("Depth to Cohesive Layer [m]",
                                 "Cohesive Layer tau_c [Pa]"))
  })
  
  output$width <- renderRHandsontable({
    rhandsontable(data = as.data.frame(matrix(rep(0, input$n_reaches), 
                                nrow = 1)))
  })
  
  output$bank_geom <- renderRHandsontable({
    rhandsontable(data = data.frame(height = rep(1, input$n_reaches),
                                    toe_height = rep(0.5, input$n_reaches),
                                    angle = rep(45, input$n_reaches),
                                    toe_angle = rep(45, input$n_reaches)),
                  colHeaders = c("Bank height [m]",
                                 "Toe height [m]",
                                 "Bank angle [degrees]",
                                 "Toe angle [degrees]"),
                  rowHeaders = 1:input$n_reaches)
  })
  
  output$fp_geom <- renderRHandsontable({
    rhandsontable(data = data.frame(angle = rep(0, input$n_reaches),
                                    left_width = rep(50, input$n_reaches),
                                    right_width = rep(50, input$n_reaches)),
                  colHeaders = c("Floodplain angle [degrees]",
                                 "Left fp width [m]",
                                 "Right fp width [m]"),
                  rowHeaders = 1:input$n_reaches)
  })
  
  output$n_val <- renderRHandsontable({
    rhandsontable(data = data.frame(chnl_n = rep(0.03, input$n_reaches),
                                    fp_n = rep(0.06, input$n_reaches)),
                  colHeaders = c("Channel n",
                                 "Floodplain n"),
                  rowHeaders = 1:input$n_reaches)
  })
  
  observeEvent(input$save_Ds, {
    Ds <- input$single_Ds / 1000
    ps <- rep(1, input$n_reaches)
    
    write.table(Ds, file.path(input$file_path, "Input Ds.txt"), sep = "\n",
                col.names = FALSE, row.names = FALSE)
    
    write.table(ps, file.path(input$file_path, "Input ps.txt"), sep = "\n",
                col.names = FALSE, row.names = FALSE)
  })
  
  #Ds_init <- reactive({matrix(1:input$n_Ds, nrow = 1)})
  Ds_init <- reactive({data.frame(Ds = 1:input$n_Ds)})
  Ds_new <- reactive({
    if(is.null(input$Ds)){
      return(Ds_init())
    }else if (!identical(Ds_init(), input$Ds)){
      df <- hot_to_r(input$Ds)
      if (input$n_Ds > nrow(df)){
        df[(nrow(df) + 1):input$n_Ds,] <- 0.0
      }else if (input$n_Ds < nrow(df)){
        df <- df[1:input$n_Ds, , drop = FALSE]
      }
      return(df)
    }
  })
  
  output$Ds <- renderRHandsontable({
    rhandsontable(Ds_new())
  })
  
  output$gsd <- renderRHandsontable({
    rhandsontable(data = data.frame(D50 = rep(0, input$n_reaches), 
                                Spread = rep(0, input$n_reaches)),
                  rowHeaders = 1:input$n_reaches)
  })
  
  #vals <- reactive({Ds})
  observeEvent(input$create_GSD, {
    GSD <- hot_to_r(input$gsd)
    Ds <- isolate(hot_to_r(input$Ds))

    ps <- t(apply(GSD, 1, function(x, Ds){
      gsd_maker(x[1], x[2], Ds, plot = FALSE)
    }, as.numeric(Ds$Ds)))
    
    write.table(Ds, file.path(input$file_path, "Input Ds.txt"),
               col.names = FALSE, row.names = FALSE, sep = "\n")
    write.table(ps, file.path(input$file_path, "Input ps.txt"),
                col.names = FALSE, row.names = FALSE, sep = " ")
    
    output$GSD_plot <- renderPlot({
      xlim <- range(as.numeric(Ds$Ds))
      plot(NA, xlim = xlim, ylim = c(0, 1), las = 1,
           xlab = "Grain size [mm]", ylab = "% Finer")
      cols <- cRamp_legend(nrow(ps), "viridis")
      for (i in 1:nrow(ps)){
        lines(as.numeric(Ds$Ds), cumsum(ps[i,]), col = cols[i], lwd = 2)
      }
      legend("topleft", legend = 1:nrow(ps), lwd = 2, col = cols, bty = "n")
      
    })
    
  })
  
  output$Q_const <- renderRHandsontable({
    rhandsontable(data = data.frame(Q = rep(0, input$n_reaches)),
                  rowHeaders = 1:input$n_reaches)
  })
  output$sed_supply1 <- renderRHandsontable({
    rhandsontable(data = data.frame(sed_supply = rep(0, input$n_reaches)),
                  rowHeaders = 1:input$n_reaches)
  })
  
  observeEvent(input$save_discharge1, {
    
    Q_matrix <- sapply(unlist(input$Q_const$data), rep, input$n_Q)
    
    write.table(Q_matrix, file.path(input$file_path, "Input Q.txt"), col.names = FALSE,
                row.names = FALSE, sep = " ")
    
    sed_supply <- sapply(unlist(input$sed_supply1$data), rep, input$n_Q)
    
    write.table(sed_supply, file.path(input$file_path, "Input sed supply.txt"), col.names = FALSE,
                row.names = FALSE, sep = " ")
  })
    
  output$DA <- renderRHandsontable({
    rhandsontable(data = data.frame(DA = rep(0, input$n_reaches)),
                  rowHeaders = 1:input$n_reaches)
  })
  output$sed_supply2 <- renderRHandsontable({
    rhandsontable(data = data.frame(sed_supply = rep(0, input$n_reaches)),
                  rowHeaders = 1:input$n_reaches)
  })
  
  observeEvent(input$save_discharge2, {
    Q <- readNWISdv(siteNumbers = input$USGS_gage, parameterCd = "00060", startDate = input$start_date,
                    endDate = input$end_date) %>%
      mutate(Q = X_00060_00003 / 35.3) %>%
      select(Q, Date)
    
    gage_DA <- readNWISsite(siteNumbers = input$USGS_gage) %>%
      select(DA = drain_area_va) %>%
      mutate(DA = DA * 2.5899) %>%
      as.numeric()
    
    Q_matrix <- matrix(rep(0, nrow(Q) * input$n_reaches), ncol = input$n_reaches)
    for (i in 1:ncol(Q_matrix)){
      Q_matrix[,i] <- Q$Q * unlist(input$DA$data)[i] / gage_DA
    }
    
    write.table(Q_matrix, file.path(input$file_path, "Input Q.txt"), col.names = FALSE,
                row.names = FALSE, sep = " ")
    
    output$Q_plot <- renderPlot({
      plot(Q ~ Date, Q, las = 1,
           xlab = "Time", ylab = "Q [cms]", type = "l")

    })
  })
  
  output$bank_soil <- renderRHandsontable({
    bank_soil <- data.frame(tau_c = rep(5, input$n_reaches),
                            k = rep(0, input$n_reaches),
                            cohesion_bank = rep(1, input$n_reaches),
                            cohesion_toe = rep(1, input$n_reaches),
                            phi_bank = rep(30, input$n_reaches),
                            phi_toe = rep(30, input$n_reaches),
                            weight_bank = rep(18, input$n_reaches),
                            weight_toe = rep(18, input$n_reaches),
                            P_bank = rep(350, input$n_reaches),
                            P_bed = rep(0, input$n_reaches),
                            bedload_bank = rep(0.5, input$n_reaches),
                            bedload_bed = rep(0.5, input$n_reaches))
    rhandsontable(data = bank_soil,
                  rowHeaders = 1:input$n_reaches)
  })
  
  output$meander_inputs <- renderRHandsontable({
    meander <- data.frame(Sinuosity = rep(1.5, input$n_reaches),
                          Rc = rep(40, input$n_reaches))
    rhandsontable(data = meander,
                  rowHeaders = 1:input$n_reaches)
  })
  
  output$knickpoints <- renderRHandsontable({
    knicks <- data.frame(reach = rep(1, input$n_knicks),
                         DS_distance = rep(100, input$n_knicks),
                         Elev = rep(0, input$n_knicks),
                         Height = rep(1, input$n_knicks),
                         Erodibility = rep(0.24, input$n_knicks))
    rhandsontable(data = knicks,
                  rowHeaders = 1:input$n_knicks)
  })
  
  ##########################################################
  #Run Model
  observeEvent(input$run_model, {
    system("cmd.exe", input = c(paste("cd", path.expand(input$file_path)),
                                "REMv0.1.0.exe"), invisible = FALSE, minimized = TRUE)
    
    # observe({ 
    #   ## read the text file once every 50 ms
    #   invalidateLater(50, session)
    #   req(file.exists(file.path(input$file_path, "test.txt"))) 
    #   txt <- paste(readLines(file.path(input$file_path, "test.txt"), warn = FALSE), collapse = "\n") 
    #   output$text <- renderText(txt) 
    # }) 
  })
  ###########################################################
  
  ############################################################
  #Plot Outputs
  observeEvent(input$dz_plot_button, {
    output$dz_plot <- renderPlot({
      dz_plot(path = input$file_path)
    }, width = 4 * 72, height = 4.5 * 72)
  })

  observeEvent(input$width_plot_button, {
    output$width_plot <- renderPlot({
      width_plot(path = input$file_path)
    }, width = 4 * 72, height = 4.5 * 72)
  })
  
  observeEvent(input$network_plot_button, {
    output$output_network_plot <- renderPlot({
      network_XS_plot(path = input$file_path, years = c(0.05, 0.1, 0.15, 0.2, 0.3))
      #plot(1, 1)
    }, width = 6.5 * 72, height = 4.5 * 72)
  })
  
  #XS Plots
  
  observeEvent(input$XS_plot_button, {
    reach <- as.numeric(unlist(strsplit(input$XS_reach, ",")))
    XS <- as.numeric(unlist(strsplit(input$XS_numbers, ",")))
    n_plots <- ceiling(length(reach) / 2)
    
    # Insert the right number of plot output objects into the web page
    output$XS_plot <- renderUI({
      plot_output_list <- lapply(1:n_plots, function(i) {
        plotname <- paste("XS_plot", i, sep="")
        plotOutput(plotname, height = 3 * 72, width = 6.5 * 72)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:n_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("XS_plot", my_i, sep="")
        reach_input <- reach[(2 * i - 1):(2 * i)]
        reach_input <- reach_input[!is.na(reach_input)]
        XS_input <- XS[(2 * i - 1):(2 * i)]
        XS_input <- XS_input[!is.na(XS_input)]
        
        output[[plotname]] <- renderPlot({
          XS_plots2(path = input$file_path, reach = reach_input, XS = XS_input)
        })
      })
    }
  #   output$XS_plot <- renderPlot({
  #     XS_plots2(path = input$file_path, reach = reach, XS = XS)
  #     #plot(1, 1)
  #   }, width = 6.5 * 72, height = 3.5 * 72)
  })
  
  #D50 plot
  observeEvent(input$D50_plot_button, {
    n_reaches <- read.table(file.path(input$file_path, "Output bank loading.txt")) %>%
      ncol() / 2
    n_plots <- ceiling(n_reaches / 2)
    
    # Insert the right number of plot output objects into the web page
    output$D50_plot <- renderUI({
      plot_output_list <- lapply(1:n_plots, function(i) {
        plotname <- paste("D50_plot", i, sep="")
        plotOutput(plotname, height = 5 * 72, width = 6.5 * 72)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:n_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("D50_plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          D50_plot(path = input$file_path)
        })
      })
    }
  })
  
  #Pollutant loading
  observeEvent(input$pollutant_plot_button, {
    type <- input$pollutant_type
    
    output$pollutant_plot <- renderPlot({
      pollutant_loading(path = input$file_path, type = type)
    }, width = 6.5 * 72, height = 4.5 * 72)
  })
  
  observeEvent(input$pollutant_plot2_button, {
    type <- input$pollutant_type2
    units <- input$pollutant_units
    
    output$pollutant_plot2 <- renderPlot({
      reach_loads(path = input$file_path, type = type, units = units)
    }, width = 4.5 * 72, height = 4.5 * 72)
  })
}