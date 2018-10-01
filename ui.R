library(shiny)
library(shinyBS)
library(rhandsontable)
library(dataRetrieval)
library(dplyr)
library(REMvisualizer)
source("GSD_maker.R")
source("Plot Functions.R")
source("Additional Functions.R")


# Define UI for REM app
ui <- navbarPage("River Erosion Model",
        
        #tags$head(tags$style(".button{float:center}")),
        
        #HOME
        tabPanel("Home",
                 h3("Model Description"),
                 p(HTML(paste("Welcome to the River Erosion Model. This application is
                   meant to help you set up simple model simulations and
                   display the output. All inputs can be further customized.
                   See the REM", a(href='REM_User_Guide.html', target='blank', 'User Guide')))),
                 
                 #tags$a(href='REM_User_Guide.html', target='blank', 'User Guide'),
                 
                 img(src = "Network Drawing.png", align = "center", height = 5 * 72),
                 img(src = "XS Drawing.png", align = "center", height = 5 * 72)
        ),
        
        #INPUTS
        navbarMenu("Inputs",
        # Main panel for getting user inputs
          tabPanel("Basic Inputs",
          
          # Get file path
          fluidRow(
            column(12, align = "center",
                   textInput("file_path", h3("Input file path:",
                                             bsButton("q1", label = "",
                                                      icon = icon("question"),
                                                      style = "info",
                                                      size = "extra-small")),
                             value = "~/WORK"),
                   bsPopover(id = "q1",
                             title = "File Path",
                             content = "Enter the file path where the model inputs and outputs will be saved.",
                             placement = "right",
                             trigger = "hover"))
          ),
          
          br(),
          
          fluidRow(
            column(12, align = "center",
                   actionButton("load_inputs", "Load Inputs from Previous Simulation",
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   bsButton("q19", label = "",
                            icon = icon("question"),
                            style = "info",
                            size = "extra-small"),
            bsPopover(id = "q19",
                      title = "Load Inputs",
                      content = "Loads input from specified file path. Does not work for spread of grain size distribution or non-constant discharge input.",
                      placement = "right",
                      trigger = "hover"))
          ),
          
          br(),
          
          #Get dx, dt, dt_output
          fluidRow(
            column(4,
                   numericInput("dx", h3("Input XS spacing [m]:"),
                                value = 100)),
            
            column(4,
                   numericInput("dt", h3("Input model time step [s]:",
                                         bsButton("q2", label = "",
                                                  icon = icon("question"),
                                                  style = "info",
                                                  size = "extra-small")),
                                value = 2400),
                   bsPopover(id = "q2",
                             title = "Time Step",
                             content = "Model stability is sensitive to the time step chosen. Must be a whole fraction of the discharge time step (e.g. 86400/2400 = 36)",
                             placement = "right",
                             trigger = "hover")),
            
            column(4,
                   numericInput("dt_output", 
                                h3("Input output time step [# of discharge time steps]:",
                                   bsButton("q3", label = "",
                                            icon = icon("question"),
                                            style = "info",
                                            size = "extra-small")),
                                value = 73),
                   bsPopover(id = "q3",
                             title = "Output Time Step",
                             content = "Model outputs printed every \"X\" discharge time steps.",
                             placement = "left",
                             trigger = "hover"))
          ),
          
          #Input sed transport type, bank_erosion, input_type, meandering
          fluidRow(
            column(4,
                   selectInput("input_type", h3("Bed profile input type:",
                                                bsButton("q4", label = "",
                                                         icon = icon("question"),
                                                         style = "info",
                                                         size = "extra-small")),
                               choices = list("Reach" = "reach", 
                                              "Profile" = "profile"),
                               selected = "Reach"),
                   bsPopover(id = "q4",
                             title = "Input Type",
                             content = "User interface currently can only do \"Reach\" type inputs.",
                             placement = "right",
                             trigger = "hover")),
            
            column(4,
                   selectInput("transport_type", h3("Sediment transport eq:"),
                               choices = list("Bedload" = "bedload",
                                              "Total load" = "total",
                                              "Both" = "both",
                                              "Eaton and Church (2011)" = "EC",
                                              "Parker et al. (2011)" = "Parker"),
                               selected = "Bedload")),
            
            column(4,
                   selectInput("bank_erosion", h3("Bank erosion type:"),
                               choices = list("None" = "none",
                                              "Fluvial only" = "fluvial",
                                              "Failure only" = "failure",
                                              "Both" = "both"),
                               selected = "None"))
                   
          ),
          
          br(),
          br(),
          
          fluidRow(
            column(12, align = "center",
                   actionButton("save_inputs", "Save Inputs",
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          )
      ),
      
      #Next page - watershed/network inputs
      tabPanel("Watershed Network Inputs",
               fluidRow(
                 column(12, align = "center",
                        numericInput("n_reaches", h3("Enter number of reaches:"),
                                    value = 2))
               ),
               
               #Input bed_z, length, and bed cohesive
               fluidRow(
                 column(3,
                        h3("Input Channel Slopes [m/m]"),
                        rHandsontableOutput("slope")),
                 column(3,
                        h3("Input Reach Lengths [m]"),
                        rHandsontableOutput("length")),
                 column(6,
                        h3("Input Cohesive Bed Inputs",
                           bsButton("q6", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q6",
                                  title = "Cohesive Bed",
                                  content = "REM can have a cohesive (e.g. clay) bed under alluvium. To not include this option, make depth to cohesive large (e.g. 50 m).",
                                  placement = "left",
                                  trigger = "hover"),
                        rHandsontableOutput("bed_cohesive"))
               ),
               br(),
               
               #Input link matrix
               fluidRow(
                 column(3),
                 column(6, 
                        h3("Input \"Link\" Matrix Specifying Network Geometry",
                           bsButton("q5", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q5",
                                  title = "Link Matrix",
                                  content = "See User Guide for details on constructing network structure.",
                                  placement = "right",
                                  trigger = "hover"),
                        rHandsontableOutput("link")),
                 column(3)
               ),
               
               #Line breaks
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("plot_network", "Show Network",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               fluidRow(
                 
                 column(12, align = "center",
                        plotOutput("network_plot"))
               ),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_network", "Save Network Inputs",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               br(),
               br()
               
      ),
      
      #Next page - channel geometry inputs
      tabPanel("Channel Geometry Inputs",
               fluidRow(
                 column(12,
                        h3("Input channel bottom widths [m]"),
                        rHandsontableOutput("width"))
               ),
               fluidRow(
                 column(12,
                        h3("Input bank geometry",
                           bsButton("q7", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q7",
                                  title = "Bank Geometry",
                                  content = "REM can have different geometry for the left and right banks but this user interface assumes they are the same.",
                                  placement = "right",
                                  trigger = "hover"),
                        rHandsontableOutput("bank_geom"))
               ),
               
               fluidRow(
                 column(8,
                        h3("Input floodplain geometry"),
                        rHandsontableOutput("fp_geom")),
                 column(4,
                        h3("Input Manning's n values"),
                        rHandsontableOutput("n_val"))
               ),
               
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_geom", "Save Geometry Inputs",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               br(),
               br()
               
      ),
      
      #Next page - grain size distribution
      tabPanel("Grain Size Distribution Inputs",
               
               h3("Input single grain size", align = "center",
                  bsButton("q8", label = "",
                           icon = icon("question"),
                           style = "info",
                           size = "extra-small")),
               bsPopover(id = "q8",
                         title = "Grain Size",
                         content = "Specify either a single grain size for all reaches (here) or grain size distribution for each reach (below).",
                         placement = "right",
                         trigger = "hover"),
               
               fluidRow(
                 column(12, align = "center",
                        numericInput("single_Ds", "Input grain size [mm]", 2))
               ),
               
               br(),
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_Ds", "Save single grain size",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               br(),
               br(),
               
               h3("Input grain size distribution", align = "center"),
               
               fluidRow(
                  column(4,
                         numericInput("n_Ds", label = "Input number of grain sizes:",
                                      value = 2)),
                  
                  column(4,
                         numericInput("omega_c", label = "Input hiding function coefficient:",
                                      value = 0.1)),
                  
                  column(4,
                         numericInput("b", label = "Input hiding function exponent:",
                                      value = -0.8))),
               
               fluidRow(
                 column(6, 
                        h3("Input grain size values [mm]",
                           bsButton("q12", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q12",
                                  title = "Grain Sizes",
                                  content = "Enter a single set of grain sizes for the whole watershed.",
                                  placement = "right",
                                  trigger = "hover"),
                        rHandsontableOutput("Ds")),
                 
                 column(6, 
                        h3("Input grain size parameters",
                           bsButton("q13", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q13",
                                  title = "Grain Sizes",
                                  content = "Each reach can have a unique grain size distribution based on supplied D50 and spread. The spread is the standard deviation of a lognormal distribution used to create the gSD.",
                                  placement = "left",
                                  trigger = "hover"),
                        rHandsontableOutput("gsd"))
               ),
               
               br(),
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("create_GSD", "Save GSD",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               fluidRow(
                 column(8, plotOutput("GSD_plot"))
               )
        ),
      
      #Discharge and sediment supply
      tabPanel("Discharge and Sediment Supply",
               
               h3("Constant Discharge by Reach", align = "center",
                  bsButton("q9", label = "",
                           icon = icon("question"),
                           style = "info",
                           size = "extra-small")),
               bsPopover(id = "q9",
                         title = "Discharge",
                         content = "Specify either a constant discharge by reach (here) or supply a USGS gage and discharges for each reach will be scaled based on drainage areas (below).",
                         placement = "right",
                         trigger = "hover"),
        
               fluidRow(

                 column(6, 
                        numericInput("n_Q", "Enter number of discharge time steps:",
                                     100)),
                 
                 column(6, 
                        numericInput("dt_Q", "Enter discharge time step [s]:",
                                     86400))
               ),
               
               fluidRow(
                 column(6,
                        h3("Input Discharge by Reach [cms]"),
                           rHandsontableOutput("Q_const")),
                 column(6,
                        h3("Input Reach Sediment Supply [-1-0]",
                           bsButton("q10", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q10",
                                  title = "Sediment Supply",
                                  content = "Sediment supply is given as a fraction of transport capacity (-1 = 100% to 0 = 0%)",
                                  placement = "left",
                                  trigger = "hover"),
                           rHandsontableOutput("sed_supply1"))
               ),
               
               br(),
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_discharge1", "Save Constant Discharge and Sed Supply",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               
               br(),
               br(),
               
               h3("Discharge from USGS Gage", align = "center"),
               
               fluidRow(
                 column(4, align = "center",
                        textInput("USGS_gage", "Enter USGS gage ID:")),
                 column(4, align = "center",
                        dateInput("start_date", "Enter Start Date")),
                 column(4, align = "center",
                        dateInput("end_date", "Enter End Date"))

               ),
               
               fluidRow(
                 column(6,
                        h3("Input Reach Drainage Areas [sq. km]"),
                        rHandsontableOutput("DA")),
                 column(6,
                        h3("Input Reach Sediment Supply [-1-0]"),
                        rHandsontableOutput("sed_supply2"))
               ),
               
               br(),
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_discharge2", "Save Discharge and Sed Supply",
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
      
              fluidRow(
                column(8, plotOutput("Q_plot"))
              )
      ),
      
      #Bank soil properties
      tabPanel("Bank Soil Properties",
               fluidRow(
                 column(12,
                        h3("Input Bank Soil Properties:",
                           bsButton("q11", label = "",
                                    icon = icon("question"),
                                    style = "info",
                                    size = "extra-small")),
                        bsPopover(id = "q11",
                                  title = "Bank Soil Properties",
                                  content = "Enter bank erosion resistance, weight, pollution concentration, etc. See User Guide for more details.",
                                  placement = "left",
                                  trigger = "hover"),
                        rHandsontableOutput("bank_soil"))
               ),
               
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        actionButton("save_soil", "Save Bank Soil Inputs",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ),
               br(),
               br()
              
              ),
      
      #Meandering and knickpoints
      tabPanel("Meandering and Knickpoints",
               fluidRow(
                 column(6,
                        selectInput("meandering", h3("Include meandering?"),
                                    choices = list("No" = 0,
                                                   "Yes" = 1),
                                    selected = "No")),
                 
                 column(6, 
                        numericInput("n_knicks", h3("Enter number of knickpoints:"), 0)
                        )
                 
               ),
               
               fluidRow(
                 column(6, 
                        conditionalPanel(condition = "input.meandering == 1",
                          h3("Input Initial Meander Geometry:",
                             bsButton("q17", label = "",
                                      icon = icon("question"),
                                      style = "info",
                                      size = "extra-small")),
                          bsPopover(id = "q17",
                                    title = "Meander Geometry",
                                    content = "Sinuosity and average bend radius of curvature (m) for each reach.",
                                    placement = "right",
                                    trigger = "hover"),
                          rHandsontableOutput("meander_inputs"))),
                 
                 column(6,
                        conditionalPanel(condition = "input.n_knicks > 0",
                          h3("Input Knickpoints:",
                             bsButton("q18", label = "",
                                      icon = icon("question"),
                                      style = "info",
                                      size = "extra-small")),
                          bsPopover(id = "q18",
                                    title = "Knickpoints",
                                    content = "Input reach of knickpoint, downstream distance from top of reach (m), elevation of the top of the knickoint (m), knickpoint height (m), and erodiblity (cm/Pa/hr).",
                                    placement = "right",
                                    trigger = "hover"),
                          rHandsontableOutput("knickpoints")))
               ),
               
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        conditionalPanel(condition = "input.meandering == 1 || input.n_knicks > 0",
                        actionButton("save_meander", "Save Meandering and Knickpoint Inputs",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
               )
            )
        ),
      
      #RUN MODEL
      tabPanel("Run Model",
               h3(HTML(paste("Once the inputs have all been created, press the button below to run the model. The REM",
                            a(".exe file", href = "http://github.com/rodlammers/REM"),
                  "must be saved in the same folder as the model inputs. You can also just double click that
                  file to run the model."))),
               
               fluidRow(
                  column(12, align = "center",
                         actionButton("run_model", "Run REM",
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               )
      ),
      
      #OUTPUTS
      navbarMenu("View Outputs",
                 tabPanel("Network Outputs",
                          
                          fluidRow(
                            column(6, align = "center",
                                    actionButton("dz_plot_button", "Plot Bed Elevation Changes",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),

                            column(6, align = "center",
                                   actionButton("width_plot_button", "Plot Width Changes",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),

                          br(),

                          fluidRow(
                            column(6, align = "center",
                                   plotOutput("dz_plot")),

                            column(6, align = "center",
                                   plotOutput("width_plot"))
                          ),

                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("network_plot_button", "Plot Network Changes",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),

                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   plotOutput("output_network_plot"))
                          )
                 ),
                 
                 tabPanel("XS Plots",
                          
                          fluidRow(
                            column(12, align = "center",
                                   numericInput("n_XS_plot", "Enter number of XS to plot", 1))
                          ),
                          
                          fluidRow(
                            column(3),
                            column(6, h3("Enter reach and XS numbers for each XS to plot:",
                                         bsButton("q16", label = "",
                                                  icon = icon("question"),
                                                  style = "info",
                                                  size = "extra-small")),
                                  bsPopover(id = "q16",
                                            title = "XS Plotting",
                                            content = "If the supplied reach-XS combination does not exisit (e.g. there is no XS 11 in reach 1), no plot will show.",
                                            placement = "right",
                                            trigger = "hover"),
                                  rHandsontableOutput("XS_plot_numbers")),
                            
                            column(3)
                            
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center", 
                                   h3(textOutput("XS_plot_error"), style = "color:red"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("XS_plot_button", "Plot XS Changes",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   uiOutput("XS_plot"))
                          )
                          
                 ),
                 
                 tabPanel("Grain Size",
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("D50_plot_button", "Plot D50 Changes",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   uiOutput("D50_plot"))
                          )
                          
                  ),
                 
                 tabPanel("Pollutant Loading",
                          fluidRow(
                            column(12, align = "center",
                                   selectInput("pollutant_type", label = h3("Select type of pollutant loading to show"),
                                               choices = list("Cumulative" = 1, "Daily" = 2), selected = 1))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("pollutant_plot_button", "Plot Pollutant Loading",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   plotOutput("pollutant_plot"))
                          ),

                          #Pollutant network plot
                          fluidRow(
                            column(6, align = "center",
                                   selectInput("pollutant_type2", label = h3("Select type of pollutant loading to show"),
                                               choices = list("Sediment" = "sed", "Pollutant" = "p"), selected = "sed")),
                            
                            column(6, align = "center",
                                   selectInput("pollutant_units", label = h3("Select units"),
                                               choices = list("kg", "ton", "1000 ton"), selected = "ton"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("pollutant_plot2_button", "Plot Pollutant Loading by Reach",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(12, align = "center",
                                   plotOutput("pollutant_plot2"))
                          )
                  )
      )
  
)