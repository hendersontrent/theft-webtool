
# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "Fulcher_Ben_MonashUniversity.png", height = '30px', hspace = '30'),
                               ""),
                   position = c("static-top"), windowTitle = "Time Series Feature Exploration",
                   id = "page_tab",
                   
                   #------------------ Home page -----------------
                   
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            # Nice lab group background
                            
                            setBackgroundImage(src = "Fulcher_Ben_MonashUniversity_trans2.png"),
                            
                            # Intro text
                            
                            fluidRow(
                              column(2),
                              column(8, style = "min-height: 150px;",
                                     HTML(home_page)
                              ),
                              column(2)
                            ),
                            
                            # Dataset upload
                            
                            fluidRow(
                                     column(2),
                                     column(11,
                                            h3("Initial Dataset Upload"),
                                            p("To get started, please use the widget below to upload your datafile depending on whether you have a single file with all information (e.g. a long or 'tidy' format) or a wide time series file and a corresponding metadata file (e.g. ID variables, class labels). Currently accepted formats are: .csv, .xlsx, .xls, .txt. More file types will be added soon. Please ensure there are no spaces in your variable names prior to uploading."),
                                            
                                            # Feature set selection
                                            
                                            radioButtons("feature_set", "Select a feature set to use", 
                                                                  choices = featuresets, selected = featuresets[1], inline = TRUE),
                                            
                                            # Data uploads
                                            
                                            tabsetPanel(id = "landing_tabs",
                                              tabPanel("Single Long Datafile",
                                                       br(),
                                                       fluidRow(
                                                         column(2,
                                                           fileInput("userUpload", HTML("Upload your time series file"),
                                                                            multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                         width = "600px"))
                                                          )
                                                         ),
                                                       fluidRow(
                                                         column(2,
                                                                textInput("input_id_var", "Enter the exact name of the unique ID variable that identifies each time series",
                                                                          value = "Enter the ID variable name...")),
                                                         column(1),
                                                         column(2,
                                                                textInput("input_group_var", "Enter the exact name of the grouping variable if one exists",
                                                                          value = "Enter the group variable name...")),
                                                         column(1),
                                                         column(2,
                                                                textInput("input_time_var", "If your data is in long (tidy) format, enter the exact name of the variable specifying the time index",
                                                                          value = "Enter the time variable name..."))
                                                       ),
                                                       fluidRow(
                                                         column(2,
                                                                textInput("input_values_var", "If your data is in long (tidy) format, enter the exact name of the variable specifying the values",
                                                                          value = "Enter the values variable name..."))
                                                        )
                                                       ),
                                              tabPanel("Wide Datafile + Metadata File",
                                                       br(),
                                                       p("Please ensure the number of rows in your metadata file corresponds to the number of rows in the time-series data. Note that the data format expected in the time-series file is only such that each column is a timepoint."),
                                                       fluidRow(
                                                         column(2,
                                                           fileInput("userUpload2", HTML("Upload your time series file"),
                                                                            multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                         width = "600px"))
                                                          )
                                                         ),
                                                       fluidRow(
                                                         column(2,
                                                         fileInput("userUpload2Meta", HTML("Upload your metadata file"),
                                                                          multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                       width = "600px"))
                                                          )
                                                         ),
                                                       fluidRow(
                                                         column(2,
                                                                textInput("input_id_var_multi", "Enter the exact name of the unique ID variable that identifies each time series",
                                                                          value = "Enter the ID variable name...")),
                                                         column(1),
                                                         column(2,
                                                                textInput("input_group_var_multi", "Enter the exact name of the grouping variable if one exists",
                                                                          value = "Enter the group variable name..."))
                                                         )
                                                        )
                                                       )
                                                      )
                                                     ),
                            fluidRow(
                              column(2),
                              column(11,
                                     h3("Run Calculations"),
                                     p("After making selections and uploading data using the controls above, click the button below to run calculations."),
                                     actionButton("run", "Run Calculations")
                             )
                            )
                           ),
                   
                   #------------------ Low dim page --------------
                   
                   tabPanel(navtab1,
                            fluidRow(h1("Low Dimension Visualisation")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises the time series features in a low-dimensional representation."),
                                h3("Plotting Controls"),
                                selectInput("inputScaler", "Select a rescaling function to apply prior to performing dimension reduction",
                                            choices = all_scalers, selected = all_scalers[1], multiple = FALSE),
                                br(),
                                radioButtons("low_dimSelect", "Select a low dimension method to use", 
                                             choices = lowdims, selected = lowdims[1], inline = TRUE),
                                br(),
                                sliderInput("perplexitySlider", "If using t-SNE, select a perplexity hyperparameter value",
                                            min = 2, max = 100, value = 30),
                                br(),
                                selectInput("selectID", "Select a unique time-series ID to explore further",
                                            choices = c("None"), selected = "None", multiple = FALSE),
                                br(),
                                radioButtons("pca_highlighter", "Do you want to highlight this specific ID on the low dimension plot?", 
                                             choices = binaries, selected = binaries[1], inline = TRUE),
                                hr(),
                                h3("Download Controls"),
                                p("The button below downloads the calculated features as a tidy formatted .csv file."),
                                downloadButton("feature_download", "Download Calculated Feature File")
                              ),
                              mainPanel(fluidRow(
                                column(8,
                                  h3("Low Dimension Plot"),
                                  shinycssloaders::withSpinner(plotlyOutput("low_dim_plot", height = "500px"))
                               )
                              ),
                              fluidRow(
                                column(8,
                                  h3("Raw Time Series"),
                                  shinycssloaders::withSpinner(plotlyOutput("raw_ts_plot", height = "300px"))
                                )
                               )
                              )
                             )
                            ),
                   
                   #------------------ Quality page --------------
                   
                   tabPanel(navtab2,
                            fluidRow(h1("Feature Calculation Quality")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises the quality of the calculated feature matrix by computing amounts of each data type."),
                              ),
                              mainPanel(fluidRow(
                                column(10,
                                  h3("Data Quality Plot"),
                                  shinycssloaders::withSpinner(plotlyOutput("data_qual_plot", height = "650px"))
                       )
                      )
                     )
                    )
                   ),
                   
                   #------------------ Matrix page ---------------
                   
                   tabPanel(navtab3,
                            fluidRow(h1("Feature Matrix Visualisation")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises the time series feature matrix as an array of different heatmap data visualisations. All data is scaled and hierarchically-clustered prior to plotting."),
                                br(),
                                selectInput("inputScaler2", "Select a rescaling function to apply prior to performing correlations, clustering, and plotting.",
                                            choices = all_scalers, selected = all_scalers[2], multiple = FALSE)
                              ),
                              mainPanel(fluidRow(
                                column(10,
                                  h3("Unique ID x Unique ID Correlation Matrix"),
                                  shinycssloaders::withSpinner(plotlyOutput("feat_mat_plot", height = "500px"))
                               )
                              ),
                              fluidRow(
                                column(10,
                                       h3("Unique ID x Feature Cluster Matrix"),
                                       shinycssloaders::withSpinner(plotlyOutput("id_by_feat_plot", height = "500px"))
                       )
                      )
                     )
                    )
                   ),
                   
                   #------------------ Classifier page -----------
                   
                   tabPanel(navtab4,
                            fluidRow(h1("Classification Performance")),
                            fluidRow(p("Classification functionality coming soon."))),
                   
                   #------------------ About page ----------------
                   
                   tabPanel(navtab5,
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #003f5c; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
                   
  )
)
