
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
                            
                            # Dope background
                            
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
                                     column(8,
                                            h2("Initial Dataset Upload"),
                                            p("To get started, please use the widget below to upload your datafile depending on whether you have a single file with all information or a time series file and a corresponding metadata file (e.g. ID variables, class labels). Currently accepted formats are: .csv, .xlsx, .xls, .txt. More file types will be added soon."),
                                            tabsetPanel(id = "landing_tabs",
                                              tabPanel("Single Datafile",
                                                       br(),
                                                       fluidRow(
                                                         fileInput("userUpload", HTML("Upload your time series file"),
                                                                          multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                       width = "600px"))
                                                         )
                                                       ),
                                              tabPanel("Datafile + Metadata File",
                                                       br(),
                                                       fluidRow(
                                                         fileInput("userUpload2", HTML("Upload your time series file"),
                                                                          multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                       width = "600px"))
                                                         ),
                                                       fluidRow(
                                                         fileInput("userUpload2Meta", HTML("Upload your metadata file"),
                                                                          multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                                                       width = "600px"))
                                                         )
                                                        )
                                                       )
                                                      ),
                                     column(2),
                                     column(6,
                                            fluidRow(
                                              textInput("input_id_var", "Enter the exact name of the unique ID variable that identifies each time series",
                                                        value = "Enter the ID variable name...")
                                            ),
                                            fluidRow(
                                              textInput("input_group_var", "Enter the exact name of the grouping variable if one exists",
                                                        value = "Enter the group variable name...")
                                            ),
                                            fluidRow(
                                              textInput("input_time_var", "If your data is in long (tidy) format, enter the exact name of the variable specifying the time index",
                                                        value = "Enter the time variable name...")
                                            ),
                                            fluidRow(
                                              textInput("input_values_var", "If your data is in long (tidy) format, enter the exact name of the variable specifying the values",
                                                        value = "Enter the ID variable name...")
                                            )
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
                                br(),
                                selectInput("inputScaler", "Select a rescaling function to apply prior to performing principal components analysis",
                                            choices = all_scalers, selected = all_scalers[1], multiple = FALSE)
                              ),
                              mainPanel(fluidRow(
                                h3("Low Dimensional Plot"),
                                shinycssloaders::withSpinner(plotlyOutput("low_dim_plot", height = "800px"))
                              ),
                              fluidRow(
                                h3("Raw Time Series"),
                                shinycssloaders::withSpinner(plotlyOutput("raw_ts_plot", height = "300px"))
                               )
                              )
                             )
                            ),
                   
                   #------------------ Classifier page -----------
                   
                   tabPanel(navtab2,
                            fluidRow(h1("Classification Performance"))),
                   
                   #------------------ Quality page --------------
                   
                   tabPanel(navtab3,
                            fluidRow(h1("Feature Calculation Quality")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises the quality of the calculated feature matrix by computing amounts of each data type."),
                              ),
                              mainPanel(fluidRow(
                                h3("Data Quality Plot"),
                                shinycssloaders::withSpinner(plotlyOutput("data_qual_plot", height = "300px"))
                                )
                               )
                              )
                   ),
                   
                   #------------------ Matrix page ---------------
                   
                   tabPanel(navtab4,
                            fluidRow(h1("Feature Matrix Visualisation")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises the time series feature matrix as a 'connectivity' matrix/heatmap of correlations between each unique time series' feature vectors."),
                              ),
                              mainPanel(fluidRow(
                                h3("Feature Matrix Correlation Plot"),
                                shinycssloaders::withSpinner(plotlyOutput("feat_mat_plot", height = "300px"))
                              )
                             )
                            )
                   ),
                   
                   #------------------ About page ----------------
                   
                   tabPanel(navtab5,
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #003f5c; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
                   
  )
)
