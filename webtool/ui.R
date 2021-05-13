
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
                                     column(2)
                                     )
                            
                   ),
                   
                   #------------------ Low dim page --------------
                   
                   tabPanel(navtab1,
                            fluidRow(h1("Low Dimension Visualisation"))),
                   
                   #------------------ Classifier page -----------
                   
                   tabPanel(navtab2,
                            fluidRow(h1("Classification Performance"))),
                   
                   #------------------ Quality page --------------
                   
                   tabPanel(navtab3,
                            fluidRow(h1("Feature Calculation Quality"))),
                   
                   #------------------ Matrix page ---------------
                   
                   tabPanel(navtab4,
                            fluidRow(h1("Feature Matrix Visualisation"))),
                   
                   #------------------ About page ----------------
                   
                   tabPanel(navtab5,
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #003f5c; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px;")
                   
  )
)
