
# Define UI for web application

shinyUI(navbarPage(theme = "corp-styles.css", 
                   title = div(img(src = "MouseConnectogram.png", height = '30px', hspace ='30'),
                               ""),
                   position = c("static-top"), windowTitle = "Time Series Feature Exploration",
                   id = "page_tab",
                   
                   #------------------ Home page -----------------
                   
                   tabPanel(navtab0,
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "corp-styles.css")
                            ),
                            
                            fluidRow(style = "background-color: white; padding-top: 0", 
                                     HTML("
                                          <center>
                                          <img src= 'MouseConnectogram.png', height = '175px'>
                                          </center>
                                          "  
                                     )
                            ),
                            
                            p(),
                            fluidRow(
                              column(2),
                              column(8, style = "min-height: 150px;",
                                     HTML(home_page)
                              ),
                              column(2)
                            ),
                            
                            # User input box
                            
                            fluidRow(
                              column(5),
                              column(4,
                                     fileInput("userUpload", HTML("Upload your time series dataset"),
                                               multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls",
                                                                            width = "600px"))
                                     ),
                              column(5)
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
                   fluidRow(style = "height: 50px; background-color: #f1f1f1;")
                   
  )
)
