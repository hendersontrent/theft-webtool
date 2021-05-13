
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
                            )
                   ),
                   
                   #------------------ Low dim page --------------
                   
                   tabPanel(navtab1),
                   
                   #------------------ Classifier page -----------
                   
                   tabPanel(navtab2),
                   
                   #------------------ Quality page --------------
                   
                   tabPanel(navtab3),
                   
                   #------------------ Matrix page ---------------
                   
                   tabPanel(navtab4),
                   
                   #------------------ About page ----------------
                   
                   tabPanel(navtab5,
                            includeMarkdown("./md/about.Rmd")
                   ),
                   
                   fluidRow(style = "height: 50px;"),
                   fluidRow(style = "height: 50px; color: white; background-color: #00264d; text-align: center;line-height: 50px;", HTML(footer)),
                   fluidRow(style = "height: 50px; background-color: #e6e6e1;")
                   
  )
)
