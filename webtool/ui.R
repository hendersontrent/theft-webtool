
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
                            
                            setBackgroundImage(src = "FlyEye_trans.png"),
                            
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
                                            p("To get started, please use the widget below to upload your datafile depending on whether you have a single file with all information (e.g. a long or 'tidy' format) or a wide time series file and a corresponding metadata file (e.g. ID variables, class labels) or a hctsa-formatted MATLAB file with the following 3 variables: 'keywords', 'labels', 'timeSeriesData'. Currently accepted formats are: .csv, .xlsx, .xls, .txt, .mat. More file types will be added soon. Please ensure there are no spaces in your variable names prior to uploading."),
                                            
                                            # Feature set selection
                                            
                                            checkboxGroupInput("feature_set", "Select a feature set (or multiple) to use. Note that tsfeatures and feasts will take considerably longer than catch22 to compute.", 
                                                               choices = featuresets, selected = featuresets[1], inline = TRUE),
                                            
                                            # Data uploads
                                            
                                            tabsetPanel(id = "landing_tabs",
                                              tabPanel("Single Long Datafile",
                                                       br(),
                                                       p("Please ensure your 'timepoint' variable is an integer or numeric. If it is a date or other format, please convert prior to uploading."),
                                                       p("Please also ensure that your variable names have no spaces in them."),
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
                                                                textInput("input_group_var", "Enter the exact name of the grouping variable if one exists. If your data has no group variable, leave this cell as-is -- do not delete the text.",
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
                                                        ),
                                              tabPanel("hctsa Formatted File",
                                                       br(),
                                                       p("Please ensure your file is formatted with the following 3 variables as per hctsa conventions: 'keywords', 'labels', 'timeSeriesData'."),
                                                       fluidRow(
                                                         column(2,
                                                                fileInput("userUpload3", HTML("Upload your time series file"),
                                                                          multiple = FALSE, accept = c(".mat", width = "600px"))
                                                         )
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
                                     actionButton("run", "Run Calculations"),
                                     br(),
                                     br(),
                                     p("After pressing the 'Run Calculations button', navigate to one of the other pages using the header at the top of the page to view results. If a message stating 'Please upload a dataset to get started.' is displayed on these other tabs, please re-click the 'Run Calculations' button.")
                             )
                            )
                           ),
                   
                   #------------------ Low dim page --------------
                   
                   tabPanel(navtab1,
                            fluidRow(h1("Low Dimensional Projection")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page projects the extracted feature matrix into a low dimensional projection."),
                                h3("Plotting Controls"),
                                selectInput("inputScaler", "Select a rescaling function to apply prior to performing dimension reduction",
                                            choices = all_scalers, selected = all_scalers[1], multiple = FALSE),
                                p("NOTE: If the selected method produces too many NA/Inf values, the application will automatically adjust to z-score."),
                                br(),
                                radioButtons("low_dimSelect", "Select a low dimension method to use", 
                                             choices = lowdims, selected = lowdims[1], inline = TRUE),
                                br(),
                                radioButtons("covarianceSlider", "If using PCA and you entered a grouping variable, do you want to show covariance ellipses?",
                                            choices = binaries, selected = binaries[2], inline = TRUE),
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
                                column(11,
                                  h3("Low Dimension Plot"),
                                  shinycssloaders::withSpinner(plotlyOutput("low_dim_plot", height = "600px"))
                               )
                              ),
                              fluidRow(
                                column(11,
                                  h3("Raw Time Series"),
                                  shinycssloaders::withSpinner(plotlyOutput("raw_ts_plot", height = "400px"))
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
                                p("This page visualises the quality of the calculated feature matrix by computing proportions of data types."),
                              ),
                              mainPanel(fluidRow(
                                column(11,
                                  h3("Data Quality Plot"),
                                  shinycssloaders::withSpinner(plotlyOutput("data_qual_plot", height = "750px"))
                       )
                      )
                     )
                    )
                   ),
                   
                   #------------------ Matrix page ---------------
                   
                   tabPanel(navtab3,
                            fluidRow(h1("Data Matrix Visualisations")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page visualises various data matrices."),
                                br(),
                                selectInput("inputScaler2", "Select a normalisation function to apply",
                                            choices = all_scalers, selected = all_scalers[3], multiple = FALSE),
                                br(),
                                selectInput("corMethod", "Select a correlation method to apply",
                                            choices = cor_methods, selected = cor_methods[1], multiple = FALSE)
                              ),
                              mainPanel(
                                tabsetPanel(id = "matrix_tabs",
                                            tabPanel("Time Series x Feature Matrix",
                                                     fluidRow(
                                column(11,
                                  shinycssloaders::withSpinner(plotlyOutput("id_by_feat_plot", height = "750px"))
                         )
                        )
                       ),
                       tabPanel("Feature x Feature Matrix",
                                fluidRow(
                                  column(11,
                                         shinycssloaders::withSpinner(plotlyOutput("feat_by_feat_plot", height = "750px"))
                                  )
                                 )
                                ),
                       tabPanel("Time Series x Time Series Matrix",
                                fluidRow(
                                  column(11,
                                         shinycssloaders::withSpinner(plotlyOutput("id_by_id_plot", height = "750px"))
                         )
                        )
                       )
                      )
                     )
                    )
                   ),
                   
                   #------------------ Classifier page -----------
                   
                   tabPanel(navtab4,
                            fluidRow(h1("Automated Time-Series Classification")),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Page Information"),
                                p("This page automates time-series classification using univariate or multivariate features as inputs to algorithms. Note that these algorithms may take a long time to run."),
                                h3("General Controls"),
                                selectInput("classifierSelect", "Select a classification algorithm",
                                            choices = classifiers, selected = classifiers[51], multiple = FALSE),
                                br(),
                                radioButtons("empiricalnullSelect", "Do you want to use an empirical null procedure to estimate p-values?", 
                                             choices = binaries, selected = binaries[1], inline = TRUE),
                                br(),
                                sliderInput("shuffleSlider", "If using empirical null, how many random class label shuffles do you want to use?",
                                            min = 1, max = 200, value = 50),
                                br(),
                                radioButtons("kfoldSelect", "Do you want to use k-fold cross-validation?", 
                                             choices = binaries, selected = binaries[1], inline = TRUE),
                                br(),
                                sliderInput("kfoldSlider", "If using k-fold cross-validation, how many folds do you want to use?",
                                            min = 1, max = 30, value = 10),
                                br(),
                                sliderInput("splitpropSlider", "What proportion of your data do you want in the train set?",
                                            min = 0.1, max = 0.9, value = 0.8, step = 0.05),
                                br(),
                                radioButtons("balancedaccuracySelect", "Do you want to use balanced classification accuracy instead of overall classification accuracy?", 
                                             choices = binaries, selected = binaries[2], inline = TRUE),
                                hr(),
                                h3("Multivariate Algorithm Controls"),
                                radioButtons("bysetSelect", "If you selected more than one feature set, do you want to analyse each feature set independently?", 
                                             choices = binaries, selected = binaries[2], inline = TRUE),
                                br(),
                                p("After making selections and uploading data using the controls above, click the button below to run calculations."),
                                actionButton("runMultivariate", "Run Multivariate Models"),
                                hr(),
                                h3("Univariate Algorithm Controls"),
                                selectInput("inputScalerUnivariate", "Select a normalisation method to apply prior to computing correlations between top features",
                                            choices = all_scalers, selected = all_scalers[3], multiple = FALSE),
                                br(),
                                selectInput("corMethodUnivariate", "Select a correlation method to apply",
                                            choices = cor_methods, selected = cor_methods[1], multiple = FALSE),
                                br(),
                                radioButtons("poolednullSelect", "If using empirical null, do you want to used a pooled empirical null (i.e., null distribution is null samples of all features)?", 
                                             choices = binaries, selected = binaries[2], inline = TRUE),
                                br(),
                                p("After making selections and uploading data using the controls above, click the button below to run calculations."),
                                actionButton("runUnivariate", "Run Univariate Models")
                              ),
                              mainPanel(
                                tabsetPanel(id = "classifier_tabs",
                                            tabPanel("Multivariate Approach",
                                                     fluidRow(
                                                       column(11,
                                                              shinycssloaders::withSpinner(plotlyOutput("multivariatePlot", height = "750px"))
                                                       )
                                                     )
                                            ),
                                            tabPanel("Univariate Approach",
                                                     fluidRow(p("Coming soon!")),
                                                     fluidRow(
                                                       column(11,
                                                              shinycssloaders::withSpinner(plotlyOutput("top_feature_cor_plot", height = "750px"))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(11,
                                                              shinycssloaders::withSpinner(plotlyOutput("top_feature_violin_plot", height = "750px"))
                                                       )
                                                     )
                              )
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
