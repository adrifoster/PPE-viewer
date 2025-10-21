#
# #########################
# Purpose: Shiny UI
# Author: Adrianna C. Foster, NSF NCAR (afoster@ucar.edu)
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

### SHINY UI ###
ui = bootstrapPage(
  shinyjs::useShinyjs(),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  tags$head(includeScript("www/selectize_click.js")),
  
  navbarPage(
  
    title = tags$a(class = "navbar-brand", style = "color: white !important;", 
                   "CLM-FATES and CLM PPE"),
    id = 'main_nav',
    windowTitle = 'CLM-FATES and CLM PPE',
    theme = bslib::bs_theme(version = 5, base_font = bslib::font_google("Inter")),
    
    ## About this study panel
    tabPanel("About this Study",
        tags$div(
          class = "about-panel",
        
          # Header
          tags$h1('CLM-FATES and CLM Perturbed Parameter Ensemble'),
          tags$h4("About this Study"),
          tags$p("This interactive app explores how individual parameter perturbations affect model outputs in the CLM and CLM-FATES models in Satellite Phenology (SP) mode. Each parameter is varied one at a time to assess its influence on key biophysical variables."),
          
          # Logo
          tags$div(
            style = 'display: flex; align-items: center; gap: 20px; margin: 10px 0;',
            tags$img(src = 'FATES_LOGO.png', width = '10%', alt = 'FATES logo'),
            tags$img(src = 'NSF-NCAR_Lockup-UCAR-Dark_102523.png', width = '25%')
          ),
          tags$br(),
          
          # How to use
          tags$h4("How to Use This App"),
          tags$p("Each tab in the app focuses on a specific way to explore and interpret the one-at-a-time parameter ensemble results:"),
          tags$ul(
            style = "list-style-type: none; padding-left: 0;",
              # Parameter Explorer
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("sliders-h", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Parameter Explorer:"), 
                        " Lets you select individual parameters to see how they affect model outputs.")
            ),
            # Ensemble Variance
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("chart-area", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Ensemble Variance:"),
                        " Provides a high-level look at ensemble ranges and variance for both models.")
            ),
            # Top Parameters
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("sort-amount-up-alt", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Top Parameters:"), 
                        " Lets you look at the most influential parameters on different model outputs.")
            ),
            # Model Comparison
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("columns", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Model Comparison:"), 
                        " Compares the CLM and CLM-FATES responses side by side for equivalent parameter perturbations.")
            ),
            # Parameter Information
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("info-circle", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Parameter Information:"),
                        " Searchable table of parameter names and descriptions.")
            ),
            # Download data
            tags$li(
              style = "margin-bottom: 10px; display: flex; align-items: center; padding: 5px; border-radius: 5px; transition: background-color 0.2s;",
              icon("download", class = "fa-lg", style = "margin-right: 10px;"),
              tags$span(tags$b("Download Data:"),
                        " Download data from this study.")
            )
          ),
          tags$p("Use the navigation bar above to switch between sections. Hover over plots for details, and use the download buttons to save data or figures."),
          tags$br(),
          
          # Methods accordion
          tags$h4("Methods"),
          
          # Nested accordion for subsections
          accordion(
            accordion_panel(
              "Model Descriptions",
              `data-icon` = "cogs",
              tags$p("High-level summary of CLM and CLM-FATES models:"),
              tags$ul(
                tags$li(tags$b("CLM 6.0 (Community Land Model):"),
                        " Simulates terrestrial energy, water, and carbon fluxes."),
                tags$li(tags$b("CLM-FATES (Functionally Assembled Terrestrial Ecosystem Simulator):"), 
                        " Adds a detailed representation of vegetation structure and function.")
              ),
              tags$p("Both models were run in satellite phenology (SP) mode, where canopy structure (LAI, SAI, height) is prescribed from satellite observations."),
              tags$p("When CLM is coupled to FATES, CLM provides site and soil conditions and atmospheric forcing, while FATES simulates plant physiological, vegetation demography, and biogeochemical processes (Fig. 1)."),
              tags$div(
                tags$img(
                  src = "FATES_schematic091025.png",
                  width = "40%",
                  alt = "Conceptual relationship between CLM and CLM-FATES"
                ),
                tags$br(),
                tags$em("Figure 1: Processes simulated in CLM-FATES by each model. Top: processes simulated by FATES when connected to CLM. Arrows in purple indicate conditions supplied to FATES by CLM. Arrows in green indicate conditions supplied to CLM by FATES. Anything in gray is not active in SP mode. Bottom: Processes simulated by CLM when connected to FATES. Green starred variables are simulated and provided by FATES or in the case of aerodynamic resistance (ra) are influenced by the FATES-provided roughness length, displacement height, and leaf dimension. Items in gray are not active in SP mode. †: Only used in FATES hydraulics mode, which is not utilized in this study.")
              ),
              tags$br(),
              tags$p("For more details, see ",
                     tags$a(href="https://escomp.github.io/CTSM/index.html", "CLM documentation"), 
                     " and ",
                     tags$a(href="https://fates-users-guide.readthedocs.io/projects/tech-doc/en/latest/index.html", "FATES documentation"), 
                     ".")
            ),
            accordion_panel(
              "Model Configurations and Spinup",
              tags$p("Both CLM and CLM-FATES were run in SP mode using prescribed GSWP3 meteorology (2000–2014)."),
              tags$p("A 45-year spinup ensured stable soil and energy conditions, and results from an additional 15 years were analyzed."),
              tags$p("To reduce computational cost, we used a 400-point ‘sparse grid’ that represents global variability following ",
                     tags$a(href="https://doi.org/10.1029/2024MS004715", "Kennedy et al. (2025)"), ". (Fig. 2)"),
              tags$div(
                tags$img(
                  src = "OAAT_Sparse_Grid_Biomes.png",
                  width = "80%",
                  alt = "Conceptual relationship between CLM and CLM-FATES"
                ),
                tags$br(),
                tags$em("Figure 2: Locations of the 400 sparse grid cells, along with Whittaker biome.")
              )
            ),
            accordion_panel(
              "Parameter Ensembles",
              tags$p("We tested how individual parameter changes affect model outputs using one-at-a-time parameter perturbations."),
              tags$ul(
                tags$li("Each parameter was run at its minimum and maximum value."),
                tags$li("Parameter ranges were derived from literature, expert judgment, and prior PPEs."),
                tags$li("In total: 204 CLM parameters and 143 FATES parameters were perturbed."),
                tags$li("Parameters were grouped as common, CLM-only, or FATES-only.")
              ),
              tags$p("Vegetation-related parameters were perturbed together across all PFTs to limit simulation count.")
              ),
            ),
          
          # Contact
          tags$br(), tags$br(),
          tags$h4("Contact"),
          tags$p("Adrianna Foster, NCAR — ", tags$a(href="mailto:afoster@ucar.edu", "afoster@ucar.edu")),
          tags$br(),
        
          # Acknowledgements 
          tags$h4("Acknowledgments"),
          tags$p("This material is based upon work supported by the NSF National Center for Atmospheric Research, which is a major facility sponsored by the National Science Foundation under Cooperative Agreement No. 1852977. Computing and data storage resources, including the Derecho supercomputer (doi:10.5065/qx9a-pg09) were provided by the Climate Simulation Laboratory at NSF-NCAR's Computational and Information Systems Laboratory (CISL)."),
          )
        ),
    
    tabPanel("Parameter Explorer",
             sidebarLayout(
               sidebarPanel(
                 class="panel-section sidebar",
                 tags$h4("Parameter Explorer"),
                 tags$p("Select a parameter and output variable to see how it affects model results. Only parameters with non-zero effects are listed."),
                 selectizeInput("paramExplorerParameterSelect", "Parameter:",
                             choices = all_nonzero_params,
                             selected= all_nonzero_params[1],
                             multiple=FALSE),
                 selectizeInput("paramExplorerVariableSelect", "Variable:",
                             choices = LONG_NAMES,
                             selected= LONG_NAMES[1],
                             multiple=FALSE),
               ),
               mainPanel(
                 tags$div(class="panel-section",
                   style = "margin-bottom: 20px;",
                   tags$br(),
                   tags$h4(uiOutput("paramExplorerParameter")),
                   tags$br(),
                   tags$h5("Summary Statistics"),
                   tags$h6(uiOutput("paramExplorerVariable")),
                   tags$div(class='plot-card', 
                            shinycssloaders::withSpinner(tableOutput("paramExplorerSummaryTable"),
                                                         color="#012169",
                                                         type=1))
                 ),
                 tabsetPanel(
                   tabPanel(
                     "Global Maps: annual means",
                     tags$br(),
                     tags$h5("Annual means"),
                     tags$div(class="plot-card", 
                              shinycssloaders::withSpinner(plotOutput("globalValuesPlot", 
                                                                      height="500px", width="700px"),
                                                           color="#012169")),
                     div(class='plot-card-btns',
                         style = "margin-top: 10px; text-align: right;",
                         downloadButton("downloadGlobalPlot", "Download Plot")
                     ),
                     tags$br(),
                     tags$h5("Annual mean differences"),
                     tags$p("Difference maps show the annual value for the ensemble member for the maximum parameter value 
                              ensemble member for the minimum parameter value."),
                     tags$div(class="plot-card", 
                              shinycssloaders::withSpinner(plotOutput("globalValuesDifferencePlot", height="500px", width="700px"),
                                                           color="#012169")),
                     div(class='plot-card-btns',
                         style = "margin-top: 10px; text-align: right;",
                         downloadButton("downloadGlobalDiffPlot", "Download Plot")
                     ),
                     ),
    
                   tabPanel(
                     "Global maps: model differences",
                            tags$div(class="plot-card", shinycssloaders::withSpinner(plotOutput("globalValuesModelDifferencePlot",
                                       height="500px",
                                       width="700px"), color="#012169")),
                     div(class='plot-card-btns',
                         style = "margin-top: 10px; text-align: right;",
                         downloadButton("downloadGlobalDiffDiffPlot", "Download Plot")
                     ),
                   ),
                   tabPanel(
                     "Climatology & Zonal Means",
                     tags$div(class="plot-card", shinycssloaders::withSpinner(plotOutput("globalValuesClimPlot", height="400px",
                                width="600px"), color="#012169")),
                     div(class='plot-card-btns',
                         style = "margin-top: 10px; text-align: right;",
                         downloadButton("downloadClimatologyPlot", "Download Plot")
                     ),
                     tags$div(class="plot-card", shinycssloaders::withSpinner(plotOutput("globalValuesZonalPlot", height="400px",
                                width="600px"), color="#012169")),
                     div(class='plot-card-btns',
                         style = "margin-top: 10px; text-align: right;",
                         downloadButton("downloadZonalPlot", "Download Plot")
                     ),
                  )
                 ),
                 )
               )
    ),
    tabPanel("Ensemble Variance",
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Ensemble Variance"),
                 tags$h5('Global Values'),
                 tags$p("Select a parameter and output variable to see how it affects model results. Only parameters with non-zero effects are listed."),

                 selectizeInput("ensembleVariableSelect", "Variable:",
                             choices = LONG_NAMES,
                             selected = "GPP",
                             multiple=FALSE),
                 selectizeInput("ensembleTypeSelect", "Summation type:",
                             choices = c("mean", "interannual variance"),
                             selected= "mean",
                             multiple=FALSE),
    
                 tags$h5('Cumulative Variance'),
                 
                 
                 tags$p("Select number of parameters to group cumulative variance by."),
                 
                 sliderInput("ensembleChunkSelect", "Number of parameters per group:",
                             min=1, max=20, value=5)
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Global",
                            tags$br(),
                            tags$h5("Global annual means"),
                            tags$br(),
                            tags$p("Each dot represents a global annual mean or interannual variance for the chosen variable from an ensemble member."),
                            tags$p("Black dots and error bars show mean, minimum, and maximum values for the entire enemble."),
                            tags$p("Hover over a point to see what parameter was perturbed and in which direction for that ensemble member."),
                            tags$br(),
                            tags$div(class='plot-card', shinycssloaders::withSpinner(plotlyOutput("ensemblePlot", height="700px", width="600px"),
                                                                                     color="#012169")),
                            ),
                   tabPanel("Cumulative Variance",
                            tags$br(),
                            tags$h5("Cumulative variance"),
                            tags$p("Variance contribution of a parameter to the global annual mean or interannual variance was calculated as the sum of the squared differences from the default simulation:"),
                            withMathJax("$$V_i = (\\bar{x}_i - x_{i,min})^2  + (x_{i,max} - \\bar{x}_i)^2$$"),
                            tags$div(class='plot-card', shinycssloaders::withSpinner(plotOutput("cumulativeVariancePlot", height="500px", width="700px"),
                                                                                     color="#012169")),
                            div(class='plot-card-btns',
                                style = "margin-top: 10px; text-align: right;",
                                downloadButton("downloadCumulativeVarPlot", "Download Plot"))
                            )
                 )
               )
             )
    ),
    tabPanel("Top Parameters",
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Top Parameters"),
                 tags$p("Select output variable, summation type, and number of parameters to include."),
                 
                 selectizeInput("topParamsVariableSelect", "Variable:",
                             choices = LONG_NAMES,
                             selected = "GPP",
                             multiple=FALSE),
                 selectizeInput("topParamsTypeSelect", "Summation type:",
                             choices = c("mean", "interannual variance"),
                             selected= "mean",
                             multiple=FALSE),
                 sliderInput("topParamsNSelect", "Number of parameters:",
                             min=1, max=30, value=10),
                 tags$p("Additionally, optionally select a grouping of parameters: 'all' for all parameters, 
                        'common' for parameters that affect both FATES and CLM, and 'distinct' for parameters that 
                        only affected either FATES or CLM"),
                 
                 selectizeInput("topParamsParameterTypeSelect", "Parameters:",
                             choices = c('all', 'distinct', 'common'),
                             selected = "all",
                             multiple=FALSE),
  
                 tags$h5('By Biome'),
                 tags$p("Select biomes to view. Note that more biomes may be
               harder to view all at once."),
               selectizeInput("topParamsBiomeSelect", "Biome:",
                              choices = unique(BIOME_DF$biome_name),
                              options = list(
                                plugins = list('remove_button')
                              ),
                              selected = 'Tropical rain forest',
                              multiple=TRUE)
                 
                 
               ),
               mainPanel(
                 tags$h5("Top n parameters for each parameter ensemble."),
                 tags$p("Dots show the global annual mean or interannual variance for that ensemble member."),
                 tabsetPanel(
                   tabPanel("Global", 
                            tags$div(class='plot-card', 
                                     shinycssloaders::withSpinner(plotOutput("topParametersPlot", width="900px", height="500px"),
                                                                  color="#012169")),
                            div(class='plot-card-btns',
                                style = "margin-top: 10px; text-align: right;",
                                downloadButton("downloadTopParamsPlot", "Download Plot"))
                            ),
                   
                   tabPanel("By Biome", 
                            tags$div(class='plot-card', 
                                     shinycssloaders::withSpinner(plotOutput("topParametersbyBiomePlot", height="700px",
                                                   width="900px"), color="#012169")),
                            div(class='plot-card-btns',
                                style = "margin-top: 10px; text-align: right;",
                                downloadButton("downloadTopParamsByBiomePlot", "Download Plot"))
                            )
                 )
                 
               )
             )
    ),
    tabPanel("Model Comparisons",
              sidebarLayout(
               sidebarPanel(
                 tags$h3("Model Comparisons"),
                 tags$p("Select output variable and summation type"),
                 selectizeInput("modelDiffVariableSelect", "Variable:",
                                choices = LONG_NAMES,
                                selected = "GPP",
                                multiple=FALSE),
                 selectizeInput("modelDiffTypeSelect", "Summation type:",
                                choices = c("mean", "interannual variance"),
                                selected= "mean",
                                multiple=FALSE),
                 sliderInput("modelDiffNSelect", "Number of parameters:",
                             min=1, max=30, value=15),
                 tags$p("Additionally, optionally select a grouping of parameters: 'all' for all parameters, 
                        'common' for parameters that affect both FATES and CLM, and 'distinct' for parameters that 
                        only affected either FATES or CLM"),
                 selectizeInput("modelDiffParamTypeSelect", "Parameters:",
                             choices = c('all', 'distinct', 'common'),
                             selected = "all",
                             multiple=FALSE),
                 tags$p("Optionally select specific biomes."),
                 selectizeInput("modelDiffBiomeSelect", "Biome:",
                             choices = c('all', unique(BIOME_DF$biome_name)),
                             selected = 'all',
                             multiple=FALSE),
                 
               ),
               mainPanel(
                 tags$h5("Percent differences in annual means for each parameter for select variables."),
                 tags$div(class='plot-card', 
                          shinycssloaders::withSpinner(plotOutput("modelDiffPlot", height="400px", width="700px"),
                                                       color="#012169")),
                 div(class='plot-card-btns',
                     style = "margin-top: 10px; text-align: right;",
                     downloadButton("downloadModelDiffPlot", "Download Plot"))
               )
             )

             
    ),
    tabPanel("Parameter Information",
             fluidPage(
               DT::dataTableOutput("paramInfoTable"),
               div(class='plot-card-btns',
                   style = "margin-top: 10px; text-align: right;",
                   downloadButton("downloadParameterTable", "Download Parameter Information")
               ),
             )
    ),
    tabPanel("Download Data",
              sidebarLayout(
                sidebarPanel(
                  class="panel-section sidebar",
                  tags$h4("Download Data"),
                  tags$p("Select datasets and variables to download. The downloaded file will be a zipped folder containing CSVs for each dataset."),
                  div(style="display: flex; align-items: center; gap: 10px;",
                      tags$label("Dataset(s):", style="font-weight: 500; margin-bottom: 0;"),
                      actionButton("downloadDataTypeSelectAll", 
                                   "Select/Deselect All", 
                                   class = "btn btn-sm btn-outline-primary")
                  ),
                  selectizeInput("downloadDataTypeSelect", NULL,
                                 choices = DATASETS,
                                 selected=NULL,
                                 multiple=TRUE,
                                 options = list(
                                   plugins = list('remove_button'),
                                   placeholder="Choose datasets..."
                                 )
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px;",
                      tags$label("Model(s):", style = "font-weight: 500; margin-bottom: 0;"),
                      actionButton("downloadModelSelectAll", "Select/Deselect All",
                                   class = "btn btn-sm btn-outline-primary")
                  ),
                  selectizeInput("downloadDataModelSelect", NULL,
                                 choices = MODELS,
                                 selected=NULL,
                                 multiple=TRUE,
                                 options = list(
                                   plugins = list('remove_button'),
                                   placeholder="Choose models..."
                                 )
                  ),
                  div(style="display: flex; align-items: center; gap: 10px;",
                      tags$label("Variables(s):", style="font-weight: 500; margin-bottom: 0;"),
                      actionButton("downloadVariableSelectAll", 
                                   "Select/Deselect All", 
                                   class="btn btn-sm btn-outline-primary")
                  ),
                  selectizeInput("downloadDataVariableSelect", NULL,
                                 choices = LONG_NAMES,
                                 selected=NULL,
                                 multiple=TRUE,
                                 options = list(
                                   plugins = list('remove_button'),
                                   placeholder="Choose variables..."
                                 )
                  ),
                  checkboxInput("includeMetadata", "Include metadata", value = TRUE)
                ),
                mainPanel(
                  tags$style(HTML("
                  button:disabled {
                  pointer-events: none !important;
                  opacity: 0.5 !important;
                  }
                                  ")),
                  tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css"),
                  
                  tags$div(class = "panel-section",
                           tags$h5("Download Preview"),
                           #tags$p("Files will be organized by dataset, with metadata included if selected."),
                           tags$p("Data download will be available soon."),
                           tags$div(class = "plot-card",
                                      tableOutput("downloadPreviewTable"),
                                      color = "#012169", type = 1
                                    )
                           
                           ),
                  actionButton(
                    "downloadTriggerButton", 
                    label=tagList(
                      icon("download"),
                      "Download Data"
                      )),
                  #downloadButton("hiddenDownloadButton", label="Download", style="visibility: hidden;")
                  )
                )
             )
  )
)