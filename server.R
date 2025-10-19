#
# #########################
# Purpose: Shiny Server
# Author: Adrianna C. Foster, NSF NCAR (afoster@ucar.edu)
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

### SHINY SERVER ###
server <- function(input, output) {
  
  ## Parameter Explorer Parameter UI ##
  output$paramExplorerParameter <- renderUI({
    
    parameter = input$paramExplorerParameterSelect
    
    sub = filter(param_info, parameter_name == parameter)
    model = sub$model
    long_name = sub$long_name
    
    HTML(paste0(
      "<div class='param-header'>",
      "<b>Parameter:</b> ", sub$parameter_name, "<br>",
      "<span class='param-meta-model'>", sub$model, " parameter</span><br>",
      "<span class='param-meta-desc'>", sub$long_name, "</span>"
    ))
  })
  
  ## Parameter Explorer Variable UI ##
  output$paramExplorerVariable <- renderUI({
    
    variable = input$paramExplorerVariableSelect
    
    HTML(paste0(
      "<span class='param-meta-desc'>", variable, "</span>"
    ))
  })
  
  ## Parameter Explorer Summary Statistics Table ##
  summary_table = reactive({
    
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    variable_name = get_variable_name(variable, 'mean')
    cols_to_select <- c("model", "parameter_name", "category", "subcategory", 
                        "type", variable_name)
    
    # create summary table
    df <- dplyr::filter(global_data_all,
                        parameter_name == parameter) %>%
      rbind(dplyr::filter(global_data_all, ensemble == 0)) %>%
      distinct_all() %>%
      dplyr::select(all_of(cols_to_select)) %>%
      mutate(type = ifelse(type == '', 'default', type)) %>%
      reshape2::dcast(model  ~ type, 
                      value.var = variable_name)
    
    # get rid of model if it's not in the ensemble
    if (parameter %in% fates_only_params) {
      df = dplyr::filter(df, model != 'CLM')
    }
    if (parameter %in% clm_only_params) {
      df = dplyr::filter(df, model != 'FATES')
    }
    df = dplyr::mutate(df, model = ifelse(model == 'FATES', 'CLM-FATES', model))
    
    # make column names nicer
    colnames(df) = c(
      "Model",
      paste0("Default value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>"),
      paste0("Ensemble max value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>"),
      paste0("Ensemble min value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>")
    )
    
    return(df)
  })
  
  output$paramExplorerSummaryTable <- renderTable(
    summary_table(),
    digits = 3, striped = TRUE, bordered = TRUE, hover = TRUE,
    sanitize.text.function=identity
    )
  
  ## Annual maps plotting ##
  global_map = reactive({
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    plot_global_vals(
      fates_map_dat, fatesclm_map_dat, clm_map_dat, parameter, variable, 
      fates_key, clm_key, fates_only_params, clm_only_params, common_params, 
      land, frac_rast
    )
  })
  
  output$globalValuesPlot <- renderPlot({
    global_map()
  })
  
  output$downloadGlobalPlot <- downloadHandler(
    filename = function() {
      parameter = input$paramExplorerParameterSelect
      variable = input$paramExplorerVariableSelect
      
      paste0("global_values_plot_", variable, '_', parameter, '_', 
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(globalPlot())
      dev.off()
    }
  )
  
  ## Annual difference maps plotting ##
  global_diff_map = reactive({
    
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    plot_global_diffs(
      fates_map_dat, fatesclm_map_dat, clm_map_dat,
      parameter, variable, fates_key, clm_key, fates_only_params, 
      clm_only_params, common_params, land, frac_rast
    )
    
  })
  output$globalValuesDifferencePlot <- renderPlot({
    global_diff_map()
  })
  
  output$downloadGlobalDiffPlot <- downloadHandler(
    filename = function() {
      parameter = input$paramExplorerParameterSelect
      variable = input$paramExplorerVariableSelect
      
      paste0("global_diff_values_plot_", variable, '_', parameter, '_', 
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(global_diff_map())
      dev.off()
    }
  )
  
  ## Annual difference difference (between models) plotting ##
  global_diff_diff_map = reactive({
    
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    if (!(parameter %in% common_params)){
      validate("Please choose a parameter that affects both CLM and CLM-FATES to view this graph.")
    }
    
    plot_diff_diffs(
      fatesclm_map_dat, clm_map_dat, parameter, variable, clm_key, 
      common_params, land, frac_rast
      )
  })
  
  output$globalValuesModelDifferencePlot <- renderPlot(global_diff_diff_map())
  
  output$downloadGlobalDiffDiffPlot <- downloadHandler(
    filename = function() {
      parameter = input$paramExplorerParameterSelect
      variable = input$paramExplorerVariableSelect
      
      paste0("global_model_diffs_values_plot_", variable, '_', parameter, '_', 
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(global_diff_diff_map())
      dev.off()
    }
  )
  
  ## Climatology plotting ##
  climatology_plot = reactive({
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    dat = get_plot_dat(
      all_clim, parameter, fates_only_params, clm_only_params, common_params
      )
    
    plot_clim_dat(
      dat, variable, parameter, facet_model=FALSE, facet_type=FALSE
      )
    
  })
  
  output$globalValuesClimPlot <- renderPlot(climatology_plot())
  
  output$downloadClimatologyPlot <- downloadHandler(
    filename = function() {
      parameter = input$paramExplorerParameterSelect
      variable = input$paramExplorerVariableSelect
      
      paste0("climatology_plot_", variable, '_', parameter, '_', 
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(climatology_plot())
      dev.off()
    }
  )
  
  ## Zonal means plotting ##
  zonal_plot = reactive({
    parameter = input$paramExplorerParameterSelect
    variable = input$paramExplorerVariableSelect
    
    dat = get_plot_dat(
      all_zonal, parameter, fates_only_params, clm_only_params, common_params
      )
    
    plot_zonal_dat(
      dat, variable, parameter, facet_model=FALSE, facet_type=FALSE
      )
    
  })
  output$globalValuesZonalPlot <- renderPlot(zonal_plot())
  
  output$downloadZonalPlot <- downloadHandler(
    filename = function() {
      parameter = input$paramExplorerParameterSelect
      variable = input$paramExplorerVariableSelect
      
      paste0("zonal_means_plot_", variable, '_', parameter, '_', 
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(zonal_plot())
      dev.off()
    }
  )

  ## Ensemble Variance Plotting ##
  ensemble_plot = reactive({
    variable = input$ensembleVariableSelect
    type = get_type(input$ensembleTypeSelect)
    
    plot_ensemble_variance(
      global_df, variable, type
      )
    
  })
  
  output$ensemblePlot = plotly::renderPlotly(ensemble_plot())
  
  ## Cumulative Variance Plotting
  cumulative_var_plot = reactive({
    variable = input$ensembleVariableSelect
    type = get_type(input$ensembleTypeSelect)
    param_chunks = input$ensembleChunkSelect
    
    plot_cumulative_variance(
      variance_data, variable, type, param_chunks
      )
  })
  
  output$cumulativeVariancePlot <- renderPlot(cumulative_var_plot())
  
  output$downloadCumulativeVarPlot <- downloadHandler(
    filename = function() {
      variable = input$ensembleVariableSelect
      type = get_type(input$ensembleTypeSelect)
      param_chunks = input$ensembleChunkSelect
      
      paste0("cumulative_variance_plot_", variable, '_', type, '_', 
             param_chunks, "chunks", "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(cumulative_var_plot())
      dev.off()
    }
  )
  
  ## Top Parameters Plotting ##
  top_params_plot = reactive({
    variable = input$topParamsVariableSelect
    type = get_type(input$topParamsTypeSelect)
    n = input$topParamsNSelect
    param_select = get_parameter_type(input$topParamsParameterTypeSelect, 
                                      fates_only_params,
                                      clm_only_params, common_params)
    
    plot_top_n(
      min_max_global_df, fates_global, clm_global,
               variable, type, n, param_select=param_select
      )
  })
  output$topParametersPlot <- renderPlot(top_params_plot())
  
  output$downloadTopParamsPlot <- downloadHandler(
    filename = function() {
      variable = input$topParamsVariableSelect
      type = get_type(input$topParamsTypeSelect)
      n = input$topParamsNSelect
      
      paste0("top_", n, "_parameters_plot_", variable, '_', type, '_',
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(top_params_plot())
      dev.off()
    }
  )
  
  ## Top Parameters By Biome Plotting ##
  top_params_biome_plot = reactive({
    variable = input$topParamsVariableSelect
    type = get_type(input$topParamsTypeSelect)
    n = input$topParamsNSelect
    param_select = get_parameter_type(input$topParamsParameterTypeSelect, 
                                      fates_only_params,
                                      clm_only_params, common_params)
    biome_select = input$topParamsBiomeSelect
    
    biome_sub = filter(BIOME_DF, BIOME_DF$biome_name %in% biome_select)
    
    plot_top_n_by_biome(
      min_max_biome_df, fates_biome, clm_biome, variable, type, n, BIOME_DF, 
      param_select=param_select, biome_select=biome_sub$biome
      )
  })
  
  output$topParametersbyBiomePlot <- renderPlot(top_params_biome_plot())
  
  output$downloadTopParamsByBiomePlot <- downloadHandler(
    filename = function() {
      variable = input$topParamsVariableSelect
      type = get_type(input$topParamsTypeSelect)
      n = input$topParamsNSelect
      
      paste0("top_", n, "_parameters_by_biome_plot_", variable, '_', type, '_',
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(top_params_biome_plot())
      dev.off()
    }
  )
  
  ## Model Differences Plotting ##
  model_diff_plot = reactive({
    variable = input$modelDiffVariableSelect
    type = get_type(input$modelDiffTypeSelect)
    param_select = input$modelDiffParamTypeSelect
    biome_select = input$modelDiffBiomeSelect
    n = input$modelDiffNSelect
    
    if (biome_select == 'all'){
      plot_var_diff(
        all_var_diff, variable, type, param_select, n_include = n
      )
      
    } else {
      biome_sub = filter(BIOME_DF, BIOME_DF$biome_name == biome_select)
      plot_var_diff(
        all_var_diff_biome, variable, type, param_select,
        biome_in = biome_sub$biome, n_include = input$n
      )
    }
    
  })
  
  output$modelDiffPlot <- renderPlot(model_diff_plot())
  
  output$downloadModelDiffPlot <- downloadHandler(
    filename = function() {
      variable = input$modelDiffVariableSelect
      type = get_type(input$modelDiffTypeSelect)

      paste0("model_comparisons_plot_", variable, '_', type, '_',
             Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 900, height = 600, res = 120)
      print(model_diff_plot())
      dev.off()
    }
  )
  
  ## Parameter Information Table ##
  output$paramInfoTable <- DT::renderDataTable({
    datatable(output_table,
              options = list(pageLength=10, autoWidth=TRUE),
              rownames=FALSE)
  })
  
  observeEvent(input$downloadModelSelectAll, {
    is_all_selected <- all(MODELS %in% input$downloadDataModelSelect)
    
    if (is_all_selected) {
      updateSelectizeInput(
        inputId = "downloadDataModelSelect",
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        inputId = "downloadDataModelSelect",
        selected = MODELS
      )
    }
  })
  
  observeEvent(input$downloadDataTypeSelectAll, {
    is_all_selected <- all(DATASETS %in% input$downloadDataTypeSelect)
    
    if (is_all_selected) {
      updateSelectizeInput(
        inputId = "downloadDataTypeSelect",
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        inputId = "downloadDataTypeSelect",
        selected = DATASETS
      )
    }
  })
  
  observeEvent(input$downloadVariableSelectAll, {
    is_all_selected <- all(VARIABLES %in% input$downloadDataVariableSelect)
    
    if (is_all_selected) {
      updateSelectizeInput(
        inputId = "downloadDataVariableSelect",
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        inputId = "downloadDataVariableSelect",
        selected = VARIABLES
      )
    }
  })
  
  
}
