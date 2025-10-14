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
  
  output$ensemblePlot = renderPlotly({
    variable = input$ensemble_variable_select
    
    if (input$ensemble_type_select == 'mean') {
      type = 'mean'
    }
    if (input$ensemble_type_select == 'interannual variance') {
      type = 'iav'
    }
    
    plot_ensemble_variance(global_df, variable, type)
    
  })

  output$cumulativeVariancePlot <- renderPlot({
    variable = input$ensemble_variable_select
    
    param_chunks <- input$variance_chunk_select
    
    if (input$ensemble_type_select == 'mean') {
      type = 'mean'
    }
    if (input$ensemble_type_select == 'interannual variance') {
      type = 'iav'
    }
    
    plot_cumulative_variance(variance_data, variable, type, param_chunks)
    
  })

  output$topParametersPlot <- renderPlot({
    
    variable <- input$top_params_variable_select
    
    if (input$top_params_type_select == 'mean') {
      type = 'mean'
    }
    if (input$top_params_type_select == 'interannual variance') {
      type = 'iav'
    }
    
    n <- input$top_params_n_select
    
    if (input$top_params_param_choice == 'all'){
      param_select <- c()
    } else if (input$top_params_param_choice == 'distinct') {
      param_select <- c(fates_only_params, clm_only_params)
    } else if (input$top_params_param_choice == 'common') {
      param_select <- common_params
    }

    plot_top_n(min_max_global_df, fates_global, clm_global,
                           variable, type, n, param_select=param_select)
  })
  
  output$topParametersbyBiomePlot <- renderPlot({
    
    variable <- input$top_params_variable_select
    
    if (input$top_params_type_select == 'mean') {
      type = 'mean'
    }
    if (input$top_params_type_select == 'interannual variance') {
      type = 'iav'
    }
    
    n <- input$top_params_n_select
    
    if (input$top_params_param_choice == 'all') {
      param_select <- c()
    } else if (input$top_params_param_choice == 'distinct') {
      param_select <- c(fates_only_params, clm_only_params)
    } else if (input$top_params_param_choice == 'common') {
      param_select <- common_params
    }
    
    biome_sub = filter(BIOME_DF, BIOME_DF$biome_name %in% input$top_params_biome_choice)
    
    plot_top_n_by_biome(min_max_biome_df, fates_biome, clm_biome,
                        variable, type, n, BIOME_DF, param_select=param_select,
                        biome_select=biome_sub$biome)
  })
  
  output$globalValuesPlot <- renderPlot({
    
    variable <- input$param_explorer_variable_select
    parameter <- input$param_explorer_parameter_select
    
    plot_global_vals(fates_map_dat, fatesclm_map_dat,
                     clm_map_dat, parameter, variable, fates_key,
                     clm_key, fates_only_params, clm_only_params, 
                     common_params, land, frac_rast)


  })
  
  output$globalValuesDifferencePlot <- renderPlot({
    
    plot_global_diffs(fates_map_dat, fatesclm_map_dat,
                      clm_map_dat, input$param_explorer_parameter_select, 
                      input$param_explorer_variable_select, fates_key,
                      clm_key, fates_only_params, clm_only_params, 
                      common_params, land, frac_rast)
  })
  
  output$globalValuesModelDifferencePlot <- renderPlot({
    
    if (!(input$param_explorer_parameter_select %in% common_params)){
      validate("Please choose a parameter that affects both CLM and CLM-FATES to view this graph.")
    }
    
    plot_diff_diffs(fatesclm_map_dat, clm_map_dat,
                    input$param_explorer_parameter_select,
                    input$param_explorer_variable_select,
                    clm_key, common_params, land, frac_rast)
  })
  
  output$globalValuesZonalPlot <- renderPlot({

    dat = get_plot_dat(all_zonal, input$param_explorer_parameter_select, 
                       fates_only_params, clm_only_params, common_params)
    plot_zonal_dat(dat, input$param_explorer_variable_select, 
                   input$param_explorer_parameter_select, 
                   facet_model=FALSE, facet_type=FALSE)
    
  })
  
  output$globalValuesClimPlot <- renderPlot({
    dat = get_plot_dat(all_clim, input$param_explorer_parameter_select, 
                       fates_only_params, clm_only_params,
                       common_params)
    plot_clim_dat(dat, input$param_explorer_variable_select, 
                  input$param_explorer_parameter_select, 
                  facet_model=FALSE,
                  facet_type=FALSE)
    
  })
  
  output$modelDiffPlot <- renderPlot({
    variable = input$model_diff_variable_select
    
    if (input$model_diff_type_select == 'mean') {
      type = 'mean'
    }
    if (input$model_diff_type_select == 'interannual variance') {
      type = 'iav'
    }
    
    param_select = input$model_diff_param_choice
    
    biome_select = input$model_diff_biome_choice
    
    if (biome_select == 'all'){
      plot_var_diff(all_var_diff, variable, type, param_select, 
                    n_include = input$model_params_n_select)
      
    } else {
      biome_sub = filter(BIOME_DF, BIOME_DF$biome_name == biome_select)
      plot_var_diff(all_var_diff_biome, variable, type, param_select, 
                    biome_in = biome_sub$biome, 
                    n_include = input$model_params_n_select)
    }
  
  })
  
  output$paramInfoTable <- DT::renderDataTable({
    datatable(output_table,
              options = list(pageLength=10, autoWidth=TRUE),
              rownames=FALSE)
  })
  
  output$param_explorer_summary_table <- renderTable({
    req(input$param_explorer_parameter_select, input$param_explorer_variable_select)
    
    variable = input$param_explorer_variable_select
    variable_name = get_variable_name(variable, 'mean')
    
    # filter to parameter and variable
    cols_to_select <- c("model", "parameter_name", "category", "subcategory", 
                        "type", variable_name)
    
    df <- dplyr::filter(global_data_all,
                        parameter_name == input$param_explorer_parameter_select) %>%
      rbind(dplyr::filter(global_data_all, ensemble == 0)) %>%
      distinct_all() %>%
      dplyr::select(all_of(cols_to_select)) %>%
      mutate(type = ifelse(type == '', 'default', type)) %>%
      reshape2::dcast(model  ~ type, 
                      value.var = variable_name)
    
    if (input$param_explorer_parameter_select %in% fates_only_params) {
      df = dplyr::filter(df, model != 'CLM')
    }
    if (input$param_explorer_parameter_select %in% clm_only_params) {
      df = dplyr::filter(df, model != 'FATES')
    }
    df = dplyr::mutate(df, model = ifelse(model == 'FATES', 'CLM-FATES', model))
    
    
    colnames(df) = c(
      "Model",
      paste0("Default value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>"),
      paste0("Ensemble max value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>"),
      paste0("Ensemble min value <i>(", UNITS_LOOKUP_LATEX[[variable]], ")</i>")
    )
    

    return(df)

  }, 
  digits = 3, striped = TRUE, bordered = TRUE, hover = TRUE, sanitize.text.function=identity)
  
  output$param_explorer_parameter <- renderUI({
    sub = filter(param_info, parameter_name == input$param_explorer_parameter_select)
    model = sub$model
    long_name = sub$long_name
    
    HTML(paste0(
      "<div class='param-header'>",
      "<b>Parameter:</b> ", sub$parameter_name, "<br>",
      "<span class='param-meta-model'>", sub$model, " parameter</span><br>",
      "<span class='param-meta-desc'>", sub$long_name, "</span>"
    ))
  })
  
}
