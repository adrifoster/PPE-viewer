#
# #########################
# Purpose: Helper functions for plots for the FATES & CLM SP PPE Shiny App
# Author: Adrianna C. Foster
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

# Plot percent differences in model variables by parameter
#
# Generates a ggplot (or combined plot) showing percent differences in a variable 
# for selected parameters, optionally filtering by biome.
#
# Args:
#   dat_in: Data frame containing model output and parameter information.
#   variable: Name of the variable to plot (character).
#   type: Type of calculation (e.g., 'mean', 'iav') (character).
#   param_select: Which parameters to plot; options: 'joint', 'distinct', 'all' (character).
#   biome_in: Optional character vector of biome(s) to filter by.
#
# Returns:
#   A ggplot object (or combined plot if param_select is 'distinct' or 'all') 
#   showing percent differences for each parameter and model.
#
# Notes:
#   - 'joint' shows parameters common to both models.
#   - 'distinct' shows parameters unique to each model with separate subplots.
#   - 'all' shows all parameters with separate subplots but combined scale.
plot_var_diff = function(dat_in, variable, type, param_select, biome_in=c(), 
                         n_include=30){
  
  if (length(biome_in) == 1) {
    dat_in = filter(dat_in, biome==biome_in)
  }
  
  variable_name = paste0(variable, '_', type)
  
  if (param_select == 'common'){
    
    dat = dat_in %>%
      dplyr::filter(parameter_name %in% common_params) %>%
      mutate(parameter_name = forcats::fct_reorder(parameter_name, 
                                                   get(variable_name)))
    
    dat_fates = dplyr::filter(dat, model == 'FATES') %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    dat_clm = dplyr::filter(dat, model == 'CLM') %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    dat_fates = dat_fates[1:n_include,]
    dat_clm = dat_clm[1:n_include,]
    
    param_fates = unique(dat_fates$parameter_name)
    param_clm = unique(dat_clm$parameter_name)
    
    dat = dplyr::filter(dat, parameter_name %in% c(param_fates, param_clm))
    
    dat = mutate(dat, model_name = ifelse(model == 'FATES', 'CLM-FATES',
                                          'CLM')) %>%
      mutate(model_name = factor(model_name, levels = c('CLM-FATES', 'CLM')))
    
    p = ggplot(dat) + 
      geom_col(aes(parameter_name, .data[[variable_name]], fill=model_name),
               position = 'dodge') +
      coord_flip() +
      scale_fill_manual(values = c(FATES_COL, CLM_COL), name=NULL) +
      xlab(NULL) +
      theme_bw() +
      ylab(paste0('Percent Difference in ', variable, ' (%)')) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            legend.text=element_text(size=14)) 
    
    
  } else if (param_select == 'distinct'){
    
    fates_param_dat = dat_in %>%
      dplyr::filter(parameter_name %in% fates_only_params & model == 'FATES')
    
    fates_dat = dat_in %>%
      dplyr::filter(parameter_name %in% fates_param_dat$parameter_name &
                      model == 'FATES') %>%
      mutate(parameter_name = forcats::fct_reorder(parameter_name, get(variable_name))) %>%
      mutate(model_name = ifelse(model == 'FATES', 'CLM-FATES', 'CLM')) %>%
      mutate(model_name = factor(model_name, levels = c('CLM-FATES', 'CLM'))) %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    clm_param_dat = dat_in %>%
      dplyr::filter(parameter_name %in% clm_only_params & model == 'CLM')
    
    clm_dat = dat_in %>%
      dplyr::filter(parameter_name %in% clm_param_dat$parameter_name &
                      model == 'CLM') %>%
      mutate(parameter_name = forcats::fct_reorder(parameter_name, get(variable_name))) %>%
      mutate(model_name = ifelse(model == 'FATES', 'CLM-FATES', 'CLM')) %>%
      mutate(model_name = factor(model_name, levels = c('CLM-FATES', 'CLM'))) %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    max_val = max(c(clm_dat[,variable_name], fates_dat[,variable_name])) + 5
    
    fates_dat = fates_dat[1:n_include,]
    clm_dat = clm_dat[1:n_include,]
    
    pfates = ggplot(fates_dat) + 
      geom_col(aes(parameter_name, .data[[variable_name]], fill=model_name),
               position = 'dodge', show.legend = T) +
      coord_flip() +
      scale_fill_manual(values = c(FATES_COL, CLM_COL), name=NULL, drop=F) +
      xlab(NULL) +
      theme_bw() +
      ylim(0, max_val) +
      ylab(paste0('Percent Difference in ', variable, ' (%)')) +
      ggtitle('CLM-FATES') +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            legend.text=element_text(size=14)) 
  
    pclm = ggplot(clm_dat) + 
      geom_col(aes(parameter_name, .data[[variable_name]], fill=model_name),
               position = 'dodge', show.legend = T) +
      coord_flip() +
      scale_fill_manual(values = c(FATES_COL, CLM_COL), name=NULL, drop=F) +
      xlab(NULL) +
      theme_bw() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            legend.text=element_text(size=14)) +
      ylim(0, max_val) +
      ylab(paste0('Percent Difference in ', variable, ' (%)')) +
      ggtitle('CLM')
    
    
    p = pfates + pclm + plot_layout(guides='collect')
    
  } else if (param_select == 'all') {
    
    fates_param_dat = dat_in %>%
      dplyr::filter(model == 'FATES') 
    
    fates_dat = dat_in %>%
      dplyr::filter(parameter_name %in% fates_param_dat$parameter_name &
                      model == 'FATES') %>%
      mutate(parameter_name = forcats::fct_reorder(parameter_name, get(variable_name))) %>%
      mutate(model_name = ifelse(model == 'FATES', 'CLM-FATES', 'CLM')) %>%
      mutate(model_name = factor(model_name, levels = c('CLM-FATES', 'CLM'))) %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    clm_param_dat = dat_in %>%
      dplyr::filter(model == 'CLM') 
    
    clm_dat = dat_in %>%
      dplyr::filter(parameter_name %in% clm_param_dat$parameter_name &
                      model == 'CLM') %>%
      mutate(parameter_name = forcats::fct_reorder(parameter_name, get(variable_name))) %>%
      mutate(model_name = ifelse(model == 'FATES', 'CLM-FATES', 'CLM')) %>%
      mutate(model_name = factor(model_name, levels = c('CLM-FATES', 'CLM'))) %>%
      dplyr::arrange(desc(!!sym(variable_name)))
    
    max_val = max(c(clm_dat[,variable_name], fates_dat[,variable_name])) + 5
    
    fates_dat = fates_dat[1:n_include,]
    clm_dat = clm_dat[1:n_include,]
    
    pfates = ggplot(fates_dat) + 
      geom_col(aes(parameter_name, .data[[variable_name]], fill=model_name),
               position = 'dodge', show.legend = T) +
      coord_flip() +
      scale_fill_manual(values = c(FATES_COL, CLM_COL), name=NULL, drop=F) +
      xlab(NULL) +
      theme_bw() +
      ylim(0, max_val) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            legend.text=element_text(size=14)) +
      ylab(paste0('Percent Difference in ', variable, ' (%)')) +
      ggtitle('CLM-FATES')
    
    
    pclm = ggplot(clm_dat) + 
      geom_col(aes(parameter_name, .data[[variable_name]], fill=model_name),
               position = 'dodge', show.legend = T) +
      coord_flip() +
      scale_fill_manual(values = c(FATES_COL, CLM_COL), name=NULL, drop=F) +
      xlab(NULL) +
      theme_bw() +
      ylim(0, max_val) +
      ylab(paste0('Percent Difference in ', variable, ' (%)')) +
      ggtitle('CLM') +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            legend.text=element_text(size=14)) 
  
    
    p = pfates + pclm + plot_layout(guides='collect')
    
  }
  
  return(p)
  
}

# Plot ensemble variance for a global variable
#
# Creates a Plotly-enhanced scatter plot showing the spread of values across ensemble 
# members for a given variable, including mean, min, and max ranges.
#
# Args:
#   global_df: Data frame containing global ensemble data with columns for models, 
#              variables, categories, and tooltips.
#   variable: Name of the variable to plot (character).
#   type: Type of variance ('mean', 'iav', etc.) (character).
#
# Returns:
#   A Plotly object displaying ensemble variance for the specified variable. 
#   Includes colored points by parameter category and error bars showing min/max range.
#
# Notes:
#   - Uses sqrt scaling for 'iav' type plots.
#   - Adds LaTeX-style annotation for the y-axis label.
plot_ensemble_variance = function(global_df, variable, type){
  
  variable_name = get_variable_name(variable, type)
  
  label = YLABEL_LOOKUP[[variable]]
  label_text = deparse(label[[1]])
  
  # mean, min, and max for ensemble
  ensemble_range = get_ensemble_range(global_df, variable, type)
  
  ensemble_range = dplyr::mutate(ensemble_range, 
                                 model_name = ifelse(model_name == 'FATES', 
                                                     'CLM-FATES', model_name))
  
  
  if (type == 'iav'){
    
    p <- ggplot() +
      geom_point(data=global_df, 
                 aes(jittered_model, sqrt(.data[[variable_name]]), 
                     colour=category_label, text=tooltip),
                 size=1.75,
                 alpha = 0.7) +
      geom_point(data=ensemble_range, aes(model_index, sqrt(means)), size=4) +
      geom_errorbar(data=ensemble_range, aes(ymin=sqrt(mins), ymax=sqrt(maxes), 
                                             x=model_index), width = 0.2) +
      scale_x_continuous(breaks = ensemble_range$model_index,
                         labels = ensemble_range$model_name) +
      theme_bw() + 
      scale_colour_manual(values = CATEGORY_COLORS,
                          name='Parameter Grouping') +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=10),
            legend.text=element_text(size=10),
            legend.title=element_text(size=10)) +
      xlab(NULL) +
      ylab(NULL)
    
  } else {
    p <- ggplot() +
      geom_point(data=global_df, 
                 aes(jittered_model, .data[[variable_name]], 
                     colour=category_label, text=tooltip),
                 size=1.75,
                 alpha = 0.7) +
      geom_point(data=ensemble_range, aes(model_index, means), size=4) +
      geom_errorbar(data=ensemble_range, aes(ymin=mins, ymax=maxes, 
                                             x=model_index), width = 0.2) +
      scale_x_continuous(breaks = ensemble_range$model_index,
                         labels = ensemble_range$model_name) +
      theme_bw() + 
      scale_colour_manual(values = CATEGORY_COLORS,
                          name='Parameter Grouping') +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=10),
            legend.text=element_text(size=10),
            legend.title=element_text(size=10)) +
      xlab(NULL) +
      ylab(NULL)
    
  }
  
  
  plotly::ggplotly(p, tooltip = "text") %>%
    layout(
      annotations = list(
        list(
          x = -0.3, y = 0.5, xref = "paper", yref = "paper",
          text = YLABEL_LOOKUP_LATEX[[variable]],
          showarrow = FALSE,
          font = list(size = 16,
                      color = "rgba(0, 0, 0, 0.7)", 
                      family = "Arial"),
          textangle = -90
        )
      ),
      margin = list(l = 100),
      xaxis = list(cliponaxis = FALSE) 
    )
  
}

# Plot cumulative variance by number of parameters
#
# Generates a bar plot showing the cumulative fraction of total variance explained
# as parameters are added in chunks, separated by model (CLM vs FATES).
#
# Args:
#   variance_df: Data frame containing variance contributions per parameter and model.
#   variable: Name of the variable to analyze (character).
#   type: Type of variance ('mean', 'iav', etc.) (character).
#   param_chunks: Vector or list defining the chunks of parameters to sum cumulatively.
#
# Returns:
#   A ggplot object displaying cumulative variance fractions for each parameter chunk,
#   with bars colored by model.
#
# Notes:
#   - The plot helps visualize how variance is distributed across parameters for each model.
#   - Colors are set using predefined constants (CLM_COL, FATES_COL).
#   - X-axis shows number of parameters, Y-axis shows fraction of total variance.
plot_cumulative_variance = function(variance_df, variable, type, param_chunks){
  
  # split by model
  fates_variance = filter(variance_df, model == 'FATES')
  clm_variance = filter(variance_df, model == 'CLM')
  
  variable_name = get_variable_name(variable, type)
  
  # calculate cumulative variance for each model
  fates_cumm_variance = get_cumulative_variance(fates_variance, variable_name,
                                                param_chunks) %>%
    mutate(model = 'CLM-FATES')
  
  clm_cumm_variance = get_cumulative_variance(clm_variance, variable_name, 
                                              param_chunks) %>%
    mutate(model = 'CLM')
  
  clm_fates_cumm_variance = rbind(fates_cumm_variance, clm_cumm_variance) %>%
    mutate(model = factor(model, levels = c('CLM-FATES', 'CLM')))
  
  # plot
  ggplot(clm_fates_cumm_variance, aes(param_chunk, propsum)) +
    geom_col(aes(fill=model), position='dodge') +
    theme_bw() +
    xlab('Number of Parameters') +
    ylab('Fraction of Total Variance') +
    scale_fill_manual(values = c(FATES_COL, CLM_COL),
                      name=NULL) +
    ggtitle(paste0('Cumulative variance for ', TYPE_LABEL_LOOKUP[[type]], 
                   ' ', LONGNAME_LOOKUP[[variable]])) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12)) 
}


# Generate a top-N parameter plot
#
# Creates a ggplot showing the top N parameters for a variable, including
# minimum and maximum values as points and connecting segments, with the
# default value shown as a dashed horizontal line. Points are colored by
# category and shaped to distinguish min vs max.
#
# Args:
#   df: Data frame containing the parameter data to plot, must include columns
#       `parameter_name`, `min`, `max`, and `category`.
#   default_data: Single-row data frame with the default value for the variable,
#       with columns `value` and `type`.
#   variable: Character string specifying the variable being plotted (used to
#       determine Y-axis label from YLABEL_LOOKUP).
#   min_vals: Numeric minimum value for the Y-axis limits.
#   max_vals: Numeric maximum value for the Y-axis limits.
#   title: Character string for the plot title.
#
# Returns:
#   A ggplot object showing min/max points and segments for each parameter,
#   colored by category, with the default value as a dashed horizontal line.
#
# Notes:
#   - Points for min and max are shaped differently (filled vs hollow).
#   - The plot is flipped horizontally for readability.
top_n_plot = function(df, default_data, variable, min_vals, max_vals, title){
  
  ggplot(df) + 
    geom_point(aes(parameter_name, min, pch = 'low value', col=category), size = 4, 
               show.legend = T) +
    geom_point(aes(parameter_name, max, pch = 'high value', col=category), size = 4,
               show.legend = T) +
    geom_segment(aes(x = parameter_name, xend = parameter_name, y = min, yend = max,
                     col=category), show.legend = T) +
    geom_hline(data=default_data, aes(yintercept = value, alpha=type), 
               linetype=2, colour='black') +
    scale_y_continuous(limits = c(min_vals, max_vals), expand = expansion(mult = c(0.5, 0.5))) +
    scale_alpha_manual(name = NULL,
                       values = c(1),
                       breaks = c("default"),
                       guide = guide_legend(override.aes = list(linetype = c(2),
                                                                shape = c(NA),
                                                                color = "black"))) +
    scale_shape_manual(values = c(19, 1), name = NULL,
                       guide = guide_legend(override.aes = list(linetype=NULL))) +
    coord_flip() +
    labs(y = YLABEL_LOOKUP[[variable]]) +
    xlab(NULL) + 
    theme_bw() +
    ggtitle(title) +
    scale_colour_manual(name=NULL,
                        values=CATEGORY_COLORS,
                        labels=CATEGORY_LABELS,
                        drop=F) +
    theme(axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          plot.title=element_text(size=16))
  
}

# Plot top-N parameter rankings for FATES and CLM
#
# Generates side-by-side plots showing the top N parameters for a variable,
# including minimum, maximum, and default values, for both FATES and CLM models.
# Uses helper functions to extract top-N parameters, prepare plotting data,
# and generate consistent min/max/default visualizations.
#
# Args:
#   fates_clm_min_max: Data frame containing min and max values for all parameters
#       across FATES and CLM models.
#   fates_ensemble: Ensemble data frame for FATES, including default values.
#   clm_ensemble: Ensemble data frame for CLM, including default values.
#   variable: Character string specifying the variable to plot.
#   type: Character string specifying the variable type (e.g., 'mean', 'iav').
#   n: Integer specifying the number of top parameters to include.
#   param_select: Optional character vector of parameter names to restrict the top-N selection.
#
# Returns:
#   A combined ggplot object showing two plots side by side (CLM and FATES rankings),
#   with min/max points, connecting segments, and dashed lines for default values.
#
# Notes:
#   - Y-axis limits are shared between the two plots for consistent comparison.
#   - The plot title includes the number of top parameters and the variable long name.
plot_top_n = function(fates_clm_min_max, fates_ensemble, clm_ensemble,
                      variable, type, n, param_select=c()){
  
  top_n = get_top_n(fates_clm_min_max, variable, type, n, param_select)
  
  variable_name = get_variable_name(variable, type)
  
  fates_default_data = get_default_data(fates_ensemble, variable_name)
  clm_default_data = get_default_data(clm_ensemble, variable_name)
  
  fates_plot_df = get_top10_plot_df(fates_ensemble, top_n$fates, 
                                    fates_default_data, variable_name)
  
  clm_plot_df = get_top10_plot_df(clm_ensemble, top_n$clm, 
                                  clm_default_data, variable_name)
  
  min_vals = min(c(clm_plot_df$max, clm_plot_df$min, fates_plot_df$min, 
                   fates_plot_df$max))
  max_vals = max(c(clm_plot_df$max, clm_plot_df$min, fates_plot_df$min, 
                   fates_plot_df$max))
  
  p1 = top_n_plot(fates_plot_df, fates_default_data, variable, min_vals, 
                  max_vals, 'CLM-FATES Rankings')
  p2 = top_n_plot(clm_plot_df, clm_default_data, variable, min_vals, 
                  max_vals, 'CLM Rankings')
  
  p = p1 + p2 + plot_layout(guides='collect') +
    plot_annotation(paste0("Top ", n, " parameters for ", 
                           LONGNAME_LOOKUP[[variable]]),
                    theme = theme(
                      plot.title = element_text(size = 16)
                    )) 
  return(p)
  
}

# Plot parameter counts by subcategory and model
#
# Generates a horizontal bar plot showing the number of parameters in each
# subcategory, separated by model (CLM vs FATES). Uses pattern fills to
# distinguish models and color fills to indicate parameter categories.
#
# Args:
#   df: Data frame containing columns `subcat_label`, `count`, `model`, and `category`.
#   title: Character string specifying the plot title.
#
# Returns:
#   A ggplot object showing parameter counts per subcategory with:
#     - Colored bars for categories.
#     - Striped vs solid patterns for CLM vs FATES parameters.
#     - Numeric labels on bars showing counts.
#
# Notes:
#   - Uses geom_col_pattern for pattern-filled bars.
#   - Bars are flipped horizontally (coord_flip) for readability.
#   - Legends are customized to show category colors and model patterns clearly.
param_count_plot = function(df, title) {
  
  ggplot(df, aes(subcat_label, count, group=model)) +
    geom_col_pattern(aes(fill=category, pattern = model),
                     color = "black", 
                     pattern_fill = "black",
                     pattern_colour = 'white',
                     pattern_angle = 45,
                     pattern_density = 0.1,
                     pattern_spacing = 0.025,
                     pattern_key_scale_factor = 0.6) +
    geom_label(aes(label = count, fill=category),
               position = position_stacknudge(y = -0.63),
               label.size=0.0,
               label.r=unit(0.15, 'lines'),
               fontface = "bold",
               colour='white',
               size = 3.5,
               show.legend = F) + 
    scale_pattern_manual(values = c('stripe', 'none'),
                         labels = c('CLM parameters',
                                    'FATES parameters'),
                         name=NULL) +
    theme_bw() +
    xlab(NULL) +
    ylab('Number of Parameters') +
    coord_flip() +
    scale_fill_manual(name=NULL,
                      values=CATEGORY_COLORS,
                      labels=CATEGORY_LABELS) +
    ggtitle(label = title) +
    guides(pattern = guide_legend(override.aes = list(fill = "white", 
                                                      pattern_colour='black')),
           fill = guide_legend(override.aes = list(pattern = "none"))) 
}

# Combine parameter count plots for FATES and CLM
#
# Generates a combined figure showing parameter counts for FATES and CLM models
# by subcategory. Calls `param_count_plot` for each dataset and arranges the
# plots vertically with a shared legend.
#
# Args:
#   fates_param_counts: Data frame of FATES parameter counts (columns include
#       `subcat_label`, `count`, `model`, `category`).
#   clm_param_counts: Data frame of CLM parameter counts (same format as above).
#
# Returns:
#   A patchwork ggplot object combining the FATES and CLM parameter count plots
#   with a shared legend.
#
# Notes:
#   - Uses `param_count_plot` internally to create each subplot.
#   - Arranges plots vertically (`p1 / p2`) using `plot_layout(guides='collect')`.
plot_parameter_counts = function(fates_param_counts, clm_param_counts){
  
  p1 = param_count_plot(fates_param_counts, 'FATES and CLM Parameters')
  p2 = param_count_plot(clm_param_counts, 'CLM Parameters')
  p = p1 / p2 + plot_layout(guides='collect')
  
  return(p)
  
}

# Plot top-N parameter rankings by biome
#
# Generates a faceted plot showing the top N parameters for a given variable,
# separately for FATES and CLM models across multiple biomes. Includes min, max,
# and default values, with parameters ordered within each biome and model.
#
# Args:
#   min_max_biome_df: Data frame of min/max parameter values per biome.
#   fates_biome: Data frame of FATES ensemble data by biome.
#   clm_biome: Data frame of CLM ensemble data by biome.
#   variable: Name of the variable to analyze (character).
#   type: Type of variable ('mean', 'iav', etc.) (character).
#   n: Number of top parameters to display per model and biome.
#   param_select: Optional character vector of parameters to restrict selection.
#   biome_select: Optional character vector of biomes to include.
#
# Returns:
#   A ggplot object with:
#     - Points for min and max parameter values.
#     - Vertical segments connecting min and max.
#     - Horizontal dashed lines for default values.
#     - Facets by biome and model.
#     - Parameters reordered within each biome and model by rank.
#
# Notes:
#   - Uses helper functions `get_top_n`, `get_top10_plot_df`, and `get_default_data`.
#   - Automatically merges biome metadata via `biome_df`.
#   - Supports flexible selection of parameters and biomes.
plot_top_n_by_biome = function(min_max_biome_df, fates_biome, clm_biome, 
                               variable, type, n, biome_df,
                               param_select=c(), biome_select=c()){
  
  variable_name = get_variable_name(variable, type)
  
  top_n_fates = data.frame()
  top_n_clm = data.frame()
  default_fates = data.frame()
  default_clm = data.frame()
  
  if (length(biome_select > 0)){
    biomes = biome_select
  } else{
    biomes = unique(min_max_biome_df$biome)
  }
  
  for (i in 1:length(biomes)){
    
    sub_min_max = filter(min_max_biome_df, biome == biomes[i])
    sub_fates = filter(fates_biome, biome == biomes[i])
    sub_clm = filter(clm_biome, biome == biomes[i])
    
    fates_default_data = get_default_data(sub_fates, variable_name) %>%
      mutate(biome = biomes[i])
    clm_default_data = get_default_data(sub_clm, variable_name) %>%
      mutate(biome = biomes[i])
    
    top_n = get_top_n(sub_min_max, variable, type, n, 
                      select_params=param_select)
    
    fates_plot_df = get_top10_plot_df(sub_fates, top_n$fates, 
                                      fates_default_data, variable_name)
    fates_plot_df$biome = biomes[i]
    
    clm_plot_df = get_top10_plot_df(sub_clm, top_n$clm, 
                                    clm_default_data, variable_name)
    clm_plot_df$biome = biomes[i]
    
    top_n_fates = rbind(top_n_fates, fates_plot_df)
    top_n_clm = rbind(top_n_clm, clm_plot_df)
    default_fates = rbind(default_fates, fates_default_data)
    default_clm = rbind(default_clm, clm_default_data)
  }
  
  top_n_fates$model = 'CLM-FATES'
  top_n_clm$model = 'CLM'
  
  top_n_fates = top_n_fates %>%
    merge(biome_df, by = 'biome') %>%
    mutate(model = 'CLM-FATES')
  
  top_n_clm = top_n_clm %>%
    merge(biome_df, by = 'biome') %>%
    mutate(model = 'CLM')
  
  default_fates = default_fates %>%
    merge(biome_df, by = 'biome') %>%
    mutate(model = 'CLM-FATES')
  
  default_clm = default_clm %>%
    merge(biome_df, by = 'biome') %>%
    mutate(model = 'CLM')
  
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    if (!is.list(within)) {
      within <- list(within)
    }
    
    new_x <- do.call(paste, c(list(x, sep = sep), within))
    stats::reorder(new_x, by, FUN = fun)
  }
  
  
  if (length(biome_select > 0)){
    default_fates = filter(default_fates, biome %in% biome_select)
    default_clm = filter(default_clm, biome %in% biome_select)
    top_n_clm = filter(top_n_clm, biome %in% biome_select)
    top_n_fates = filter(top_n_fates, biome %in% biome_select)
  }
  
  all_top_n = rbind(top_n_clm, top_n_fates) %>%
    mutate(biome_name = as.factor(biome_name),
           model = as.factor(model)) %>%
    mutate(parameter_name = reorder_within(parameter_name, desc(rank), list(biome_name, model)))
  all_default = rbind(default_clm, default_fates)
  
  
  all_top_n = mutate(all_top_n, 
                     model = factor(model, levels=c('CLM-FATES', 'CLM')))
  
  all_default = mutate(all_default, 
                       model = factor(model, levels=c('CLM-FATES', 'CLM')))
  
  ggplot(filter(all_top_n, biome != 0)) + 
    geom_point(aes(parameter_name, min, pch = 'low value', col=category), size = 4, 
               show.legend = T) +
    geom_point(aes(parameter_name, max, pch = 'high value', col=category), size = 4,
               show.legend = T) +
    geom_segment(aes(x = parameter_name, xend = parameter_name, y = min, yend = max,
                     col=category), show.legend = T) +
    geom_hline(data=filter(all_default, biome != 0), aes(yintercept = value, alpha=type), 
               linetype=2, colour='black') +
    scale_alpha_manual(name = NULL,
                       values = c(1),
                       breaks = c("default"),
                       guide = guide_legend(override.aes = list(linetype = c(2),
                                                                shape = c(NA),
                                                                color = "black"))) +
    scale_shape_manual(values = c(19, 1), name = NULL,
                       guide = guide_legend(override.aes = list(linetype=NULL))) +
    coord_flip() +
    ylab(YLABEL_LOOKUP[[variable]]) +
    xlab(NULL) + 
    theme_bw() +
    scale_colour_manual(name=NULL,
                        values=CATEGORY_COLORS,
                        labels=CATEGORY_LABELS,
                        drop=F) +
    facet_wrap(biome_name~model, scales = 'free', ncol=2) + 
    scale_x_reordered() +
    theme(axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          strip.text = element_text(size = 14),
          plot.title=element_text(size=16))
}


# Plot spatial differences for a variable
#
# Generates a raster map showing differences in a variable across space, optionally
# as a model-to-model difference. Includes a diverging color scale centered at zero
# and overlays land boundaries.
#
# Args:
#   dat: Data frame containing columns `x`, `y`, and `<variable>_diff` (or
#        `<variable>_model_diff` if `model_diff = TRUE`).
#   variable: Name of the variable to plot (character).
#   max_diff_val: Maximum absolute value for the color scale (numeric).
#   plot_title: Title of the plot (character).
#   model_diff: Logical indicating whether to plot model-to-model differences.
#   land:       shapefile with land boundaries
#
# Returns:
#   A ggplot object showing spatial differences:
#     - Raster of difference values with diverging color scale.
#     - Overlay of land boundaries from `land` shapefile.
#     - Minimal theme, no axis labels.
difference_plot = function(dat, variable, max_diff_val, plot_title, land, 
                           model_diff=FALSE){
  
  if (model_diff){
    var_name = paste0(variable, '_model')
  } else {
    var_name = variable
  }
  ggplot() +
    geom_raster(data = dat, aes(x=x, y=y, 
                                fill=.data[[paste0(var_name, '_diff')]])) +
    scale_fill_gradient2(midpoint=0,
                         limits = c(-max_diff_val, max_diff_val),
                         name=YLABEL_LOOKUP_DELTA[[variable]]) + 
    geom_sf(data = land, fill = NA, color = "black", size = 0.3) +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    theme(
      legend.title = element_text(size = 16),    
      legend.text = element_text(size = 12)      
    ) +
    ggtitle(plot_title)
  
}

# Plot a global raster map for a variable
#
# Generates a raster map of a variable across the globe for a given model and type.
# Uses a color palette from `PALETTE_LOOKUP`, overlays land boundaries, and applies
# consistent fill limits and labels.
#
# Args:
#   dat: Data frame containing columns `x`, `y`, and `<variable>_<type>` for plotting.
#   variable: Name of the variable to plot (character).
#   fill_limits: Numeric vector of length 2 specifying min and max values for the color scale.
#   type: Type of the variable to plot (e.g., 'mean', 'iav') (character).
#   model: Name of the model (character), used in the plot title.
#   land:       shapefile with land boundaries
#
# Returns:
#   A ggplot object displaying:
#     - Raster of the variable values with the specified color palette and limits.
#     - Land boundaries overlaid from the `land` shapefile.
#     - Minimal theme, no axis labels, horizontal legend at bottom.
#     - Plot title combining model name and type.
global_plot = function(dat, variable, fill_limits, type, model, land){
  
  if (PALETTE_LOOKUP[[variable]] == 'Spectral'){
    dir = -1
  } else{
    dir=1
  }
  
  ggplot() +
    geom_raster(data = dat, aes(x=x, y=y, 
                                fill=.data[[paste0(variable, '_', type)]])) +
    scale_fill_distiller(limits=fill_limits,
                         palette=PALETTE_LOOKUP[[variable]],
                         name=YLABEL_LOOKUP[[variable]],
                         direction=dir) +
    geom_sf(data = land, fill = NA, color = "black", size = 0.3) +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    ggtitle(paste0(model, ': ', type)) +
    theme(
      legend.direction = "horizontal",
      legend.position = 'bottom',
      legend.title = element_text(size = 16),    
      legend.text = element_text(size = 14)      
    ) +
    theme(plot.margin = margin(0, 0, 0, 0, "pt")) 
  
}

# Plot global variable maps for CLM and FATES
#
# Generates a 2x2 grid of global raster maps showing the minimum and maximum
# values of a variable for two models (CLM and FATES). The color scale is
# shared across all plots for direct comparison.
#
# Args:
#   dat_clm: Data frame containing CLM variable values with columns
#             `<variable>_min` and `<variable>_max`, plus `x` and `y`.
#   dat_fates: Data frame containing FATES variable values with columns
#              `<variable>_min` and `<variable>_max`, plus `x` and `y`.
#   variable: Name of the variable to plot (character).
#   land:          shapefile with land boundaries
#
# Returns:
#   A combined ggplot object (2x2) displaying:
#     - Top row: min values for CLM and FATES.
#     - Bottom row: max values for CLM and FATES.
#     - Shared color scale across all plots.
#     - Horizontal legends at the bottom.
#     - Minimal theme with land boundaries overlaid.
plot_global_vals_2models = function(dat_clm, dat_fates, variable, land){
  
  combined_vals <- c(dat_clm[[paste0(variable, "_min")]], 
                     dat_clm[[paste0(variable, "_max")]],
                     dat_fates[[paste0(variable, "_min")]], 
                     dat_fates[[paste0(variable, "_max")]])
  fill_limits <- range(combined_vals, na.rm = TRUE)
  
  p_min_fates = global_plot(dat_fates, variable, fill_limits, 'min', 'CLM-FATES', 
                            land)
  p_max_fates = global_plot(dat_fates, variable, fill_limits, 'max', 'CLM-FATES',
                            land)
  
  p_min_clm = global_plot(dat_clm, variable, fill_limits, 'min', 'CLM',
                          land)
  p_max_clm = global_plot(dat_clm, variable, fill_limits, 'max', 'CLM',
                          land)
  
  p_out = (p_min_clm + p_min_fates) / (p_max_clm + p_max_fates) +
    plot_layout(heights = c(5,5), widths=c(7,7), guides='collect') +
    plot_annotation(theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )) &
    theme(legend.direction = "horizontal",
          legend.position='bottom',
          legend.title = element_text(size = 16),    
          legend.text = element_text(size = 12),
          legend.margin = margin(t = 0, unit = "pt"))
  
  
  return(p_out)
  
}

# Plot global difference maps for CLM and FATES
#
# Generates a 2x1 grid of global raster maps showing the difference between
# maximum and minimum values of a variable for two models (CLM and FATES).
# The color scale is symmetric around zero and shared across both plots.
#
# Args:
#   dat_clm: Data frame containing CLM variable values with a column
#             `<variable>_diff`, plus `x` and `y`.
#   dat_fates: Data frame containing FATES variable values with a column
#              `<variable>_diff`, plus `x` and `y`.
#   variable: Name of the variable to plot (character).
#   land:          shapefile with land boundaries
#
# Returns:
#   A combined ggplot object (2x1) displaying:
#     - Top: CLM difference map (max - min).
#     - Bottom: FATES difference map (max - min).
#     - Shared diverging color scale centered at 0.
#     - Minimal theme with land boundaries overlaid.
plot_global_diffs_2models = function(dat_clm, dat_fates, variable, land){
  
  max_diff_val <- max(c(abs(dat_clm[[paste0(variable, '_diff')]]),
                        abs(dat_fates[[paste0(variable, '_diff')]])), 
                      na.rm = TRUE)
  
  p_diff_fates = difference_plot(dat_fates, variable, max_diff_val, 
                                 paste0('CLM-FATES', ': max - min'), land)
  p_diff_clm = difference_plot(dat_clm, variable, max_diff_val, 
                               paste0('CLM', ': max - min'), land)
  
  
  p_out = p_diff_clm / p_diff_fates + plot_layout(guides='collect')
  
  return(p_out)
  
}

# Plot global difference map for a single model
#
# Generates a global raster map showing the difference between maximum and
# minimum values of a variable for a single model. The color scale is symmetric
# around zero.
#
# Args:
#   dat: Data frame containing variable values with a column `<variable>_diff`,
#        plus `x` and `y` coordinates.
#   variable: Name of the variable to plot (character).
#   model: Name of the model (character), used in the plot title.
#   land: sf object representing land boundaries to overlay on the map.
#
# Returns:
#   A ggplot object displaying:
#     - Difference map (max - min) for the specified model.
#     - Diverging color scale centered at 0.
#     - Land boundaries overlaid.
#     - Minimal theme with no axis labels.
plot_global_diffs_1model = function(dat, variable, model, land){
  
  max_diff_val <- max(abs(dat[[paste0(variable, '_diff')]]), na.rm = TRUE)
  
  p_diff = difference_plot(dat, variable, max_diff_val, 
                           paste0(model, ': max - min'), land)
  
  return(p_diff)
  
}

# Plot global minimum and maximum maps for a single model
#
# Generates side-by-side global raster maps showing the minimum and maximum
# values of a variable for a single model, with a shared color scale.
#
# Args:
#   dat: Data frame containing variable values with columns `<variable>_min` and
#        `<variable>_max`, plus `x` and `y` coordinates.
#   variable: Name of the variable to plot (character).
#   model: Name of the model (character), used in plot titles.
#   land: sf object representing land boundaries to overlay on the maps.
#
# Returns:
#   A patchwork ggplot object displaying:
#     - Min and max raster maps stacked vertically.
#     - Shared color scale based on the combined range of min and max.
#     - Land boundaries overlaid.
#     - Minimal theme with legend arranged vertically.
plot_global_vals_1model = function(dat, variable, model, land){
  
  combined_vals <- c(dat[[paste0(variable, "_min")]], 
                     dat[[paste0(variable, "_max")]])
  fill_limits <- range(combined_vals, na.rm = TRUE)
  
  p_min = global_plot(dat, variable, fill_limits, 'min', model, land)
  p_max = global_plot(dat, variable, fill_limits, 'max', model, land)
  
  p_out = (p_min / p_max)  + plot_layout(guides='collect') &
    theme(legend.direction = "vertical",
          legend.title = element_text(size = 16),    
          legend.text = element_text(size = 12)) 
  
  return(p_out)
  
  
}

# Plot global values for a parameter across models
#
# Generates global raster plots for a given parameter and variable. Handles
# FATES-only, CLM-only, and parameters present in both models, selecting the
# appropriate ensemble data and plotting function.
#
# Args:
#   fates_global_dat: Data frame of FATES-only ensemble data.
#   fatesclm_global_dat: Data frame of combined FATES+CLM ensemble data.
#   clm_global_dat: Data frame of CLM-only ensemble data.
#   parameter: Name of the parameter to plot (character).
#   variable: Name of the variable (character) to plot (used in column names).
#   fates_key: Data frame with ensemble indices for FATES parameters.
#   clm_key: Data frame with ensemble indices for CLM parameters.
#   fates_only: Character vector of parameter names only in FATES.
#   clm_only: Character vector of parameter names only in CLM.
#   fates_and_clm: Character vector of parameter names present in both models.
#   land: sf object representing land boundaries to overlay on the plots.
#
# Returns:
#   A ggplot or patchwork object showing the global distribution of the variable
#   for the requested parameter. Uses single-model or two-model plotting as appropriate.
#   Prints a message if the parameter is not found in any ensemble.
plot_global_vals = function(fates_global_dat, fatesclm_global_dat,
                            clm_global_dat, parameter, variable, fates_key,
                            clm_key, fates_only, clm_only, fates_and_clm,
                            land, frac_rast){
  
  if (parameter %in% fates_only) {
    if (parameter != 'zsno') {
      fates_dat = get_ensemble_df(fates_global_dat, variable, parameter, 
                                  fates_key, frac_rast)
      plot_global_vals_1model(fates_dat, variable, 'CLM-FATES', land)
    } else {
      fates_dat = get_ensemble_df(fatesclm_global_dat, variable, parameter, 
                                  clm_key, frac_rast)
      plot_global_vals_1model(fates_dat, variable, 'CLM-FATES', land)
    }
    
    
  } else if (parameter %in% clm_only) {
    clm_dat = get_ensemble_df(clm_global_dat, variable, parameter, 
                              clm_key, frac_rast)
    plot_global_vals_1model(clm_dat, variable, 'CLM', land)
  } else if (parameter %in% fates_and_clm) {
    fates_dat = get_ensemble_df(fatesclm_global_dat, variable, parameter, 
                                clm_key, frac_rast)
    clm_dat = get_ensemble_df(clm_global_dat, variable, parameter, 
                              clm_key, frac_rast)
    plot_global_vals_2models(clm_dat, fates_dat, variable, land)
  } else {
    print('Not a parameter in this ensemble.')
  }
  
}

# Plot global differences for a parameter across models
#
# Generates raster plots of the difference (max - min) for a given parameter
# and variable. Handles FATES-only, CLM-only, and parameters present in both
# models, selecting the appropriate ensemble data and plotting function.
#
# Args:
#   fates_global_dat: Data frame of FATES-only ensemble data.
#   fatesclm_global_dat: Data frame of combined FATES+CLM ensemble data.
#   clm_global_dat: Data frame of CLM-only ensemble data.
#   parameter: Name of the parameter to plot differences for (character).
#   variable: Name of the variable (character) to plot (used in column names).
#   fates_key: Data frame with ensemble indices for FATES parameters.
#   clm_key: Data frame with ensemble indices for CLM parameters.
#   fates_only: Character vector of parameter names only in FATES.
#   clm_only: Character vector of parameter names only in CLM.
#   fates_and_clm: Character vector of parameter names present in both models.
#   land: sf object representing land boundaries to overlay on the plots.
#
# Returns:
#   A ggplot or patchwork object showing the global difference (max - min)
#   of the variable for the requested parameter. Uses single-model or two-model
#   plotting as appropriate. Prints a message if the parameter is not found
#   in any ensemble.
plot_global_diffs = function(fates_global_dat, fatesclm_global_dat,
                             clm_global_dat, parameter, variable, fates_key,
                             clm_key, fates_only, clm_only, fates_and_clm,
                             land, frac_rast){
  
  if (parameter %in% fates_only) {
    if (parameter != 'zsno') {
      fates_dat = get_ensemble_df(fates_global_dat, variable, parameter, 
                                  fates_key, frac_rast)
      plot_global_diffs_1model(fates_dat, variable, 'CLM-FATES', land)
    } else {
      fates_dat = get_ensemble_df(fatesclm_global_dat, variable, parameter, 
                                  clm_key, frac_rast)
      plot_global_diffs_1model(fates_dat, variable, 'CLM-FATES', land)
    }
    
    
  } else if (parameter %in% clm_only) {
    clm_dat = get_ensemble_df(clm_global_dat, variable, parameter, 
                              clm_key, frac_rast)
    plot_global_diffs_1model(clm_dat, variable, 'CLM', land)
  } else if (parameter %in% fates_and_clm) {
    fates_dat = get_ensemble_df(fatesclm_global_dat, variable, parameter, 
                                clm_key, frac_rast)
    clm_dat = get_ensemble_df(clm_global_dat, variable, parameter, 
                              clm_key, frac_rast)
    plot_global_diffs_2models(clm_dat, fates_dat, variable, land)
  } else {
    print('Not a parameter in this ensemble.')
  }
  
}

# Plot difference of differences (∆FATES - ∆CLM) for a parameter
#
# Computes and plots the difference in max-min values between FATES and CLM
# for parameters present in both models. Returns a raster plot of model differences.
#
# Args:
#   fatesclm_global_dat: Data frame of combined FATES+CLM ensemble data.
#   clm_global_dat: Data frame of CLM-only ensemble data.
#   parameter: Name of the parameter to plot (character).
#   variable: Name of the variable to plot (character, used in column names).
#   clm_key: Data frame with ensemble indices for CLM parameters.
#   fates_and_clm: Character vector of parameters present in both models.
#   land: sf object representing land boundaries for plotting.
#
# Returns:
#   A ggplot object showing the difference of differences (∆FATES - ∆CLM)
#   for the specified variable. Prints a message if the parameter is not in both models.
plot_diff_diffs = function(fatesclm_global_dat, clm_global_dat, parameter, variable, 
                           clm_key, fates_and_clm, land, frac_rast){
  
  if (parameter %in% fates_and_clm) {
    
    fates_dat = get_ensemble_df(fatesclm_global_dat, variable, parameter, 
                                clm_key, frac_rast)
    clm_dat = get_ensemble_df(clm_global_dat, variable, parameter, 
                              clm_key, frac_rast)
    
    dat = merge(fates_dat, clm_dat, by = c('x', 'y'),
                suffixes=c('_FATES', '_CLM'))
    
    dat[,paste0(variable, '_model_diff')] = 
      abs(dat[,paste0(variable, '_diff', '_FATES')]) -
      abs(dat[,paste0(variable, '_diff', '_CLM')])
    
    max_diff_val <- max(abs(dat[[paste0(variable, '_model_diff')]]), na.rm = TRUE)
    
    p_diff = difference_plot(dat, variable, max_diff_val, '∆CLM-FATES - ∆CLM',
                             land, model_diff=TRUE)
    
    return(p_diff)
    
  } else {
    print('Not a parameter that affected both CLM-FATES and CLM')
  }
}

# Plot zonal mean data for a variable and parameter
#
# Generates a line plot of a variable across latitude, showing min/max (or other types)
# values and optionally faceting by model and/or type.
#
# Args:
#   dat: Data frame containing the data to plot. Must include columns for latitude,
#        the variable of interest, type (e.g., 'min'/'max'), and model.
#   variable: Character string for the column name of the variable to plot.
#   parameter: Name of the parameter being plotted (character, used in the plot title).
#   facet_model: Logical, whether to facet by model (default FALSE).
#   facet_type: Logical, whether to facet by type (default FALSE).
#
# Returns:
#   A ggplot object showing the zonal profile of the variable, optionally faceted
#   by model and/or type, with min/max (or other) values distinguished by color and line type.
plot_zonal_dat = function(dat, variable, parameter,
                          facet_model=FALSE, facet_type=FALSE){
  
  dat = mutate(dat, model = ifelse(model == 'FATES', 'CLM-FATES', 'CLM'))
  
  p = ggplot() + 
    geom_line(data=dat, aes(latitude, .data[[variable]], 
                            colour = type, linetype=model), size=1,
              show.legend = T) + 
    scale_colour_manual(values=c(MAX_COL, MIN_COL), name=NULL) +
    scale_linetype_manual(values=c(1, 2), 
                          name=NULL, drop=F) +
    coord_flip() +
    ylab(YLABEL_LOOKUP[[variable]]) +
    theme_bw() +
    theme(axis.text = element_text(size=14),
          legend.text = element_text(size=14),
          strip.text = element_text(size=16),
          axis.title=element_text(size=16),
          plot.title=element_text(size=16),
          legend.title=element_text(size=14))
  
  if (facet_model & !facet_type) {
    p = p + facet_grid(model~.)
  } else if (facet_model & facet_type){
    p = p + facet_grid(model~type)
  } else if (!facet_model & facet_type){
    p = p + facet_grid(.~type)
  }
  
  return(p)
  
  
}

# Plot climatological monthly data for a variable and parameter
#
# Generates a line plot of a variable across months, showing min/max (or other types)
# values and optionally faceting by model and/or type.
#
# Args:
#   dat: Data frame containing the data to plot. Must include columns for month,
#        the variable of interest, type (e.g., 'min'/'max'), and model.
#   variable: Character string for the column name of the variable to plot.
#   parameter: Name of the parameter being plotted (character, used in the plot title).
#   facet_model: Logical, whether to facet by model (default FALSE).
#   facet_type: Logical, whether to facet by type (default FALSE).
#
# Returns:
#   A ggplot object showing the monthly profile of the variable, optionally faceted
#   by model and/or type, with min/max (or other) values distinguished by color and line type.
plot_clim_dat = function(dat, variable, parameter, facet_model=FALSE,
                         facet_type=FALSE){
  
  dat = mutate(dat, model = ifelse(model == 'FATES', 'CLM-FATES', 'CLM'))
  
  p = ggplot() + 
    geom_line(data=dat, aes(month, .data[[variable]], 
                            colour = type, linetype=model), size=1,
              show.legend = T) + 
    scale_colour_manual(values=c(MAX_COL, MIN_COL), name=NULL) +
    scale_linetype_manual(values=c(1, 2), 
                          name=NULL, drop=F) +
    ylab(YLABEL_LOOKUP[[variable]]) +
    xlab(NULL) +
    theme_bw() +
    scale_x_continuous(breaks=seq(1,12),
                       labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May',
                                  'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
                                  'Nov', 'Dec')) +
    theme(axis.text = element_text(size=14),
          legend.text = element_text(size=14),
          strip.text = element_text(size=16),
          axis.title=element_text(size=16),
          plot.title=element_text(size=16),
          legend.title=element_text(size=14))
  
  if (facet_model & !facet_type) {
    p = p + facet_grid(model~.)
  } else if (facet_model & facet_type){
    p = p + facet_grid(model~type)
  } else if (!facet_model & facet_type){
    p = p + facet_grid(.~type)
  }
  
  return(p)
  
  
}

