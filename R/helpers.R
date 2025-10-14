#
# #########################
# Purpose: Helper functions for the FATES & CLM SP PPE Shiny App
# Author: Adrianna C. Foster
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

# Combines a variable name and a type string into a single identifier.
# 
# Args:
#    variable: A character string giving the base variable name.
#    type: A character string specifying the type.
#
# Returns: 
#    A character string in the format "<variable>_<type>".
get_variable_name <- function(variable, type) {
  paste0(variable, "_", type)
}


# Load and preprocess global ensemble data
#
# Reads a CSV of global model data, merges it with category metadata, filters rows, 
# and adds helper columns for plotting.
#
# Args:
#   global_data_all: Data frame with global annual data
#   category_df: Data frame with category metadata. Must include `category` column.
#   category_labels: Character vector specifying the order of category labels.
#
# Returns:
#   Data frame with filtered and annotated global data. Includes:
#     - category_label: Factor-ordered category labels.
#     - tooltip: Text combining parameter name and type.
#     - model_index: Numeric index for each model.
#     - category_index: Numeric index for each category.
#     - jittered_model: Model index with jitter applied for plotting.
#
# Notes:
#   - Rows with sum_diff <= 0 are removed.
#   - The 'X' column from the CSV (if present) is dropped automatically.
get_global_df = function(global_data_all, category_df, category_labels){
  
  global_df = global_data_all %>%
    dplyr::select(-X) %>%
    filter(sum_diff > 0.0) %>%
    merge(category_df, by ='category', all.x=T) %>%
    mutate(category_label = as.factor(category_label)) %>%
    mutate(category_label = factor(category_label, levels = category_labels),
           tooltip = paste(parameter_name, "-", type),
           model_index = as.numeric(factor(model, levels = c('FATES', 'CLM'))),
           category_index = as.numeric(factor(category)),
           jittered_model = model_index + 
             (category_index - mean(unique(category_index)))*0.15 +
             runif(n(), -0.05, 0.05)) 
  
  return(global_df)
}

# Get ensemble min, mean, and max for a variable
#
# Computes summary statistics (mean, minimum, maximum) for a given variable
# across each model in a global ensemble data frame.
#
# Args:
#   global_df: Data frame containing ensemble data. Must include `model_index` column.
#   variable: Base name of the variable to summarize (character).
#   type: Variable type used to construct the column name (character, e.g., 'mean', 'iav').
#
# Returns:
#   A data frame with one row per model, containing:
#     - model_index: Identifier for the model.
#     - means: Mean value of the variable for that model.
#     - mins: Minimum value of the variable for that model.
#     - maxes: Maximum value of the variable for that model.
#
# Notes:
#   - Uses `get_variable_name()` internally to construct the full column name.
#   - Useful for plotting ensemble ranges or error bars in visualizations.
get_ensemble_range = function(global_df, variable, type){
  
  variable_name = get_variable_name(variable, type)
  
  models = sort(unique(global_df$model_index))
  means = numeric(length(models))
  mins = numeric(length(models))
  maxes = numeric(length(models))
  names = numeric(length(models))
  
  for (i in seq_along(models)){
    mod_dat = dplyr::filter(global_df, model_index == models[i])
    names[i] = unique(mod_dat[['model']])
    means[i] = mean(mod_dat[[variable_name]], na.rm=T)
    mins[i] = min(mod_dat[[variable_name]], na.rm=T)
    maxes[i] = max(mod_dat[[variable_name]], na.rm=T)
  }
  ensemble_range = data.frame(model_index=models,
                              model_name=names,
                              means=means, 
                              mins=mins, 
                              maxes=maxes)
  
  return(ensemble_range)
  
}

# Count parameters per model and category
#
# Groups parameter metadata by model, category, and subcategory, counts the number
# of parameters in each group, and attaches human-readable labels for subcategories.
# Factor ordering is applied to categories for consistent plotting.
#
# Args:
#   param_info: Data frame of parameter metadata. Must include columns `model`, `category`, and `subcategory`.
#   label_df: Data frame containing subcategory labels, with columns `subcategory` and `subcat_label`.

# Returns:
#   A data frame with one row per model/category/subcategory combination, containing:
#     - model: Identifier for the model.
#     - category: Factor-ordered category.
#     - subcategory: Subcategory identifier.
#     - count: Number of parameters in that group.
#     - subcat_label: Human-readable label for the subcategory.
#
# Notes:
#   - Useful for generating plots of parameter counts across models and categories.
#   - Ensures consistent ordering and labeling of categories and subcategories in visualizations.
get_param_counts = function(param_info, label_df){
  
  param_counts = group_by(param_info, model, category, subcategory) %>%
    reframe(count = n()) %>%
    mutate(category = as.factor(category)) %>%
    mutate(category = factor(category, CATEGORY_LEVELS)) %>%
    merge(label_df, by = 'subcategory', all.x = T) %>%
    mutate(subcat_label = forcats::fct_reorder(subcat_label, 
                                               desc(as.numeric(category))))
  
  return(param_counts)
}

# Build a list of colors for table backgrounds
#
# Combines separate parameter lists and their associated colors to generate a
# single color vector corresponding to all parameters. Used for consistent
# coloring of tables showing parameter groups.
#
# Args:
#   params: Vector or list of all parameter names.
#   fates_only: Vector of parameters exclusive to FATES.
#   clm_only: Vector of parameters exclusive to CLM.
#   fates_and_clm: Vector of parameters common to both FATES and CLM.
#   fates_color: Hex code for FATES-only parameters.
#   clm_color: Hex code for CLM-only parameters.
#   joint_color: Hex code for parameters present in both models.
#
# Returns:
#   A vector of colors the same length as `params`, with each element corresponding
#   to the appropriate parameter group.
#
# Notes:
#   - Useful for coloring tables or plots to visually distinguish parameter groups.
#   - Ensures consistent color mapping across tables and figures.
build_col_list = function(params, fates_only, clm_only, fates_and_clm, 
                          fates_color, clm_color, joint_color){
  col_list = numeric(length(params))
  
  for (i in 1:length(params)){
    if (params[i] %in% fates_only) {
      col_list[i] = fates_color
    } else if (params[i] %in% clm_only){
      col_list[i] = clm_color
    } else if (params[i] %in% fates_and_clm){
      col_list[i] = joint_color
    }
  }
  
  return(col_list)
  
}

# Compute cumulative variance by parameter chunks
#
# Calculates cumulative variance contributions as parameters are added in 
# specified chunks. Useful for understanding how much variance is captured 
# as more parameters are included in the analysis.
#
# Args:
#   variance_df: Data frame containing variance values per parameter. Must include
#                a `parameter` column and a column matching `variable_name`.
#   variable_name: Name of the column in `variance_df` containing the variance values.
#   param_chunks: Integer or vector defining the size or grouping of parameters for
#                 cumulative summation.
#
# Returns:
#   A data frame with one row per parameter chunk, including:
#     - param_chunk: Index of the parameter chunk.
#     - varsum: Sum of variance values in that chunk.
#     - cumsum: Cumulative sum of variance up to that chunk.
#     - propsum: Fraction of total variance captured up to that chunk.
#
# Notes:
#   - Helps visualize the distribution of variance across parameters.
#   - Typically used to compare cumulative variance contributions between models.
get_cumulative_variance = function(variance_df, variable_name, param_chunks){
  
  parameters = unique(variance_df$parameter)
  sum_variance = sum(variance_df[,variable_name])
  
  cumm_variance = dplyr::arrange(variance_df, desc(.data[[variable_name]])) %>%
    dplyr::mutate(param_chunk = param_chunks + 
             param_chunks*floor(seq(0, length(parameters)-1)/param_chunks)) %>%
    dplyr::group_by(param_chunk) %>%
    dplyr::reframe(varsum = sum(.data[[variable_name]])) %>%
    dplyr::mutate(
      cumsum = cumsum(varsum),
      propsum = cumsum/sum_variance
      ) %>%
    dplyr::filter(param_chunk <= 50)
  
  return(cumm_variance)
}



# Get top N parameters for a variable by model
#
# Identifies the top N parameters ranked by a specified variable separately for 
# FATES and CLM models. Can optionally restrict the selection to a subset of parameters.
#
# Args:
#   min_max_df: Data frame containing parameter values per model. Must include
#               `model`, `parameter_name`, and the column for the variable of interest.
#   variable: Name of the base variable to rank parameters by (character).
#   type: Type of the variable (used with `get_variable_name()`) (character).
#   n: Number of top parameters to return per model (integer).
#   select_params: Optional character vector of parameter names to restrict selection.
#
# Returns:
#   A list with two elements:
#     - fates: Data frame of top N parameters for FATES, with a `rank` column.
#     - clm: Data frame of top N parameters for CLM, with a `rank` column.
#
# Notes:
#   - Helps identify which parameters have the largest impact on the chosen variable.
#   - Useful for both joint and model-specific parameter analyses.
get_top_n = function(min_max_df, variable, type, n, select_params=c()) {
  
  if (length(select_params) > 0){
    min_max_df = dplyr::filter(min_max_df, parameter_name %in% select_params)
  }
  
  variable_name = get_variable_name(variable, type)
  
  fates_ranked = dplyr::filter(min_max_df, model == 'FATES') %>%
    dplyr::arrange(desc(.data[[variable_name]]))
  fates_top_n = fates_ranked[1:n,]
  fates_top_n$rank = seq_len(n)
  
  
  clm_ranked = dplyr::filter(min_max_df, model == 'CLM') %>%
    dplyr::arrange(desc(.data[[variable_name]]))
  clm_top_n = clm_ranked[1:n,]
  clm_top_n$rank = seq_len(n)
  
  return(list(fates=fates_top_n, clm=clm_top_n))
  
}

# Get default value from an ensemble data frame
#
# Extracts the default value for a specified variable from an ensemble data frame.
# By default, the ensemble with index 0 is selected.
#
# Args:
#   ensemble_df: Data frame containing ensemble data, including `ensemble` and the variable column.
#   variable_name: Column name of the variable to extract (character).
#   ensemble_ind: Index of the ensemble to use as default (integer, default = 0).
#
# Returns:
#   A data frame with a single row containing:
#     - type: Character string 'default'.
#     - value: Value of the specified variable from the selected ensemble.
#
# Notes:
#   - Useful for comparing default model outputs against parameter perturbations.
#   - The returned data frame is structured for easy integration into plotting or further analysis.
get_default_data = function(ensemble_df, variable_name, ensemble_ind=0){
  default_df = dplyr::filter(ensemble_df, ensemble == ensemble_ind)
  default_data = data.frame(type='default', 
                            value=default_df[1, variable_name])
  return(default_data)
}

# Get default values for a variable by biome from an ensemble
#
# Extracts default values for a specified variable across all biomes from an ensemble data frame.
# By default, the ensemble with index 0 is selected.
#
# Args:
#   ensemble_df: Data frame containing ensemble data, including `ensemble`, `biome`, and the variable column.
#   variable_name: Column name of the variable to extract (character).
#   ensemble_ind: Index of the ensemble to use as default (integer, default = 0).
#
# Returns:
#   A data frame with one row per biome containing:
#     - type: Character string 'default'.
#     - value: Value of the specified variable for that biome.
#     - biome: Biome identifier.
#
# Notes:
#   - Useful for comparing default model outputs across different biomes.
#   - The returned data frame is structured for easy integration into plotting or further analysis.
get_default_biome_data = function(ensemble_df, variable_name, ensemble_ind=0){
  default_df = dplyr::filter(ensemble_df, ensemble == ensemble_ind)
  default_data = data.frame(type='default',
                            value = default_df[,variable_name],
                            biome = default_df$biome)
  return(default_data)
}

# Prepare top-N parameter data for plotting
#
# Generates a data frame suitable for plotting the top N parameters of a variable,
# including min, max, and default values. Reorders parameters by rank and ensures
# categorical variables are properly formatted for plotting.
#
# Args:
#   ensemble_df: Data frame containing ensemble data with columns `parameter_name`, `category`, 
#                `subcategory`, `long_name`, `type`, and `model`.
#   top_n_df: Data frame of the top N parameters with columns `parameter_name` and `rank`.
#   default_data: Single-row data frame containing the default value for the variable (`value` column).
#   variable_name: Column name of the variable to use (character).
#
# Returns:
#   A data frame ready for plotting with columns:
#     - rank: Rank of the parameter.
#     - parameter_name: Factor-ordered parameter names.
#     - min: Minimum value of the parameter (filled with default if missing).
#     - max: Maximum value of the parameter (filled with default if missing).
#     - default: Default value for the parameter.
#     - subcategory: Subcategory identifier.
#     - long_name: Long descriptive name of the parameter.
#     - type: Type of the variable (e.g., 'mean', 'min', 'max').
#     - model: Model identifier.
#
# Notes:
#   - Ensures all necessary columns are formatted for ggplot plotting.
#   - Handles missing min/max values by filling with the default.
get_top10_plot_df = function(ensemble_df, top_n_df, default_data, variable_name){
  
  # create the data frame
  plot_df = dplyr::filter(ensemble_df, parameter_name %in% top_n_df$parameter_name) %>%
    merge(dplyr::select(top_n_df, parameter_name, rank), by = 'parameter_name') %>%
    dplyr::select(all_of(c('rank', 'parameter_name', variable_name, 'category', 
                           'subcategory', 'long_name', 'type', 'model'))) %>%
    reshape2::dcast(rank + category + parameter_name ~ type, value.var = variable_name) %>%
    dplyr::mutate(parameter_name = forcats::fct_reorder(parameter_name, desc(rank))) %>%
    dplyr::mutate(category = as.factor(category)) %>%
    dplyr::mutate(category = factor(category, levels = CATEGORY_LEVELS))
  
  # correct in case we have a missing min/max
  plot_df$min[is.na(plot_df$min)] = default_data$value
  plot_df$max[is.na(plot_df$max)] = default_data$value
  
  return(plot_df)
  
}

# Get ensemble indices for a parameter
#
# Returns the ensemble indices corresponding to the minimum and maximum values
# of a given parameter. If no min/max entries exist, a default index is returned.
#
# Args:
#   key: Data frame containing ensemble information with columns `parameter_name`, 
#        `type` (must include 'min' and 'max'), and `ensemble`.
#   parameter: Character string specifying the parameter name to look up.
#   default_ind: Integer specifying the default ensemble index to return if no min or max 
#                entry is found (default is 0).
#
# Returns:
#   A list with two elements:
#     - ensemble_min: Ensemble index for the minimum value of the parameter, or `default_ind` if missing.
#     - ensemble_max: Ensemble index for the maximum value of the parameter, or `default_ind` if missing.
#
# Notes:
#   - Useful for quickly identifying which ensemble members correspond to extreme parameter values.
get_ensemble_index = function(key, parameter, default_ind=0){
  ensemble_min_df = dplyr::filter(key, parameter_name == parameter &
                                    type == 'min')
  if (nrow(ensemble_min_df) == 0){
    ensemble_min = default_ind
  } else{
    ensemble_min = ensemble_min_df[,'ensemble'] 
  }
  
  ensemble_max_df = dplyr::filter(key, parameter_name == parameter &
                                    type == 'max')
  if (nrow(ensemble_max_df) == 0){
    ensemble_max = default_ind
  } else{
    ensemble_max = ensemble_max_df[,'ensemble'] 
  }
  return(list(ensemble_min=ensemble_min,
              ensemble_max=ensemble_max))
}

# Prepare a 2D array for raster conversion
#
# Shifts longitudes to [-180, 180], reorders the array by longitude, and flips 
# the latitude dimension if necessary. Returns a transposed array along with 
# the processed longitude and latitude vectors, ready for raster conversion.
#
# Args:
#   arr: 2D numeric array with dimensions longitude x latitude.
#   lon: Numeric vector of longitudes corresponding to the array columns.
#   lat: Numeric vector of latitudes corresponding to the array rows.
#
# Returns:
#   A list containing:
#     - arr: Transposed, longitude-ordered, latitude-corrected array.
#     - lon: Longitude vector shifted to [-180, 180].
#     - lat: Latitude vector (flipped if originally descending).
#
# Notes:
#   - Ensures compatibility with raster creation functions.
#   - Latitude is flipped only if the vector is descending.
prep_raster_array = function(arr, lon, lat){
  
  # shift longitudes to [-180, 180] if necessary
  lon_shifted = ifelse(lon > 180, lon - 360, lon)
  order_idx = order(lon_shifted)
  lon_ordered = lon_shifted[order_idx]
  
  # reorder array according to longitude
  arr_ordered <- arr[order_idx, ]
  
  # reverse latitude if necessary
  if (is.unsorted(lat)) {
    lat = rev(lat)
    arr_ordered = arr_ordered[, ncol(arr_ordered):1]
  }
  
  list(arr = t(arr_ordered), lon = lon_shifted, lat = lat)
}

# Convert a 2D array to a raster
#
# Prepares a 2D array using prep_raster_array (shifts longitudes, orders columns, 
# flips latitudes if needed) and converts it into a terra::rast object with 
# geographic coordinates (EPSG:4326).
#
# Args:
#   arr: 2D numeric array with dimensions longitude x latitude.
#   lon: Numeric vector of longitudes corresponding to array columns.
#   lat: Numeric vector of latitudes corresponding to array rows.
#
# Returns:
#   A terra::rast object with the array values, properly oriented, with correct 
#   spatial extent and CRS.
#
# Notes:
#   - Uses prep_raster_array internally to ensure longitude and latitude ordering.
#   - The raster is flipped vertically to match typical geographic orientation.
array_to_rast = function(arr, lon, lat){
  
  prep = prep_raster_array(arr, lon, lat)
  
  # create raster
  r_var = terra::rast(prep$arr,
               extent = c(min(prep$lon), max(prep$lon), min(prep$lat), max(prep$lat)),
               crs = "EPSG:4326")
  
  # flip to correct orientation
  r_var = terra::flip(r_var, direction='vertical')
  
  return(r_var)
  
}

# Extract ensemble min/max rasters and compute differences
#
# Reads ensemble indices from a NetCDF file, converts the min and max slices
# of a variable to raster format, applies a scaling factor, and returns a 
# combined data frame with min, max, and difference values.
#
# Args:
#   nc: NetCDF object (from ncdf4::nc_open) containing the variable and ensemble dimension.
#   variable: Name of the variable in the NetCDF file (character).
#   parameter: Parameter name for which ensemble min/max is selected (character).
#   key: Data frame containing ensemble indices for parameters (used by get_ensemble_index).
#   frac_rast: Raster of numeric factor applied to the raster values.
#
# Returns:
#   A data frame with columns:
#     x: Longitude coordinates.
#     y: Latitude coordinates.
#     <variable>_min: Minimum value of the variable for the ensemble.
#     <variable>_max: Maximum value of the variable for the ensemble.
#     <variable>_diff: Difference between max and min values.
#
# Notes:
#   - Min and max rasters are computed using array_to_rast().
#   - The function returns a tidy data frame ready for plotting or analysis.
get_ensemble_df = function(nc, variable, parameter, key, frac_rast){
  
  ensembles = ncdf4::ncvar_get(nc, 'ensemble')
  lon = ncdf4::ncvar_get(nc, "lon")
  lat = ncdf4::ncvar_get(nc, "lat")
  ensemble_keys = get_ensemble_index(key, parameter)
  min_index = which(ensembles == ensemble_keys$ensemble_min)
  max_index = which(ensembles == ensemble_keys$ensemble_max)
  
  arr_min = ncdf4::ncvar_get(nc, variable)[,,min_index]
  arr_max = ncdf4::ncvar_get(nc, variable)[,,max_index]
  r_min = array_to_rast(arr_min, lon, lat)*frac_rast
  r_max = array_to_rast(arr_max, lon, lat)*frac_rast
  
  # convert rasters to data frames
  dat_r_min = as.data.frame(r_min, xy = TRUE, na.rm = TRUE) %>%
    rename_with(.fn = ~ variable, .cols = all_of('lyr.1'))
  dat_r_max = as.data.frame(r_max, xy = TRUE, na.rm = TRUE) %>%
    rename_with(.fn = ~ variable, .cols = all_of('lyr.1'))
  
  # merge min/max and compute difference
  dat = merge(dat_r_min, dat_r_max, by = c('x', 'y'), 
              suffixes=c('_min', '_max'))
  dat[,paste0(variable, '_diff')] = dat[,paste0(variable, '_max')] -
    dat[,paste0(variable, '_min')]
  
  return(dat)
  
}

# Reorder factor levels within groups
#
# Reorders the levels of a factor `x` based on a summary statistic of `by`,
# but separately within one or more grouping factors. Useful for grouped
# plotting (e.g., faceted ggplot bar charts) where ordering should respect groups.
#
# Args:
#   x: Factor or character vector to be reordered.
#   by: Numeric vector used to determine the ordering of `x`.
#   within: Factor, character vector, or list of factors defining groups within which to reorder `x`.
#   fun: Summary function applied to `by` within each level of `x` (default: mean).
#   sep: Character string used to separate `x` and `within` when creating a temporary combined factor.
#   ...: Additional arguments passed to `FUN`.
#
# Returns:
#   A reordered factor with levels adjusted within each group defined by `within`.
#
# Notes:
#   - Often used with ggplot2 and scale_x_reordered() for clean grouped plots.
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  if (!is.list(within)) {
    within <- list(within)
  }
  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}


# Helper to get min/max rows or fallback
#
# Returns the rows corresponding to the minimum and maximum values for a given
# parameter and model, or falls back to a default row if min/max are not available.
#
# Args:
#   dat: Data frame containing ensemble data (must include columns 'model' and 'parameter_name').
#   model: Character string specifying the model to subset.
#   parameter: Character string specifying the parameter to subset.
#
# Returns:
#   A data frame containing only the min and max rows for the specified parameter
#   and model, or the default row if no min/max entries exist.
get_min_max <- function(dat, model_name, parameter) {
  sub <- dplyr::filter(dat, model == model_name & parameter_name == parameter)
  
  get_row <- function(type_val) {
    row <- dplyr::filter(sub, type == type_val)
    if (nrow(row) == 0) {
      row <- dplyr::filter(dat, model == model_name & ensemble == 0)
      row$type <- type_val
    }
    row
  }
  
  rbind(get_row("min"), get_row("max"))
}

# Prepare plotting data for a parameter (refactored)
#
# Subsets and formats ensemble data a given parameter. Handles
# FATES-only, CLM-only, and parameters present in both models. Ensures that
# min/max entries exist, substituting ensemble 0 if missing.
#
# Args:
#   dat: Data frame containing ensemble data (must include columns `model`, `parameter_name`, `type`, `ensemble`).
#   parameter: Character string specifying the parameter to subset.
#   fates_only: Character vector of parameter names present only in FATES.
#   clm_only: Character vector of parameter names present only in CLM.
#   fates_and_clm: Character vector of parameter names present in both FATES and CLM.
#
# Returns:
#   A subsetted data frame ready for mapping, with `model` as a factor (levels: FATES, CLM).
get_plot_dat = function(dat, parameter, fates_only, clm_only, fates_and_clm){
  
  if (parameter %in% fates_only) {
    sub <- get_min_max(dat, "FATES", parameter)
    
  } else if (parameter %in% clm_only) {
    sub <- get_min_max(dat, "CLM", parameter)
    
  } else if (parameter %in% fates_and_clm) {
    sub_fates <- get_min_max(dat, "FATES", parameter)
    sub_clm <- get_min_max(dat, "CLM", parameter)
    sub <- rbind(sub_fates, sub_clm)
  }
    
  sub = sub %>%
    dplyr::mutate(model = as.factor(model)) %>%
    dplyr::mutate(model = factor(model, levels = c('FATES', 'CLM')))
  
  return(sub)
}


