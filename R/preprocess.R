#
# #########################
# Purpose: Preprocess datasets for the FATES & CLM SP PPE Shiny App
# Author: Adrianna C. Foster, NSF NCAR (afoster@ucar.edu)
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

# ---- data files ----

# directory with all data
INPUT_DIR = 'input_data'

# parameter info files
FATES_PARAMS_FILE = file.path(INPUT_DIR, 'fates_only.txt')
CLM_PARAMS_FILE = file.path(INPUT_DIR, 'clm_only.txt')
COMMON_PARAMS_FILE = file.path(INPUT_DIR, 'fates_and_clm.txt')
PARAM_INFO_FILE = file.path(INPUT_DIR, 'parameter_info.csv')
FATES_KEY_FILE = file.path(INPUT_DIR, 'fates_oaat_key.csv')
CLM_KEY_FILE = file.path(INPUT_DIR, 'clm6sp_oaat_key.csv')

# land information files
LAND_FRAC_FILE = file.path(INPUT_DIR, 'land_frac.nc')
LAND_OUTLINE_FILE = file.path(INPUT_DIR, 'land.shp')

# data files
GLOBAL_DATA_FILE = file.path(INPUT_DIR, 'fates_clm_global_annual_data.csv')
BIOME_DATA_FILE = file.path(INPUT_DIR, 'fates_clm_biome_annual_data.csv')
VARIANCE_DATA_FILE = file.path(INPUT_DIR, 'fates_clm_global_annual_variance.csv')
MIN_MAX_GLOBAL_DF_FILE = file.path(INPUT_DIR, 'fates_clm_min_max_global.csv')
BIOME_DATA_FILE = file.path(INPUT_DIR, 'fates_clm_biome_annual_data.csv')
MIN_MAX_BIOME_FILE = file.path(INPUT_DIR, 'fates_clm_min_max_biome.csv')
ZONAL_FILE = file.path(INPUT_DIR, 'fates_clm_zonal_data.csv')
CLIMATOLOGY_FILE = file.path(INPUT_DIR, 'fates_clm_clim_data.csv')
VAR_DIFF_FILE = file.path(INPUT_DIR, 'fates_clm_var_diff_data.csv')
VAR_DIFF_BIOME_FILE = file.path(INPUT_DIR, 'fates_clm_biome_var_diff_data.csv')
FATES_MAPS_FILE = file.path(INPUT_DIR, 'fates_oaat_annual_maps_nonzero.nc')
CLM_MAPS_FILE = file.path(INPUT_DIR, 'clm_oaat_annual_maps_nonzero.nc')
FATESCLM_MAPS_FILE = file.path(INPUT_DIR, 'fates_oaat_clmpars_annual_maps_nonzero.nc')

SPECIAL_PARS = c('fates_vcmaxse_clmdef', 'fates_vcmaxhd_clmdef',
                'fates_vcmaxha_clmdef', 'fates_jmaxse_clmdef',
                'fates_jmaxhd_clmdef', 'fates_jmaxha_clmdef',
                'fates_new_def')

# ---- end files ----

# --- data setup ---

# land outline
land = st_read(LAND_OUTLINE_FILE)

# land fraction
land_frac_dat = nc_open(LAND_FRAC_FILE)
land_frac = ncvar_get(land_frac_dat, 'landfrac')
frac_lon = ncvar_get(land_frac_dat, "lon")
frac_lat = ncvar_get(land_frac_dat, "lat")
frac_rast = array_to_rast(land_frac, frac_lon, frac_lat)

# parameter information
fates_only_params = readLines(FATES_PARAMS_FILE)
clm_only_params = readLines(CLM_PARAMS_FILE)
common_params = readLines(COMMON_PARAMS_FILE)
all_nonzero_params = unique(c(fates_only_params, clm_only_params, common_params))

fates_key = read.csv(FATES_KEY_FILE) %>%
  dplyr::select(-X)
clm_key = read.csv(CLM_KEY_FILE, header = FALSE)
colnames(clm_key) = c('ensemble_name', 'parameter_name', 'type')
clm_key$ensemble = as.numeric(substr(clm_key$ensemble_name, 
                                     nchar(clm_key$ensemble_name) - 3, 
                                     nchar(clm_key$ensemble_name)))

# formatting for parameter table
col_list = build_col_list(all_nonzero_params, fates_only_params, 
                          clm_only_params, common_params, FATES_COL, CLM_COL,
                          COMMON_COL)
table_info = paste0("background:", col_list, ";")
table_info = paste0(table_info, "color:white;")
table_info = paste0(table_info, "font-family: Arial;")
table_info = paste0(table_info, "font-weight: bold;")

# filter out inactive parameters
param_info = read.csv(PARAM_INFO_FILE) %>%
  dplyr::select(-X) %>%
  filter(parameter_name %in% all_nonzero_params) %>%
  dplyr::arrange(model, parameter_name) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

output_table = param_info
colnames(output_table) = c('Parameter', 'Category', 'Subcategory', 'Description', 
                           'Model')

# split up by model
fates_param_info = filter(param_info, parameter_name %in% c(fates_only_params, common_params))
clm_param_info = filter(param_info, parameter_name %in% c(clm_only_params, common_params))

# get parameter counts
fates_param_counts = get_param_counts(fates_param_info, LABEL_DF)
clm_param_counts = get_param_counts(clm_param_info, LABEL_DF)

# global annual values
global_data_all = read.csv(GLOBAL_DATA_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))
global_df = get_global_df(global_data_all, CATEGORY_DF, CATEGORY_LABELS)
fates_global = filter(global_data_all, model == 'FATES')
clm_global = filter(global_data_all, model == 'CLM')

# global annual variance 
variance_data = read.csv(VARIANCE_DATA_FILE) %>%
  filter(!(parameter %in% SPECIAL_PARS))

# global annual min/max 
min_max_global_df = read.csv(MIN_MAX_GLOBAL_DF_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# global annual variable differences
all_var_diff = read.csv(VAR_DIFF_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# biome annual values
biome_data_all = read.csv(BIOME_DATA_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))
fates_biome = filter(biome_data_all, model == 'FATES')
clm_biome = filter(biome_data_all, model == 'CLM')

# biome annual min/max
min_max_biome_df = read.csv(MIN_MAX_BIOME_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# biome variable differences
all_var_diff_biome = read.csv(VAR_DIFF_BIOME_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# zonal data
all_zonal = read.csv(ZONAL_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# climatology data
all_clim = read.csv(CLIMATOLOGY_FILE) %>%
  filter(!(parameter_name %in% SPECIAL_PARS))

# maps
fates_map_dat = nc_open(FATES_MAPS_FILE)
fatesclm_map_dat = nc_open(FATESCLM_MAPS_FILE)
clm_map_dat = nc_open(CLM_MAPS_FILE)

# ---- end setup ----
