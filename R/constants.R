#
# #########################
# Purpose: Define constants for the FATES & CLM SP PPE Shiny App
# Author: Adrianna C. Foster, NSF NCAR (afoster@ucar.edu)
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

# variables that can be plotted
VARIABLES = c('GPP', 'EFLX_LH_TOT', 'FSH', 'EF', 'SOILWATER_10CM',
              'FSR', 'FSA', 'FIRE', 'RLNS', 'TV', 'ASA', 'RN', 'BTRANMN',
              'QRUNOFF', 'QSOIL', 'QVEGT', 'QVEGE')

# types that can be plotting
TYPES = c('mean', 'iav')

# colors for max/min plotting
MAX_COL = 'firebrick'
MIN_COL = 'dodgerblue4'

# set colors for categories
CATEGORY_COLORS = c('#104E8B', '#8B008B', '#008B00', '#8B5A2B')
CATEGORY_LABELS = c('Hydrology', 'Biophysics', 
                    'Stomatal Conductance \n& Photosynthesis',
                    'Biogeochemistry')
CATEGORY_LEVELS = c('hydrology', 'biophysics', 'stomatal', 'biogeochemistry')

# mapping from subcategory input to subcategory label for actual plotting
LABEL_DF = data.frame(subcategory = c('radiation', 'canopy aerodynamics', 
                                      'canopy evaporation', 'soil water',
                                      'thermal', 'snow', 'vegetation water',
                                      'photosynthesis', 'LUNA',
                                      'bhs', 'respiration',
                                      'allocation', 'phenology',
                                      'decomposition', 'fire',
                                      'acclimation', 'vegetation dynamics',
                                      'allometry', 'land use', 'mortality',
                                      'nutrient update', 'recruitment'),
                      subcat_label = c('Radiation', 'Canopy Aerodynamics',
                                       'Canopy Evaporation',
                                       'Soil hydraulics',
                                       'Soil thermal properties', 'Snow',
                                       'Vegetation water', 'Photosynthesis',
                                       'LUNA', 'Biomass heat storage',
                                       'Respiration', 'Allocation',
                                       'Phenology', 'Decomposition',
                                       'Fire', 'Acclimation',
                                       'Vegetation dynamics', 'Allometry',
                                       'Land use', 'Mortality', 
                                       'Nutrient Uptake', 'Recruitment'))

CATEGORY_DF = data.frame(category = CATEGORY_LEVELS,
                         category_label = CATEGORY_LABELS)

BIOME_NAMES = c("Ice sheet", "Tropical rain forest", 
                "Tropical seasonal forest/savanna",
                "Subtropical desert", "Temperate rain forest",
                "Temperate seasonal forest", "Woodland/shrubland",
                "Temperate grassland/desert", "Boreal forest", "Tundra")


BIOME_DF = data.frame(biome = seq(0, 9),
                      biome_name = BIOME_NAMES) %>%
  filter(biome != 0)

# colors
FATES_COL = '#3B7D23'
CLM_COL =  '#78206E'
COMMON_COL = "#b8b8b8"

# variable y labels
YLABEL_LOOKUP <- list(
  GPP = expression(GPP~(PgC~yr^{-1})),
  EFLX_LH_TOT = expression(Latent~Heat~(W~m^{-2})),
  FSH = expression(Sensible~Heat~(W~m^{-2})),
  EF = expression(Evaporative~Fraction~(0-1)),
  ASA = expression(Albedo~(0-1)),
  SOILWATER_10CM = expression(Surface~Soil~Water~(PgH[2]*O)),
  BTRANMN = expression(Minimum~Beta_t~(0-1)),
  FSDS = expression(Downward~Shortwave~Radiation~(W~m^{-2})),
  FSR = expression(Upward~Shortwave~Radiation~(W~m^{-2})),
  FSA = expression(Net~Shortwave~Radiation~(W~m^{-2})),
  FIRE = expression(Upward~Longwave~Radiation~(W~m^{-2})),
  RLNS = expression(Net~Longwave~Radiation~(W~m^{-2})),
  RN = expression(Net~Radiation~(W~m^{-2})),
  TV = expression(Vegetation~Temperature~(K)),
  QRUNOFF = expression(Total~Liquid~Runoff~(mm~yr^{-1})),
  QSOIL = expression(Ground~Evaporation~(mm~yr^{-1})),
  QVEGT = expression(Canopy~Transpiration~(mm~yr^{-1})),
  QVEGE = expression(Canopy~Evaporation~(mm~yr^{-1}))
)


# variable y labels
YLABEL_LOOKUP_DELTA <- list(
  GPP = expression(Delta~GPP~(PgC~yr^{-1})),
  EFLX_LH_TOT = expression(Delta~Latent~Heat~(W~m^{-2})),
  FSH = expression(Delta~Sensible~Heat~(W~m^{-2})),
  EF = expression(Delta~Evaporative~Fraction~(0-1)),
  ASA = expression(Delta~Albedo~(0-1)),
  SOILWATER_10CM = expression(Delta~Surface~Soil~Water~(PgH[2]*O)),
  BTRANMN = expression(Delta~Minimum~Beta_t~(0-1)),
  FSDS = expression(Delta~Downward~Shortwave~Radiation~(W~m^{-2})),
  FSR = expression(Delta~Upward~Shortwave~Radiation~(W~m^{-2})),
  FSA = expression(Delta~Net~Shortwave~Radiation~(W~m^{-2})),
  FIRE = expression(Delta~Upward~Longwave~Radiation~(W~m^{-2})),
  RLNS = expression(Delta~Net~Longwave~Radiation~(W~m^{-2})),
  RN = expression(Delta~Net~Radiation~(W~m^{-2})),
  TV = expression(Delta~Vegetation~Temperature~(K)),
  QRUNOFF = expression(Delta~Total~Liquid~Runoff~(mm~yr^{-1})),
  QSOIL = expression(Delta~Ground~Evaporation~(mm~yr^{-1})),
  QVEGT = expression(Delta~Canopy~Transpiration~(mm~yr^{-1})),
  QVEGE = expression(Delta~Canopy~Evaporation~(mm~yr^{-1}))
)

YLABEL_LOOKUP_LATEX <- list(
  GPP = "GPP (PgC yr<sup>-1</sup>)",
  EFLX_LH_TOT = "Latent Heat (W m<sup>-2</sup>)",
  FSH = "Sensible Heat (W m<sup>-2</sup>)",
  EF = "Evaporative Fraction (0-1)",
  ASA = "Albedo (0-1)",
  SOILWATER_10CM = "Surface Soil Water (PgH<sub>2</sub>O)",
  BTRANMN = "Minimum β<sub>t</sub> (0-1)",
  FSDS = "Downward Shortwave Radiation (W m<sup>-2</sup>)",
  FSR = "Upward Shortwave Radiation (W m<sup>-2</sup>)",
  FSA = "Net Shortwave Radiation (W m<sup>-2</sup>)",
  FIRE = "Upward Longwave Radiation (W m<sup>-2</sup>)",
  RLNS = "Net Longwave Radiation (W m<sup>-2</sup>)",
  RN = "Net Radiation (W m<sup>-2</sup>)",
  TV = "Vegetation Temperature (K)",
  QRUNOFF = "Total Liquid Runoff (mm yr<sup>-1</sup>)",
  QSOIL = "Ground Evaporation (mm yr<sup>-1</sup>)",
  QVEGT = "Canopy Transpiration (mm yr<sup>-1</sup>)",
  QVEGE = "Canopy Evaporation (mm yr<sup>-1</sup>)"
)

UNITS_LOOKUP_LATEX <- list(
  GPP = "PgC yr<sup>-1</sup>",
  EFLX_LH_TOT = "W m<sup>-2</sup>",
  FSH = "W m<sup>-2</sup>)",
  EF = "0-1",
  ASA = "0-1",
  SOILWATER_10CM = "PgH<sub>2</sub>O",
  BTRANMN = "0-1",
  FSDS = "W m<sup>-2</sup>",
  FSR = "W m<sup>-2</sup>",
  FSA = "W m<sup>-2</sup>",
  FIRE = "W m<sup>-2</sup>",
  RLNS = "W m<sup>-2</sup>",
  RN = "W m<sup>-2</sup>",
  TV = "K",
  QRUNOFF = "mm yr<sup>-1</sup>",
  QSOIL = "mm yr<sup>-1</sup>",
  QVEGT = "mm yr<sup>-1</sup>",
  QVEGE = "mm yr<sup>-1</sup>"
)

# variable long names
LONGNAME_LOOKUP <- list(
  GPP = "GPP",
  EFLX_LH_TOT = "latent heat",
  FSH = "sensible heat",
  EF = "evaporative fraction",
  ASA = "albedo",
  SOILWATER_10CM = "surface soil water",
  BTRANMN = 'water stress',
  FSDS = "downward shortwave radiation",
  FSR = "upward shortwave radiation",
  FSA = "net shortwave radiation",
  FIRE = "upward longwave radiation",
  RLNS = "net longwave radiation",
  RN = "net radiation",
  TV = "vegetation temperature",
  QRUNOFF = "total liquid runoff",
  QSOIL = "ground evaporation",
  QVEGT = "Canopy Transpiration",
  QVEGE = "canopy evaporation"
  
)

# variable y labels
PALETTE_LOOKUP <- list(
  GPP = 'YlGn',
  EFLX_LH_TOT = 'BrBG',
  FSH = 'Spectral',
  EF = 'BrBG',
  ASA = 'Spectral',
  SOILWATER_10CM = 'BrBG',
  BTRANMN = 'BrBG',
  FSDS = 'Spectral',
  FSR = 'Spectral',
  FSA = 'Spectral',
  FIRE = 'Spectral',
  RLNS = 'Spectral',
  RN = 'Spectral',
  TV = 'Spectral',
  QRUNOFF = 'BrBG',
  QSOIL = 'BrBG',
  QVEGE = 'BrBG',
  QVEGT = 'BrBG'
  
)

# type long names
TYPE_LABEL_LOOKUP <- list(
  mean = 'mean',
  iav = 'interannual variance of'
)

DATASETS = c("Global annual data", "Biome-specific annual data", 
             "Climatology data", "Zonal means data", "Annual maps")

MODELS = c("CLM-FATES", "CLM")
