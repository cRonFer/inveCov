
# Use here package for file paths

cat("Project directory:", here::here(), "\n")
setwd('C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Movidas/package/inveCov')
setwd('C:/Users/MNCN-JHICA/Desktop/proyectoInveCov/inveCov')
### RUN SCRIPT 1 - grid
### RUN SCRIPT 2 - inventory completeness
### RUN SCRIPT packages and create_set_wd

# Load study area
study_area_pol <- ne_countries(country = c("Madagascar"), scale = "large", returnclass = "sf")
# Load occurrences dataset (3 column format: Species, Longitude, Latitude)
data <- fread('Data/Frogs.csv', sep=';')
# data_points <- sf::st_as_sf(data,
#                             coords = c("Longitude", "Latitude"),
#                             crs = 4326)
# data_points <- vect(data_points)

inventory_completeness(data = data,
                       dir_e = 'invComplAnalysis_hex2', # name of folder and analysis
                       resolution = 2, # grid resolution
                       cell_shape = 'hex', # shape of cell ('hex', 'square') if NA square
                       Records_t = 15,
                       Ratio_t = 5,
                       Slope_t = 0.1,
                       Completeness_t = 75,
                       crs = 4326)


# Environmental coverage
# RUN SCRIPT 4 - env spaces
# Load Climate data #####
rlist <- list.files('C:/Users/Joaquin Hortal/Desktop/gis_layers/CHELSA_10km', pattern = "*.tif$")
rlist <- list.files('C:/Users/MNCN-JHICA/Downloads/chelsa', pattern = "*.tif$")
setwd('C:/Users/MNCN-JHICA/Downloads/chelsa')
climRaster <- rast(rlist)
raster_mask <- mask(climRaster, study_area_pol)
raster_mask <- crop(raster_mask, study_area_pol)
climRaster_res <- raster_mask
setwd('C:/Users/MNCN-JHICA/Desktop/proyectoInveCov/inveCov')
# Aggregate raster cells to your study resolution
# r <- raster_mask[[1]] # extract 1 raster to check resolution of cells
# res(r) # cell res
# climRaster_res <- aggregate(raster_mask, fact = 1, fun = mean) # aggregation = x12
# saveRDS(climRaster_res, 'climaticCoverage/climRaster_res')
# climRaster_res <- readRDS('climaticCoverage/climRaster_res')
# r <- climRaster_res[[1]] # extract 1 raster to check NEW resolution of cells
# res <- res(r)

# WS_cent <- fread('centroides.csv', sep = ";")
env_space_calc(env_space_res = 0.1, replications = 500,
               rarity = TRUE) # if TRUE environmental rarity analysis is run



