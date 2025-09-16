
# Use here package for file paths
# All file paths should use here::here("folder", "file.ext")
cat("Project directory:", here::here(), "\n")

study_area_pol <- ne_countries(country = c("Madagascar"), scale = "large", returnclass = "sf")
data <- fread('Data/Frogs.csv', sep=';')
inventory_completeness()

# Climatic coverage
# Load Climate data #####
rlist <- list.files(pattern = "*.tif$")
climRaster <- rast(rlist)
raster_mask <- mask(climRaster, study_area_pol)
raster_mask <- crop(raster_mask, study_area_pol)

# Now aggregate raster cells to your study resolution
r <- raster_mask[[1]] # extract 1 raster to check resolution of cells
res(r) # cell res
climRaster_res <- aggregate(raster_mask, fact = 1, fun = mean) # aggregation = x12
# setwd(wd)
# saveRDS(climRaster_res, 'climaticCoverage/climRaster_res')
# climRaster_res <- readRDS('climaticCoverage/climRaster_res')
r <- climRaster_res[[1]] # extract 1 raster to check NEW resolution of cells
res <- res(r)

# WS_cent <- fread('centroides.csv', sep = ";")
env_space_calc(env_space_res = 0.1, replications = 1000)

rarity_calc()

