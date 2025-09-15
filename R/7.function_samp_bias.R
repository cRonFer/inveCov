
library(sampbias)
samp_bias_data <- data
grid_wgs64 <- grid %>% st_transform(crs=4326)
setnames(samp_bias_data, 'Longitude', 'decimalLongitude')
setnames(samp_bias_data, 'Latitude', 'decimalLatitude')
out <- calculate_bias(samp_bias_data, terrestrial = TRUE,inp_raster = grid_wgs64)

summary(out)
plot(out)

proj <- project_bias(out)
map_bias(proj)
map_bias(proj, type='diff_to_max')
map_bias(proj, type='log_sampling_rate')

