## Functions ####

#1 Calculate inventory completeness using KnowBR ####
inventory_completeness <- function(data = data,
                                   dir_e = 'invComplAnalysis',
                                   resolution = 30, # grid resolution
                                   cell_shape = NULL,  # shape of cell ('hex', 'square') if NA square
                                   Records_t = 15,
                                   Ratio_t = 5,
                                   Slope_t = 0.1,
                                   Completeness_t = 75,
                                   crs <- 4326){

  # Study area shpfiles
  study_area_pol_crs <- st_transform(study_area_pol, crs)
  data(adworld) # knowBR needs add world polygon to work

  # Create working directory
  dir.create(paste0('output/',dir_e))
  here::here(paste0('output/',dir_e))

  # Create grid
  grid <- create_grid(resolution = resolution,
                      shapefile = study_area_pol_crs,
                      cell_shape = cell_shape,
                      crs = crs)
  grid_spatial <- st_transform(grid, crs = 4326)
  grid_spatial <- sf::as_Spatial(grid_spatial)
  grid_ea <- grid_spatial
  est_sum <- grid[0, ]
  # Occurrence records
  # Filter dataset to only records with consensus species name:
  data <- data[!is.na('Species'), ]
  # Add field of abundance for knowBR
  data$Counts <- 1
  # Transform data to spatial points ####
  data_points <- sf::st_as_sf(data,
                              coords = c("Longitude", "Latitude"),
                              crs = 4326)
  # Plot of study area + grid + occurrence records subsets
  map_points <- occ_gen_map(data1 = data_points, title = 'Distribuion map')
  ggsave('map_points_combinedFrogs.png', map_points, width = 7, height = 10)

  # Inventory completeness analysis
  KnowBPolygon(data = data, shape = grid_spatial,
               admAreas = FALSE,  # Use predefined grid as personalized polygons
               shapenames = "cell_id", Maps = FALSE,
               dec = ".")
  # Check the estimators from knowBR output:
  est <- read.csv('Estimators.CSV', header = TRUE, sep = ",")
  # Distribution of completeness values by other estimators:
  p1 <- threshold_plot(est$Records, Records_t, 'Records') + ylab('Completeness')
  p2 <- threshold_plot(est$Ratio, Ratio_t, 'Ratio')
  p3 <- threshold_plot(est$Slope, Slope_t, 'Slope')
  completn_thresholds <- p1|p2|p3
  ggsave('_completeness_thresholds.png', completn_thresholds,
         width = 12, height = 5)
  # plot completeness values study area map
  comp_shp <- merge(grid, est, by.x = 'cell_id', by.y = 'Area', all.x = TRUE) # Write here 'by.x' = the unique identifier name of your grid shapefile
  est_sum <- bind_rows(est_sum, comp_shp)
  comp_shp <- comp_shp %>%
    filter(!is.na(Completeness)) %>%
    st_transform(crs)
  complt_map <- complt_plotMap(comp_shp, dir_e)
  ggsave('_completeness_Map.png', complt_map, dpi = 500,
         width = 6, height = 8)
  # Filter est dataset selecting WELL-SURVEY CELLS:
  estWS <- comp_shp %>% filter(Records >= Records_t) %>%
    filter(Ratio >= Ratio_t) %>%
    filter(Slope <= Slope_t) %>%
    filter(Completeness >= Completeness_t)
  # Map of Completeness
  WS_map <- complt_plotMap(estWS,dir_e)
  ggsave('wellSurveyCells_Map.png', WS_map, dpi = 500, width = 6, height = 8)

  # Extract centroids of Well Survey cells for the environmental Space analysis
  WS_cent <- st_centroid(estWS)
  WS_cent <- WS_cent %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                       lat = sf::st_coordinates(.)[,2]) %>%
    dplyr::select(c(cell_id, lon, lat))
  WS_cent$geometry <- NULL
  fwrite(WS_cent, '_centroids.csv', sep = ";")
}

# 2. Plot map of occurrences by Gen information ####
occ_gen_map <- function(data1, title){
  map_points <- ggplot() +
    geom_sf(data= study_area_pol, fill = 'lightgrey', color = "lightgrey", linewidth = 0.7) +
    geom_sf(data = grid, fill = "transparent", color = "black") +
    geom_sf(data = data1, color = 'purple') +
    # geom_sf(data = data2, color = 'orange') +
    coord_sf(crs = crs) +
    ggtitle(title) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = 'white', color = 'white'))
}
# 3. Plot completeness estimators for establishing threshold of well-surveyed cells ----
threshold_plot <- function(var, x, xtitle){
  ggplot(est) +
    geom_point(aes(var, Completeness), pch = 19, size = 1) +
    geom_vline(xintercept = x, col = 'red3', lwd = 1, lty = 2) +
    geom_hline(yintercept = 75, col = 'grey', lwd = 1, lty = 2) + # Here completeness threshold =70%
    theme_minimal() +
    ylab('') +
    xlab(xtitle) +
    theme(strip.text.y = element_blank(),
          axis.text = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10, face = "bold"))
}
#. 4. Plot of inv. completeness values map
complt_plotMap <- function(comp_shp,estWS, title){
  ggplot() +
    geom_sf(data = study_area_pol, fill = 'lightgrey', color = 'darkgrey') +
    geom_sf(data = comp_shp, aes(fill = Completeness), color = "transparent") +
    geom_sf(data = estWS, fill = 'transparent', color = "black", linewidth = 1) +
    scale_fill_viridis_c(limits = c(0, 100), option = "plasma", name = "",
                         direction = -1) +
    theme_minimal() +
    labs(title = title)+
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
}

