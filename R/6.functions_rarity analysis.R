############ Rarity analyses ########
# We can check how many environmental space has been sampled
# and how does these cells look like.
rarity_calc <- function( ){
  surface <- WS_values
  surface[is.na(area_values) | area_values == 0] <- NA
  sampled <- sum(surface > 0, na.rm = TRUE) /
              sum(surface >= 0, na.rm = TRUE) * 100
  percen <- round(sampled, 2)
  prin <- paste0(percen, "% of our study area climate types covered by well-sampled cells")
  print(prin)
  # Is this env. space sampled corresponding to rare climates?
  # First, we make values vary from 0 to 1 according to their rarity:
  # Values close to 0, are very common, values close to 1 very rare
  mini <- min(area_values, na.rm = TRUE) # less frequent value
  # rarity index, also called Min-Max scalling
  area_values01 <- abs(1 - (area_values - mini) /
                            (max(area_values, na.rm = TRUE) - mini))
  # see http://rasbt.github.io/mlxtend/user_guide/preprocessing/minmax_scaling/

  # Kruskal-Wallis test to see if the distribution of rarities for the sampled
  # occurrences differs from the distribution observed in the entire area
  x_kw <- c(area_values01, area_values01[surface > 0])
  g_kw <- as.factor(c(rep("area", length(area_values01)),
                        rep("ws", length(area_values01[surface > 0]))))
  save_kruskal_csv(x_kw ~ g_kw, "rarity_kruskalWallis_stats.txt")
  # Kolmogorov smirnov test
  area_values03 <- na.omit(area_values01)
  area_values04 <- na.omit(area_values01[surface > 0])
  writeLines(capture.output(
    ks.test(area_values03, area_values04)),
    paste('Axis', pcaAxis, "rarity_ks_two_sample.txt"))


  # Finally MAP the climatic rarity
  rarity_env <- env_space
  values(rarity_env) <- area_values01
  rarity_Percell <- extract(rarity_env, v4)
  rarity_map <- climRaster_res[[1]]
  values(rarity_map) <- rarity_Percell

  rarity_plotMap <- function(){
    ggplot() +
      geom_spatraster(data = rarity_map, aes(fill = chelsa_ai_1981_2010_last)) +
      geom_sf(data = estWS, fill = 'transparent', color = "black", linewidth = 1) +
      theme_minimal() +
      labs(title = 'Rarity map')+
      theme(plot.background = element_rect(fill = "white", color = "transparent"),
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
  }
  rarity_plotMap()

}
