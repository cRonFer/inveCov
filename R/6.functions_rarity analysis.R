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
  kruskal.test(x_kw ~ g_kw)
  # Kolmogorov smirnov test
  area_values03 <- (na.omit(area_values01))
  area_values04 <- (na.omit(area_values01[surface > 0]))
  ks.test(area_values03, area_values04)
  # This result indicates that the under-sampled area is composed
  # mainly by rare climates. You can inspect the environmental space figures
  # to check which areas were not sampled.
  rarity_plot()
}
