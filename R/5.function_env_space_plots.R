# Plot PCA and save
# png("PCAbiplot.png", width = 900, height = 900)
# biplot.psych(myPCA, xlim.s= c(-2,4), ylim.s= c(-3,2), main='Biplot PCA - Madagascar at 0.1') # change limits to plot the whole pca
# dev.off()
env_space_plot <- function(){
# Environmental space plots
my_window <- ext(stack) # Env. space limits
## functions ####
simple_labels <- function(x) {format(round(x, 1), nsmall = 1)} # Rounds to 1 decimal place
envspace_palette <- colorRampPalette(c("#495970", "#6C7B8B", "#8DB6CD", "#FFB6C1", "#F08080"))
env_space_gradient_colors <- envspace_palette(100)
env_space_plot <- function(rdata, pal_col){
  ggplot() +
    geom_spatraster(data = rdata, aes(fill = after_stat(value))) +
    scale_fill_gradientn(colours = pal_col,
                         na.value = "transparent", name = '') +
    scale_y_continuous(labels = simple_labels) +
    scale_x_continuous(labels = simple_labels) +
    theme_minimal()
}
### Env space comparison (study area vs occurrences vs WS cells)
  envs1_plot <- env_space_plot(stack[[1]], env_space_gradient_colors)
  envs2_plot <- env_space_plot(stack[[2]], env_space_gradient_colors)
  envs3_plot <- env_space_plot(stack[[3]], env_space_gradient_colors)

  combPlot <- envs1_plot + envs2_plot + envs3_plot
  ggsave('envSpacePlots.png', combPlot, height = 6, width = 15, dpi = 600)

}
#### frequency plots #####
# d1 <- as.data.frame(myPCA$scores[, 1])
# d1$ax2 <- as.data.frame(myPCA$scores[, 2])
#
# dd1 <- ggplot(d1, aes(x = myPCA$scores[, 1])) +
#   geom_density(fill = "grey", alpha = 0.7, color = "black") +
#   theme_void() +
#   theme(plot.margin = margin(0, 0, 0, 0))
#
# dd2 <- ggplot(d2, aes(x = myPCA$scores[, 2])) +
#   geom_density(fill = "grey", alpha = 0.7, color = "black") +
#   theme_void() +
#   theme(plot.margin = margin(0, 0, 0, 0)) +
#   coord_flip()

# egg::ggarrange(dd1, arr1,
#                nrow=2,
#                ncol=1,
#                widths=c(1,7)
# )
#
# rdf <- as.data.frame(stack[[2]], xy = TRUE)
# ggplot(rdf, aes(x = rdf$`Species Occurrences`)) +
#   geom_density(fill = "grey", alpha = 0.7, color = "black") +
#   theme_void() +
#   theme(plot.margin = margin(0, 0, 0, 0)) +
#   coord_flip()

## rarity ####
# Rarity map plot
rarity_plotMap <- function(){
  rarity_palette <- colorRampPalette(c( "#8B8B7A","#CDCDB4", "#FFFFE0",
                                       "#FFC0CB",  "#CD6889", "#8B475D"))
  rarity_gradient_colors <- rarity_palette(100)
  envspace_rarity_plot <- env_space_plot(stack[[4]], rarity_gradient_colors)

  rarPlot <- ggplot() +
    geom_spatraster(data = rarity_map, aes(fill = after_stat(value))) +
    scale_fill_gradientn(colours = rarity_gradient_colors,
                        na.value = "transparent", name ='') +
    geom_sf(data = WS_cent, color = "black", size = 3) +
    theme_minimal() +
    labs(title = 'Rarity map') +
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))

  combPlot2 <- envspace_rarity_plot + rarPlot
  ggsave('rarity_Plots.png', combPlot2, height = 6, width = 12, dpi = 600)
} ## add histograms aligned
#### frequency plots
# hist(area_values01,
#      breaks = ncol(env_space),
#      freq = FALSE, col = "grey",
#      main = "",
#      xlab = "Climate rarity", border = FALSE,
#      ylim = c(0, 4),
#      font = 2, font.lab = 2,
#      cex.lab = 1.6, cex.axis = 1.6)
# lines(density(na.omit(area_values01)), col = "black", lwd = 2)
#
# hist(area_values01[surface > 0],
#      breaks = 5,
#      freq = FALSE,
#      add = TRUE,
#      col = rgb(0, 1, 0, .5),
#      border = FALSE)
# lines(density(na.omit(area_values01[surface > 0])), col = "lightgreen", lwd = 2)







