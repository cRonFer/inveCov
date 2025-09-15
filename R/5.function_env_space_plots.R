# Plot PCA and save
# png("PCAbiplot.png", width = 900, height = 900)
# biplot.psych(myPCA, xlim.s= c(-2,4), ylim.s= c(-3,2), main='Biplot PCA - Madagascar at 0.1') # change limits to plot the whole pca
# dev.off()
env_space_plot <- function(){
# Environmental space plots
my_window <- ext(stack) # Env. space limits
## functions ####
simple_labels <- function(x) {format(round(x, 1), nsmall = 1)} # Rounds to 1 decimal place
env_space_plot <- function(rdata, title){
  ggplot() +
    geom_spatraster(data = rdata, aes(fill = after_stat(value))) +
    scale_fill_viridis_c(na.value = "transparent", name = names(rdata)[1]) +
    ggtitle(title) +
    scale_y_continuous(labels = simple_labels) +
    scale_x_continuous(labels = simple_labels) +
    theme_minimal()
}
### Env space comparison (study area vs occurrences vs WS cells)
envs1_plot <- env_space_plot(stack[[1]], "Study area environmental space")
envs2_plot <- env_space_plot(stack[[2]], "Occurrences environmental space")
envs3_plot <- env_space_plot(stack[[3]], "Well survey cells environmental space")
combPlot <- envs1_plot + envs2_plot + envs3_plot
ggsave('envSpacePlots.png', combPlot, height = 6, width = 15, dpi = 600)
}


hist(myPCA$scores[, pcaAxis],
     breaks = ncol(env_space),
     freq = F,
     col = "grey",
     border = FALSE,
     xlim= c(xmin, xmax),
     ylim = c(ymin, ymax),
     main = "",
     xlab = "PC1",
     font = 2, font.lab = 2,
     cex.lab = 1.2, cex.axis = 1.2)
lines(density(na.omit(myPCA$scores[, pcaAxis])), col = "black", lwd = 2)

hist(coords_All[, pcaAxis],
     add = TRUE,
     col = rgb(1, 0, 0, .5),
     freq = F,
     border = FALSE)
lines(density(na.omit(coords_All[, pcaAxis])), col = "red", lwd = 2)

#### histograms
d2 <- as.data.frame(myPCA$scores[, 2])
D2_plot <- ggplot(d2) +
  aes(x = `myPCA$scores[, 2]`) +
  geom_histogram(bins = 30L, fill = "#61779E") +
  labs(x = " ", y = " ") +
  theme_minimal() +
  coord_flip()

d1 <- as.data.frame(myPCA$scores[, 1])

D1_plot <- ggplot(d1) +
  aes(x = `myPCA$scores[, 1]`) +
  geom_histogram(bins = 30L, fill = "#61779E") +
  labs(x = " ", y = " ") +
  theme_minimal()

(env_rast_plot | D2_plot)/ (D1_plot|lay) + plot_layout(nrow = 2, widths = c(1, 1,1))


aligned <- align_plots(D1_plot, env_rast_plot, align = "v", axis = "l")

ggdraw() +
  draw_plot(aligned[[1]], x = 0, y = 0, width = 0.2, height = 0.8) +
  draw_plot(aligned[[2]], x = 0.2, y = 0, width = 0.8, height = 0.8)

env_space_plot_with_histograms <- function(rdata, title) {
  # Convertir raster a data frame
  rdf <- as.data.frame(stack[[1]], xy = TRUE)
  var_name <- names(stack[[1]])[1]

  main_plot <- ggplot(rdf, aes(x = x, y = y)) +
    geom_raster(aes(fill = .data[[var_name]])) +
    scale_fill_viridis_c(na.value = "transparent", name = var_name) +
    ggtitle('title') +
    scale_y_continuous(labels = simple_labels) +
    scale_x_continuous(labels = simple_labels) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 40, r = 40, b = 40, l = 40),
      legend.position = 'none'
    )
  ext <- ext(rdata)
  x_limits <- c(ext$xmin, ext$xmax)
  y_limits <- c(ext$ymin, ext$ymax)
  # Gráfico de densidad para el eje X
  x_density <- ggplot(rdf, aes(x = x)) +
    geom_density(fill = "blue", alpha = 0.3, color = "darkblue", linewidth = 0.8) +
    scale_x_continuous(limits = x_limits) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 40, b = 0, l = 40))

  # Gráfico de densidad para el eje Y
  y_density <- ggplot(rdf, aes(x = y)) +
    geom_density(color = "black", fill="grey", linewidth = 0.8) +
    scale_x_continuous(limits = y_limits) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 40, b = 0, l = 40)) +
    coord_flip()

  aligned_plots <- align_plots(
    main_plot, y_density, x_density,
    align = "hv",
    axis = "tblr"
  )
  # Combinar todo
  plot_grid(
    main_plot,
    y_density,
    x_density,
    NULL,
    nrow = 2, ncol = 2,
    rel_widths = c(4, 1),
    rel_heights = c(4,1),
    align = "hv", axis = "tblr"
  )
}

envs1_plotb <- env_space_plot_with_histograms(stack[[1]], "Madagascar environmental space")
ggplot(rdf, aes(x = x)) +
  geom_density(fill = "blue", alpha = 0.3, color = "darkblue", linewidth = 0.8) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
