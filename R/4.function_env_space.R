env_space_calc <- function(env_space_res,
                           replications,
                           rarity_test = TRUE){
# Standardization of the variables (Normalization - Mean= 0 and Std= 1)
std <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}
# Schoener's D: quantifies the overlap between the location of well-sampled
# sites and cells with most frequent conditions
# The Schoener's D index varies from zero (total lack of congruence) to one
# (total congruence) indicating that the location of well-sampled
# sites coincide with climate conditions that are frequently found in the study area
SchoenersD <- function(x, y) {
  sub_values <- abs(x - y)
  D <- 1 - (sum(sub_values, na.rm = TRUE) / 2)
  return(D)
}
# Function to extract and save test statistics
save_kruskal_csv <- function(formula, filename = "kruskal_stats.txt"){
  result <- kruskal.test(formula)
  # Create data frame with results
  results_df <- data.frame(
    Test = deparse(formula),
    Chi_squared = result$statistic,
    DF = result$parameter,
    P_value = result$p.value,
    Method = result$method,
    StringsAsFactors = FALSE
  )

  # Save to CSV
  write.csv(results_df, filename, row.names = FALSE)
  message(paste("Results saved to:", filename))
  return(results_df)
}
# Rarity analysis
rarity_calc <- function(){
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
  save_kruskal_csv(x_kw ~ g_kw, "rarity_kruskal_Wallis_stats.txt")
  # Kolmogorov smirnov test
  area_values03 <- na.omit(area_values01)
  area_values04 <- na.omit(area_values01[surface > 0])
  writeLines(capture.output(
    ks.test(area_values03, area_values04)),
    paste('Axis', pcaAxis, "rarity_ks_two_sample.txt"))

  # Finally MAP the climatic rarity
  rarity_env <- env_space

  stack <- c(stack, rarity_env) # Join the new raster from order
  names(stack[[4]]) <- 'Rarity index'

  values(rarity_env) <- area_values01
  rarity_Percell <- extract(rarity_env, v4)
  rarity_map <- climRaster_res[[1]]
  values(rarity_map) <- rarity_Percell
  return(rarity_map)
}
# Create working directory
create_and_set_directory('envCoverage_analysis')
# PCA  ####
# First we can reduce the variables to fewer variables using a PCA.
v <- as.data.frame(values(climRaster_res)) # get env values from rasters
# str(v)
v2 <- apply(v, 2, std) # apply standardization function to dataframe of climate values
rem <- apply(is.na(v2), 1, any)
PCAdata <- as.data.frame(v2[!rem, ])
# Run the PCA
mat <- matrix(runif(nrow(PCAdata) * ncol(PCAdata), 0.00001, 0.00009),
              ncol = ncol(PCAdata)) # add a very small randomness to avoid singularity
PCAdata2 <- PCAdata + mat
myPCA <- principal(PCAdata2,
            nfactors = 2,
            rotate = "varimax",
            scores = T)
# prop.table(myPCA$values)

# Values of PCA of our study area ####
# Instead of assuming the absolute values of all bioclimatic variables,
# the next map assumes values of the linear combinations between these variables (PCA scores).
v3 <- v2[, 1:2] # create a vector with the same length as v2 but 2 columns
v3[!rem, ] <- myPCA$scores # insert PCA_scores (2axis-2columns) to the new vector
climate_PCA <- subset(climRaster_res, 1:2) # extract 2 raster layer as a layerbase to insert our pca values
values(climate_PCA) <- v3 # insert pca values of v3 into raster
names(climate_PCA) <- c("PC1", "PC2") # rename

# Environmental space ####
# The next step is to create the environmental space using the two PCA scores.
# Creates a Cartesian plan with PC1 and PC2 scores
v4 <- values(climate_PCA) # get env values from PCA
# Transform the two vars we want into the env space,
# by taking the min and max scores (PCA score values) for each PCA axis
# and create a raster object.
xmin <- min(v4[, 1], na.rm = TRUE)
xmax <- max(v4[, 1], na.rm = TRUE)
ymin <- min(v4[, 2], na.rm = TRUE)
ymax <- max(v4[, 2], na.rm = TRUE)
# This function creates the cartesian plan comprising the min and max PCA scores
env_space <- rast(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax,
                    res = env_space_res) # SET HERE size of bins
values(env_space) <- 0
env_space_area <- env_space # duplicate this object for the next step
# Insert our PCA values into this env_space
# Extract PC1 and PC2, convert it in "classes of values" to plot in the map
env_space_v <- extract(env_space,
                       y = na.omit(v4),
                       cells = TRUE)[, 1]
n_env_space_v <- table(env_space_v) # counts the frequency of "climates" (PC scores)

values(env_space_area)[as.numeric(names(n_env_space_v))] <- n_env_space_v
area_values <- values(env_space_area)
area_values[area_values == 0] <- NA
nCellTotal <- length(area_values[!is.na(area_values)])
values(env_space_area) <- area_values
stack <- env_space_area

# Env. Space of all occurrences of order
data_points <- sf::st_as_sf(data,
       coords = c("Longitude", "Latitude"),
        crs = 4326)
data_points <- vect(data_points)
values_All <- cells(climRaster_res,
                        data_points)[, 2]
coords_All <- v4[values_All, ]
cell_All <- extract(env_space,
                    coords_All,
                            cells = TRUE)[, 1]
n_All <- table(cell_All)
env_space_All <- env_space
values(env_space_All)[as.numeric(names(n_All))] <- n_All
env_space_All[env_space_All == 0] <- NA

stack <- c(stack, env_space_All) # Join the new raster from order
names(stack[[1]]) <- 'Study area'
names(stack[[2]]) <- 'Species Occurrences'

# Well surveyed cells #########
# Env. Space of all occurrences of order
estWS <- vect(estWS)
values_WS <- cells(climRaster_res, estWS)[, 2]
coords_WS <- v4[values_WS, ]
cell_WS <- extract(env_space,
                      coords_WS,
                            cells = TRUE)[, 1]
n_WS <- table(cell_WS)
env_space_WS <- env_space
values(env_space_WS)[as.numeric(names(n_WS))] <- n_WS
env_space_WS[env_space_WS == 0] <- NA
all_WS <- unique(cell_WS)
stack <- c(stack, env_space_WS) # Join the new raster from order
names(stack[[3]]) <- 'Well Surveyed cells'
assign('stack', stack, envir = .GlobalEnv)
# Plot environmental spaces
env_space_plot()

# Schoener's D ####
# Transform the abundance of each cell into probabilities.
# Relative frequency of climate type for all the study area
area_values <- area_values/sum(area_values, na.rm = TRUE)
WS_values <- values(env_space_All)
# Relative frequency of climate type for well-sampled cells
WS_values <- WS_values/sum(WS_values, na.rm = TRUE)

D <- SchoenersD(area_values, WS_values)
print(paste("Climate overlap between well-sampled cells and the study area,",
            "given by the observed Schoener's D equals = ",
            round(D,3), "%"))

# We create a null model to test if D values is different from a
# random distribution of D values calculated from randomly sampling occurrence
# records.
set.seed(0)
D_rnd <- numeric(replications)
for (i in 1:replications){
  rnd <- sample(env_space_v, length(data), replace = TRUE)
  n_rnd <- table(rnd)
  env_space_rnd <- env_space
  values(env_space_rnd)[as.numeric(names(n_rnd))] <- n_rnd
  rnd_values <- values(env_space_rnd)
  rnd_values <- rnd_values/sum(rnd_values, na.rm = TRUE)
  D_rnd[i] <- SchoenersD(area_values, rnd_values)
}
# p-value from previous analysis
# If p <0.05, it means that the location of well-sampled sites does not
# coincide with areas with climate conditions frequently found in your study area
p <- (sum(D > D_rnd) + 1) / (length(D_rnd) + 1) # Unicaudal test
print(paste("p value equals = ", round(p, 3)))

# Kruskal-Wallis and kolmogorov Smirnov tests ####
for (pcaAxis in 1:2){
  # X axis is a probability density
  # Kruskal-Wallis verifies whether 1) the distribution of well-sampled sites
  # is an unbiased subset of the entire climate conditions of the Atlantic forest.
  # If this is so, p > 0.05
  x_axis <- c(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])
  g_axis <- as.factor(c(rep("area", length(myPCA$scores[, pcaAxis])),
                     rep("WS", length(coords_WS[, pcaAxis]))))
  stats_df <- save_kruskal_csv(x_axis ~ g_axis,
                               paste('Axis', pcaAxis,"kruskalWallis_stats.txt"))
  # Kolmogorov smirnov test###
  writeLines(capture.output(
   ks.test(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])),
      paste('Axis', pcaAxis, "ks_two_sample.txt"))
}
# Rarity test ####
if (rarity_test){
      rarity_calc()
      rarity_plotMap()
  }else{
  print("Processing enronmental coverage without rarity analysis")
  }

}
