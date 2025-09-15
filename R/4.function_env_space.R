env_space_calc <- function(env_space_res = env_space_res,
                           replications = replications){
# Standardization of the variables (Normalization - Mean= 0 and Std= 1)
std <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}
# Schoener's D: quantifies the overlap between the location of well-sampled
# sites and cells with most frequent conditions
# The Schoener's D index varies from zero (total lack of congruence) to one
# (total congruence) indicating that the location of well-sampled
# sites coincide with climate conditions
# that are frequently found in the study area
SchoenersD <- function(x, y) {
  sub_values <- abs(x - y)
  D <- 1 - (sum(sub_values, na.rm = TRUE) / 2)
  return(D)
}
# PCA  ####
# First we can reduce the variables to fewer variables using a PCA.
# Here we standardize and prepare the data.
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
prop.table(myPCA$values)

# Values of PCA of our study area ####
# Instead of assuming the absolute values of all Worldclim variables,
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
values_All <- cells(climRaster_res,
                        datapoints)[, 2]
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
WS_cent <- vect(WS_cent)
# Env. Space of all occurrences of order
values_WS <- cells(climRaster_res, WS_cent)[, 2]
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
env_space_plot()
# Schoener's D #######################################
# Transform the abundance of each cell into probabilities.
# Relative frequency of climate type for all the study area
area_values <- area_values/sum(area_values, na.rm = TRUE)
WS_values <- values(env_space_All)
# Relative frequency of climate type for well-sampled cells
WS_values <- WS_values/sum(WS_values, na.rm = TRUE)

D <- SchoenersD(area_values, WS_values)
print(paste("Climate overlap between well-sampled cells and the study area,
            given by the observed Schoener's D equals = ",
            round(D,3), "%"))

# We create a null model to test if D values is different from a
# random distribution of D values calculated from randomly sampling occurrence
# records.
set.seed(0)
D_rnd <- numeric(replications)
for (i in 1:replications) {
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

#Kruskal-Wallis test ####
# The following map shows the distribution of well-sampled cells (red bars)
# vs the study area cells (gray bars)
for (pcaAxis in 1:2){
# X axis is a probability density
# Kruskal-Wallis verifies whether 1) the distribution of well-sampled sites
# is an unbiased subset of the entire climate conditions of the Atlantic forest.
# If this is so, p > 0.05
x_axis <- c(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])
g_axis <- as.factor(c(rep("area", length(myPCA$scores[, pcaAxis])),
                   rep("WS", length(coords_WS[, pcaAxis]))))
kruskal.test(x_axis ~ g_axis)
# Kolmogorov smirnov test###
ks.test(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])
}
}
