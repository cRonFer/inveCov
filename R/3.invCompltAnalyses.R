# Packages ---------------------------------------------------------
setup_packages <- function(packages) {
  installed <- installed.packages()
  to_install <- packages[!packages %in% installed]
  if (length(to_install) > 0) {
    message("Installing packages: ", paste(to_install, collapse = ", "))
    install.packages(to_install)
  }
  invisible(lapply(packages, library, character.only = TRUE))
}
# Required packages
required_pkgs <- c('data.table', 'string', 'tidyverse', 'KnowBR', 'sf', 'terra',
                   'rnaturalearth', 'ggplot2', 'svglite', 'patchwork', 'here',
                   'ggExtra', 'biscale', 'cowplot', 'tidyterra')


# Setup packages
setup_packages(required_pkgs)
# Use here package for file paths
# All file paths should use here::here("folder", "file.ext")
cat("Project directory:", here::here(), "\n")


study_area_pol <- ne_countries(country = c(""), scale = "large", returnclass = "sf")
data <- fread('.csv')
inventory_completeness()
