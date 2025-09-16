
# Packages ---------------------------------------------------------
setup_packages <- function(packages){

  installed <- installed.packages()

  to_install <- packages[!packages %in% installed]

  if (length(to_install) > 0) {
    message("Installing packages: ", paste(to_install, collapse = ", "))
    install.packages(to_install)
  }
  # Load
  invisible(lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      stop("No se pudo cargar el paquete: ", pkg)
    }
  }))
}
# Required packages
required_pkgs <- c('data.table', 'stringr', 'tidyverse', 'KnowBR', 'sf', 'terra',
                   'rnaturalearth', 'ggplot2', 'svglite', 'patchwork', 'here',
                   'ggExtra', 'biscale', 'cowplot', 'tidyterra', 'maps',
                   'nFactors', 'rSDM', 'psych')


# Setup packages
setup_packages(required_pkgs)
