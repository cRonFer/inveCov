
create_grid <- function(resolution, extent = NULL, shapefile = NULL, crs = NULL, cell_shape = "square"){
  # Validate inputs
  if (is.null(extent) && is.null(shapefile)) {
    stop("You must provide either an extent (xmin, ymin, xmax, ymax) or a shapefile")
  }
  
  # Validate cell shape
  cell_shape <- tolower(cell_shape)
  if (!cell_shape %in% c("square", "hexagon", "hex")) {
    stop("cell_shape must be either 'square' or 'hexagon'")
  }
  
  if (!is.null(extent)) {
    # Check if extent is properly formatted
    if (length(extent) != 4 || !is.numeric(extent)) {
      stop("Extent must be a numeric vector of length 4 (xmin, ymin, xmax, ymax)")
    }
    # Extract extent values
    xmin <- extent[1]
    ymin <- extent[2]
    xmax <- extent[3]
    ymax <- extent[4]
    # Validate extent values
    if (xmin >= xmax || ymin >= ymax) {
      stop("xmax must be greater than xmin and ymax must be greater than ymin")
    }
    
    # Create bbox from extent
    bbox <- st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = st_crs(crs))
    
  } else if (!is.null(shapefile)) {
    # Check if shapefile is an sf object or path to file
    if (is.character(shapefile)) {
      if (!file.exists(shapefile)) {
        stop("The specified shapefile path does not exist")
      }
      shapefile <- st_read(shapefile, quiet = TRUE)
    } else if (!inherits(shapefile, "sf")) {
      stop("shapefile must be either an sf object or a path to a shapefile")
    }
    
    # Get extent from shapefile
    bbox <- st_bbox(shapefile)
    crs <- st_crs(shapefile)$epsg  # Use shapefile's CRS if not specified
    
    message(paste("Using extent from shapefile:",
                  "\nX range:", bbox["xmin"], "to", bbox["xmax"],
                  "\nY range:", bbox["ymin"], "to", bbox["ymax"]))
  }
  
  # Validate resolution
  if (!is.numeric(resolution) || resolution <= 0) {
    stop("Resolution must be a positive number")
  }
  
  # Create grid based on cell shape
  if (cell_shape %in% c("hexagon", "hex")) {
    # For hexagons, cellsize is the distance between opposite edges (flat-to-flat)
    grid <- st_make_grid(bbox, 
                         cellsize = resolution, 
                         square = FALSE,  # Creates hexagons
                         what = "polygons")
    
    # Calculate actual edge length (distance between adjacent vertices)
    # edge_length <- resolution / sqrt(3)
    
  } else {
    # For squares
    grid <- st_make_grid(bbox, 
                         cellsize = resolution, 
                         square = TRUE, 
                         what = "polygons")
    # edge_length <- resolution
  }
  
  # Convert to sf object with cell IDs
  grid_sf <- st_sf(
    cell_id = 1:length(grid),
    cell_shape = cell_shape,
    # edge_length = edge_length,
    geometry = grid
  )
  
  # Calculate and add cell area (in CRS units)
  grid_sf$area <- st_area(grid_sf)
  
  # Add grid parameters as attributes
  attr(grid_sf, "grid_params") <- list(
    resolution = resolution,
    extent = bbox,
    crs = crs,
    cell_shape = cell_shape,
    creation_date = Sys.Date())
  return(grid_sf)
}

  
