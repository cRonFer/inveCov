# Create working directory
create_and_set_directory <- function(dir_e) {
  # Create directory if it doesn't exist
  if (!dir.exists(dir_e)) {
    dir.create(dir_e, recursive = TRUE)
    message(paste("Directory created:", dir_e))
  } else {
    message(paste("Directory already exists:", dir_e))
  }

  # Set the working directory using here::here()
  setwd(dir_e)
  message(paste("Working directory set to:", getwd()))
  return(getwd())
}
