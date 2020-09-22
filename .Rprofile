if (dir.exists("renv/")) {
  source("renv/activate.R")
  if (interactive()) {
    renv::status()
  }
} else {
  cat("* Run renv::init() to install the R packages for this project")
}
