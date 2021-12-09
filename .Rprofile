
if (dir.exists("renv/")) {
  if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
  } else {
    cat("* renv may have been incompletely set up. Run renv::init() to continue\n")
  }
  if (interactive()) {
    renv::status()
  }
} else {
  cat("* Run renv::init() to install the R packages for this project\n")
}
