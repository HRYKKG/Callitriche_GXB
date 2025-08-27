# Prep.R
# Installs required packages for the Shiny app supplied in app.R / server_msa.R
# ---------------------------------------------------------------
# This script checks for required CRAN and Bioconductor packages
# and installs any that are missing, then verifies they load.
# ---------------------------------------------------------------
# ---- set CRAN mirror ------------------------------------------
if (is.null(getOption("repos")[["CRAN"]]) ||
    getOption("repos")[["CRAN"]] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

## ---- helper ---------------------------------------------------
install_if_missing <- function(pkgs, installer, ...) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) {
    message("Installing: ", paste(missing, collapse = ", "))
    installer(missing, ...)
  } else {
    message("All requested packages already installed: ", paste(pkgs, collapse = ", "))
  }
}

## ---- CRAN packages -------------------------------------------
cran_pkgs <- c(
  "shiny", "shinydashboard", "DT", "heatmaply", "cowplot",
  "ggsci", "tidyverse", "svglite", "rclipboard", "DBI", "RSQLite","pool"
)

install_if_missing(
  pkgs = cran_pkgs,
  installer = function(pkgs, ...) install.packages(pkgs, dependencies = TRUE, ...)
)

## ---- Bioconductor packages -----------------------------------
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

bio_pkgs <- c("msa", "Biostrings", "ggtree", "ggmsa")

install_if_missing(
  pkgs = bio_pkgs,
  installer = function(pkgs, ...) BiocManager::install(pkgs, ask = FALSE, update = FALSE, ...)
)

## ---- verify ---------------------------------------------------
message("\nVerifying package load...")
for (pkg in c(cran_pkgs, bio_pkgs)) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
message("All packages loaded successfully.")
