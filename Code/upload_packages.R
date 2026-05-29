##### Upload libraries ####


library_upload = function (package1, ...) {
  packages = c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}

packages  = c("here", "data.table", "tidyverse", "reshape2","mapdata",
              "purrr","h2o","readxl","cowplot","kableExtra","hrbrthemes",
              "plotly","LearnBayes","assertthat","readr","babynames","viridis",
              "zoo",'corrr','rstan','usmap','fixest','sp',
              'spdep','tmap','classInt','grid','gridExtra','lattice','biscale',
              'broom','plm','writexl','vdemdata','wpp2024','openxlsx',
              'patchwork','broom','fixest','purrr','dplyr',
              'sf','spdep','dplyr','Matrix','nimble','nimbleHMC','parallel',
              'MCMCvis','modelsummary','fixest','cowplot')

library_upload(packages)

