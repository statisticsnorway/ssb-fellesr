# -*- coding: utf-8 -*-
# Roxygen documentation
devtools::document()

renv::init()

# +
# # Add imports
# usethis::use_pipe()
# usethis::use_package("dplyr", type = "imports")
# usethis::use_package("RODBC", type = "suggests") # OBS: kun installert i RStudio Windows
# usethis::use_package("ROracle", type = "suggests") # OBS: ikke installert i RStudio Windows
# usethis::use_package("tidyr", type = "imports")
# usethis::use_package("dbplyr", type = "imports")
# usethis::use_package("getPass", type = "imports")
# usethis::use_package("DBI", type = "imports")
# usethis::use_package("renv", type = "imports")
# usethis::use_package("arrow", type = "imports")
# usethis::use_package("httr", type = "imports")
# usethis::use_package("gargle", type = "imports")
# usethis::use_package("googleCloudStorageR", type = "imports")
# usethis::use_package("jsonlite", type = "imports")
# usethis::use_package("sf", type = "imports")
# usethis::use_package("XML", type = "imports")
# usethis::use_package("klassR", type = "imports")
# usethis::use_package("ggplot2", type = "imports")
# usethis::use_package("rlang", type = "imports")
# usethis::use_package("stringr", type = "imports")
# usethis::use_package("sfarrow", type = "imports")
usethis::use_package("arsenal", type = "imports")
# -

# sfarrow?

# Vignette #
devtools::load_all()

detach_package(fellesr)
# usethis::use_pkgdown() # OBS: docs legges i .gitignore (må fjernes)

options(pkgdown.internet = FALSE) # OBS: denne må kjøres i prodsonen for at byggingen av pakken ikke feiler
pkgdown::build_site()

testthat::test_local()


