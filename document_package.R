
# Roxygen documentation
devtools::document()

# Add imports
usethis::use_package("dplyr", type = "imports")

usethis::use_package("RODBC", type = "suggests") # OBS: kun installert i RStudio Windows
usethis::use_package("ROracle", type = "suggests") # OBS: ikke installert i RStudio Windows

usethis::use_pipe()

# Vignette #
devtools::load_all()
usethis::use_pkgdown() # OBS: docs legges i .gitignore (må fjernes)
pkgdown::build_site()

