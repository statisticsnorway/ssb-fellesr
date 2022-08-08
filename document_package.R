
# Roxygen documentation
devtools::document()

# Add imports
usethis::use_package("dplyr", type = "imports")

# Vignette #
devtools::load_all()
usethis::use_pkgdown() # OBS: docs legges i .gitignore (må fjernes)
pkgdown::build_site()

