
#' Fix files by replacement of holders
#'
#' @param destination Where the file should be stored
#' @param file Target file
#' @param find Term to find
#' @param replace Replacement term
#'
#' @return NULL
fix_file <- function(destination, file, find, replace){
    path <- file.path(destination, file)
    content <- readLines(license_path)
    content <- gsub(find(), replace, content)
    writeLines(content, path)
}


#' SSB package template
#'
#' Create an R package with standard SSB format
#'
#' @param path Where the package should be created.
#' @param description A short description of what the package does.
#' @param firstname First name of the author
#' @param surname Surname of the Author
#' @param github Boolean for whether to create a github repository for the package. Default = TRUE
#'
#' @return NULL
#' @export
ssb_rtemplate <- function(path, description,
                          firstname, surname, github){

  base_dir <- dirname(path)
  package_name <- basename(path)

  prefixed_name <- paste0("ssb-", package_name)

  # Create the local directory with the prefixed name
  destination <- file.path(base_dir, prefixed_name)
  dir.create(destination, recursive = TRUE)

  # Specify other variables
  year <- substring(Sys.Date(), 1, 4)
  user <- Sys.info()['user']
  email <- paste0(user, '@ssb.no')

  # copy template to destination
  template_path <- system.file("templates/ssb-rtemplate", package = "fellesr")
  file.copy(template_path, destination, recursive = TRUE)

  # Fix Readme file
  fix_file(destination, "README.md", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(destination, "README.md", find = "{{PACKAGE_DESCRIPTION}}", description)

  # Fix description
  fix_file(destination, "DESCRIPTION", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(destination, "DESCRIPTION", find = "{{PACKAGE_DESCRIPTION}}", description)
  fix_file(destination, "DESCRIPTION", find = "{{AUTHOR_NAME1}}", firstname)
  fix_file(destination, "DESCRIPTION", find = "{{AUTHOR_NAME2}}", surname)
  fix_file(destination, "DESCRIPTION", find = "{{AUTHOR_EMAIL}}", email)

  # Fix Licence files
  fix_file(destination, "LICENSE.md", find = "{{YEAR}}", year)
  fix_file(destination, "LICENSE", find = "{{YEAR}}", year)

  # Fix NEWS
  fix_file(destination, "NEWS.md", find = "{{PACKAGE_NAME}}", package_name)

  # Fix SECURITY
  fix_file(destination, "SECURITY.md", find = "{{PACKAGE_NAME}}", package_name)

  # Add example data
  test_data <- data.frame(x = runif(10), y=runif(10))
  usethis::use_data(test_data)

  # Add NAMESPACE and documents
  roxygen2::roxygenise()

  # Add comments file
  usethis::use_cran_comments()

  # Create github repo
  usethis::use_git()
  usethis::use_github(organisation = "statisticsnorway",
                      visability = "internal")

  # Set up pkgdown
  usethis::use_pkgdown_github_pages()

  # Set up tests
  usethis::use_testthat()
  usethis::use_test("hello_world.R")

  # Set up other things
  use_github_links()

}



