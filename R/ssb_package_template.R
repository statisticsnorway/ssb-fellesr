
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

  content <- readLines(path)
  content <- gsub(find, replace, content, fixed = TRUE)

  # Check if the last line is a newline; if not, add one
  if(length(content) > 0 && nchar(content[length(content)]) > 0){
    content <- c(content, "")  # Add an empty string as the new final line
  }

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

  if (substr(package_name, 1,4) == "ssb-"){
    prefixed_name <- package_name
    package_name <- substring(package_name, 5, nchar(pacakge_name))
  } else {
    prefixed_name <- paste0("ssb-", package_name)
  }

  # Create the local directory with the prefixed name
  destination <- file.path(base_dir, prefixed_name)
  dir.create(destination, recursive = TRUE)

  # Specify other variables
  year <- substring(Sys.Date(), 1, 4)
  user <- Sys.info()['user']
  email <- paste0(user, '@ssb.no')

  # Get the list of files and directories inside the template_path
  template_path <- system.file("rstudio/templates/project", package = "fellesr")
  template_contents <- list.files(template_path, full.names = TRUE)
  template_contents <- template_contents[!grepl("create_ssb_package", template_contents)]

  # Copy each file and directory in template_contents to destination
  for (file in template_contents) {
    file.copy(file, destination, recursive = TRUE)
  }
  Sys.sleep(2)

  # Fix Readme file
  fix_file(destination, "README.md", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(destination, "README.md", find = "{{PACKAGE_NAME_CODE}}", prefixed_name)
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

  # Fix name of project file
  setwd(destination)
  file.rename( "packagename.rproj", paste0(package_name, ".rproj"))

  # Add comments file
  usethis::use_cran_comments()

  # Add example data
  test_data <- data.frame(x = runif(10), y=runif(10))
  usethis::use_data(test_data, overwrite=TRUE)

  # Add NAMESPACE and documents
  roxygen2::roxygenise()

  # Start git
  usethis::use_git_config(user.name = firstname, user.email = email)
  usethis::use_git()
  usethis::git_default_branch_configure(name = "main")

  # Set up tests
  usethis::use_testthat()
  usethis::use_test("hello_world.R")

  # Set up github
  if (github){

    print("Project setting up. Preparing to create a repo on github...")
    if (Sys.getenv("GITHUB_PAT") == ""){
      Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priveldges):"))
    }

    #gitcreds::gitcreds_set()
    #print("credentials set?")
    usethis::use_github(organisation = "statisticsnorway",
                        visibility = "internal", protocol = "https")

    # Set up test action
    usethis::use_github_action("check-standard", badge = TRUE)

    # Set up pkgdown
    usethis::use_pkgdown_github_pages()

    # Set up other things
    usethis::use_github_links()

    # Push all changes
    git2r::add(path=".")
    git2r::commit(message="Initial commit.")
    git2r::push(credentials=git2r::cred_token())

    # Rename repo
    url <- paste0("https://api.github.com/repos/statisticsnorway/", package_name)
    response <- httr::PATCH(url, body = list(name = prefixed_name),
                            httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                            encode = "json")
    httr::content(response)
  }

}


