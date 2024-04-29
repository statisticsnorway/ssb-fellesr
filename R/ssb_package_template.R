
#' Fix files by replacement of holders
#'
#' @param destination Where the file should be stored
#' @param file Target file
#' @param find Term to find
#' @param replace Replacement term
#'
#' @return NULL
fix_file <- function(destination, file, find, replace){
  destination_path <- file.path(destination, file)

  content <- readLines(destination_path)
  content <- gsub(find, replace, content, fixed = TRUE)

  # Check if the last line is a newline; if not, add one
  if(length(content) > 0 && nchar(content[length(content)]) > 0){
    content <- c(content, "")  # Add an empty string as the new final line
  }

  writeLines(content, destination_path)
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
  wd_dir <- getwd()
  base_dir <- dirname(path)
  package_name <- basename(path)
  original_path <- NULL

  if (substr(package_name, 1, 4) == "ssb-"){
    prefixed_name <- package_name
    package_name <- substring(package_name, 5, nchar(package_name))
  } else {
    original_path <- path
    prefixed_name <- paste0("ssb-", package_name)
    path <- file.path(base_dir, prefixed_name)
  }

  # Create the local directory with the prefixed name
  dir.create(path, recursive = TRUE)
  message("Project created at: ", path)

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
    file.copy(file, path, recursive = TRUE)
  }
  Sys.sleep(2)

  # Fix Readme file
  fix_file(path, "README.md", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(path, "README.md", find = "{{PACKAGE_NAME_CODE}}", prefixed_name)
  fix_file(path, "README.md", find = "{{PACKAGE_DESCRIPTION}}", description)

  # Fix description
  fix_file(path, "DESCRIPTION", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(path, "DESCRIPTION", find = "{{PACKAGE_DESCRIPTION}}", description)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_NAME1}}", firstname)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_NAME2}}", surname)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_EMAIL}}", email)

  # Fix Licence files
  fix_file(path, "LICENSE.md", find = "{{YEAR}}", year)
  fix_file(path, "LICENSE", find = "{{YEAR}}", year)

  # Fix NEWS
  fix_file(path, "NEWS.md", find = "{{PACKAGE_NAME}}", package_name)

  # Fix SECURITY
  fix_file(path, "SECURITY.md", find = "{{PACKAGE_NAME}}", package_name)

  # Fix name of project file
  setwd(path)
  file.rename( "packagename.Rproj", paste0(prefixed_name, ".Rproj"))

  # Add comments file
  usethis::use_cran_comments(open=F)

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
  usethis::use_test("hello_world.R", open = F)

  # Set up github
  if (github){

    print("Project setting up. Preparing to create a repo on github...")
    if (Sys.getenv("GITHUB_PAT") == ""){
      Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priveldges):"))
    }

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

    # Add branch protection
    response <- gh::gh(
      "PUT /repos/:owner/:repo/branches/:branch/protection",
      owner = "statisticsnorway",
      repo = prefixed_name,
      branch = "main",
      .token = Sys.getenv("GITHUB_PAT"),
      required_status_checks = NA,  # No specific status checks are mentioned
      enforce_admins = TRUE,  # Do not allow bypassing of the settings
      required_pull_request_reviews = list(
        dismiss_stale_reviews = TRUE,  # Dismiss stale pull request approvals when new commits are pushed
        require_code_owner_reviews = FALSE,  # No specific requirement for code owner reviews was mentioned
        required_approving_review_count = 1  # Require at least one approval
      ),
      restrictions = NA,  # No specific user or team restrictions mentioned
      required_pull_requests_reviews_enforcement_level = "everyone"  # Enforce rules on everyone, including admins
    )
  }

  # Open new project
  Sys.sleep(2)
  setwd(wd_dir)
  rstudioapi::openProject(path = path, newSession = FALSE)

  # Cancel further processes - this supresses Rstudio from creating a duplicate project (without suffix)
  # This is currently sending an error message which I haven't been able to suppress.
  stop("Finished setting up package! This looks like an error but isn't (Just click ok).")
}


