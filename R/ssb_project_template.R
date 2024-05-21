
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
ssb_rproject <- function(path, description,
                         firstname, surname, github){

    withr::local_options(usethis.quiet = getOption("usethis.quiet", default = FALSE)) # Suppress git messages

    # Establish directories
    wd_dir <- getwd()
    base_dir <- dirname(path)
    project_name <- basename(path)
    original_path <- NULL

    # Establish standard project name
    if (substr(project_name, 1, 4) == "stat-"){
        prefixed_name <- project_name
        project_name <- substring(project_name, 5, nchar(project_name))
    } else {
        original_path <- path
        prefixed_name <- paste0("stat-", project_name)
        path <- file.path(base_dir, prefixed_name)
    }

    # Create the local directory with the prefixed name
    dir.create(path, recursive = TRUE)
    message("Project created at: ", path)

    # Specify user variables
    user <- Sys.info()['user']
    email <- paste0(user, '@ssb.no')

    # Copy files to project
    get_files(path, "project")
    get_standard_files_offline(path)

    # Fix and replace placeholders
    fix_files(path, project_name, prefixed_name, description, firstname, surname, email, type = "project")

    # Fix name of project file
    create_project_file(path, prefixed_name = prefixed_name,
                        project_type = "project")
    setwd(path)
    print(paste0("Project files all copied to: ", path))

    # Start git
    usethis::use_git_config(user.name = firstname, user.email = email)
    usethis::use_git()
    usethis::git_default_branch_configure(name = "main")

    # Set up tests
    usethis::use_testthat()
    usethis::use_test("hello_world.R", open = F)

    # set up renv
    renv::init(restart = FALSE, load=FALSE, bare = TRUE)

    # Install pacakages - not working well so taken out
    #print("Installing packages ...")
    # List of required packages including those not directly called
    #required_packages <- c("arrow", "ggplot2", "testthat", "statisticsnorway/fellesr",
    #                       "class", "foreign", "KernSmooth", "MASS", "mgcv", "Matrix", "nlme")
    # Install all required packages
    #for (pkg in required_packages) {
    #    renv::install(library=renv::paths$library(),
    #                  package=pkg,
    #                  prompt = FALSE)
    #}
    # Commit changes
    #git2r::add(path=".")
    #git2r::commit(message="Initial commit.")
    #renv::snapshot(library=renv::paths$library(), prompt = FALSE)

    # Set up github
    if (github){

        print("Project setting up. Preparing to create a repo on github...")
        if (Sys.getenv("GITHUB_PAT") == ""){
            Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priviledges):"))
        }

        usethis::use_github(organisation = "statisticsnorway",
                            visibility = "internal", protocol = "https")

        # Set up test action
        add_github_actions(path, type = "project")

        # Push all changes
        git2r::add(path=".")
        git2r::commit(message="Initial commit.")
        git2r::push(credentials=git2r::cred_token())

        # Rename repo
        url <- paste0("https://api.github.com/repos/statisticsnorway/", project_name)
        httr::PATCH(url, body = list(name = prefixed_name),
                                httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                                encode = "json")


        # Add branch protection
        add_branch_protect(prefixed_name)
    }

    # Open new project
    Sys.sleep(2)
    setwd(wd_dir)
    rstudioapi::openProject(path = path, newSession = FALSE)

    # Cancel further processes - this supresses Rstudio from creating a duplicate project (without suffix)
    # This is currently sending an error message which I haven't been able to suppress.
    stop("Finished setting up package! This looks like an error but isn't (Just click ok).")
}


