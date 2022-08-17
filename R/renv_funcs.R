
#' Create a dependencies file
#'
#' @param lines Additional lines to include in the file
#' @param force Whether the file should forcibly overwrite if exists
#' @param warning Whether a warning should be given for force overwrite
#'
#' @return None. A dependencies.R file is created
#' @export
create_dependencies <- function(lines = NULL, force = FALSE, warning = TRUE){
    if (file.exists("dependencies.R")){
        if (!force) {
            stop("A dependencies.R file already exists. If you would like to overwrite it use the parameter: force = TRUE")
            } else {
            if (warning){
                respon <- getPass("Are you sure you want to overwrite the dependencies file from before?")
                if (!tolower(respon) %in% c("yes", "y","j","ja")) stop()
                }
            }
        }
    if (missing(lines)){
        lines <- c("library(renv)", "renv::init()", "renv::snapshot()")
        }
    fileConn <- file("dependencies.R")
    writeLines(lines, fileConn)
    close(fileConn)
    }


#' Snapshot a SSB project
#' Wrapper function for renv::snapshot
#'
#' @param ignore List of packages to ignore
#'
#' @return None. Both lock and dependencies.R files will be updated. For more
#' details see <https://rstudio.github.io/renv/reference/snapshot.html>
#' @export
snapshot <- function(ignore, ...){
    nodename <- Sys.info()["nodename"]
    if (grepl("jupyter", nodename)){
        repo <- 'https://cran.uib.no'
        } else {
        repo <- 'https://nexus.ssb.no/repository/CRAN/'
        }
    if (Sys.getenv("RSTUDIO") == "1"){
        renv::snapshot(repos=repo, ...)
        } else {
        dep_lines <- read.delim("dependencies.R", header = FALSE)$V1
        h <- match("renv::init()", dep_lines)
        package_list <- .packages()
        .basepkg <- rownames(installed.packages(priority="base"))
        if (!missing(ignore)){
            eksklud <- c(.basepkg, "devtools", "renv", "here", ignore)
            } else {
            eksklud <- c(.basepkg, "devtools", "renv", "here")
            }
        package_list <- package_list[!package_list %in% eksklud]
        for (i in package_list){
            new_line <- paste0("library(", i, ")")
            if (any(grep(new_line, dep_lines))){
                break()
                } else {
                dep_lines <- append(dep_lines, new_line, after = h)
                }
        }
        create_dependencies(lines = dep_lines, force = TRUE, warning = FALSE)

        source(here::here('dependencies.R'))
    }
}


#' Install packages in SSB from CRAN
#' Wrapper function for utils::install.packages
#'
#' @param pkgs Name of the packages to install
#' @param lib Character vector giving library directory
#' @param repos Character vector giving CRAN repository to use
#' @param ... Addition parameters (see https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/install.packages)
#'
#' @return Invisible `NULL`
#' @export
install.packages <- function(pkgs, lib, repos, ...){
    nodename <- Sys.info()["nodename"]
    if (grepl("jupyter", nodename)){
        repo <- 'https://cran.uib.no'
        } else {
        repo <- 'https://nexus.ssb.no/repository/CRAN/'
        }
    if (missing(lib)) {
        lib <- .libPaths()[1]
        }
    utils::install.packages(pkgs, lib, repos = repo, ...)
    }


#' Restore a SSB project
#' Wrapper function for renv::restore()
#'
#' @param repo Character vector pointing to which repository should be used
#' @param ...
#'
#' @return Invisible `NULL`
#' @export
restore <- function(repo, ...){
    nodename <- Sys.info()["nodename"]
    if (missing(repo)){
        if (grepl("jupyter", nodename)){
            repo <- 'https://cran.uib.no'
            } else {
            repo <- 'https://nexus.ssb.no/repository/CRAN/'
            }
    }
    renv::restore(repos = repo)
    }


#' Library function for a difficult package
#' Some packages are not able to be installed by the user for various reasons. In this case the package may be able to be called from the common SSB library using this function.
#'
#' @param package Character vector with the name of the package
#' @param ...
#'
#' @return Invisible `NULL`
#' @export
ssb_library <- function(package, ...){
    tryCatch(library(package, character.only = T, ...), silent = TRUE,
             error = try_common_library(package))
    }


#' Internal library function for a difficult package
#'
#' @param pkg Character vector with the name of the package
#' @param
#'
#' @return Invisible `NULL`
#' @keywords internal
try_common_library <- function(pkg){
    print("In try")
    hm <- Sys.getenv('R_HOME')
    home_files <- list.files(hm)
    if ("site-library" %in% home_files) libr <- file.path(hm, "site-library")
    if ("library" %in% home_files) libr <- c(libr, file.path(hm, "library"))

    mes = "called"
    while (length(mes) > 0){
        while(any(grepl("called", mes))){
            print("In while")
            mes <- try(library(pkg, lib.loc = libr, character.only = T), silent = TRUE)
            print(paste("message null:", is.null(mes)))
            find_problem(pkg, libr)
        }
        }
    }


#' Internal library function for a difficult package
#'
#' @param pkg Character vector with the name of the package
#' @param lib Character vector with the path to the librarys
#'
#' @return Invisible `NULL`
#' @keywords internal
find_problem <- function(pkg, lib){
    print("In find")
    if (length(pkg)==0) return("Done")
    mes <- try(library(pkg, lib.loc = lib, character.only = T), silent = TRUE)
    mes_vec <- unlist(strsplit(mes, split = " "))
    pkg_ind <- which(mes_vec == "called") + 1
    pkg2 <- gsub("[[:space:]]", "", mes_vec[pkg_ind], fixed = FALSE)
    pkg2 <- gsub("[^[:alnum:]]", "", pkg2)
    print(paste("Loading:", pkg2))
    find_problem(pkg2, lib)
    }
