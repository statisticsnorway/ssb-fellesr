
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


#' Restore am SSB project
#' Wrapper function for renv::restore()
#'
#' @param repo Character vector pointing to which repository should be used  
#' @param ... 
#'
#' @return
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
