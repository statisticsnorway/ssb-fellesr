library(here)
library(getPass)

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


snapshot <- function(ignore){
    nodename <- Sys.info()["nodename"]
    if (grepl("jupyter", nodename)){
        repo <- 'https://cran.uib.no'
        } else {
        repo <- 'https://nexus.ssb.no/repository/CRAN/'
        }
    if (Sys.getenv("RSTUDIO") == "1"){
        renv::snapshot()
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

restore <- function(repos, ...){
    nodename <- Sys.info()["nodename"]
    if (grepl("jupyter", nodename)){
        repo <- 'https://cran.uib.no'
        } else {
        repo <- 'https://nexus.ssb.no/repository/CRAN/'
        }
    renv::restore(repos = repo)
    }
