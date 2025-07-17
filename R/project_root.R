#' Find the Project Root Directory
#'
#' Recursively walks up the directory tree from the current working directory
#' to locate a project root, identified by a given target file or file pattern.
#' Common use cases include locating files like `renv.lock`, `pyproject.toml`, or
#' any `.Rproj` file.
#'
#' This function does not change the working directory; it only returns metadata
#' about the search.
#'
#' @param target A character string indicating the target file to search for.
#'        If set to `".Rproj"`, the function will match any file ending in `.Rproj`.
#' @param max.iter Maximum number of parent directories to search.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{success}{Logical indicating whether the target was found.}
#'   \item{project.root}{The path to the directory where the target was found, or `NA` if not found.}
#'   \item{original.dir}{The directory where the search started.}
#'   \item{user.root}{The user's home directory (search stops here).}
#'   \item{target}{The search target used.}
#'   \item{iter}{The number of directory levels ascended during the search.}
#' }
#'
#' @examples
#' find_project_root("renv.lock")
#' find_project_root(".Rproj")
#'
#' @export
find_project_root <- function(target = "renv.lock", max.iter = 100) {
  user_root    <- normalizePath("~")
  original_dir <- getwd()
  
  found <- FALSE
  i     <- 0
  
  while (!found && i < max.iter) {
    files <- list.files()
    
    if (target == ".Rproj") {
      found <- any(grepl("\\.Rproj$", files))
    } else {
      found <- target %in% files
    }
    
    if ((i <- i + 1) > max.iter || found || 
        getwd() == user_root || getwd() == "/") break
    
    setwd("..")
  }
  
  project_root <- if (found) getwd() else NA
  setwd(original_dir)  # always restore

  return(invisible(list(
    success = found,
    project.root = project_root,
    original.dir = original_dir,
    user.root = user_root,
    target = target,
    iter = i
  )))
}

#' Set Working Directory to Project Root
#'
#' Uses \code{find_project_root()} to locate a project root based on a specified target
#' file, and sets the working directory to that root if found.
#'
#' @inheritParams find_project_root
#'
#' @return Invisibly returns the same list as \code{find_project_root()}, including
#'         `project.root`, `original.dir`, `success`, etc.
#'
#' @details This function changes the working directory as a side effect. If no target
#' is found, the working directory remains unchanged and a warning is issued.
#'
#' @examples
#' set_project_root("renv.lock")
#' set_project_root(".Rproj")
#'
#' @seealso \code{\link{find_project_root}}
#'
#' @export
set_project_root <- function(target = "renv.lock", max.iter = 100) {
  info <- find_project_root(target, max.iter)
  
  if (info$success) {
    setwd(info$project.root)
    message("Working directory set to project root: ", info$project.root)
  } else {
    warning("Project root not found (target = '", target, "')")
  }

  return(invisible(info))
}
