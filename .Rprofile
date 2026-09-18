source("renv/activate.R")

# Auto-open RStudio project if available (robust)
if (interactive() && Sys.getenv("RSTUDIO") == "1") {
  # Kjør maks én gang per sesjon
  if (!identical(getOption("auto_open_rproj_done"), TRUE)) {
    options(auto_open_rproj_done = TRUE)

    # Sett opp stier
    repo <- tryCatch(normalizePath(dirname("/home/onyxia/work/ssb-fellesr/ssb-fellesr.Rproj"), mustWork = FALSE), error = function(e) NULL)

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        requireNamespace("later", quietly = TRUE)) {

      later::later(function() {
        if (rstudioapi::isAvailable()) {
          cur <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
          cur_norm <- tryCatch(normalizePath(cur, mustWork = FALSE), error = function(e) NULL)

          # Åpne bare hvis vi IKKE allerede står i samme prosjektmappe
          if (!identical(cur_norm, repo)) {
            try(rstudioapi::openProject(repo, newSession = FALSE), silent = TRUE)
          }
        }
      }, delay = 0.5)
    }
  }
}


