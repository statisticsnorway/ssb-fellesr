#!/bin/bash
set -euo pipefail

REPO_NAME=$(basename "${GIT_REPOSITORY}" .git)
PROJECT_DIR="$HOME/work/$REPO_NAME"
RPROFILE_FILE="$PROJECT_DIR/.Rprofile"
RPROJ_FILE="$PROJECT_DIR/$REPO_NAME.Rproj"

export REPO_NAME PROJECT_DIR RPROFILE_FILE

# -- RStudio auto-open kun når RStudio faktisk brukes ----------------------
ensure_rprofile() {
  mkdir -p "$PROJECT_DIR"
  touch "$RPROFILE_FILE"

  # Legg til blokk kun én gang
  if ! grep -q "## AUTO: RStudio/Jupyter init" "$RPROFILE_FILE" 2>/dev/null; then
    cat << 'RPROFILE' >> "$RPROFILE_FILE"

## AUTO: RStudio/Jupyter init ----
local_proj_dir <- normalizePath(".", mustWork = FALSE)
local_has_renv <- file.exists(file.path(local_proj_dir, "renv.lock")) ||
                  dir.exists(file.path(local_proj_dir, "renv"))
local_rproj    <- list.files(local_proj_dir, pattern = "[.]Rproj$", full.names = TRUE)
local_is_rstudio <- identical(Sys.getenv("RSTUDIO"), "1")
local_is_jupyter <- nzchar(Sys.getenv("JPY_PARENT_PID")) || nzchar(Sys.getenv("JUPYTERHUB_USER")) ||
                    !local_is_rstudio

# I Jupyter/ren R: setwd til prosjektkatalogen (kun hvis vi ikke allerede er der)
if (!local_is_rstudio) {
  proj <- tryCatch(normalizePath("{{PROJECT_DIR}}", mustWork = FALSE), error = function(e) NULL)
  if (!is.null(proj) && dir.exists(proj)) {
    cur <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) NULL)
    if (!identical(cur, proj)) {
      try(setwd(proj), silent = TRUE)
    }
  }
}

# Aktiver renv hvis tilgjengelig
if (local_has_renv) {
  try({
    if (!"renv" %in% rownames(installed.packages())) install.packages("renv", repos = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"))
    renv::activate()
  }, silent = TRUE)
}

# Åpne RStudio-prosjekt automatisk (kun i RStudio)
if (interactive() && local_is_rstudio) {
  if (!identical(getOption("auto_open_rproj_done"), TRUE)) {
    options(auto_open_rproj_done = TRUE)
    repo <- tryCatch(normalizePath(dirname("{{RPROJ_FILE}}"), mustWork = FALSE), error = function(e) NULL)
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        requireNamespace("later", quietly = TRUE) &&
        !is.null(repo)) {
      later::later(function() {
        if (rstudioapi::isAvailable()) {
          cur <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
          cur_norm <- tryCatch(normalizePath(cur, mustWork = FALSE), error = function(e) NULL)
          if (!identical(cur_norm, repo)) {
            try(rstudioapi::openProject(repo, newSession = FALSE), silent = TRUE)
          }
        }
      }, delay = 0.5)
    }
  }
}
## END AUTO ---------------------------------------------------------------

RPROFILE
    # Fyll inn faktiske stier
    sed -i "s|{{PROJECT_DIR}}|$PROJECT_DIR|g" "$RPROFILE_FILE"
    sed -i "s|{{RPROJ_FILE}}|$RPROJ_FILE|g" "$RPROFILE_FILE"
  fi

  echo ".Rprofile oppdatert i $PROJECT_DIR"
}

# -- Renviron for tokens/repo ---------------------------------------------
setup_renviron() {
  {
    echo "RENV_CONFIG_REPOS_OVERRIDE=${CRAN:-https://cran.r-project.org}"
    [ -n "${GIT_PERSONAL_ACCESS_TOKEN:-}" ] && echo "GITHUB_PAT=$GIT_PERSONAL_ACCESS_TOKEN"
  } > "$HOME/.Renviron"
}

# -- renv restore (med rensing av ukjent kilde) ---------------------------
renv_restore() {
  cd "$PROJECT_DIR" || { echo "Kunne ikke cd til $PROJECT_DIR"; exit 1; }

  if [ -f "renv.lock" ]; then
    ORIG_BAK="$HOME/renv.lock.orig"
    CLEAN_BAK="$HOME/renv.lock.cleaned"

    cp -p renv.lock "$ORIG_BAK"
    jq '.Packages |= with_entries(select(.value.Source != "unknown"))' renv.lock > "$CLEAN_BAK"
    if ! jq empty "$CLEAN_BAK" >/dev/null 2>&1; then
      echo "Ugyldig renset renv.lock ($CLEAN_BAK)"
      exit 1
    fi

    mv renv.lock renv.lock._orig_in_use
    cp -p "$CLEAN_BAK" renv.lock

    echo "Kjører restore med renset renv.lock ..."
    Rscript -e "renv::activate();
                renv::restore(prompt = FALSE, repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'));
                renv::install(c('rstudioapi','later','IRkernel'),
                              repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'),
                              prompt = FALSE, lock = TRUE)" \
      > "$HOME/renv-startup.log" 2>&1 || true

    mv -f renv.lock._orig_in_use renv.lock
    rm -f "$ORIG_BAK"
  else
    echo "Fant ikke renv.lock – kjører restore uten rensing."
    Rscript -e "renv::activate();
                renv::restore(prompt = FALSE, repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'));
                renv::install(c('rstudioapi','later','IRkernel'),
                              repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'),
                              prompt = FALSE, lock = TRUE)" \
      > "$HOME/renv-startup.log" 2>&1 || true
  fi

  cd - >/dev/null
}

# -- Sett RStudios working-dir hvis RStudio finnes ------------------------
maybe_set_rstudio_workdir() {
  local CONF="/etc/rstudio/rsession.conf"
  if [ -f "$CONF" ]; then
    # Skriv til temp, så bytt
    tmp="$(mktemp)"
    # Fjern eksisterende innstillinger
    grep -v '^session-default-working-dir=' "$CONF" > "$tmp" || true
    echo "session-default-working-dir=$PROJECT_DIR" >> "$tmp"
    sudo mv "$tmp" "$CONF" 2>/dev/null || mv "$tmp" "$CONF"
  fi
}

# -- Kjør -----------------------------------------------------------------
ensure_rprofile
setup_renviron
renv_restore
maybe_set_rstudio_workdir

echo "Init ferdig. renv blir aktivert via .Rprofile. I RStudio åpnes .Rproj automatisk; i Jupyter/IRkernel settes working directory til prosjektet."
