#!/bin/bash

# Dette skriptet gjør følgende:
# [x] 1. Installerer pakkene i renv-miljøet i det angitte repoet
# [ ] 2. Setter working-directory til det angitte repoet
# [ ] 3. Aktiverer renv-miljøet (så lenge det finnes en .Rprofile med renv::autoload())

REPO_NAME=$(basename "$GIT_REPOSITORY" .git)
PROJECT_DIR="$HOME/work/$REPO_NAME"
RPROFILE_FILE="$PROJECT_DIR/.Rprofile"
RPROJ_FILE="$PROJECT_DIR/$REPO_NAME.Rproj"

export REPO_NAME PROJECT_DIR RPROFILE_FILE

# Sørg for at prosjektkatalog finnes
# mkdir -p "$PROJECT_DIR"

# Sørg for at .Rprofile finnes
# touch "$RPROFILE_FILE"

# Legg til auto-open av RStudio-prosjektet dersom .Rproj finnes
if [ -f "$RPROJ_FILE" ]; then
  if ! grep -q "rstudioapi::openProject" "$RPROFILE_FILE" 2>/dev/null; then
    cat << EOF >> "$RPROFILE_FILE"

# Auto-open RStudio project if available (robust)
if (interactive() && Sys.getenv("RSTUDIO") == "1") {
  # Kjør maks én gang per sesjon
  if (!identical(getOption("auto_open_rproj_done"), TRUE)) {
    options(auto_open_rproj_done = TRUE)

    # Sett opp stier
    repo <- tryCatch(normalizePath(dirname("$RPROJ_FILE"), mustWork = FALSE), error = function(e) NULL)

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


EOF
  fi
fi

echo ".Rprofile oppdatert i $PROJECT_DIR"



su onyxia <<'EOF'
set -eu

# setup .Renviron ---------------------------------------------------------
echo "RENV_CONFIG_REPOS_OVERRIDE=$CRAN" > "$HOME/.Renviron"
echo "GITHUB_PAT=$GIT_PERSONAL_ACCESS_TOKEN" >> "$HOME/.Renviron"

# setup Renv --------------------------------------------------------------
# REPO_NAME=$(basename "$GIT_REPOSITORY" .git)
# PROJECT_DIR="$HOME/work/$REPO_NAME"

cd "$PROJECT_DIR" || { echo "Kunne ikke cd til $PROJECT_DIR"; exit 1; }

if [ -f "renv.lock" ]; then
  ORIG_BAK="$HOME/renv.lock.orig"
  CLEAN_BAK="$HOME/renv.lock.cleaned"

  # 1) Backup av original
  cp -p renv.lock "$ORIG_BAK"

  # 2) Lag renset versjon og lagre som egen backup
  jq '.Packages |= with_entries(select(.value.Source != "unknown"))' renv.lock > "$CLEAN_BAK"
  if ! jq empty "$CLEAN_BAK" >/dev/null 2>&1; then
    echo "Ugyldig renset renv.lock ($CLEAN_BAK)"
    exit 1
  fi

  # 3) Bytt midlertidig til renset for restore
  mv renv.lock renv.lock._orig_in_use
  cp -p "$CLEAN_BAK" renv.lock


  echo "Kjører restore med renset renv.lock ..."
  Rscript -e "renv::activate(); 
              renv::restore(prompt = FALSE, repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')); 
              renv::install(c('rstudioapi', 'later'), repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'), prompt = FALSE, lock = TRUE)" > "$HOME/renv-startup.log" 2>&1

  # 4) Legg tilbake original renv.lock
  mv -f renv.lock._orig_in_use renv.lock
  rm "$ORIG_BAK"

else
  echo "Fant ikke renv.lock i $(pwd) – kjører restore uten rensing."
  Rscript -e "renv::activate(); 
              renv::restore(prompt = FALSE, repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')); 
              renv::install(c('rstudioapi', 'later'), repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'), prompt = FALSE, lock = TRUE)" > "$HOME/renv-startup.log" 2>&1
fi

cd ..
exit
EOF


# setup working directory -------------------------------------------------

CONF="/etc/rstudio/rsession.conf"

if [ -f "$CONF" ]; then
sed "/session-default-working-dir/d" "$CONF" > "$CONF"
echo "session-default-working-dir=$PROJECT_DIR" >> "$CONF"
fi

echo "Personal init script completed. RStudio will auto-restore renv environment via .Rprofile."
