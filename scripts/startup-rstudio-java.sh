#!/bin/bash

# Dette skriptet gjør følgende:
# [x] 1. Konfigurerer Java-versjon
# [x] 2. Installerer pakkene i renv-miljøet i det angitte repoet
# [ ] 3. Setter working-directory til det angitte repoet
# [ ] 4. Aktiverer renv-miljøet (så lenge det finnes en .Rprofile med renv::autoload())


# setup Java ---------------------------------------------------------------

# Java-versjon kan angis som første argument til skriptet.
#
# Eksempel:
#   startup-rstudio.sh 17
#
# Dersom argumentet ikke er angitt, brukes Java 25.
#
# SSB_JAVA_VERSION kan også settes som miljøvariabel.
# Prioritet:
#   1. Første argument
#   2. SSB_JAVA_VERSION
#   3. Java 25

SSB_JAVA_VERSION="${1:-${SSB_JAVA_VERSION:-25}}"

# Kontroller at Java-versjonen er et heltall
if ! [[ "$SSB_JAVA_VERSION" =~ ^[0-9]+$ ]]; then
  echo "Ugyldig Java-versjon: $SSB_JAVA_VERSION"
  echo "Angi for eksempel 17, 21 eller 25."
  exit 1
fi

echo "Ønsket Java-versjon: $SSB_JAVA_VERSION"


# Finn installert OpenJDK for valgt Java-versjon
JAVA_HOME=$(
  find -L /usr/lib/jvm \
    -maxdepth 1 \
    -mindepth 1 \
    -type d \
    -name "java-${SSB_JAVA_VERSION}-openjdk-*" \
    -print \
    -quit
)


# Avbryt dersom ønsket Java-versjon ikke finnes
if [ -z "$JAVA_HOME" ] || [ ! -x "$JAVA_HOME/bin/java" ]; then

  echo ""
  echo "Fant ikke Java ${SSB_JAVA_VERSION}."
  echo ""
  echo "Tilgjengelige Java-installasjoner:"

  find -L /usr/lib/jvm \
    -maxdepth 1 \
    -mindepth 1 \
    -type d \
    -name "java-*-openjdk-*" \
    -print 2>/dev/null |
    sort

  exit 1

fi


export SSB_JAVA_VERSION
export JAVA_HOME

# Sørg for at valgt Java brukes
export PATH="$JAVA_HOME/bin:$PATH"

# Sørg for at libjvm.so fra valgt Java finnes først
export LD_LIBRARY_PATH="$JAVA_HOME/lib/server:${LD_LIBRARY_PATH:-}"


echo ""
echo "Bruker JAVA_HOME:"
echo "$JAVA_HOME"

echo ""
echo "Java-versjon:"
"$JAVA_HOME/bin/java" -version


# Konfigurer R mot valgt Java
echo ""
echo "Konfigurerer R mot Java ${SSB_JAVA_VERSION} ..."

JAVA_HOME="$JAVA_HOME" R CMD javareconf


# setup Java for RStudio sessions -----------------------------------------

# RStudio Server kan være startet med en annen Java-versjon.
# /etc/rstudio/rsession-profile leses før RStudio-sesjonen starter,
# slik at JAVA_HOME og LD_LIBRARY_PATH er satt før R startes.

RSESSION_PROFILE="/etc/rstudio/rsession-profile"

touch "$RSESSION_PROFILE"


# Fjern eventuell tidligere Java-konfigurasjon lagt inn av dette skriptet
sed -i \
  '/# BEGIN SSB JAVA/,/# END SSB JAVA/d' \
  "$RSESSION_PROFILE"


cat << EOF >> "$RSESSION_PROFILE"

# BEGIN SSB JAVA
export SSB_JAVA_VERSION="$SSB_JAVA_VERSION"
export JAVA_HOME="$JAVA_HOME"
export PATH="\$JAVA_HOME/bin:\$PATH"
export LD_LIBRARY_PATH="\$JAVA_HOME/lib/server:\${LD_LIBRARY_PATH:-}"
# END SSB JAVA

EOF


# setup project ------------------------------------------------------------

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
    repo <- tryCatch(
      normalizePath(
        dirname("$RPROJ_FILE"),
        mustWork = FALSE
      ),
      error = function(e) NULL
    )

    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
      requireNamespace("later", quietly = TRUE)
    ) {

      later::later(
        function() {

          if (rstudioapi::isAvailable()) {

            cur <- tryCatch(
              rstudioapi::getActiveProject(),
              error = function(e) NULL
            )

            cur_norm <- tryCatch(
              normalizePath(
                cur,
                mustWork = FALSE
              ),
              error = function(e) NULL
            )

            # Åpne bare hvis vi IKKE allerede står i samme prosjektmappe
            if (!identical(cur_norm, repo)) {

              try(
                rstudioapi::openProject(
                  repo,
                  newSession = FALSE
                ),
                silent = TRUE
              )

            }

          }

        },
        delay = 0.5
      )

    }

  }

}

EOF

  fi

fi


echo ".Rprofile oppdatert i $PROJECT_DIR"


# setup user environment and renv -----------------------------------------

su onyxia <<'EOF'

set -eu


# setup .Renviron ---------------------------------------------------------

echo "RENV_CONFIG_REPOS_OVERRIDE=$CRAN" > "$HOME/.Renviron"

echo "GITHUB_PAT=$GIT_PERSONAL_ACCESS_TOKEN" >> "$HOME/.Renviron"

echo "SSB_JAVA_VERSION=$SSB_JAVA_VERSION" >> "$HOME/.Renviron"

echo "JAVA_HOME=$JAVA_HOME" >> "$HOME/.Renviron"


# setup Renv --------------------------------------------------------------

# REPO_NAME=$(basename "$GIT_REPOSITORY" .git)
# PROJECT_DIR="$HOME/work/$REPO_NAME"

cd "$PROJECT_DIR" || {
  echo "Kunne ikke cd til $PROJECT_DIR"
  exit 1
}


if [ -f "renv.lock" ]; then

  ORIG_BAK="$HOME/renv.lock.orig"

  CLEAN_BAK="$HOME/renv.lock.cleaned"


  # 1) Backup av original
  cp -p renv.lock "$ORIG_BAK"


  # 2) Lag renset versjon og lagre som egen backup
  jq \
    '.Packages |= with_entries(select(.value.Source != "unknown"))' \
    renv.lock \
    > "$CLEAN_BAK"


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
              renv::install(c('rstudioapi', 'later'), repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'), prompt = FALSE, lock = TRUE)" \
    > "$HOME/renv-startup.log" 2>&1


  # 4) Legg tilbake original renv.lock
  mv -f renv.lock._orig_in_use renv.lock

  rm "$ORIG_BAK"


else

  echo "Fant ikke renv.lock i $(pwd) – kjører restore uten rensing."


  Rscript -e "renv::activate();
              renv::restore(prompt = FALSE, repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'));
              renv::install(c('rstudioapi', 'later'), repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE'), prompt = FALSE, lock = TRUE)" \
    > "$HOME/renv-startup.log" 2>&1

fi


cd ..

exit

EOF


# setup working directory -------------------------------------------------

CONF="/etc/rstudio/rsession.conf"

if [ -f "$CONF" ]; then

  sed -i \
    '/^session-default-working-dir=/d' \
    "$CONF"

  echo "session-default-working-dir=$PROJECT_DIR" >> "$CONF"

fi


echo ""
echo "Personal init script completed."
echo "RStudio will auto-restore renv environment via .Rprofile."

echo ""
echo "Java-konfigurasjon:"
echo "  SSB_JAVA_VERSION=$SSB_JAVA_VERSION"
echo "  JAVA_HOME=$JAVA_HOME"
