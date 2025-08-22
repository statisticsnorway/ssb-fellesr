#!/bin/bash

# Set GitHub token environment variable
if ! grep -q "GITHUB_PAT" "$HOME/.Renviron"; then
  echo "GITHUB_PAT=${GIT_PERSONAL_ACCESS_TOKEN}" >> "$HOME/.Renviron"
fi

# Set repo and project variables
REPO_NAME=$(basename "$GIT_REPOSITORY" .git)
PROJECT_DIR="$HOME/work/$REPO_NAME"
RPROJ_FILE="$REPO_NAME.Rproj"

if [ ! -d "$PROJECT_DIR" ]; then
  echo "Error: Project directory '$PROJECT_DIR' does not exist."
  exit 1
fi

RPROFILE_FILE="$PROJECT_DIR/.Rprofile"

PROJECTS_SETTINGS="/home/onyxia/.local/share/rstudio/projects_settings"
LASTPROJ_FILE="$PROJECTS_SETTINGS/last-project-path"

echo "$LASTPROJ_FILE"
echo $(ls "/home/onyxia/.local")
echo $(ls "/home/onyxia/.local/share")

echo $(ls "/home/onyxia/.local/share/rstudio")
echo $(ls $PROJECTS_SETTINGS)
echo $(ls $LASTPROJ_FILE)

mkdir -p "/home/onyxia/.local/share/rstudio/"

# mkdir -p "$PROJECTS_SETTINGS"
# touch "$LASTPROJ_FILE"
# echo "/home/onyxia/work/$REPO_NAME/$RPROJ_FILE" > "$LASTPROJ_FILE"


# cd $PROJECT_DIR
# Rscript -e 'install.packages("renv", repos = Sys.getenv("CRAN")); Sys.setenv("RENV_CONFIG_REPOS_OVERRIDE" = Sys.getenv("CRAN")); renv::restore(prompt = FALSE)'
# cd ..

# # Sørg for at fila finnes
# touch "$RPROFILE_FILE"

# # Legg til RENV_CONFIG_REPOS_OVERRIDE helt først hvis det ikke allerede finnes
# if ! grep -q "RENV_CONFIG_REPOS_OVERRIDE" "$RPROFILE_FILE"; then
#   { 
#     echo 'Sys.setenv("RENV_CONFIG_REPOS_OVERRIDE" = Sys.getenv("CRAN"))'
#     cat "$RPROFILE_FILE"
#   } > "$RPROFILE_FILE.tmp" && mv "$RPROFILE_FILE.tmp" "$RPROFILE_FILE"
# fi

# # Legg til auto-restore-blokken hvis den ikke finnes fra før
# if ! grep -q "renv::restore" "$RPROFILE_FILE"; then
#   cat << 'EOF' >> "$RPROFILE_FILE"

# # Auto-restore renv environment on R session startup
# if (requireNamespace("renv", quietly = TRUE)) {
#   tryCatch({
#     message("Restoring renv environment...")
#     renv::restore(prompt = FALSE, repos = Sys.getenv("CRAN"))
#     message("renv environment restored.")
#   }, error = function(e) {
#     message("renv::restore failed: ", e$message)
#   })
# } else {
#   message("Package 'renv' not available.")
# }
# EOF
# fi

# Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = Sys.getenv("CRAN"))'
# Rscript -e 'if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi", repos = Sys.getenv("CRAN"))'

# # Rscript -e 'rstudioapi::openProject("$RPROJECT_DIR/$RPROJ_FILE")'

echo "Personal init script completed. RStudio will auto-restore renv environment via .Rprofile."
