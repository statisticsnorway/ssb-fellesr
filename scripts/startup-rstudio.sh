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
if ! grep -q "renv::restore" "$RPROFILE_FILE"; then
  cat << 'EOF' >> "$RPROFILE_FILE"
  
# Auto-restore renv environment on R session startup
if (requireNamespace("renv", quietly = TRUE)) {
  tryCatch({
    message("Restoring renv environment...")
    renv::restore(prompt = FALSE, repos = Sys.getenv("CRAN"))
    message("renv environment restored.")
  }, error = function(e) {
    message("renv::restore failed: ", e$message)
  })
} else {
  message("Package 'renv' not available.")
}
EOF
fi

Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = Sys.getenv("CRAN"))'
Rscript -e 'if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi", repos = Sys.getenv("CRAN"))'

# Rscript -e 'rstudioapi::openProject("$RPROJECT_DIR/$RPROJ_FILE")'

echo "Personal init script completed. RStudio will auto-restore renv environment via .Rprofile."
