#!/bin/bash

# Set GitHub token environment variable
export GITHUB_PAT="$GITHUB_ACCESS_TOKEN"

# Set repo and project variables
REPO_URL="$GIT_REPOSITORY"
REPO_NAME=$(basename "$REPO_URL" .git)
PROJECT_DIR="$HOME/work/$REPO_NAME"
RPROJ_FILE="$REPO_NAME.Rproj"

# Navigate to the project directory
cd "$PROJECT_DIR" || exit

# Restore renv environment
Rscript -e 'if (requireNamespace("renv", quietly = TRUE)) renv::restore() else install.packages("renv"); renv::restore()'
