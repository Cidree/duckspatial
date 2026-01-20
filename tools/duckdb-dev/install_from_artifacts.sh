#!/bin/bash
set -e

# =============================================================================
# Install DuckDB R Package + Spatial Extension from GitHub Actions Artifacts
# =============================================================================
#
# Usage:
#   ./tools/duckdb-dev/install_from_artifacts.sh [options] [run_id_or_url]
#
# Options:
#   -y, --force    Skip confirmation prompt (override without asking)
#
# Examples:
#   ./tools/duckdb-dev/install_from_artifacts.sh           # Auto-detect latest
#   ./tools/duckdb-dev/install_from_artifacts.sh 12345678
#   ./tools/duckdb-dev/install_from_artifacts.sh --force   # Auto-detect, no prompt
#   ./tools/duckdb-dev/install_from_artifacts.sh -y 12345678
#
# This script:
# 1. Shows currently installed DuckDB version (if any)
# 2. Asks for confirmation before overriding
# 3. Downloads and installs DuckDB R package + spatial extension
#
# =============================================================================

# Parse flags
FORCE="false"
RUN_ID_ARG=""

for arg in "$@"; do
    case $arg in
        -y|--force)
            FORCE="true"
            shift
            ;;
        *)
            RUN_ID_ARG="$arg"
            ;;
    esac
done

# Check for existing installation and prompt for override (skip if forced)
if [ "$FORCE" != "true" ] && command -v Rscript &> /dev/null; then
    if Rscript -e "packageVersion('duckdb')" &>/dev/null 2>&1; then
        echo "=========================================="
        echo "Currently installed:"
        echo "=========================================="
        Rscript --vanilla -e "
          suppressPackageStartupMessages(library(duckdb))
          con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = "true")))
          v <- DBI::dbGetQuery(con, 'PRAGMA version')
          cat('DuckDB R Package:\n')
          cat('  Version:   ', v\$library_version, '\n')
          cat('  Source ID: ', v\$source_id, '\n')
          tryCatch({
            DBI::dbExecute(con, 'LOAD spatial')
            ext <- DBI::dbGetQuery(con, \"SELECT extension_version FROM duckdb_extensions() WHERE extension_name='spatial'\")
            if (nrow(ext) > 0) {
              cat('\nSpatial Extension:\n')
              cat('  Version:   ', ext\$extension_version, '\n')
            }
          }, error = function(e) {
            cat('\nSpatial Extension: not installed\n')
          })
          DBI::dbDisconnect(con, shutdown=TRUE)
        " 2>/dev/null || echo "  (Could not read version details)"
        echo "=========================================="
        echo ""
        
        read -p "Override with dev build? [y/N]: " confirm
        if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
            echo "Aborted."
            exit 0
        fi
        echo ""
    fi
fi

PLATFORM_ARCH=$(uname -m)

# Detect platform
PLATFORM_OS=$(uname -s)

# Detect platform
if [ "$PLATFORM_OS" = "Linux" ]; then
    if [ "$PLATFORM_ARCH" = "x86_64" ]; then
        ARTIFACT_EXT="spatial-extension-linux-x64"
        ARTIFACT_PKG="duckdb-r-package-linux-x64"
        DUCKDB_PLATFORM="linux_amd64"
    elif [ "$PLATFORM_ARCH" = "aarch64" ] || [ "$PLATFORM_ARCH" = "arm64" ]; then
        ARTIFACT_EXT="spatial-extension-linux-arm64"
        ARTIFACT_PKG="duckdb-r-package-linux-arm64"
        DUCKDB_PLATFORM="linux_arm64"
    else
        echo "Unsupported Linux architecture: $PLATFORM_ARCH"
        exit 1
    fi
elif [ "$PLATFORM_OS" = "Darwin" ]; then
    if [ "$PLATFORM_ARCH" = "arm64" ]; then
        ARTIFACT_EXT="spatial-extension-macos-arm64"
        ARTIFACT_PKG="duckdb-r-package-macos-arm64"
        DUCKDB_PLATFORM="osx_arm64"
    else
        # macOS Intel (x86_64) - if we support it later
        ARTIFACT_EXT="spatial-extension-macos-amd64" # Guessing name, not in CI yet
        ARTIFACT_PKG="duckdb-r-package-macos-amd64"
        DUCKDB_PLATFORM="osx_amd64"
        echo "Warning: macOS Intel support is experimental/untested in CI"
    fi
else
    echo "Unsupported OS: $PLATFORM_OS"
    exit 1
fi

# Parse input - or auto-detect latest successful run
WORKFLOW_NAME="dev_build_duckdb-r-spatial.yml"
PRIMARY_REPO="Cidree/duckspatial"
FALLBACK_REPO="e-kotov/duckspatial"

if [ -z "$RUN_ID_ARG" ]; then
    echo "No run ID provided. Fetching latest successful build..."
    echo ""
    
    # Check if gh CLI is available
    if ! command -v gh &> /dev/null; then
        echo "Error: gh CLI not found. Please install it or provide a run ID."
        echo ""
        echo "Usage: $0 [run_id_or_url]"
        echo ""
        echo "To install gh CLI:"
        echo "  Linux:  sudo apt-get install gh"
        echo "  macOS:  brew install gh"
        echo ""
        echo "Or download manually from:"
        echo "  https://github.com/$PRIMARY_REPO/actions/workflows/$WORKFLOW_NAME"
        echo "  https://github.com/$FALLBACK_REPO/actions/workflows/$WORKFLOW_NAME"
        exit 1
    fi
    
    # Function to find the latest run that actually has our artifact
    find_latest_run_with_artifact() {
        local repo="$1"
        local artifact="$2"
        local found_id=""
        
        echo -n "Checking $repo... " >&2
        
        # Get last 5 runs (allow stderr to show errors)
        local runs=$(gh run list --repo "$repo" --workflow "$WORKFLOW_NAME" --limit 5 --json databaseId --jq '.[].databaseId')
        
        for run_id in $runs; do
            # Debug: Check artifacts via API (most reliable)
            # Remove 2>/dev/null to see permission errors if any
            local arts=$(gh api "repos/$repo/actions/runs/$run_id/artifacts" --jq '.artifacts[].name')
            
            # Check if our artifact is in the list
            if echo "$arts" | grep -q "^$artifact$"; then
                 found_id="$run_id"
                 break
            fi
        done
        
        if [ -n "$found_id" ]; then
            echo "found suitable run $found_id" >&2
            echo "$found_id"
            return 0
        else
            echo "no builds with $artifact found" >&2
            # Echo analyzed runs for debugging
            echo "  (Checked runs: $(echo $runs | tr '\n' ' '))" >&2
            return 1
        fi
    }
    
    # Try primary repo
    RUN_ID=$(find_latest_run_with_artifact "$PRIMARY_REPO" "$ARTIFACT_PKG") || true
    
    if [ -n "$RUN_ID" ]; then
        REPO="$PRIMARY_REPO"
    else
        # Try fallback repo
        RUN_ID=$(find_latest_run_with_artifact "$FALLBACK_REPO" "$ARTIFACT_PKG") || true
        if [ -n "$RUN_ID" ]; then
            REPO="$FALLBACK_REPO"
        fi
    fi
    
    if [ -z "$RUN_ID" ]; then
        echo ""
        echo "=========================================="
        echo "ERROR: No workflow runs found!"
        echo "=========================================="
        echo ""
        echo "Checked:"
        echo "  - $PRIMARY_REPO"
        echo "  - $FALLBACK_REPO"
        echo ""
        echo "Please specify a run ID manually:"
        echo "  $0 <run_id>"
        exit 1
    fi
    
    echo ""
    echo "Using run $RUN_ID from $REPO"
else
    INPUT="$RUN_ID_ARG"
    
    # Extract run ID from URL if needed
    if [[ "$INPUT" =~ https://github.com/([^/]+/[^/]+)/actions/runs/([0-9]+) ]]; then
        REPO="${BASH_REMATCH[1]}"
        RUN_ID="${BASH_REMATCH[2]}"
    elif [[ "$INPUT" =~ ^[0-9]+$ ]]; then
        RUN_ID="$INPUT"
        # We have a run ID but don't know the repo. Check where it exists.
        if gh run view "$RUN_ID" --repo "$PRIMARY_REPO" &>/dev/null; then
            REPO="$PRIMARY_REPO"
        elif gh run view "$RUN_ID" --repo "$FALLBACK_REPO" &>/dev/null; then
            REPO="$FALLBACK_REPO"
        else
            # Default to primary if not found (will fail later with better error)
            REPO="$PRIMARY_REPO"
        fi
    else
        echo "Error: Invalid input. Provide a run ID or GitHub Actions URL."
        exit 1
    fi
fi

echo "=========================================="
echo "Installing from GitHub Actions Run: $RUN_ID"
echo "Repository: $REPO"
echo "Platform: $PLATFORM_ARCH"
echo "=========================================="
echo ""

# Check if gh CLI is available
if ! command -v gh &> /dev/null; then
    echo "Error: gh CLI not found. Installing..."
    sudo apt-get update && sudo apt-get install -y gh || {
        echo "Failed to install gh CLI. Please install manually:"
        echo "  https://github.com/cli/cli#installation"
        exit 1
    }
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo "Downloading artifacts..."
echo ""

# Download R package artifact
echo -n "[1/2] Downloading DuckDB R package ($ARTIFACT_PKG)... "
if ! gh run download "$RUN_ID" --repo "$REPO" --name "$ARTIFACT_PKG" --dir "$TEMP_DIR/r-package" 2>/dev/null; then
    echo "FAILED"
    echo ""
    echo "Error: Failed to download R package artifact."
    echo "Make sure:"
    echo "  - The run ID is correct"
    echo "  - The workflow completed successfully"
    echo "  - You have access to the repository"
    exit 1
fi
echo "done"

# Download spatial extension artifact
echo -n "[2/2] Downloading Spatial Extension ($ARTIFACT_EXT)... "
if ! gh run download "$RUN_ID" --repo "$REPO" --name "$ARTIFACT_EXT" --dir "$TEMP_DIR/spatial-ext" 2>/dev/null; then
    echo "FAILED"
    echo "Error: Failed to download spatial extension artifact."
    exit 1
fi
echo "done"

echo ""
echo "=========================================="
echo "Installing artifacts..."
echo "=========================================="
echo ""

# Install R package
echo -n "[1/2] Installing DuckDB R package... "
# Try to find binary package first (.tgz for macOS/some Linux)
PKG_FILE=$(find "$TEMP_DIR/r-package" -name "duckdb_*.tgz" | head -n 1)

# If no binary, look for source or other format (.tar.gz)
if [ -z "$PKG_FILE" ]; then
    PKG_FILE=$(find "$TEMP_DIR/r-package" -name "duckdb_*.tar.gz" | head -n 1)
fi

if [ -z "$PKG_FILE" ]; then
    echo "FAILED"
    echo "Error: No R package file found in artifact."
    exit 1
fi

R CMD INSTALL "$PKG_FILE" >/dev/null 2>&1 && echo "done" || { echo "FAILED"; exit 1; }

# Get DuckDB version info
echo ""
echo "2. Detecting DuckDB version..."
DUCKDB_SHA=$(Rscript -e "library(duckdb); con <- DBI::dbConnect(duckdb::duckdb()); cat(DBI::dbGetQuery(con, 'PRAGMA version')\$source_id); DBI::dbDisconnect(con, shutdown=TRUE)")
DUCKDB_VERSION=$(Rscript -e "library(duckdb); con <- DBI::dbConnect(duckdb::duckdb()); cat(DBI::dbGetQuery(con, 'PRAGMA version')\$library_version); DBI::dbDisconnect(con, shutdown=TRUE)")

echo "   Version: $DUCKDB_VERSION"
echo "   SHA:     $DUCKDB_SHA"

# Install spatial extension
echo ""
echo "3. Installing Spatial Extension..."
EXT_FILE="$TEMP_DIR/spatial-ext/spatial.duckdb_extension"

if [ ! -f "$EXT_FILE" ]; then
    echo "Error: Spatial extension file not found."
    exit 1
fi

# Install spatial extension using DuckDB's INSTALL command
echo "3. Installing Spatial Extension from file..."
EXT_FILE="$TEMP_DIR/spatial-ext/spatial.duckdb_extension"

if [ ! -f "$EXT_FILE" ]; then
    echo "Error: Spatial extension file not found."
    exit 1
fi

echo "   Extension file: $EXT_FILE"
echo ""

# Use R to INSTALL from the file path and then LOAD/Verify
Rscript -e "args <- commandArgs(trailingOnly=TRUE); ext_path <- args[1]; cat(sprintf('Installing extension from: %s\n', ext_path)); library(duckdb); con <- DBI::dbConnect(duckdb::duckdb(config=list(allow_unsigned_extensions='true'))); DBI::dbExecute(con, sprintf(\"FORCE INSTALL '%s'\", ext_path)); DBI::dbExecute(con, 'LOAD spatial'); result <- DBI::dbGetQuery(con, \"SELECT ST_AsText(ST_Point(1, 2)) as point\"); print(result); DBI::dbDisconnect(con, shutdown=TRUE)" "$EXT_FILE"

echo ""
echo "Success! You can now use DuckDB with the spatial extension."
