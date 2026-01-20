# =============================================================================
# Install DuckDB R Package + Spatial Extension from GitHub Actions Artifacts
# =============================================================================
#
# Usage (PowerShell):
#   .\tools\duckdb-dev\install_from_artifacts.ps1                    # Auto-detect latest
#   .\tools\duckdb-dev\install_from_artifacts.ps1 12345678
#   .\tools\duckdb-dev\install_from_artifacts.ps1 -Force             # No confirmation prompt
#
# Requirements:
#   - gh CLI (GitHub CLI) - https://cli.github.com/
#   - R with Rscript in PATH
#
# =============================================================================

param(
    [Parameter(Mandatory=$false, Position=0)]
    [string]$RunIdOrUrl,
    
    [Parameter(Mandatory=$false)]
    [switch]$Force
)

$ErrorActionPreference = "Stop"
$WORKFLOW_NAME = "dev_build_duckdb-r-spatial.yml"
$PRIMARY_REPO = "Cidree/duckspatial"
$FALLBACK_REPO = "e-kotov/duckspatial"

# Only Windows x64 is supported (ARM64 not built in CI)
$arch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
if ($arch -ne "X64") {
    Write-Error "Only Windows x64 is supported. Detected: $arch"
    exit 1
}

$ARTIFACT_EXT = "spatial-extension-windows-x64"
$ARTIFACT_PKG = "duckdb-r-package-windows-x64"
$DUCKDB_PLATFORM = "windows_amd64"

# Check for existing installation and prompt for override (skip if forced)
if (-not $Force) {
    $duckdbInstalled = $false
    try {
        $null = & Rscript -e "packageVersion('duckdb')" 2>$null
        $duckdbInstalled = $true
    } catch {
        $duckdbInstalled = $false
    }
    
    if ($duckdbInstalled) {
        Write-Host "=========================================="
        Write-Host "Currently installed:"
        Write-Host "=========================================="
        
        $rScript = @"
suppressPackageStartupMessages(library(duckdb))
con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = "true")))
v <- DBI::dbGetQuery(con, 'PRAGMA version')
cat('DuckDB R Package:\n')
cat('  Version:   ', v`$library_version, '\n')
cat('  Source ID: ', v`$source_id, '\n')
tryCatch({
  DBI::dbExecute(con, 'LOAD spatial')
  ext <- DBI::dbGetQuery(con, "SELECT extension_version FROM duckdb_extensions() WHERE extension_name='spatial'")
  if (nrow(ext) > 0) {
    cat('\nSpatial Extension:\n')
    cat('  Version:   ', ext`$extension_version, '\n')
  }
}, error = function(e) {
  cat('\nSpatial Extension: not installed\n')
})
DBI::dbDisconnect(con, shutdown=TRUE)
"@
        & Rscript -e $rScript 2>$null
        
        Write-Host "=========================================="
        Write-Host ""
        
        $confirm = Read-Host "Override with dev build? [y/N]"
        if ($confirm -notmatch "^[Yy]$") {
            Write-Host "Aborted."
            exit 0
        }
        Write-Host ""
    }
}

# Check if gh CLI is available (needed for both auto-detect and download)
if (-not (Get-Command "gh" -ErrorAction SilentlyContinue)) {
    Write-Error @"
GitHub CLI (gh) not found. Please install it:
  winget install --id GitHub.cli
  
Or download from: https://cli.github.com/
"@
    exit 1
}

# Parse input - or auto-detect latest successful run
if ([string]::IsNullOrEmpty($RunIdOrUrl)) {
    Write-Host "No run ID provided. Fetching latest successful build..."
    Write-Host ""
    
    function Find-LatestRunWithArtifact {
        param (
            [string]$Repo,
            [string]$Artifact
        )
        
        Write-Host -NoNewline "Checking $Repo... "
        
        # Get last 5 runs
        $runsJson = gh run list --repo $Repo --workflow $WORKFLOW_NAME --limit 5 --json databaseId 2>$null
        if (-not $runsJson) {
            Write-Host "no runs found"
            return $null
        }
        
        $runs = $runsJson | ConvertFrom-Json
        
        foreach ($run in $runs) {
            $runId = $run.databaseId
            
            # Check for artifact existence
            # Method 1: Check jobs/artifacts via run view
            $artifactsJson = gh run view $runId --repo $Repo --json artifacts 2>$null
            if ($artifactsJson -and ($artifactsJson | Select-String -Pattern "$Artifact")) {
                Write-Host "found suitable run $runId"
                return $runId
            }
            
            # Method 2: API check for failing runs
            $apiArtifacts = gh api "repos/$Repo/actions/runs/$runId/artifacts" --jq '.artifacts[].name' 2>$null
            if ($apiArtifacts -and ($apiArtifacts -match "^$Artifact$")) {
                Write-Host "found suitable run $runId"
                return $runId
            }
        }
        
        Write-Host "no builds with $Artifact found"
        return $null
    }
    
    # Try primary repo
    $RUN_ID = Find-LatestRunWithArtifact -Repo $PRIMARY_REPO -Artifact $ARTIFACT_PKG
    $REPO = $PRIMARY_REPO
    
    # Fallback to secondary if needed
    if (-not $RUN_ID) {
        $RUN_ID = Find-LatestRunWithArtifact -Repo $FALLBACK_REPO -Artifact $ARTIFACT_PKG
        $REPO = $FALLBACK_REPO
    }
    
    if (-not $RUN_ID) {
        Write-Host ""
        Write-Host "=========================================="
        Write-Host "ERROR: No runs with $DUCKDB_PLATFORM artifacts found!"
        Write-Host "=========================================="
        Write-Host ""
        Write-Host "Checked both repositories:"
        Write-Host "  - $PRIMARY_REPO"
        Write-Host "  - $FALLBACK_REPO"
        Write-Host ""
        Write-Host "Please download manually or wait for a new build."
        exit 1
    }
    
    Write-Host "Using run $RUN_ID from $REPO"
} elseif ($RunIdOrUrl -match "https://github.com/([^/]+/[^/]+)/actions/runs/(\d+)") {
    $REPO = $Matches[1]
    $RUN_ID = $Matches[2]
} elseif ($RunIdOrUrl -match "^\d+$") {
    $RUN_ID = $RunIdOrUrl
    # We have a run ID but don't know the repo. Check where it exists.
    if (gh run view "$RUN_ID" --repo "$PRIMARY_REPO" 2>$null) {
        $REPO = $PRIMARY_REPO
    } elseif (gh run view "$RUN_ID" --repo "$FALLBACK_REPO" 2>$null) {
        $REPO = $FALLBACK_REPO
    } else {
        # Default to primary if not found (will fail later with better error)
        $REPO = $PRIMARY_REPO
    }
} else {
    Write-Error "Invalid input. Provide a run ID or GitHub Actions URL."
    exit 1
}

# Create temp directory
$TEMP_DIR = Join-Path $env:TEMP "duckdb_artifacts_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
New-Item -ItemType Directory -Path $TEMP_DIR -Force | Out-Null

try {
    Write-Host "Downloading artifacts..."
    Write-Host ""

    # Download R package artifact
    Write-Host "1. Downloading DuckDB R package ($ARTIFACT_PKG)..."
    $pkgDir = Join-Path $TEMP_DIR "r-package"
    gh run download $RUN_ID --repo $REPO --name $ARTIFACT_PKG --dir $pkgDir
    if ($LASTEXITCODE -ne 0) {
        Write-Error @"
Failed to download R package artifact.
Make sure:
  - The run ID is correct
  - The workflow completed successfully
  - You have access to the repository (run 'gh auth login')
"@
        exit 1
    }

    # Download spatial extension artifact
    Write-Host "2. Downloading Spatial Extension ($ARTIFACT_EXT)..."
    $extDir = Join-Path $TEMP_DIR "spatial-ext"
    gh run download $RUN_ID --repo $REPO --name $ARTIFACT_EXT --dir $extDir
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to download spatial extension artifact."
        exit 1
    }

    Write-Host ""
    Write-Host "=========================================="
    Write-Host "Installing artifacts..."
    Write-Host "=========================================="
    Write-Host ""

    # Install R package
    Write-Host "1. Installing DuckDB R package..."
    $pkgFile = Get-ChildItem -Path $pkgDir -Filter "duckdb_*.zip" | Select-Object -First 1
    if (-not $pkgFile) {
        Write-Error "No R package file (.zip) found in artifact."
        exit 1
    }
    Write-Host "   Package: $($pkgFile.Name)"
    & Rscript -e "install.packages('$($pkgFile.FullName -replace '\\', '/')', repos = NULL, type = 'win.binary')"
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to install R package."
        exit 1
    }

    # Get DuckDB version info
    Write-Host ""
    Write-Host "2. Detecting DuckDB version..."
    $duckdbInfo = & Rscript -e "library(duckdb); con <- DBI::dbConnect(duckdb::duckdb()); info <- DBI::dbGetQuery(con, 'PRAGMA version'); cat(paste0(info`$library_version, '|', info`$source_id)); DBI::dbDisconnect(con, shutdown=TRUE)" 2>&1
    $parts = $duckdbInfo -split '\|'
    $DUCKDB_VERSION = $parts[0]
    $DUCKDB_SHA = $parts[1]
    Write-Host "   Version: $DUCKDB_VERSION"
    Write-Host "   SHA:     $DUCKDB_SHA"

    # Install spatial extension
    Write-Host ""
    Write-Host "3. Installing Spatial Extension..."
    $extFile = Join-Path $extDir "spatial.duckdb_extension"
    if (-not (Test-Path $extFile)) {
        Write-Error "Spatial extension file not found."
        exit 1
    }
    Write-Host "   Extension file: $extFile"

    # Use DuckDB's INSTALL from file path
    $extFilePath = $extFile -replace '\\', '/'
    $rScript = @"
library(duckdb)
con <- DBI::dbConnect(duckdb::duckdb(config=list(allow_unsigned_extensions='true')))
DBI::dbExecute(con, sprintf("INSTALL '%s'", '$extFilePath'))
DBI::dbExecute(con, 'LOAD spatial')
result <- DBI::dbGetQuery(con, "SELECT ST_AsText(ST_Point(1, 2)) as point")
print(result)
DBI::dbDisconnect(con, shutdown=TRUE)
"@
    & Rscript -e $rScript
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to install spatial extension."
        exit 1
    }

    Write-Host ""
    Write-Host "=========================================="
    Write-Host "SUCCESS! DuckDB + Spatial extension installed."
    Write-Host "=========================================="

} finally {
    # Cleanup
    if (Test-Path $TEMP_DIR) {
        Remove-Item -Path $TEMP_DIR -Recurse -Force -ErrorAction SilentlyContinue
    }
}
