# Installing DuckDB + Spatial Extension from Artifacts

This directory contains helper scripts to install pre-built DuckDB R package and spatial extension from GitHub Actions artifacts.

## Why Use This?

- **Fast setup**: No compilation needed (~30 seconds vs ~45 minutes)
- **Version control**: Choose exactly which build you want
- **Guaranteed compatibility**: DuckDB R package and spatial extension are built from the same commit

## Quick Start

The scripts automatically install the **latest successful build** when run without arguments. If DuckDB R package is already installed, you'll be prompted before overriding.

> [!IMPORTANT]
> **After Installation**: You MUST enable unsigned extensions to use this dev build.
>
> Restart your R session and connect like this:
> ```r
> library(duckdb)
> con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = "true")))
> ```
> The extension will be loaded automatically (if persistent) or use `DBI::dbExecute(con, "LOAD spatial")`.

### Linux / macOS (Bash)

```bash
# Install latest build (auto-detect, prompts if already installed)
./tools/duckdb-dev/install_from_artifacts.sh

# Skip confirmation prompt
./tools/duckdb-dev/install_from_artifacts.sh --force

# Specify a run ID
./tools/duckdb-dev/install_from_artifacts.sh 12345678
```

### Windows (PowerShell)

```powershell
# Install latest build (auto-detect, prompts if already installed)
.\tools\duckdb-dev\install_from_artifacts.ps1

# Skip confirmation prompt
.\tools\duckdb-dev\install_from_artifacts.ps1 -Force

# Specify a run ID
.\tools\duckdb-dev\install_from_artifacts.ps1 12345678
```

### From R (Cross-Platform)

```r
source("tools/duckdb-dev/install_from_artifacts.R")

# Install latest dev version (prompts if already installed)
install_dev_duckdb_spatial()

# Skip confirmation prompt
install_dev_duckdb_spatial(force = TRUE)

# Specify a run ID
install_dev_duckdb_spatial(12345678)

# Revert to stable CRAN version
install_cran_duckdb_spatial()

# Check installed versions
check_duckdb_spatial_version()
```

### Finding a Specific Run ID

Visit [GitHub Actions](https://github.com/e-kotov/duckspatial/actions/workflows/dev_build_duckdb-r-spatial.yml) to browse available builds.

## What Gets Installed?

1.  **DuckDB R Package**: A standard CRAN-like source or binary package (depending on platform) built from the `v1.1.4-dev` branch.
2.  **Spatial Extension**: A `.duckdb_extension` binary file, installed into your local DuckDB extensions directory (e.g., `~/.duckdb/extensions/v1.1.4-dev/spatial.duckdb_extension`).
    - Note: This is an **unsigned extension**, which is why the `allow_unsigned_extensions` setting is required.

## Troubleshooting

-   **"gh: command not found"**: You need the GitHub CLI.
    -   macOS: `brew install gh`
    -   Windows: `winget install GitHub.cli`
    -   Linux: Check distro package manager or build from source.
-   **"Resource not accessible by integration"**: Try running `gh auth login` to refresh your credentials.
-   **"Spatial Extension: not installed"**: Ensure you enabled `allow_unsigned_extensions = "true"` when connecting. The script does this automatically during its version check.
