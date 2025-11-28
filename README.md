# Simulation Code for "The Bit Scale"

This repository contains the replication code for the simulation study presented in the paper:

> **Wallmark, J., & Wiberg, M. (2025).** The bit scale: A metric score scale for unidimensional item response theory models. *Psychometrika*. [DOI Link]

The simulation evaluates the asymptotic properties (Bias, SE, RMSE) of the proposed **Bit Scale** compared to the traditional latent trait scale ($\theta$) under the 2PL model.

## Software Requirements and Installation

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies. This ensures that the exact package versions used to produce the paper's results are installed, improving reproducibility.

**Key Dependencies:**
* `mirt` (Item Response Theory estimation)
* `bitscale` (Proposed metric scale transformation)
* `dplyr`, `purrr`, `tibble` (Data manipulation)
* `ggplot2` (Visualization)
* `furrr` (Parallel processing)

### How to reproduce the environment

1.  Clone or download this repository.
2.  Open the project in RStudio (or start R in the project directory).
3.  Run the following command in the R console to install the exact package versions defined in `renv.lock`:

```r
if (!require("renv")) install.packages("renv")
renv::restore()
```

**Note**: `renv` will automatically handle the installation of the `bitscale` package from GitHub, as well as all other dependencies.

## Repository Structure

- 01_run_simulation.R: The main simulation script.
- 02_generate_figures.R: Visualization and summary.

## Instructions to Reproduce

1. Run the Simulation: Execute 01_run_simulation.R. This will generate four data files in your working directory:
  - df_theta_all.rds
  - df_bit_all.rds
  - df_theta_se_all.rds
  - df_bit_se_all.rds
2. Generate Figures: Execute 02_generate_figures.R. This will output:
  - combined_metrics.pdf (Figure 7 in the paper)
  - combined_se.pdf (Figure 8 in the paper)
