# Gender Interactions Replication Package

This package contains the minimal code needed to run the analyses on simulated data. The CSV in `data/` is fully simulated and is included only so the code can be executed. Results from these data will not reproduce the manuscript estimates.

## License

The analysis code is released under the MIT License. See `LICENSE`.

## System Requirements

Tested on macOS 15.6.1 with Stata 19 and R 4.2.3. No non-standard hardware is required; a normal desktop or laptop computer is sufficient.

Required Stata user-written commands:

- `estout`/`esttab`
- `reghdfe`
- `coefplot`
- `ivreghdfe`

Required R packages:

- `pacman`
- `haven`
- `estimatr`
- `texreg`
- `janitor`
- `tidyverse`
- `skimr`
- `compareGroups`
- `progress`
- `data.table`
- `Matrix`
- `knitr`

## Installation

Install the Stata dependencies before running `code/analysis.do`:

```stata
ssc install estout, replace
ssc install ftools, replace
ssc install reghdfe, replace
ssc install coefplot, replace
ssc install ivreg2, replace
ssc install ranktest, replace
ssc install ivreghdfe, replace
```

Install the R dependencies before running `code/permutation_diagnostics.R`:

```r
install.packages(c(
  "pacman", "haven", "estimatr", "texreg", "janitor", "tidyverse",
  "skimr", "compareGroups", "progress", "data.table", "Matrix", "knitr"
))
```

Typical installation time on a normal desktop computer is under 10 minutes, depending on internet speed and whether dependencies are already installed.

## Files

- `data/my_data_simulated.csv`: simulated student-course data.
- `code/analysis.do`: Stata code for the main and supplementary estimates.
- `code/permutation_diagnostics.R`: R code for the sample summaries and permutation diagnostics.
- `LICENSE`: MIT License for the analysis code.

## Demo and Instructions for Use

From the `code/` folder, run the Stata script:

```stata
do analysis.do
```

The Stata script reads `../data/my_data_simulated.csv` by default and writes output to `../output/`. Expected output includes `analysis.log` and `fig_fxbelong.pdf`.

To run the R code from the `code/` folder:

```r
source("permutation_diagnostics.R")
```

The R script reads `../data/my_data_simulated.csv` by default and writes output to `../output/`. Expected output includes `fig_perm_randomization_cdf.pdf` and `tab_perm_randomization_diagnostics.tex`.

Expected runtime for each script on a normal desktop computer is a few minutes.

To use a different dataset or output folder, pass paths to the Stata do-file:

```stata
do analysis.do path/to/data.csv path/to/output_folder
```

For the R script, set environment variables before running:

```r
Sys.setenv(GENDER_DATA_CSV = "path/to/data.csv")
Sys.setenv(GENDER_OUTPUT_DIR = "path/to/output_folder")
source("permutation_diagnostics.R")
```
