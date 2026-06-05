# Gender Interactions Replication Package

This package contains the minimal code needed to run the analyses on simulated data. The CSV in `data/` is fully simulated and is included only so the code can be executed. Results from these data will not reproduce the manuscript estimates.

## Files

- `data/my_data_simulated.csv`: simulated student-course data.
- `code/analysis.do`: Stata code for the main and supplementary estimates.
- `code/permutation_diagnostics.R`: R code for the sample summaries and permutation diagnostics.

## Run

From the `code/` folder, run:

```stata
do analysis.do
```

The Stata script writes logs and figures to `output/`.

To run the R code from the `code/` folder:

```r
source("permutation_diagnostics.R")
```

The R script also writes outputs to `output/`.
