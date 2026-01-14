# Replication code (peer review)

This folder contains the Stata code to reproduce the tables and appendix results for Fairlie et al.

## Files
- `gender_tables_replication.do`: replication script (tables/figures).

## Software requirements
- Stata 16+ (recommended).

## How to run
1. Edit the import path at the top of the do-file to point to the dataset location on your machine:
   `import delimited using "XXX/.../my_data.csv", clear`
2. From Stata, run:
   `do gender_tables_replication.do`

## Notes
- To suppress regression output, set: `local detail = 1`.
- The script saves a processed temporary dataset (`tempfile full`) used across sections.
- The script currently stops after creating the tempfile (`stop`); remove/comment out that line to run the full replication end-to-end.
- One figure export path is user-specific; update the `graph export` path if you want to reproduce the saved figure output.
