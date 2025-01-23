# Pantry (v1.0)
2025-01-23

*Basic functions to keep in the data cleaning & analysis 'pantry'*



## Required packages 

* tidyverse
* data.table


## clean_descr.R

Data Renaming & Cleaning Functions: 

* add_descr_labels: Add descriptive labels to a variable (option to set factor/order)

Outliers & Missing data: 

* outliers: Remove outliers by SD (options: remove (returns data with outliers recoded as NA) or find (returns data with outliers recoded as $value 
* recode_na: Recode missing values (option: missing as NA or 1/0
* median_imp_ukb: Median impute for negative or missing values
* zscore: Calculate z-score
* winsorize: Winsorize data by SD

Basic descriptive functions:

* mean_sd: Print continuous vars as mean +- SD 
* n_pct: Print categorical vars as n (%)
* median_25to75: Print continuous vars as median [25th, 75th %tile]
* format_p: Print P-values as rounded or scientific (if <0.01)



## regress.R

