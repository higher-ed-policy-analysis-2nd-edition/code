# Chapter 4: Creating Datasets and Managing Data

## Overview

This repository contains the complete, executable Stata code for Chapter 4 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition) by Marvin A. Titus.

**Chapter Topics:**
- Setting up working directories
- Creating datasets from primary data sources
- Importing and managing secondary data
- Creating cross-sectional datasets
- Building time-series datasets
- Constructing panel (cross-sectional time-series) datasets
- Merging multiple datasets

## Repository Structure

```
code/ch4/
├── README.md                          # This file
├── ch4_complete.do                    # Complete Stata script for all examples
├── ch4_section_4_2_1_primary.do      # Section 4.2.1: Primary data examples
├── ch4_section_4_2_2_crosssection.do # Section 4.2.2: Cross-sectional data
├── ch4_section_4_2_2_timeseries.do   # Section 4.2.2: Time-series data
└── ch4_section_4_2_2_panel.do        # Section 4.2.2: Panel data creation
```

## Prerequisites

### Software Requirements
- **Stata 19 or later** (tested on Stata 19.5)
  - Stata/SE or Stata/MP recommended for large datasets
  - Stata/IC will work for most examples
  
### Required Stata Packages
Install these packages before running the code:

```stata
* Install statastates package for FIPS codes and state abbreviations
ssc install statastates, replace
```

### Data Requirements
All data files must be downloaded from the book's data repository:
- **Data Repository:** [https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4)

**Required Data Files:**
- `tabn302_50.xlsx` - NCES Table 302.50 (High school graduates enrolled in PSE by state)
- `tabn302_10.xlsx` - NCES Table 302.10 (College enrollment rates 1960-2016)
- `tabn304_70.xlsx` - NCES Table 304.70 (Undergraduate enrollment by state)
- `Example_4_2_1.csv` - Sample primary data file

## Getting Started

### Step 1: Set Up Your Working Directory

Create the following directory structure on your local computer:

```
C:/Users/YourName/Documents/book-materials/
├── ch4/
│   ├── data/        # Place downloaded data files here
│   └── code/        # Place .do files here
├── ch5/
└── ...
```

**For Mac/Linux users:** Adjust paths accordingly (e.g., `/Users/YourName/Documents/book-materials/ch4/data`)

### Step 2: Download Data Files

Download all required data files from the data repository and save them to your local `ch4/data/` directory, or use the `copy` command in Stata to download files directly (as shown in the code examples).

### Step 3: Update File Paths

Open the Stata .do files and update the global path at the beginning:

```stata
* REQUIRED: Update this path to match your local directory
global ch4data "C:/Users/YourName/Documents/book-materials/ch4/data"
cd "$ch4data"
```

### Step 4: Run the Code

You can either:
- **Run the complete script:** Execute `ch4_complete.do` to run all examples in sequence
- **Run individual sections:** Execute specific section scripts for targeted examples

## Code Examples by Section

### Section 4.2.1: Primary Data
**File:** `ch4_section_4_2_1_primary.do`

**Topics Covered:**
- Creating datasets using the `input` command
- Entering data manually in Stata
- Using the Stata data editor
- Importing data from CSV files

**Key Commands:**
```stata
input variable_x variable_y variable_z
list
save "Example_4_2_1.dta"
edit
insheet using "Example_4_2_1.csv", comma
```

### Section 4.2.2: Secondary Data - Cross-Sectional Datasets
**File:** `ch4_section_4_2_2_crosssection.do`

**Topics Covered:**
- Importing data from Excel files
- Adding FIPS codes and state abbreviations
- Creating variable labels
- Data cleaning and formatting

**Key Commands:**
```stata
copy "https://raw.githubusercontent.com/.../tabn302_50.xlsx" "tabn302_50.xlsx", replace
import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear
ssc install statastates, replace
statastates, name(State)
order state_abbrev state_fips, before(State)
lab var stateid "State id"
```

**Datasets Created:**
- `US high school graduates in 2012 enrolled in PSE, by state.dta`

### Section 4.2.2: Time-Series Datasets
**File:** `ch4_section_4_2_2_timeseries.do`

**Topics Covered:**
- Creating time-series datasets from Excel
- Generating year variables
- Declaring time-series structure
- Time-series data management

**Key Commands:**
```stata
gen year = 1959 + _n
order year, first
tsset year, yearly
lab var totalpct "Percent of HS graduates enrolled in PSE"
```

**Datasets Created:**
- `Percent of US high school graduates in PSE, 1960 to 2016.dta`
- `Example_4_2_2_TS.dta`

### Section 4.2.2: Panel Datasets
**File:** `ch4_section_4_2_2_panel.do`

**Topics Covered:**
- Creating panel (cross-sectional time-series) datasets
- Reshaping data from wide to long format
- Declaring panel data structure
- Merging multiple variables
- Using `joinby` to combine datasets

**Key Commands:**
```stata
reshape long Ugrad, i(id) j(year)
xtset id year, yearly
xtdescribe
joinby id year using "HSGrad - Long"
```

**Datasets Created:**
- `Undergraduate enrollment data - Wide.dta`
- `Undergraduate enrollment data - Long.dta`
- `HSGrad - Long.dta`
- `Complete_Panel_Dataset.dta`

## Important Notes

### Working Directory Management
Always verify your working directory before running code:
```stata
pwd  # Print working directory
```

### File Path Conventions
- This book uses **forward slashes (/)** in file paths for cross-platform compatibility
- Windows users can use backslashes (\\) if preferred - both work in Stata

### Excel File Import
When importing Excel files:
- Ensure data is properly formatted (no non-numeric characters in numeric columns)
- Use the `firstrow` option if the first row contains variable names
- Specify the worksheet using `sheet("worksheetname")`

### Panel Data Structure
Understanding panel data balance:
- **Strongly balanced:** Same number of observations for each panel unit
- **Strongly balanced with gaps:** Balanced but missing some time periods
- **Weakly balanced:** Varying number of observations across units

### Variable Naming Conventions
- Use descriptive names following Stata conventions
- Variable labels cannot exceed 80 characters
- Avoid special characters in variable names

## Troubleshooting

### Common Issues

**Issue:** "File not found" error
**Solution:** 
- Verify you've downloaded all required data files
- Check that your working directory is set correctly using `pwd`
- Ensure file paths use correct slashes (/ or \\)

**Issue:** "statastates not found"
**Solution:** 
```stata
ssc install statastates, replace
```

**Issue:** Excel import fails
**Solution:** 
- Verify the Excel file is not open in another program
- Check that the worksheet name matches exactly (case-sensitive)
- Ensure the `firstrow` option is used if variable names are in the first row

**Issue:** Panel data merge errors
**Solution:** 
- Verify that id and year variables exist in both datasets
- Check for duplicate observations using `duplicates report id year`
- Ensure datasets are in long format before merging

**Issue:** Memory limitations with large datasets
**Solution:** 
```stata
* For Stata/MP:
set maxvar 60000

* For Stata/SE:
set maxvar 32000

* Increase memory allocation:
set memory 2g
```

## Additional Resources

### Related Repositories
- **Data Repository:** [https://github.com/higher-ed-policy-analysis-2nd-edition/data](https://github.com/higher-ed-policy-analysis-2nd-edition/data)
- **Figures Repository:** [https://github.com/higher-ed-policy-analysis-2nd-edition/figures](https://github.com/higher-ed-policy-analysis-2nd-edition/figures)
- **Tables Repository:** [https://github.com/higher-ed-policy-analysis-2nd-edition/tables](https://github.com/higher-ed-policy-analysis-2nd-edition/tables)

### Data Sources
- **NCES Digest of Education Statistics:** [https://nces.ed.gov/programs/digest/](https://nces.ed.gov/programs/digest/)
  - Table 302.10: Percentage of recent high school completers enrolled in college
  - Table 302.50: First-time postsecondary enrollment by state
  - Table 304.70: Total undergraduate enrollment by state

### Stata Resources
- **Stata Manual:** [https://www.stata.com/manuals/](https://www.stata.com/manuals/)
- **Stata YouTube Channel:** [https://www.youtube.com/user/statacorp](https://www.youtube.com/user/statacorp)
- **Statalist Forum:** [https://www.statalist.org/](https://www.statalist.org/)

## Chapter Appendix

The complete Stata code for all examples is also available in the Chapter 4 Appendix of the textbook, which provides:
- Fully documented code with extensive comments
- Section-by-section organization
- Copy-and-paste ready examples
- Troubleshooting guidance

## Reproducibility

All code in this repository has been tested and verified to produce results consistent with those reported in the textbook. To ensure reproducibility:

1. Use Stata version 19 or later
2. Download data files from the official data repository
3. Update file paths to match your local directory structure
4. Install all required packages before running code
5. Run code in the order presented in each script

## Citation

If you use this code in your research, please cite:

```
Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques 
(2nd ed.). Springer.
```

## Support

For questions or issues:
- Review the troubleshooting section above
- Check the book's appendix for additional documentation
- Consult the Stata documentation for specific command syntax
- Visit the Statalist forum for community support

## License

This code is provided for educational purposes in conjunction with the textbook *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition). Please refer to the book for terms of use.

---

**Author:** Marvin A. Titus  
**Last Updated:** November 17, 2025  
**Stata Version:** 19.5  
**Book Website:** [Springer](https://www.springer.com/)  
**GitHub Organization:** [higher-ed-policy-analysis-2nd-edition](https://github.com/higher-ed-policy-analysis-2nd-edition)
