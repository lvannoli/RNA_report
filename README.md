# RNA REPORT

## Overview
The RNA Report project is designed to generate comprehensive reports and visualizations for the Rapid Needs Assessment (RNA) in Syria. The project processes data once the analysis is performed, and generates HTML reports with interactive plots and tables.

## Features
- The script `RNA_report.Rmd` generates tables for each survey question. These are divided into three levels of aggregation: overall (no aggregation), district level, and subdistrict level.
- The script `RNA_overall_graphs.Rmd` generates graphs and saves them in an HTML file for each survey question at the overall aggregation level.

# Requirements
- R (version 4.0 or higher)
- R packages:
  - `readxl`
  - `dplyr`
  - `stringr`
  - `ggplot2`
  - `knitr`
  - `kableExtra`

## Installation
1. Install R from [CRAN](https://cran.r-project.org/).
2. Install the required R packages:
   ```r
   install.packages(c("readxl", "dplyr", "stringr", "ggplot2", "knitr", "kableExtra"))
   ```

## Usage

1. Place the input data files in the inputs/data/ directory.
2. Place the logo images in the inputs/logos/ directory.
3. Open the RNA_report.Rmd file in RStudio or your preferred R IDE.
4. Knit the RMarkdown file to generate the HTML report:
5. Open the RNA_overall_graphs.Rmd file in RStudio or your preferred R IDE.
6. Knit the RMarkdown file to generate the HTML file with graphs:
7. The generated reports will be saved in the outputs/ directory.

## Customization
- You can customize the report template by editing the RNA_report.Rmd and RNA_overall_graphs.Rmd files.
- Update the CSS styles in updated_style.css to change the appearance of the report.