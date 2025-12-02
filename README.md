# AssociationExplorer2

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/AssociationExplorer2)](https://CRAN.R-project.org/package=AssociationExplorer2)
[![R-CMD-check](https://github.com/AntoineSoetewey/AssociationExplorer2/actions/workflows/rhub.yaml/badge.svg)](https://github.com/AntoineSoetewey/AssociationExplorer2/actions)
<!-- badges: end -->

**AssociationExplorer2** is an R package that provides a Shiny application for exploring statistical associations within multivariate datasets.

The app offers interactive tools for examining relationships between variables, including:

- correlation networks  
- bivariate visualizations (numeric–numeric, numeric–categorical, categorical–categorical)  
- summary tables describing variable distributions  

The application supports **optional survey weights** and **range-based filters** for association strengths, making it suitable for exploring complex or survey-based datasets.

---

## Installation

### From CRAN

```r
install.packages("AssociationExplorer2")
```

### Development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("AntoineSoetewey/AssociationExplorer2")
```

---

## Launching the Shiny Application

You can launch the Shiny application using the following command:

```r
library(AssociationExplorer2)
run_associationexplorer()
```

This opens the interactive Shiny interface in your default web browser.

---

## Features

- Correlation networks

Visualize associations between variables using weighted network diagrams.
Supports numeric–numeric, numeric–categorical, and categorical–categorical associations.

- Bivariate visualizations

Generate scatter plots, mean plots, and contingency tables depending on variable types.

- Survey weights

Users may optionally specify a survey weight variable.
Weighted statistics and associations are computed where applicable.

- Range-based association filtering

Instead of setting a single cutoff threshold, users can filter associations based on minimum and maximum ranges.

- Data upload interface

Users can load their own datasets in common formats (CSV, Excel, etc.).
A small demonstration dataset is included with the package.

---

## Exemple

```r
library(AssociationExplorer2)

# Launch the application
run_associationexplorer()
```

Upload your dataset through the interface, select the variables of interest, adjust the thresholds or weights, and explore the resulting association structures.

---

## Included Data

The package includes a small demonstration dataset suitable for illustrating the app’s key functionalities.
Users can upload CSV or Excel files through the interface to analyze their own data.

---

## Reporting Issues

If you encounter a bug or would like to request a feature, please open an issue:

https://github.com/AntoineSoetewey/AssociationExplorer2/issues

---

## License

This package is released under the MIT license.
See the LICENSE file for details.

---

## Citation

If you use AssociationExplorer2 in your work, please cite the associated paper:

Soetewey, A., Heuchenne, C., Claes, A., & Descampe, A. (2025).
AssociationExplorer: A user-friendly Shiny application for exploring statistical associations.
Available at SSRN: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5637359

You may also cite the R package itself. A complete citation for both the package and the paper can be obtained via:

```r
citation("AssociationExplorer2")
```

