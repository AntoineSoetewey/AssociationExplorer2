# AssociationExplorer2 0.1.0

## Initial release

- First published version of the **AssociationExplorer2** package.
- Provides an interactive Shiny application for exploring statistical associations.
- Includes:
  - Correlation networks for numeric–numeric, numeric–categorical, and categorical–categorical associations.
  - Bivariate visualizations (scatter plots, mean plots, contingency tables).
  - Support for **survey weights** in applicable computations [see https://github.com/AntoineSoetewey/AssociationExplorer2/commit/72725331a2b461f534355eb4b65051e088efb471].
  - **Range-based filtering** of association strengths (minimum and maximum thresholds) [see https://github.com/AntoineSoetewey/AssociationExplorer2/commit/ed89fc139b4d3c2e818a25a7a98c9d48b720f386].
  - Data upload interface supporting CSV and Excel files.
- Includes a small demonstration dataset for testing and illustration.
- Provides the `run_associationexplorer()` function to launch the application.
