# ProjetIAExam

This project contains various R scripts for data analysis, classification, regression, and visualization. It is part of an exam or practical study on machine learning approaches and dimensionality reduction applied to a dataset.

## Directory Content

- **LICENSE**: License file defining the terms of use for the project.

- **README.md**: Documentation file explaining the project's structure and purpose.

- **classificaiton.R**:
  - Implements various classification models: k-NN, Naive Bayes, SVM, MClust, and Decision Tree.
  - Includes data cleaning steps, standardization, and creation of confusion matrices for performance evaluation.

- **data_clean_augmented.R**:
  - Performs initial data cleaning (removal of outliers, duplicates, and NAs).
  - Creates derived variables (e.g., `TotalAmount`, `TransactionSizeCategory`) to enrich the dataset.
  - Exports a cleaned and enriched dataset as a CSV file.

- **kmeans_pca_variable_explanation.R**:
  - Combines Principal Component Analysis (PCA) with clustering (K-Means).
  - Generates cluster visualizations and analyzes the contributions of key variables.
  - Provides descriptive statistics for identified clusters.

- **memory_opt_kmeans.txt**:
  - Notes on memory optimization techniques for handling large datasets with PCA and K-Means.
  - Includes approaches to reduce dataset size and limit computational overhead.

- **pca_plot_done.R**:
  - Generates visualizations to explore variable contributions and correlations in PCA.
  - Compares PCA approaches with original variables and derived variables.

- **pca_variable_explanation.R**:
  - Compares variable contributions and correlations across two PCA configurations (original and derived).
  - Provides in-depth analysis of temporal patterns (by hour and day).

- **reducer.R**:
  - Contains a function to reduce dataset size while preserving data structure.
  - Enables data sampling for faster analysis.

- **regression.R**:
  - Implements regression models to analyze relationships between numerical variables (e.g., `Quantity` and `TotalAmount`).
  - Generates visualizations such as scatterplots, histograms, and Q-Q plots to explore data distribution and check normality assumptions.

## Installation and Requirements

### Requirements
- **R** (version 4.0 or higher)
- Required R packages:
  - `class`
  - `e1071`
  - `mclust`
  - `tree`
  - `caret`
  - `FactoMineR`
  - `factoextra`
  - `dplyr`
  - `ggplot2`
  - `lubridate`
  - `tidyr`

### Installation
1. Clone this private repository (ensure you have access):
   ```bash
   git clone git@github.com:Andrei-Cos/ProjetIAExam.git
   ```

2. Open the R scripts in RStudio or a compatible editor.

3. Execute the scripts in the desired order of analysis.

## Usage

1. **Data Preparation**:
   - Run `data_clean_augmented.R` to prepare the dataset.

2. **Exploratory Analyses**:
   - Use `pca_plot_done.R` or `kmeans_pca_variable_explanation.R` to explore data structure.

3. **Classification**:
   - Run `classificaiton.R` to train and evaluate various classification models.

4. **Dimensionality Reduction**:
   - `reducer.R` includes dimensionality reduction approaches to simplify analysis.

5. **Regression**:
   - Apply `regression.R` to perform predictions based on regression models.

## Results
- Classification results include error rates and confusion matrices.
- PCA and K-Means model results are explained in associated files.
- All results can be exported and visualized using the provided scripts.

## Contribute
If you wish to contribute to this project:
1. Fork the project.
2. Create a branch for your modifications:
   ```bash
   git checkout -b feature/your_feature_name
   ```
3. Make your modifications and commit them.
4. Submit a pull request.

## Author
- **Andrei-Cos**

## License
This project is licensed under [LICENSE] - see the `LICENSE` file for details.

