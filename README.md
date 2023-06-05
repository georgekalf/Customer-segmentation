<p align="center">
  <img src="too-broad-customer-segmentation.jpeg" width="900" height="500">
</p>

# Customer-segmentation

# Description:

The customer segmentation analysis in this GitHub repository is performed using R programming. The goal is to predict the classification of a given dataset. The code leverages various libraries and techniques to preprocess the data, visualize key variables, and generate insights.

# Key Steps:

1. Data Loading: The customer data is loaded into the R environment from a CSV file ('customer-personality.csv').
2. Data Exploration: Initial exploration of the dataset is conducted, including checking variable types, identifying duplicates, and handling missing values.
3. Data Preprocessing: Certain data preprocessing steps are implemented, such as dropping unnecessary columns (e.g., 'ID'), replacing variable names, converting 'year_birth' to 'age', and grouping customers by age.
4. Visualization: Several visualizations are created to gain insights into the dataset. These include box plots to examine the distribution of various variables like age, income, recency, and purchase patterns of different product categories (wines, fruits, meat, fish, etc.). Correlation matrices, density plots, dot plots, lollipop plots, and treemaps are also generated to understand the relationships between different variables.
5. Outlier Handling: Outliers are identified and removed from the 'Income' and 'Age' variables to ensure data integrity.
6. Data Conversion: Factor values in the dataset are converted to their appropriate types, while numerical values are normalized for further analysis.
7. Correlation Analysis: The correlation matrix is computed to explore the relationships between variables, and a correlation plot is generated using pie charts.
8. Advanced Visualizations: Additional visualizations are created, such as density plots to analyze age distribution, proportion plots to show customer distribution by age, lollipop plots to compare education levels and income, and treemaps to visualize income distribution by marital status.
9. Bar Plots: Bar plots are generated to examine the amount spent on various products (wines, fruits, meat, fish, sweets, gold) based on customer education levels.
10. Age Group Analysis: A separate analysis is performed to study the number of purchases (web, catalog, deals, store) by different age groups. Polar bar plots are created to represent the data.

# Key Findings:

* The dataset contains customer information related to demographics, income, and purchase behavior.
* Age, income, recency, and purchase patterns show variations among customers.
* Some outliers were identified and removed from the 'Income' and 'Age' variables.
* Correlation analysis reveals the relationships between different variables.
* Visualization provides insights into customer segments based on age, education, and product preferences.
* The analysis helps identify patterns and trends in customer behavior and preferences.
