# Comprehensive Credit Card Debt Analysis: MLR & SARIMA Forecasting

This repository contains a full-cycle data analytics project focused on predicting and forecasting credit card debt levels. The study utilizes a dual-methodology approach, combining **Multiple Linear Regression (MLR)** for causal factor analysis and **SARIMA** for time-series trend forecasting.

## 🚀 Key Project Highlights
* **Predictive Accuracy:** Achieved an **Adjusted R-squared of 0.689** in the final regression model.
* **Forecasting Precision:** Delivered 12-month projections with **94.7% prediction interval coverage** using seasonal time-series modeling.
* **Statistical Rigor:** Fully validated all **Gauss-Markov assumptions**, including normality, homoscedasticity, and independence of residuals.
* **Optimization:** Utilized **Lasso/Ridge (L1/L2) Regularization** and Stepwise selection to mitigate overfitting and multicollinearity.

## 🛠️ Tech Stack & Tools
* **Language:** R (Programming Language)
* **Libraries:** `tidyverse`, `forecast`, `ggplot2`, `olsrr`, `car`, `tseries`, `gridExtra`
* **Methodologies:** Ordinary Least Squares (OLS), SARIMA, Winsorization, Cross-Validation.

## 📊 Data Insights & Visualizations

### 1. Model Validation (Predicted vs. Actual)
The model shows a strong linear relationship between the predictors (Income, Credit Limit, etc.) and actual debt levels.
![Predicted vs Actual](Model_Evaluation_Plots.pdf) *Refer to Model_Evaluation_Plots.pdf in the repository.*

### 2. Exploratory Data Analysis (EDA)
Rigorous data cleaning was performed, including **1st/99th percentile Winsorization** to handle outliers and ensure normal distributions across key features.
![EDA Plots](Exploratory_Data_Analysis.pdf) *Refer to Exploratory_Data_Analysis.pdf.*

## 📂 Repository Structure
* `Credit_Analysis_Main.R`: The complete R script containing data cleaning, modeling, and diagnostics.
* `Technical_Report.pdf`: A comprehensive professional report detailing the methodology, results, and business implications.
* `Model_Evaluation_Plots.pdf`: Visualization of residuals, fitted values, and predictive accuracy.
* `Exploratory_Data_Analysis.pdf`: Distribution plots and descriptive statistics.

## 📈 Conclusion
The analysis successfully identifies income and credit limits as primary determinants of debt. By integrating these insights with SARIMA forecasting, this project provides a robust framework for financial risk management and portfolio optimization.

---
**Author:** Dattathreya Chintalapudi  
**Course:** MSc in Data Analytics (National College of Ireland)
