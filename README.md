# Econometrics: Wage Determinants Analysis

An econometric analysis project developed using R and R Shiny to investigate factors associated with wage levels through descriptive statistics, correlation analysis, and Ordinary Least Squares (OLS) regression.

## Project Overview

This project examines the relationship between wages and several explanatory variables, including education, work experience, job tenure, and gender. An interactive R Shiny dashboard is developed to facilitate data exploration, regression analysis, statistical testing, and model diagnostics.

## Variables

| Variable | Description |
|---|---|
| `wage` | Wage level (dependent variable) |
| `educ` | Years of education |
| `exper` | Work experience |
| `tenure` | Job tenure |
| `female` | Gender indicator |

## Methodology

- Descriptive Statistics
- Correlation Analysis
- Ordinary Least Squares (OLS) Regression
- Shapiro-Wilk Normality Test
- Variance Inflation Factor (VIF)
- Breusch-Pagan Test
- Durbin-Watson Test
- Ramsey RESET Test
- Residual Diagnostic Analysis

## Dashboard Features

1. Dataset Preview
2. Descriptive Statistics
3. Correlation Matrix and Visualization
4. OLS Regression Results
5. Classical Assumption Tests
6. Residual Diagnostic Plots
7. Academic Regression Table
8. Final Model Diagnostic Summary

## Technologies

- R Programming
- R Shiny
- bslib
- dplyr
- psych
- car
- lmtest
- corrplot
- stargazer
- DT

## How to Run

1. Install R and RStudio.
2. Clone or download this repository.
3. Open the project directory in RStudio.
4. Install the required packages if they are not already installed.
5. Run `app.R`.
6. Upload an Excel dataset (`.xlsx`) containing the required variables.
7. Click **Proses Analisis** to generate the analytical results.

## Project Scope

This project focuses on applied econometric analysis, regression modeling, statistical inference, and interactive data visualization.

---

**Course:** Econometrics  
**Programming Language:** R  
**Application Framework:** R Shiny
