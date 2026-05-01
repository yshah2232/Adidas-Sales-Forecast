# Adidas Sales Forecast

## Project Overview

This project performs **time series analysis and forecasting** of Adidas quarterly revenue from 2000 to 2017. Using historical quarterly sales data, the project applies multiple advanced forecasting techniques to identify patterns, trends, and seasonal effects, ultimately comparing different methods to determine the most accurate forecast for future sales.

**Key Objective**: Predict Adidas quarterly revenue using statistical time series methods and compare model performance.

## Dataset Description

- **File**: `adidas_revenue1.csv`
- **Time Period**: Q1 2000 - Q1 2017 (72 quarterly observations)
- **Variables**: Quarterly revenue figures ($millions)
- **Data Split**:
  - Training: Q1 2000 - Q4 2013 (56 quarters)
  - Testing: Q1 2014 - Q1 2017 (16 quarters)

## Key Findings

- **Seasonal Pattern**: Strong quarterly seasonality with Q3 consistently outperforming other quarters
- **Trend**: Clear upward trend in Adidas revenue over the 17-year period
- **Autocorrelation**: Strong correlation with lagged data, indicating time series dependency
- **Recommendation**: Multiplicative and additive Holt-Winters models best capture the seasonal and trend components

## Dependencies & Requirements

### Software
- **R** (version 3.x or higher)

### R Libraries
```r
install.packages("fpp")    # Forecasting: Principles and Practice
install.packages("fpp2")   # Updated version of fpp
```

To install all dependencies, run:
```r
packages <- c("fpp", "fpp2")
install.packages(packages)
```

## Installation & Setup

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yshah2232/Adidas-Sales-Forecast.git
   cd Adidas-Sales-Forecast
   ```

2. **Open R or RStudio** and set your working directory:
   ```r
   setwd("path/to/Adidas-Sales-Forecast")
   ```

3. **Install dependencies** (if not already installed):
   ```r
   library(fpp)
   library(fpp2)
   ```

4. **Ensure the CSV file is in the working directory**:
   - Verify `adidas_revenue1.csv` is present
   - *Note*: The R script references a hardcoded path (`C:/Users/Yash/Downloads/...`).
   - **TODO**: Update the script to use `read.csv("adidas_revenue1.csv")` for portability

## Usage

### Running the Analysis

1. **Execute the main script**:
   ```r
   source("Adidas Sales Forecast.R")
   ```

2. **What the script performs**:
   - Loads and converts raw CSV data to time series format
   - Generates exploratory plots (raw sales, moving averages, seasonal patterns)
   - Computes autocorrelation analysis
   - Fits multiple forecasting models
   - Visualizes model comparisons

3. **Output**:
   - Console output with time series data and statistics
   - Multiple plots showing sales trends, seasonality, and forecasts
   - Model fitted values and forecast predictions

## Analysis Steps Explained

### 1. Data Loading & Transformation
```r
data <- read.csv("adidas_revenue1.csv")
data <- ts(data, start=c(2000,1), end=c(2017,1), frequency=4)
y <- data[,2]  # Extract revenue column
```
Converts CSV data into R's time series object with quarterly frequency.

### 2. Exploratory Data Analysis (EDA)
- **Raw plot**: Visualizes revenue over time with 9-quarter moving average overlay
- **Seasonal plot**: Shows patterns across quarters and years
- **Autocorrelation (ACF)**: Identifies temporal dependencies
- **Conclusion**: Strong seasonality and trend detected

### 3. Train-Test Split
```r
train_data <- window(y, start=c(2000,1), end=c(2013,4))
test_data <- window(y, start=2014)
```
Data split allows model validation against held-out test period.

### 4. Forecasting Models Applied

#### Simple Exponential Smoothing (SES)
- Tests three smoothing parameters: α = 0.2, 0.6, 0.89
- Best for data with no trend or seasonal patterns
- **Finding**: Lower alpha (0.2) performs better for conservative forecasts

#### Holt's Linear Trend Method
- Captures level and trend components
- Generates 15-quarter ahead forecasts
- **Best for**: Data with trend but minimal seasonality

#### Holt-Winters Seasonal Methods
- **Additive Model**: For constant seasonal variations
- **Multiplicative Model**: For seasonal variations proportional to level
- **Best for**: Data with both trend and seasonality (Adidas data)
- These models provide the most comprehensive forecasts

### 5. Model Comparison
Visual comparison plots overlaying all methods show that Holt-Winters (both variants) provides the best fit for Adidas' seasonal and trending sales pattern.

## Key Results & Interpretations

| Model | Strengths | Limitations |
|-------|-----------|-------------|
| **Simple Exponential Smoothing** | Fast, interpretable | Ignores trend and seasonality |
| **Holt's Linear Trend** | Captures trend | Doesn't handle seasonality well |
| **Holt-Winters Additive** | Handles trend + seasonality | Assumes fixed seasonal swings |
| **Holt-Winters Multiplicative** | Handles trend + proportional seasonality | Most complex; requires more data |

**Recommendation**: The **Holt-Winters multiplicative model** best suits Adidas data due to:
- Increasing trend requiring multiplicative seasonal adjustment
- Quarterly seasonality that scales with revenue level
- Strong historical pattern matching

## Files Generated

- `Adidas_Sales_Forecast.R.pdf` - Compiled output/report of the analysis

## Future Enhancements

- [ ] Implement ARIMA/SARIMA models for comparison
- [ ] Add forecast accuracy metrics (RMSE, MAE, MAPE)
- [ ] Include prediction intervals and confidence bands
- [ ] Extend forecast horizon beyond current data
- [ ] Create interactive visualizations with ggplot2
- [ ] Implement cross-validation for robust model evaluation
- [ ] Fix hardcoded file paths for portability
- [ ] Add more recent data (2018 onwards) if available

## Technical Notes

- **Frequency**: Quarterly data (4 observations per year)
- **Time span**: 72 quarters (18 years)
- **R Version Compatibility**: Tested with R 3.x and fpp/fpp2 packages
