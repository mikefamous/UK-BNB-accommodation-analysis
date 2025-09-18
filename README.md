# UK-BNB-accommodation-analysis
Quarterly UK data (2012–2015) on households in BnB temporary accommodation.
# BnB Accommodation Analysis

**Tech stack:** R, tidyverse, ggplot2 (update with exact packages)

## Business Problem
Quantify and forecast quarterly BnB usage by families (nationwide and by local authority) to inform capacity planning, procurement, and prevention strategies.

## Data
- Source: (public link or description)
- Rows/Columns: (fill in)
- Key features: (list)

## Methodology
- EDA highlights
- Feature engineering
- Models/Techniques used (e.g., SVM, ARIMA, LP)
- Validation approach

## Results
- Key metrics (e.g., MAE, accuracy, cost savings)
- Business impact in one or two sentences

## How to Run
```r
# In R
# install.packages(c("tidyverse","caret","forecast","lpSolve"))
rmarkdown::render("bnb_analysis.Rmd")
```

## Repository Structure
```
bnb-accommodation-analysis/
├── bnb_analysis.Rmd
├── data/            # (optional) place small, shareable data here or use a ./data/.gitkeep
├── scripts/         # (optional) if you split R code into scripts
├── visuals/         # (optional) figures/plots for README
└── README.md
```
---
title: "BnB Accommodation Analysis"
output: html_document
date: "2025-04-17"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Load Libraries
```{r libraries}
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(VIM)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forecast)
```

# Data Import & Initial Inspection
```{r data-import}
data <- read_csv("/Users/enomaenakhigbe/Downloads/families-with-children-in-bnb-qtrly-to-dec2015-xls_0 (2).csv")
head(data)
str(data)
summary(data)
typeof(data)
colSums(is.na(data))
```

# Visualize Missing Data
```{r missing-plot}
aggr(data, numbers = TRUE, sortVars = TRUE, cex.axis = .7, gap = 3)
```

# Data Cleaning & Transformation
```{r data-cleaning}
data_cleaning <- data[!apply(is.na(data), 1, all), ]
colSums(is.na(data_cleaning))

colnames(data_cleaning)[3:ncol(data_cleaning)] <- c(
  "Dec_2012", "Mar_2013", "Jun_2013", "Sep_2013", "Dec_2013",
  "Mar_2014", "Jun_2014", "Sep_2014", "Dec_2014",
  "Mar_2015", "Jun_2015", "Sep_2015", "Dec_2015"
)

data_longing <- pivot_longer(
  data_cleaning,
  cols = 3:ncol(data_cleaning),
  names_to = "Quarter",
  values_to = "Households"
)

duplicated_rows <- data_cleaning[duplicated(data_cleaning), ]
nrow(duplicated_rows)

data_longing$Quarter <- parse_date_time(data_longing$Quarter, orders = "b_Y")
data_longing$`Local Authority` <- as.factor(data_longing$`Local Authority`)
data_longing$`ONS Code` <- as.factor(data_longing$`ONS Code`)
data_longing$Households <- as.numeric(data_longing$Households)
sum(is.na(data_longing$Households))
```

# Summary Statistics
```{r summary-stats}
summary_stats <- data_longing %>%
  group_by(Quarter) %>%
  summarise(
    mean_households = mean(Households, na.rm = TRUE),
    median_households = median(Households, na.rm = TRUE),
    sd_households = sd(Households, na.rm = TRUE)
  )
print(summary_stats)
```

# Visualizations

## Mean Households Over Time
```{r mean-households-plot}
ggplot(summary_stats, aes(x = Quarter, y = mean_households)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Mean Number of Households in BnB Accommodation by Quarter",
    x = "Quarter",
    y = "Mean Households"
  ) +
  theme_minimal()
```

## Time Series of Households
```{r time-series-plot}
ggplot(data_longing %>% filter(!is.na(Households)), aes(x = Quarter, y = Households)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Average BnB Use Over Time", x = "Quarter", y = "Mean Number of Households")
```

## Histogram by Authority
```{r histogram-authority}
ggplot(data_longing %>% filter(!is.na(Households)), aes(x = Households)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ `Local Authority`, scales = "free_y") +
  labs(title = "Distribution of BnB Use by Authority")
```

## Boxplots by Local Authority
```{r boxplot-authority}
ggplot(data_longing %>% filter(!is.na(Households)), aes(x = `Local Authority`, y = Households)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Boxplot of BnB Use by Local Authority")

ggplot(data_longing %>% filter(!is.na(Households)), 
       aes(x = `Local Authority`, y = Households)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot of BnB Use by Local Authority", 
       x = "Local Authority", y = "Households") +
  theme_minimal()
```

# ANOVA Analysis
```{r anova}
anova_result <- aov(Households ~ `Local Authority`, data = data_longing)
summary(anova_result)
```

# Time-Series Analysis (ARIMA)
```{r arima}
national_ts <- data_longing %>%
  group_by(Quarter) %>%
  summarise(total_households = sum(Households, na.rm = TRUE)) %>%
  arrange(Quarter)

ts_data <- ts(national_ts$total_households, start = c(2012, 4), frequency = 4)
fit_arima <- auto.arima(ts_data)
summary(fit_arima)
```

## ARIMA Model Plot
```{r arima-plot}
autoplot(ts_data) +
  autolayer(fitted(fit_arima), series = "Fitted", color = "blue") +
  labs(title = "ARIMA Model Fit", y = "Households in BnB", x = "Time") +
  theme_minimal()
```

## Forecast Plot
```{r forecast-plot}
forecast_arima <- forecast(fit_arima, h = 4)
autoplot(forecast_arima) +
  labs(title = "Forecast of BnB Use", y = "Households", x = "Quarter") +
  theme_minimal()
```

## Residual Check
```{r residuals}
checkresiduals(fit_arima)
```
