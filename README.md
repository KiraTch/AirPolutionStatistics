# Global Air Pollution and Climate Analysis

This project analyzes the relationship between air pollution, temperature changes, mortality rates, and forest coverage across different countries, with a particular focus on the world's largest nations and G20 countries.

## Features

- Analysis of Air Quality Index (AQI) values across the 30 largest countries
- Interactive visualization of death percentages related to air pollution in G20 countries (1990-2019)
- Correlation analysis between temperature changes and mortality rates
- Forest coverage analysis and its relationship with air pollution
- Interactive Shiny dashboards for data exploration

## Interactive Visualizations

The project includes several interactive visualizations:
- Bar charts of AQI values in the largest 30 countries
- Time series of death percentages across G20 countries
- Correlation plots between temperature changes and mortality rates
- Forest coverage analysis across major nations
- Interactive Shiny dashboard allowing users to select the number of countries to display

## Dependencies

- tidyverse
- httr2
- plotly
- shiny
- shinydashboard
- dplyr
- readr

## Data Sources

This analysis uses data from several reputable sources:

- [IMF Climate Change Data](https://climatedata.imf.org/pages/climatechange-data)
- [Our World in Data - Air Pollution](https://ourworldindata.org/air-pollution)
- [Our World in Data - Air Pollution Mortality](https://ourworldindata.org/air-pollution#air-pollution-is-one-of-the-world-s-leading-risk-factors-for-death)

## Key Findings

1. The analysis reveals patterns in air pollution levels across major nations
2. Shows temporal trends in pollution-related mortality rates
3. Demonstrates correlations between temperature changes and death percentages
4. Illustrates the relationship between forest coverage and air quality

## Interactive Features

The Shiny dashboard allows users to:
- Select the number of countries to display (5-30)
- View dynamic visualizations of air pollution data
- Explore forest coverage statistics
- Compare different environmental metrics across nations

## Usage

To run the analysis:

1. Install the required R packages
2. Load the datasets from the specified sources
3. Run the R script to generate visualizations
4. Launch the Shiny dashboard for interactive exploration

