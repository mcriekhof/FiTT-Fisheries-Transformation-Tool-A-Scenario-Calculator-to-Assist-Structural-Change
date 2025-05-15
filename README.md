# FiTT – Fisheries Transformation Tool

**A Scenario-Based Simulation Model for Small-Scale Fisheries in the Western Baltic Sea**

The scenario tool has been developed for the German Western Baltic Sea fisheries. It enables a quantitative assessment of changes in the ecological, economic and governmental boundary conditions and their effects, especially on fishery income and structure of the fisheries sector. In total, 4 fisheries business types have been identified based multiple categories (e.g. organisation type). The different scenarios, building on potential fish stock development (positiv or stagnant), focus on the year 2035.
The scenario tool allows to adapt parameters to demonstrate the impact from changing parameters on the economic viability of the fishery. The settings can be made for just one fishery business type or more.

## Access and Citation

The online version of the Fisheries Transformation Tool (FiTT) can be accessed at:

[https://ceos.shinyapps.io/ScenarioToolBalticSeaFishery/](https://ceos.shinyapps.io/ScenarioToolBalticSeaFishery/)

This repository contains the complete source code for the tool, including user interface components, simulation parameters, and all underlying calculations.

For methodological background, assumptions, and a detailed analysis, please refer to the following publication:

**Marie-Catherine Riekhof\*, Tanja Hartig-Thiemann, Christian Möllmann, Lotta Siebert, Rudi Voss & Heike Schwermer**  
*Shaping structural change – A scenario tool to guide transformation in the fisheries sector and beyond*  
(*Forthcoming*)

## Repository Structure

```
.
├── app.R                     # Main R Shiny application (UI and server logic)
├── default_values.R          # Default parameters (e.g. quotas, costs, prices)
├── libraries_functions.R     # Helper functions and library dependencies
├── WWW/  
└── README.md                 
```

## Installation and Execution

### Requirements

Ensure R (≥ 4.0.0) is installed. The following packages must also be available:

```r
install.packages(c(
  "shiny", "ggplot2", "shinythemes", "tibble", "tidyr", "ggpubr",
  "readxl", "scales", "shinyWidgets", "dipsaus", "openxlsx", "plotly"
))
```

## Customization

### Adjusting Assumptions

Default simulation parameters are stored in `default_values.R`, including:

- Number of enterprises per fishery type  
- Quotas for herring, cod, and other species  
- Cost structures (fixed and variable)  
- Fish prices by species and fishery type  
- Assumptions for stock development scenarios and landings  
- Target monthly net income per enterprise  
- Depreciation and national landing share estimates  

All computational logic (e.g., income tax model, sliders, conditional UI panels, and income calculations) is implemented in `libraries_functions.R`.


## Acknowledgements

Developed by the [Center for Ocean and Society](https://oceanandsociety.org/) 

---

© 2023 Center for Ocean and Society – All rights reserved.
