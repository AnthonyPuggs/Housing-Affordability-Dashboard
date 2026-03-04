# Australian Macroeconomic Dashboard (Shiny + readabs)

This project provides a stylish and interactive Australian macro dashboard built in **R** with:

- `shiny` for the dashboard app
- `ggplot2` for publication-quality time-series graphics
- `plotly` for interactivity (zoom, hover, pan)
- `readabs` for live ABS data pulls

## Run

1. Install required packages:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "purrr", "stringr", "scales", "plotly", "readabs"))
```

2. Start the app:

```r
shiny::runApp("/Users/anthonyp/Documents/New project")
```

## Included macro indicators

- Nominal GDP and Real GDP
- CPI level and CPI inflation (YoY)
- Nominal and Real household consumption
- Employment, unemployment rate, participation rate
- Wage indicator (AWOTE)
- Nominal cash rate (when available via ABS macro indicators or a supplied series ID)
- Real cash rate (derived as nominal rate minus CPI YoY inflation)

You can also add custom ABS series IDs from the sidebar.
