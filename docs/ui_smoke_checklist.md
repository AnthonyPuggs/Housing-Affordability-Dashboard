# UI Smoke Checklist

Use this checklist after UI, module, chart, theme or interaction changes. The static contract test guards wiring, but this manual pass checks the live Shiny bindings in the Codex in-app browser.

## Start The App

From the repository root:

```bash
Rscript -e "shiny::runApp('.', host = '127.0.0.1', port = 3971, launch.browser = FALSE)"
```

Open the Codex in-app browser at:

```text
http://127.0.0.1:3971/
```

Keep the browser console visible. The pass condition for every step is that charts render, controls remain responsive and the console shows no Shiny binding errors.

## Desktop Pass

1. Overview
   - Confirm the four KPI value boxes render.
   - Confirm the median price chart and affordability chart render.
   - Change the date range and the nominal/index transform.
   - Expected: the median price chart redraws and the right-side city labels remain visible.

2. Price Trends
   - Open Dwelling Price Index and Rent CPI tabs.
   - Change capital-city selections, dwelling type, transform, rent CPI data type and date ranges.
   - Expected: both Plotly charts redraw without blank panels.

3. Affordability
   - Open Indices, Calculator, Housing Stress and Cost Burden tabs.
   - Change selected indicators, date range, serviceability deposit, loan term and assessment buffer.
   - Change calculator price, income, rate, deposit, term, savings rate, expenses and other debt.
   - Expected: the serviceability chart and calculator outputs update, with the 30% stress reference still visible.

4. Geographic Affordability
   - Change state, tenure, metric and capital/rest-of-state selections.
   - Expected: all four SIH geography-aligned charts redraw.

5. Market Context
   - Change the date range.
   - Expected: mortgage rates, labour market and population demand charts redraw.

6. Housing Supply
   - Change date range, states, building type and sector.
   - Expected: Building Approvals updates to the selected state/type/sector combination, and CPI New Dwelling Purchase remains bound to the Housing Supply date range.

7. Rental Market
   - Change Survey Year (NHHA), States/Territories and Rental Costs By.
   - Expected: Rental Market year/state/breakdown controls work, NHHA state bars update, and NHHA heatmap tiles show values.

8. Methodology
   - Confirm the derived indicator table renders.
   - Confirm the Download Methodology Summary button is visible.

## Theme And Mobile Pass

1. Toggle dark/light mode from Overview and again after visiting an extracted module page.
   - Expected: Plotly charts remain mounted and readable.

2. Resize to a narrow mobile viewport.
   - Open the navbar, select a different page, then confirm the mobile navbar collapses.
   - Open Rental Market and confirm chart cards are full-width and not visibly clipped.

3. Final console check.
   - Expected: no Shiny binding errors, missing output errors or JavaScript errors related to `main_nav`, `theme_mode`, Plotly or Bootstrap collapse.

## Static Guard

Run the static smoke contract before the browser pass:

```bash
Rscript tests/test_ui_smoke_contracts.R
```

This tranche intentionally uses no new browser-testing dependencies. The committed guard is a base-R wiring contract, while this checklist is the release smoke pass for live browser behaviour.
