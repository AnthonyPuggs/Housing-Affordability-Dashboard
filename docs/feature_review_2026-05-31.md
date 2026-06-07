# Housing Dashboard Feature Review

Date: 2026-05-31

Scope: current modular Shiny dashboard (`app.R` and page modules under `R/`), saved dashboard CSVs under `data/`, local SIH workbooks and methodology references under `resources/`, graphify output, existing quality reviews and static/runtime checks. Legacy `app_old.R` was treated only as historical context.

## Executive Verdict

The current dashboard has a coherent public-policy dashboard structure and has addressed several issues identified in earlier reviews. The present source uses explicit indicator source selection, full CPI history is available, NHHA duplicate keys are gone, KPI change labels are frequency-aware, the Housing Supply construction-cost output is wired, the Overview Plotly click event is registered, and the tablet/mobile responsive fixes are present in source.

The main feature risk is now product emphasis rather than basic viability. The dashboard still leads with a modelled market-entry score, price KPIs and price charts, while the strongest affordability evidence is in the official SIH/NHHA household burden surfaces. For a housing affordability dashboard aimed at policy-literate users, the Overview should put official stress/burden evidence beside the stylised market-entry score, not below it or on separate deep-dive pages only.

No current P0 feature should be removed immediately. The highest-priority changes are P1 adjustments and additions:

- Add an official SIH/NHHA burden snapshot to the Overview.
- Add a Recent Buyers / First-Home Buyer entry feature from the parsed SIH File 9 data.
- Correct the "National Median Price" KPI label because the server reads `national_mean_price`.
- Convert the "Renter entry" calculator preset into a true renter pathway or rename it.
- Keep price, supply, labour, migration and construction-cost features as context, not as headline affordability measures.
- Continue improving income denominators toward household disposable income where possible, because WPI and AWE are imperfect proxies for household affordability capacity.

## Evidence Base

- Graphify: `graphify-out/GRAPH_REPORT.md` identifies the main hubs as capital city median prices, NHHA rental stress charts, master pipeline, building approvals, housing serviceability and affordability indicator derivation. It also groups current review risks under "Housing Dashboard Measurement Validity Problems".
- Current app architecture: `app.R` sources modular page files and builds an eight-page `page_navbar`; the modules cover Overview, Price Trends, Affordability, Geographic Affordability, Market Context, Housing Supply, Rental Market and Methodology.
- Prior reviews: `quality_reports/housing_dashboard_full_review.md` and `docs/ui_review_2026-05-21.md` were cross-checked against current source. Several earlier findings are now fixed in source.
- Methodology resources: the NZ CHAI methodology frames market-entry affordability around renting, saving a deposit and servicing a mortgage, each relative to median household disposable income. The NZ living-cost index background supports household-group-specific expenditure and owner-occupied interest/payment treatments. The RBA credit-constraint paper warns that financing constraints affect prices through marginal buyers, not average households. The ABS housing affordability guide PDF had limited extractable text, so the SIH workbook titles and parsed data were used as the stronger local ABS evidence.
- SIH workbooks consulted: Files 4, 5, 8, 9 and 13. These cover housing costs as a proportion of income, stress-band ranges, lower-income state measures, recent home buyers and NHHA lower-income renter stress.
- Data checks: `data/affordability_indices.csv` has 623 data rows across 11 indicators; `data/sih_recent_buyers_2020.csv` has 982 rows but is not currently exposed in the UI; `data/sih_nhha_rental_stress.csv` has 546 rows and zero duplicate long-format key rows in the current saved data.
- Current source checks: `R/dashboard_formatting.R` now supports explicit `period_label` and `change_type`; `R/housing_supply_module.R` has `output$supply_cpi_construction`; `R/overview_module.R` calls `plotly::event_register("plotly_click")`; `app.R` includes the tablet score-panel breakpoint at `max-width: 1024px`.
- Render attempt: the local app sourced successfully and served HTML from `http://127.0.0.1:3971` with HTTP 200 and the expected page tabs/output IDs. Full visual browser automation could not be completed in this session because headless Chrome failed under the sandbox and Computer Use access to Firefox/Chrome was denied.
- Context7: Shiny documentation supports `validate()`/`need()` and `bindCache()` for robust and performant outputs; bslib documentation supports cards, value boxes, navsets, sidebars and dark-mode theming; Plotly documentation supports explicit event and hover configuration plus responsive chart configuration.

## Judgement Scale

| Judgement | Meaning |
|---|---|
| Keep | Economically justified and well placed. |
| Keep but adjust | Useful, but needs labelling, source, layout, interaction or methodology changes. |
| Move to context | Useful explanatory variable, but not a direct affordability outcome. |
| Deprioritise/remove | Weak, duplicative or likely to mislead if kept as a major feature. |
| Add | Missing feature that should be implemented. |

| Priority | Meaning |
|---|---|
| P0 | Misleading, analytically unsafe or broken core affordability feature. |
| P1 | Required feature missing, unclear or materially under-specified. |
| P2 | Useful feature needing refinement, relocation or clearer framing. |
| P3 | Polish, layout, labelling or optional enhancement. |

## Feature Matrix

| Page | Feature | Variables/Sources | Economic role | Current judgement | Priority | Recommended action | Evidence |
|---|---|---|---|---|---|---|---|
| Overview | National Market-Entry Affordability Score | `affordability_indices.csv`; mortgage serviceability, rental entry, deposit barrier | Stylised national market-entry composite | Keep but adjust | P1 | Keep the score, but pair it with official SIH/NHHA burden evidence on the first screen and keep "not household stress" language visible. | `R/national_affordability_score.R` uses 40/35/25 weights; `R/overview_module.R` labels it as relative, not household stress. |
| Overview | Score click-through trend and component contribution panel | `national_affordability_score_ts`, component scores | Explains the score over time and by pathway | Keep | P2 | Keep. The current event registration and contribution wording address prior interaction warnings. | `R/overview_module.R` registers `plotly_click` and shows score, weight and contribution points. |
| Overview | National "Median" Price KPI | `national_mean_price`, city `"National Avg"` | Housing price pressure context | Keep but adjust | P1 | Rename to "National Mean Dwelling Price" or change the source to a true median. | `R/overview_module.R` title says "National Median Price" but server reads `national_mean_price`. |
| Overview | Sydney Median Price KPI | `median_house_prices`, Sydney | Local high-cost market context | Move to context | P2 | Keep if used as a recognisable benchmark, but do not over-emphasise one city on the national Overview. Consider replacing with highest/lowest capital city or user-selected city. | `R/overview_module.R` hard-codes Sydney as a KPI. |
| Overview | Modelled Serviceability KPI | `serviceability_ts` | Stylised mortgage repayment burden | Keep but adjust | P1 | Keep as market-entry burden, but show it beside official mortgage-owner stress where possible. | Module labels it as a stylised mortgage scenario. |
| Overview | Rental Affordability KPI | Rental Affordability Index | Rental cost pressure proxy | Keep but adjust | P1 | Keep, but add an official lower-income renter stress KPI beside it because CPI rents are not a new-tenancy burden measure. | `R/overview_module.R` uses derived rent cost pressure; SIH/NHHA data are available elsewhere. |
| Overview | Capital City Median House Prices chart | Capital city price series | Price pressure context | Move to context | P2 | Keep below the core affordability summary. Price charts explain pressure but are not affordability outcomes by themselves. | Graphify marks it as a god node; Price Trends already covers the detailed price role. |
| Overview | Affordability Indices chart | Price-to-income, mortgage serviceability, rental cost pressure | Market-entry and cost-pressure comparison | Keep but adjust | P1 | Keep mortgage and rental cost pressure; de-emphasise price-to-income as a diagnostic because it overlaps strongly with deposit burden. | `docs/national_affordability_score_methodology.md` records high redundancy between deposit gap and price-to-income. |
| Global navigation | Data vintage badge and dark/light mode | `data_vintage.csv`, bslib theme | Trust, provenance and accessibility | Keep | P3 | Keep. Ensure data-vintage text remains concise on mobile. | App header includes `data_vintage_badge()` and `input_dark_mode()`. |
| Price Trends | Dwelling Price Index by capital city | ABS dwelling price indexes | Market price context | Move to context | P2 | Keep under Price Trends; do not treat as household affordability. Add clearer distinction between index and median/mean price. | `R/price_trends_module.R` note correctly says price indexes are not affordability or borrowing capacity. |
| Price Trends | Dwelling type and city controls | City, Total/Houses/Units, date, transform | Decomposition of price pressure | Keep | P3 | Keep. Useful for separating detached and attached dwelling cycles. | Source module supports city multi-select and transform controls. |
| Price Trends | Rent CPI by capital city | ABS CPI rents | Rent price context | Move to context | P1 | Keep, but label as CPI rent stock/price-index evidence and add a backlog item for advertised/new-tenancy rent evidence. | NZ CHAI uses new-tenancy rents; current module notes CPI rents are not lower-income burden. |
| Affordability | Indicator selector | Price-to-income, mortgage serviceability, rental index, deposit gap, modelled serviceability | User-controlled market-entry comparison | Keep but adjust | P2 | Keep, but group choices into "core pathway", "diagnostic" and "stylised scenario" so users do not treat all indicators as equal. | `R/affordability_module.R` currently presents all choices together. |
| Affordability | Price-to-Income Ratio | RPPI/WPI | Broad ownership pressure proxy | Deprioritise/remove | P1 | Remove from default selection or move to diagnostics because it duplicates deposit and serviceability channels and omits interest rates. | Current score methodology keeps it out of the headline score. |
| Affordability | Mortgage Serviceability Index | RPPI, WPI, RBA mortgage rate | Ownership monthly servicing pressure | Keep but adjust | P1 | Keep as a core market-entry channel. Continue caveating WPI and price proxies; prefer household disposable income when available. | Indicator registry marks it as derived, not official. |
| Affordability | Rental Affordability Index | CPI rents/WPI | Rental entry cost pressure proxy | Keep but adjust | P1 | Keep as a timely proxy, but always pair with NHHA lower-income renter stress and note that CPI rents may lag new-lease stress. | Methodology page already includes this caveat. |
| Affordability | Deposit Gap (Years) | RPPI-scaled base price, AWE, fixed savings rate | Ownership upfront barrier | Keep but adjust | P1 | Keep, but make savings-rate and income assumptions more prominent and add Recent Buyer empirical context. | Pipeline uses a fixed 15 per cent savings rate and SIH-base price. |
| Affordability | Modelled Serviceability chart controls | Assessment buffer, deposit, loan term | Scenario sensitivity | Keep but adjust | P2 | Keep. Add presets that distinguish first-home buyer, upgrader and high-LVR borrower rather than mixing renter language into mortgage calculations. | Current controls are useful but stylised. |
| Affordability | Calculator | Price, income, rate, deposit, term, expenses, debt, savings rate | Household scenario tool | Keep but adjust | P1 | Keep, but rename "Renter entry" preset or add a true rental-entry pathway using rent, bond, upfront moving costs and income. | `R/market_entry_scenarios.R` calculates mortgage/deposit outcomes for all presets. |
| Affordability | Housing Cost Stress Bands | SIH File 5 stress-band ranges | Official household burden distribution | Keep | P1 | Promote as a core affordability surface. Keep lower-income bottom 40 per cent toggle prominent. | `R/affordability_module.R` uses official SIH burden bands and reliability notes. |
| Affordability | Cost Burden heatmap | SIH File 4 cost-to-income ratios | Official tenure/demographic burden | Keep | P1 | Keep as a primary diagnostic of who is burdened. Consider adding a first-screen summary of highest-burden groups. | Graphify community highlights tenure and age burden patterns. |
| Affordability | Distributional Stress Explorer | NHHA, lower-income state and demographic SIH measures | Distributional official stress comparison | Keep but adjust | P1 | Keep and move closer to the top of Affordability. Add clearer defaults for renters, mortgage owners and lower-income households. | Current module labels it as official SIH/NHHA, not modelled market entry. |
| Geographic Affordability | State SIH Cost-to-Income Trend | SIH File 12 | Geography-aligned official burden trend | Keep | P1 | Keep. This is one of the most defensible geography features because numerator and denominator are aligned. | `R/geographic_affordability_module.R` explicitly prevents proxy geography mixing. |
| Geographic Affordability | Latest State Comparison | SIH File 12 | Cross-state official burden comparison | Keep | P2 | Keep; add sort by latest value and optional national reference line if not already clear. | Module uses state series and latest year. |
| Geographic Affordability | Lower-Income State Burden | SIH File 8 | 30/40-rule-adjacent state stress evidence | Keep | P1 | Keep and promote. This is core affordability evidence for vulnerable households. | Parsed lower-income state file has 567 rows across stress and cost metrics. |
| Geographic Affordability | Capital/rest-of-state comparison | SIH File 11 | Within-state urban/regional household burden comparison | Keep | P2 | Keep; add a concise explanation that this is SIH geography, not capital-city RPPI market entry. | Module note already states this boundary. |
| Market Context | Interest rates on residential mortgages | RBA mortgage rates and cash rate | Financing-condition context | Move to context | P2 | Keep, but keep outside headline score. Rates should explain serviceability pressure, not be a separate affordability outcome. | RBA paper supports careful treatment of financing conditions and marginal buyers. |
| Market Context | Labour spare capacity | ABS unemployment, underemployment, underutilisation | Income-security context | Move to context | P2 | Keep as context. Avoid causal wording about affordability unless linked to income and tenure groups. | Module labels KPI changes as percentage-point changes. |
| Market Context | Population demand | ABS net overseas migration | Demand-pressure context | Move to context | P2 | Keep as context, not as a direct affordability metric. Add household formation/supply lag caveats. | Population demand chart uses NOM annualised flow. |
| Market Context | Unemployment, NOM, participation KPIs | ABS labour/population | Macro context | Keep but adjust | P2 | Retain unemployment and NOM; consider replacing participation with household income or real disposable income once available. | Participation is useful macro context but less direct than household income. |
| Housing Supply | NSW/VIC approval KPIs | ABS building approvals | Supply pipeline context | Move to context | P2 | Keep, but expand beyond NSW/VIC or make state selection drive KPIs. Current hard-coded states limit national relevance. | `R/housing_supply_module.R` hard-codes NSW and Victoria KPI titles. |
| Housing Supply | Building Approvals chart | ABS approvals by state, type, sector | Medium-run supply pipeline | Move to context | P2 | Keep. Add completion/dwelling stock if available before making supply adequacy claims. | Module note correctly says approvals are not completed dwellings. |
| Housing Supply | CPI New Dwelling Purchase chart/KPI | ABS CPI new dwelling purchase | Construction-cost context | Move to context | P2 | Keep as construction cost pressure, but not as household affordability. Consider moving KPI below approvals. | Current output is wired and labelled as not household burden. |
| Housing Supply | Houses share KPI | Approvals by type | Dwelling-mix context | Keep but adjust | P3 | Keep only if connected to dwelling mix and household needs; otherwise de-emphasise. | KPI computes houses as share of total approvals. |
| Rental Market | NHHA Rental Stress by State | SIH/NHHA File 13 | Official lower-income renter stress | Keep | P1 | Keep and promote. This is a core rental affordability feature. | Current data have zero duplicate keys and proportion/count metrics are separated. |
| Rental Market | NHHA Rental Stress Trends | SIH/NHHA File 13 | Official stress over time and geography | Keep | P1 | Keep; verify hover alignment in browser QA because earlier review saw ggplotly matrix warnings. | Current module retains heatmap/tile hover text. |
| Rental Market | Rental Affordability Index | CPI rents/WPI | Timely rental cost-pressure proxy | Keep but adjust | P2 | Keep below official NHHA stress; label as proxy and not new-lease burden. | Module note says higher = less affordable. |
| Rental Market | Weekly Rental Costs by Demographics | SIH rental costs | Survey rental-cost distribution | Keep | P1 | Keep. Consider adding rent-to-income ratios alongside dollars to avoid nominal cost-only interpretation. | Current chart uses SIH rental costs by age, family type and income quintile. |
| Rental Market | State/year/cost breakdown controls | NHHA survey year, states, cost breakdown | User navigation | Keep but adjust | P3 | Keep; default to Australia plus states, and check mobile sidebar defaults because prior UI review found mobile density issues. | Current sidebar is open by default. |
| Methodology | Official/stylised boundary cards | SIH/NHHA, derived indicators, score | Interpretation guardrail | Keep | P1 | Keep and make this distinction visible on each page, not only Methodology. | `R/methodology_module.R` separates official measures from market-entry indexes. |
| Methodology | Score diagnostics | Contribution and sensitivity tables | Composite transparency | Keep | P1 | Keep. Earlier vector formatting error appears fixed by vectorised formatters; preserve tests. | `fmt_index()` is now vector-safe through `replace_missing_labels()`. |
| Methodology | Indicator registry table | `R/indicator_registry.R` | Formula/source transparency | Keep | P1 | Keep. Add a compact public version and a technical full table to reduce page density. | Registry documents source, formula, caveat and minimum rows. |
| Methodology | Quality and release confidence | `sih_estimate_quality.csv`, release checklist | Trust and uncertainty | Keep | P1 | Keep and expose reliability markers wherever users see SIH estimates. | SIH quality chain is a graphify hyperedge. |
| Methodology | Provenance download | `methodology_provenance_report()` | Reproducible public documentation | Keep | P2 | Keep. Add date/version in the filename and ensure it mirrors current score weights. | Module has a download handler. |
| Missing | Overview official burden snapshot | SIH Files 4, 5, 8, 13 | Official affordability headline | Add | P1 | Add top-line SIH/NHHA metrics: lower-income renter stress, lower-income households over 30 per cent, owner-with-mortgage burden, highest-stress group. | Current Overview has stylised score and price KPIs but no official stress KPI. |
| Missing | Recent Buyers / First-Home Buyer entry page or tab | SIH File 9, `sih_recent_buyers_2020.csv` | Empirical purchase-entry conditions | Add | P1 | Add after normalising labels. Use purchase price, deposit, loan and first-home/changeover splits to ground stylised deposit metrics. | Parsed data exist: 982 rows, proportion/dollars/count. |
| Missing | Household disposable income denominator | SIH, Census, ATO/HES-style model, or documented proxy | Income capacity | Add | P1 | Investigate an Australian household disposable income series or modelled proxy. Keep WPI/AWE caveats until replaced. | NZ CHAI methodology uses median household disposable income. |
| Missing | Advertised/new-tenancy rent evidence | Rental bond/new tenancy or credible advertised-rent source | Rental entry pressure | Add | P1 | Add if a defensible source is available. Keep CPI rents as fallback/context. | NZ CHAI uses new-tenancy rents; current dashboard uses CPI rents. |
| Missing | Residual-income/living-cost lens | Household expenditure or living-cost index inputs | After-housing-cost welfare capacity | Add | P2 | Add as future extension only after data design. Start with methodology note and candidate data audit. | Living-cost methodology supports household-group-specific expenditure patterns. |
| Missing | Housing utilisation/overcrowding | SIH File 7 | Adequacy, not just cost | Add | P2 | Add a later "suitability" tab if the dashboard expands beyond cost affordability. Do not mix it into market-entry score. | Resource directory includes housing utilisation workbook. |

## Page-by-Page Review

### Overview

The Overview is visually and conceptually clearer than the inherited macro dashboard. The National Market-Entry Affordability Score is defensible as a stylised market-entry summary because it combines mortgage servicing, rental entry pressure and deposit barriers, and the current UI repeatedly states that it is not an official ABS/NHHA statistic or lender assessment.

The page should still be rebalanced. A housing affordability dashboard should not make users scroll or navigate away before seeing official household burden evidence. Add a compact official-burden strip beside the score, using lower-income renter stress, lower-income households over 30 per cent, owner-with-mortgage burden and the highest-stress demographic group. This would let the score explain market-entry conditions while the SIH/NHHA metrics anchor observed household stress.

The "National Median Price" KPI needs correction because the code reads `national_mean_price` and `city == "National Avg"`. This is a material labelling issue, not just polish.

### Price Trends

The Price Trends page makes sense as a contextual page. Dwelling prices and rent CPI are important upstream pressures, but neither is a household affordability measure on its own. The current module already says this for both dwelling prices and CPI rents, which should be preserved.

The main adjustment is prominence. Price Trends should remain context, not the conceptual centre of the dashboard. Rent CPI should be paired in the backlog with a stronger new-tenancy or advertised-rent source because CPI rents can understate entry stress when new leases move faster than the measured stock of rents.

### Affordability

This is the core analytical page, but it mixes concept classes too evenly. Official SIH stress bands and cost-burden heatmaps are stronger affordability evidence than price-to-income and other broad macro ratios. The page should make the conceptual hierarchy explicit:

1. Official household burden and stress.
2. Stylised market-entry pathways.
3. Diagnostic cost-pressure indexes.

Mortgage serviceability, rental cost pressure and deposit barriers should remain. Price-to-income should move out of default views because it overlaps with deposit and serviceability channels while omitting mortgage rates. The calculator is useful but the "Renter entry" preset is misleading unless it becomes a true rental pathway; the current function is mortgage/deposit based.

### Geographic Affordability

This page is one of the best-aligned pages methodologically. It uses SIH measures where housing costs and household denominators are measured within the same geography, and its source notes correctly warn users not to read the page as modelled market-entry indexes.

The page should be kept and strengthened with sorting, clearer national references and potentially a summary callout for the latest lower-income renter and owner-with-mortgage state burdens. Avoid adding state-level market-entry indexes unless the income, price and rent inputs are geography-consistent.

### Market Context

Interest rates, labour spare capacity and population demand all make sense as context. They should not be promoted to direct affordability metrics because they are upstream, endogenous and often ambiguous in sign once income, prices and supply responses are considered.

The participation-rate KPI is the weakest current feature in this group. It is macroeconomically relevant, but household disposable income, wage distribution or income-security measures would be more directly relevant when available.

### Housing Supply

Building approvals and construction-cost CPI are useful context for medium-run supply and construction pressure. The page correctly states that approvals are not completions and that construction-cost CPI is not a household burden measure.

The current hard-coded NSW and Victoria approval KPIs should be adjusted. Either make the KPIs respond to the state selector or use national approvals/default selected jurisdictions. Add completions, dwelling stock or vacancy context before making claims about supply adequacy.

### Rental Market

The Rental Market page should be retained and promoted. NHHA lower-income renter stress by state and over time is one of the dashboard's strongest official affordability surfaces. Weekly rental costs by demographics are also valuable, but they should eventually be complemented by rent-to-income ratios so users do not mistake nominal costs for affordability.

The current source shows the mobile chart grammar has already been improved relative to the 2026-05-21 UI review: the state stress chart is now rendered as a horizontal bar chart. Browser QA should still re-check hover alignment and mobile readability when a browser automation path is available.

### Methodology

The Methodology page is required and should remain public-facing. It carries the official/stylised boundary, score diagnostics, release confidence and provenance download. It is currently dense, which is acceptable for technical users but should be layered: a compact public summary first, then full tables and diagnostics.

The methodology page should not be the only place where caveats appear. The most important caveats should remain attached to the charts themselves, especially for CPI rents, AWE/WPI income proxies, modelled serviceability and SIH sampling error.

## Prioritised Backlog

### Core Economic Validity Fixes

| Priority | Item | Required change |
|---|---|---|
| P1 | Rebalance Overview around official burden evidence | Add official SIH/NHHA burden KPIs beside the National Market-Entry Affordability Score. |
| P1 | Correct national price KPI semantics | Rename the KPI to national mean dwelling price or source a true median. |
| P1 | Distinguish price pressure from affordability | Move price-only features and price-to-income out of headline/default affordability framing. |
| P1 | Improve income denominator | Audit Australian household disposable income options; until then, keep WPI/AWE caveats visible. |
| P1 | Fix "Renter entry" calculator concept | Build a true renter-entry pathway or rename the preset to a mortgage/deposit scenario. |
| P1 | Pair timely rental proxy with official renter stress | Show CPI rent pressure with NHHA lower-income renter stress whenever used as an affordability signal. |

### Required Feature Additions

| Priority | Item | Required change |
|---|---|---|
| P1 | Recent Buyers / First-Home Buyer entry feature | Build a tab or page from `sih_recent_buyers_2020.csv`, after normalising labels and units. |
| P1 | Official Overview stress strip | Surface lower-income renter stress, lower-income state burden and tenure burden metrics on the first screen. |
| P1 | New-tenancy or advertised rent evidence | Identify a defensible Australian source; add only if source quality and coverage are adequate. |
| P2 | Residual-income/living-cost framework | Add a methodology section and candidate data audit before implementing calculations. |
| P2 | Housing utilisation/adequacy | Add SIH File 7 overcrowding/utilisation only as a separate suitability lens, not as a cost-pressure metric. |
| P2 | User-selected jurisdiction summary | Let selected states/cities drive KPIs on price, supply and geographic pages where relevant. |

### Feature Removals Or De-Emphasis

| Priority | Item | Required change |
|---|---|---|
| P1 | Price-to-income default prominence | Remove from default chart selection or label as diagnostic. |
| P2 | Participation-rate KPI | Replace with a more direct income or income-security measure when available. |
| P2 | Construction-cost KPI prominence | Keep the chart, but move the KPI below more direct supply indicators. |
| P2 | Sydney-only Overview KPI | Replace or make selectable so the national dashboard does not overweight one city. |
| P3 | Houses share KPI | Keep only if connected to household suitability or dwelling-mix interpretation. |

### Implementation Adjustments

| Priority | Item | Required change |
|---|---|---|
| P1 | Preserve explicit indicator source mapping | Keep `get_series_exact()` style selection and tests so RPPI contamination cannot recur. |
| P1 | Add feature-level metadata | Each chart should expose measure class: official survey, derived index, stylised scenario or context. |
| P1 | Normalise recent-buyer data before UI | Convert File 9 labels into clear metric names, buyer type, dwelling type, first-home/changeover and stat type. |
| P2 | Add page-level source notes consistently | Make ABS catalogue/workbook/source notes visible near charts, not only in Methodology. |
| P2 | Browser QA path | Add a repeatable browser smoke path when Browser/Chrome automation is available. |
| P2 | Keep Plotly caching discipline | Preserve `bindCache()` on expensive renderers and ensure cache keys follow user inputs and theme state. |

### UI, Responsiveness And Interaction

| Priority | Item | Required change |
|---|---|---|
| P1 | Re-check mobile Rental Market and Geographic charts | Current source has mobile fixes, but full visual QA was unavailable in this session. |
| P2 | Layer Methodology page density | Add compact summary sections or tabs before full tables. |
| P2 | Improve Affordability page hierarchy | Put official burden tabs before broad derived index charts. |
| P2 | Make sidebar defaults mobile-friendly | Check pages with open sidebars, especially Rental Market and Affordability. |
| P3 | Keep dark/light mode consistency | Retain bslib-native theming and continue checking chart contrast. |

## Do Not Overbuild

These are interesting but should not become headline features without stronger data or methodology:

- Credit-constraint or borrowing-capacity structural models. RBA evidence supports caution because financing conditions affect prices through marginal buyers and heterogeneous demand, not average households.
- CGE or macro-structural modelling inside the dashboard. This would be a separate research model, not a dashboard feature, unless there is a clear calibrated policy experiment.
- Real wage growth, real mortgage rates and real house-price growth as headline affordability features. They are useful context but duplicate or confound direct burden ratios.
- Migration, unemployment, participation or approvals as causal explanations of affordability. Keep them as context unless the dashboard estimates a defensible model.
- Price-to-rent or investor yield metrics as core affordability indicators. They may be useful for tenure-arbitrage or valuation analysis but do not directly measure household burden.
- Full lender assessment claims. The calculator should stay stylised unless it incorporates lender-specific policy, tax, expenses, credit history, borrower composition and regulatory buffers.
- A single "affordability pass/fail" score. The current score is acceptable only as a historical-relative market-entry monitoring index with components and caveats.

## Recommended Implementation Order

1. Add the Overview official-burden strip and correct the national price KPI label.
2. Add Recent Buyers / First-Home Buyer entry analysis from SIH File 9.
3. Rework the Affordability page order so official stress and burden views lead, followed by market-entry scenarios and derived diagnostics.
4. Replace or rename the renter-entry calculator preset.
5. Add a data-source audit for household disposable income and new-tenancy rents.
6. Re-run browser visual QA on desktop, tablet and mobile once an approved browser automation path is available.
7. Convert accepted recommendations into implementation tickets with tests per page/module.

## Verification Notes

Checks completed while preparing this review:

- `graphify-out/GRAPH_REPORT.md` read before raw file/code inspection.
- `graphify query "For the Housing Affordability Dashboard feature review, list the user-facing pages, charts, indicators, and known quality issues that should be assessed."`
- `Rscript -e "source('plot_setup.R'); source('app.R'); cat('APP_SOURCE_OK\n')"` completed with `APP_SOURCE_OK`.
- Local Shiny server started on `http://127.0.0.1:3971`.
- `curl` request to the local app returned HTTP 200 and 172,419 bytes of HTML with the expected eight navigation tabs and key output IDs.
- Full browser visual QA was attempted but not completed because headless Chrome failed in the sandbox and Computer Use access to Firefox/Chrome was denied.
- `Rscript tests/test_app_output_ids.R` passed: 24 Plotly outputs checked across app and modules.
- The following plan-requested checks passed when run sequentially with `Rscript --vanilla`:
  - `tests/test_methodology_module.R`
  - `tests/test_affordability_module.R`
  - `tests/test_rental_market_module.R`
  - `tests/test_geographic_affordability_module.R`
  - `tests/test_housing_supply_module.R`
  - `tests/test_price_trends_module.R`
  - `tests/test_overview_module.R`
  - `tests/test_ui_smoke_contracts.R`
  - `tests/test_responsive_ui_contracts.R`
- Environment caveat: a plain `Rscript` batch loop hung on `tests/test_methodology_module.R` after prior parallel Rscript calls, consistent with renv/session contention in this sandbox. The same test passed with `Rscript --vanilla`.
