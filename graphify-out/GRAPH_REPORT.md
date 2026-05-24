# Graph Report - codex-scheduled-refresh-vintage  (2026-05-24)

## Corpus Check
- 103 files · ~174,473 words
- Verdict: corpus is large enough that graph structure adds value.

## Summary
- 462 nodes · 589 edges · 41 communities detected
- Extraction: 73% EXTRACTED · 25% INFERRED · 2% AMBIGUOUS · INFERRED: 146 edges (avg confidence: 0.83)
- Token cost: 0 input · 0 output

## Community Hubs (Navigation)
- [[_COMMUNITY_Community 0|Community 0]]
- [[_COMMUNITY_Community 1|Community 1]]
- [[_COMMUNITY_Community 2|Community 2]]
- [[_COMMUNITY_Community 3|Community 3]]
- [[_COMMUNITY_Community 4|Community 4]]
- [[_COMMUNITY_Community 5|Community 5]]
- [[_COMMUNITY_Community 6|Community 6]]
- [[_COMMUNITY_Community 7|Community 7]]
- [[_COMMUNITY_Community 8|Community 8]]
- [[_COMMUNITY_Community 9|Community 9]]
- [[_COMMUNITY_Community 10|Community 10]]
- [[_COMMUNITY_Community 11|Community 11]]
- [[_COMMUNITY_Community 12|Community 12]]
- [[_COMMUNITY_Community 13|Community 13]]
- [[_COMMUNITY_Community 14|Community 14]]
- [[_COMMUNITY_Community 15|Community 15]]
- [[_COMMUNITY_Community 16|Community 16]]
- [[_COMMUNITY_Community 17|Community 17]]
- [[_COMMUNITY_Community 18|Community 18]]
- [[_COMMUNITY_Community 19|Community 19]]
- [[_COMMUNITY_Community 20|Community 20]]
- [[_COMMUNITY_Community 21|Community 21]]
- [[_COMMUNITY_Community 22|Community 22]]
- [[_COMMUNITY_Community 24|Community 24]]
- [[_COMMUNITY_Community 25|Community 25]]
- [[_COMMUNITY_Community 26|Community 26]]
- [[_COMMUNITY_Community 27|Community 27]]
- [[_COMMUNITY_Community 28|Community 28]]
- [[_COMMUNITY_Community 29|Community 29]]
- [[_COMMUNITY_Community 30|Community 30]]
- [[_COMMUNITY_Community 31|Community 31]]
- [[_COMMUNITY_Community 32|Community 32]]
- [[_COMMUNITY_Community 33|Community 33]]
- [[_COMMUNITY_Community 34|Community 34]]
- [[_COMMUNITY_Community 35|Community 35]]
- [[_COMMUNITY_Community 36|Community 36]]
- [[_COMMUNITY_Community 37|Community 37]]
- [[_COMMUNITY_Community 38|Community 38]]
- [[_COMMUNITY_Community 39|Community 39]]
- [[_COMMUNITY_Community 40|Community 40]]
- [[_COMMUNITY_Community 41|Community 41]]

## God Nodes (most connected - your core abstractions)
1. `Capital City Median House Prices` - 14 edges
2. `NHHA Rental Stress Trends Over Time Chart` - 14 edges
3. `NHHA Rental Stress by State Chart` - 13 edges
4. `Master Data Pipeline Driver` - 9 edges
5. `Building Approvals Chart` - 9 edges
6. `Housing Serviceability` - 9 edges
7. `Age Group Axis` - 9 edges
8. `Age Group Axis` - 9 edges
9. `Saved Dashboard CSV Data Model` - 8 edges
10. `Affordability Indicator Derivation` - 8 edges

## Surprising Connections (you probably didn't know these)
- `Legacy CPI Rent Series Diagnostic` --references--> `Saved Dashboard CSV Data Model`  [INFERRED]
  _check_cpi.R → README.md
- `Legacy Transform and Plot Pipeline` --semantically_similar_to--> `Shared ggplot and Plotly Theme Helpers`  [INFERRED] [semantically similar]
  app_old.R → R/dashboard_theme.R
- `Empty Project Plan File` --conceptually_related_to--> `Household Affordability Dashboard Project Overview`  [AMBIGUOUS]
  project_plan.md → AGENTS.md
- `SIH Housing Stress and Cost Burden Surface` --references--> `ABS SIH Housing Occupancy and Costs Resources`  [INFERRED]
  R/affordability_module.R → AGENTS.md
- `Legacy CPI Rent Series Diagnostic` --conceptually_related_to--> `ABS SIH Housing Occupancy and Costs Resources`  [INFERRED]
  _check_cpi.R → AGENTS.md

## Hyperedges (group relationships)
- **Dashboard Runtime Architecture** — app_shiny_app_shell, plot_setup_compatibility_entrypoint, data_loader_dashboard_csvs, app_page_module_orchestration, chart_builders_page_plots, dashboard_theme_plot_theme [EXTRACTED 1.00]
- **Methodology and Provenance Stack** — indicator_registry_derived_indicators, methodology_module_page, market_entry_scenarios_calculator, indicator_registry_official_stylised_boundary, readme_methodology_metadata [EXTRACTED 1.00]
- **SIH Affordability Surfaces** — agents_abs_sih_resources, affordability_module_sih_burden_surface, geographic_affordability_sih_alignment, readme_sih_quality_metadata [EXTRACTED 1.00]
- **Canonical Data Refresh Pipeline** — pipeline_05_driver_master_pipeline, pipeline_00_config_shared_pipeline_config, pipeline_01_process_sih_sih_workbook_parser, pipeline_02_fetch_abs_timeseries_abs_live_time_series_fetch, pipeline_02b_fetch_abs_supply_supply_demand_fetch, pipeline_03_fetch_rba_rba_rates_fetch, pipeline_04_derive_indicators_affordability_indicator_derivation, pipeline_06_validate_outputs_fail_fast_validation [EXTRACTED 1.00]
- **SIH Quality Metadata Display Chain** — pipeline_01_process_sih_sih_estimate_quality_metadata, sih_quality_helpers_join_sih_quality, sih_quality_helpers_reliability_annotation, rental_market_module_rental_market_page, rental_market_module_nhha_rental_stress [INFERRED 0.85]
- **Dashboard Chart Rendering Pattern** — precomputed_series_dashboard_series, overview_module_overview_page, price_trends_module_price_trends_page, rental_market_module_rental_market_page, plotly_helpers_dashboard_ggplotly, ui_style_system_policy_components, visual_semantics_economic_colour_semantics [INFERRED 0.85]
- **Housing Dashboard Measurement Validity Problems** — housing_dashboard_full_review_rppi_selector_contamination, housing_dashboard_full_review_insufficient_real_indicator_history, housing_dashboard_full_review_nhha_duplicate_metric_parsing, housing_dashboard_full_review_serviceability_deposit_assumptions, housing_dashboard_full_review_indicator_registry [EXTRACTED 1.00]
- **renv Reproducible Restore Workflow** — faq_snapshot_lockfile, ci_continuous_integration_restore, package_sources_inference, news_repository_strict_restore, activate_project_library [INFERRED 0.85]
- **renv CI and Container Cache Workflow** — ci_github_actions_cache, docker_layer_cache, docker_buildkit_cache_mount, config_cache_enabled, news_cache_only_use [INFERRED 0.85]
- **Housing Affordability Indicator Method Family** — chai_rental_affordability_index, chai_deposit_affordability_index, chai_mortgage_affordability_index, chai_median_household_disposable_income [EXTRACTED 1.00]
- **Credit Constraint Demand Curve Framework** — rba2023_collateral_constraints, rba2023_willingness_to_pay, rba2023_demand_curve_method, rba2023_marginal_buyer, rba2023_heterogeneous_user_cost_model [EXTRACTED 1.00]
- **Dashboard Quality Contract Suite** — test_affordability_module_contract, test_methodology_text_contract, test_plotly_output_id_contract, test_plotly_cache_contract, test_chart_builders_contract, test_dependency_reproducibility_contract [EXTRACTED 1.00]
- **Modular Shiny Page Contracts** — test_overview_module_overview_module_contract, test_price_trends_module_price_trends_contract, test_market_context_module_market_context_contract, test_housing_supply_module_supply_module_contract, test_geographic_affordability_module_geographic_module_contract, test_methodology_module_methodology_module_contract [EXTRACTED 1.00]
- **Pipeline Validation Surface** — test_pipeline_driver_stage_gates_driver_stage_gate_order, test_pipeline_stage_contracts_stage_contract_surface, test_pipeline_outputs_pipeline_output_contract, test_pipeline_outputs_sih_benchmark_validation [EXTRACTED 1.00]
- **Public Methodology Release Surface** — test_methodology_page_methodology_page_content_contract, test_provenance_report_methodology_provenance_report, test_release_checklist_release_checklist_contract, test_public_release_hygiene_public_release_hygiene_contract, test_pipeline_stage_contracts_external_source_manifest [EXTRACTED 1.00]
- **Rental Market Contract Suite** — test_rental_market_module_rental_market_module_contract, test_rental_market_interaction_contracts_rental_market_interaction_contract, test_rental_market_mobile_contracts_rental_market_mobile_layout_contract, test_ui_smoke_contracts_ui_smoke_contract [INFERRED 0.85]
- **SIH Quality Assurance Suite** — test_sih_estimate_quality_sih_estimate_quality_dataset_contract, test_sih_quality_helpers_sih_quality_helpers_contract, test_sih_uncertainty_intervals_sih_uncertainty_interval_contract, test_sih_workbook_benchmarks_sih_workbook_benchmark_contract [INFERRED 0.90]
- **Dashboard UI Contract Suite** — test_responsive_ui_contracts_responsive_ui_contract, test_theme_infrastructure_bslib_theme_infrastructure_contract, test_ui_style_system_public_policy_ui_style_system_contract, test_visual_semantics_visual_semantics_contract, test_ui_smoke_contracts_ui_smoke_contract [INFERRED 0.85]

## Communities

### Community 0 - "Community 0"
Cohesion: 0.08
Nodes (41): Overview Cost Pressure Indicators, Overview Page Module, ABS Normalisation Schema, RBA Table Fetcher, Shared Pipeline Configuration, Write Pipeline CSV Helper, SIH Estimate Quality Metadata, SIH Workbook Parser (+33 more)

### Community 1 - "Community 1"
Cohesion: 0.09
Nodes (40): Affordability Market Entry Surface, Affordability Page Module, SIH Housing Stress and Cost Burden Surface, ABS SIH Housing Occupancy and Costs Resources, Housing Affordability Methodology Concepts, Normalised ABS Long Schema Rule, Household Affordability Dashboard Project Overview, Dashboard Dark and Light Theme System (+32 more)

### Community 2 - "Community 2"
Cohesion: 0.08
Nodes (28): ABS Data API Beta SDMX Service, ABS REST API XML JSON CSV Formats, ANSI Help and Run Links, renv Bootstrap, Bootstrap renv, Bootstrap Download Sources, Profile-Specific renv Paths, Project Library Loading (+20 more)

### Community 3 - "Community 3"
Cohesion: 0.09
Nodes (28): Guide to Housing Affordability Statistics, Change in Housing Affordability Indicators, Deposit Affordability Index, Interest Price Index, Market Entry Affordability, Median Household Disposable Income, Mortgage Affordability Index, Rental Affordability Index (+20 more)

### Community 4 - "Community 4"
Cohesion: 0.1
Nodes (27): Loader Reserved SIH Geographic Files, SIH Geographic Output Contracts, Geographic Affordability Module Contract, SIH Quality Annotations, Housing Supply Approval Legend Parser, Housing Supply Module Contract, Indicator Registry Contract, Market Entry Scenario Methodology Note (+19 more)

### Community 5 - "Community 5"
Cohesion: 0.13
Nodes (22): 15 to 24 Age Group, 15 to 24 Age Group Has Highest Visible Stress Share, Cost/Income 25-30% Band, 25 to 34 Age Group, Cost/Income 30-50% Band, 35 to 44 Age Group, 45 to 54 Age Group, 55 to 64 Age Group (+14 more)

### Community 6 - "Community 6"
Cohesion: 0.13
Nodes (20): 2013 Elevated Rental Stress Cluster, 2019 Mixed State Rental Stress Outcomes, ACT Rental Stress Series, Australia Rental Stress Series, Cross-State Dispersion in Rental Stress, NHHA Rental Stress Measure, NHHA Rental Stress Trends Over Time Chart, No Explicit Threshold Line Visible (+12 more)

### Community 7 - "Community 7"
Cohesion: 0.17
Nodes (19): 2019-20 Reference Period, ACT Rental Stress, ACT Lowest Rental Stress, All States and Territories Above Reference Line, Dashed Average Reference Line Around 45 Percent, NHHA Rental Stress by State Chart, Cross-Jurisdiction Rental Stress Comparison, NHHA Rental Stress Measure (+11 more)

### Community 8 - "Community 8"
Cohesion: 0.15
Nodes (18): Rental Market Interaction Contract, Rental Market Mobile Layout Contract, Rental Market Module Contract, SIH Quality Overlay Contract, Responsive UI Contract, market_entry_serviceability_series, Serviceability Scenario Controls Contract, SIH Estimate Quality Dataset Contract (+10 more)

### Community 9 - "Community 9"
Cohesion: 0.14
Nodes (18): 15 to 24 Age Group, 25 to 34 Age Group, 35 to 44 Age Group, 45 to 54 Age Group, 55 to 64 Age Group, 65 and over Age Group, 65 to 74 Age Group, 75 and over Age Group (+10 more)

### Community 10 - "Community 10"
Cohesion: 0.15
Nodes (15): Adelaide median house price series, Benchmark capital city housing affordability pressure using median prices, Brisbane median house price series, Canberra median house price series, Capital City Median House Prices, Darwin median house price series, Hobart median house price series, Melbourne median house price series (+7 more)

### Community 11 - "Community 11"
Cohesion: 0.15
Nodes (14): Building Approvals, Building Approvals Chart, Dwellings Excluding Houses, Houses, Housing Supply Pipeline Indicator, Mid-2010s Building Approvals Peak, Multi-Series Time Trend, New South Wales (+6 more)

### Community 12 - "Community 12"
Cohesion: 0.22
Nodes (14): 2013 to 2021 Serviceability Below Threshold, 2020 Serviceability Trough Near 24 Percent, 2022 Threshold Crossing, 2023 to 2025 Elevated Housing Stress, Assess Mortgage Or Housing Cost Burden Against Stress Benchmark, Housing Serviceability, Housing Stress Threshold (30%), Overview Serviceability Chart Image (+6 more)

### Community 13 - "Community 13"
Cohesion: 0.24
Nodes (13): Change from Base Period Axis, Change in Affordability Indices Since Base Period Chart, Cross-Indicator Affordability Comparison, Deposit Affordability, Deposit Affordability Approx 160% Above Base Period Latest, Mortgage Affordability, Mortgage Affordability Approx 130% Above Base Period Latest, Percentage Scale from 0% to Above 150% (+5 more)

### Community 14 - "Community 14"
Cohesion: 0.18
Nodes (12): Housing Affordability Dashboard Full Review, Housing Supply Output ID Mismatch, Strict Indicator Registry, Insufficient Real Indicator History, KPI Change Label Misclassification, Market Entry Scenario Measures, NHHA Duplicate Metric Parsing, Official SIH and NHHA Measures (+4 more)

### Community 15 - "Community 15"
Cohesion: 0.18
Nodes (12): Continuous Integration Restore Workflow, GitHub Actions renv Cache, BuildKit Cache Mount for renv, Containerised renv Project, Docker Layer Cache for renv Restore, Docker Multi-Stage Builds, Docker System Dependencies, Snapshot and Lockfile Update FAQ (+4 more)

### Community 16 - "Community 16"
Cohesion: 0.23
Nodes (12): Analytical purpose: compare capital city dwelling price growth as housing affordability pressure input, Brisbane shows a pronounced late-period surge after about 2020, Brisbane dwelling price index series, Dwelling Price Index by Capital City Chart, Y-axis: Index, Melbourne rises earlier but is comparatively flat after about 2021, Melbourne dwelling price index series, Capital city dwelling price indexes generally rise substantially after 2012 (+4 more)

### Community 17 - "Community 17"
Cohesion: 0.23
Nodes (12): 2024 to 2026 Rebound Toward 100, Rental Affordability Trend Monitoring Purpose, Index Baseline Around 100, Rental Affordability Index Chart, Early-2010s Index Peak Around 105, Early-2020s Index Trough Around 93, Red Line Index Series, Mid-2000s Index Trough Around 94 (+4 more)

### Community 18 - "Community 18"
Cohesion: 0.47
Nodes (10): Cash Rate & Mortgage Rates Chart, Interest Rate Transmission to Housing Costs, Investor 3yr Fixed Mortgage Rate, Investor Variable Discounted Mortgage Rate, Mortgage Rate and Cash Rate Spread, Owner-occupied 3yr Fixed Mortgage Rate, Owner-occupied Variable Discounted Mortgage Rate, Percentage Rate Axis (+2 more)

### Community 19 - "Community 19"
Cohesion: 0.22
Nodes (10): Analytical Purpose Construction Cost Inflation Input For Housing Affordability Assessment, CPI New Dwelling Purchase Chart, Index Ticks 70 80 90 100, New Dwelling Purchase Construction Cost Index Series, Index Rises From About 69 To Above 101, CPI New Dwelling Purchase (Construction Cost), Sharp Acceleration Around 2021 To 2022 Followed By Continued Growth, Brief Plateau Near Index 98 To 99 Before Rising Again (+2 more)

### Community 20 - "Community 20"
Cohesion: 0.36
Nodes (8): Affordability Indicators Chart, Affordability Indicators, Mortgage Serviceability Index, Mortgage Serviceability Volatility, Post-2021 Housing Affordability Deterioration, Price-to-Income Ratio, Long-Run Price-to-Income Uptrend, Time Series Comparison of Housing Affordability Metrics

### Community 21 - "Community 21"
Cohesion: 0.29
Nodes (8): 2019-20 Housing Affordability Period, Compare Housing Affordability Stress by Tenure and Age Cohort, Age Groups from 15 to 24 through 75 and Over, Housing Cost-to-Income Ratio Heatmap by Tenure and Age, Housing Cost-to-Income Percentage, Private Renter Cost Burden Remains Material Across Age Groups, Tenure Groups: All Households, All Renters, Owners with Mortgage, Private Renters, Younger Households Show Higher All-Household Cost Burdens

### Community 22 - "Community 22"
Cohesion: 0.36
Nodes (8): Labour Market Context for Household Affordability Analysis, Labour Market Spare Capacity Chart, COVID-era Spike in Labour Spare Capacity, Labour Underutilisation Rate, Percentage Time Series from Early 2000s to Mid 2020s, Recent Labour Market Tightening After 2020 Peak, Underemployment Rate, Unemployment Rate

### Community 24 - "Community 24"
Cohesion: 0.38
Nodes (6): Time Axis from 2000 to 2025, COVID-era Net Migration Collapse, Population Pressure Context for Housing Affordability, Net Overseas Migration (NOM) per Annum, Post-2022 Migration Rebound, Thousands

### Community 25 - "Community 25"
Cohesion: 0.47
Nodes (6): renv documentation figure asset, Hexagonal badge, Leaf-like green symbol, www.rstudio.com text, renv logo.svg, renv wordmark

### Community 26 - "Community 26"
Cohesion: 0.4
Nodes (5): Package Retrieval in CI, Global Package Cache Configuration, Global Package Cache Compared With Packrat, JSON Lockfile Compared With Packrat, Packrat to renv Migration

### Community 27 - "Community 27"
Cohesion: 0.67
Nodes (3): crandb Lookup Configuration, Repository-Aware Restore Strictness, Custom R Package Repositories

### Community 28 - "Community 28"
Cohesion: 0.67
Nodes (3): CRAN Submission Without renv Infrastructure, Package Development with renv, Project Library Isolation

### Community 29 - "Community 29"
Cohesion: 1.0
Nodes (2): Transactional Install Configuration, Rebuild and Restore Package Installation

### Community 30 - "Community 30"
Cohesion: 1.0
Nodes (2): External Libraries Configuration, Bioconductor Package Sources

### Community 31 - "Community 31"
Cohesion: 1.0
Nodes (2): Install Jobs Configuration, Parallel Install and Restore

### Community 32 - "Community 32"
Cohesion: 1.0
Nodes (2): Lockfile Generation in CI, renv Plan Lockfile Resolution

### Community 33 - "Community 33"
Cohesion: 1.0
Nodes (2): Known Vulnerability Check FAQ, renv Vulnerability Querying

### Community 34 - "Community 34"
Cohesion: 1.0
Nodes (2): Credential Stripping in Lockfiles, Package Installation Authentication

### Community 35 - "Community 35"
Cohesion: 1.0
Nodes (2): Package Download Failures FAQ, Package Download Overrides

### Community 36 - "Community 36"
Cohesion: 1.0
Nodes (2): ABI Compatibility, Build-Time Dependencies

### Community 37 - "Community 37"
Cohesion: 1.0
Nodes (1): Dependency Error Handling Configuration

### Community 38 - "Community 38"
Cohesion: 1.0
Nodes (1): Cache-Only renv Use

### Community 39 - "Community 39"
Cohesion: 1.0
Nodes (1): CI Vignette knitr Options

### Community 40 - "Community 40"
Cohesion: 1.0
Nodes (1): Development Dependencies FAQ

### Community 41 - "Community 41"
Cohesion: 1.0
Nodes (1): Package Configure Arguments

## Ambiguous Edges - Review These
- `Household Affordability Dashboard Project Overview` → `Empty Project Plan File`  [AMBIGUOUS]
  project_plan.md · relation: conceptually_related_to
- `External Libraries Configuration` → `Bioconductor Package Sources`  [AMBIGUOUS]
  renv/library/macos/R-4.5/aarch64-apple-darwin20/renv/doc/package-sources.html · relation: conceptually_related_to
- `Deposit Affordability` → `Deposit Affordability Approx 160% Above Base Period Latest`  [AMBIGUOUS]
  plots/overview_afford_change.png · relation: references
- `Mortgage Affordability` → `Mortgage Affordability Approx 130% Above Base Period Latest`  [AMBIGUOUS]
  plots/overview_afford_change.png · relation: references
- `Rent Affordability` → `Rent Affordability Near Base Period Latest`  [AMBIGUOUS]
  plots/overview_afford_change.png · relation: references
- `Housing Serviceability` → `2020 Serviceability Trough Near 24 Percent`  [AMBIGUOUS]
  plots/overview_serviceability.png · relation: references
- `New Dwelling Purchase Construction Cost Index Series` → `Index Rises From About 69 To Above 101`  [AMBIGUOUS]
  plots/price_cpi_construction.png · relation: rationale_for
- `45 to 54 Age Group` → `Highest Mean Weekly Rental Costs Around Ages 35 to 44`  [AMBIGUOUS]
  plots/rental_costs_demo.png · relation: conceptually_related_to
- `Tasmania Rental Stress` → `Western Australia Rental Stress`  [AMBIGUOUS]
  plots/rental_stress_state.png · relation: semantically_similar_to
- `NHHA Rental Stress Measure` → `No Explicit Threshold Line Visible`  [AMBIGUOUS]
  plots/rental_stress_trend.png · relation: conceptually_related_to
- `Australia Rental Stress Series` → `2019 Mixed State Rental Stress Outcomes`  [AMBIGUOUS]
  plots/rental_stress_trend.png · relation: conceptually_related_to
- `Cost/Income 30-50% Band` → `15 to 24 Age Group Has Highest Visible Stress Share`  [AMBIGUOUS]
  plots/stress_chart.png · relation: contributes_to
- `Cost/Income Above 50% Band` → `15 to 24 Age Group Has Highest Visible Stress Share`  [AMBIGUOUS]
  plots/stress_chart.png · relation: contributes_to

## Knowledge Gaps
- **151 isolated node(s):** `Legacy Claude Project Instructions`, `Pipeline Refresh and Output Gates`, `Dashboard Dark and Light Theme System`, `Policy Source Notes and SIH Sampling Warning Helpers`, `Empty Project Plan File` (+146 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **Thin community `Community 29`** (2 nodes): `Transactional Install Configuration`, `Rebuild and Restore Package Installation`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 30`** (2 nodes): `External Libraries Configuration`, `Bioconductor Package Sources`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 31`** (2 nodes): `Install Jobs Configuration`, `Parallel Install and Restore`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 32`** (2 nodes): `Lockfile Generation in CI`, `renv Plan Lockfile Resolution`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 33`** (2 nodes): `Known Vulnerability Check FAQ`, `renv Vulnerability Querying`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 34`** (2 nodes): `Credential Stripping in Lockfiles`, `Package Installation Authentication`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 35`** (2 nodes): `Package Download Failures FAQ`, `Package Download Overrides`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 36`** (2 nodes): `ABI Compatibility`, `Build-Time Dependencies`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 37`** (1 nodes): `Dependency Error Handling Configuration`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 38`** (1 nodes): `Cache-Only renv Use`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 39`** (1 nodes): `CI Vignette knitr Options`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 40`** (1 nodes): `Development Dependencies FAQ`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 41`** (1 nodes): `Package Configure Arguments`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **What is the exact relationship between `Household Affordability Dashboard Project Overview` and `Empty Project Plan File`?**
  _Edge tagged AMBIGUOUS (relation: conceptually_related_to) - confidence is low._
- **What is the exact relationship between `External Libraries Configuration` and `Bioconductor Package Sources`?**
  _Edge tagged AMBIGUOUS (relation: conceptually_related_to) - confidence is low._
- **What is the exact relationship between `Deposit Affordability` and `Deposit Affordability Approx 160% Above Base Period Latest`?**
  _Edge tagged AMBIGUOUS (relation: references) - confidence is low._
- **What is the exact relationship between `Mortgage Affordability` and `Mortgage Affordability Approx 130% Above Base Period Latest`?**
  _Edge tagged AMBIGUOUS (relation: references) - confidence is low._
- **What is the exact relationship between `Rent Affordability` and `Rent Affordability Near Base Period Latest`?**
  _Edge tagged AMBIGUOUS (relation: references) - confidence is low._
- **What is the exact relationship between `Housing Serviceability` and `2020 Serviceability Trough Near 24 Percent`?**
  _Edge tagged AMBIGUOUS (relation: references) - confidence is low._
- **What is the exact relationship between `New Dwelling Purchase Construction Cost Index Series` and `Index Rises From About 69 To Above 101`?**
  _Edge tagged AMBIGUOUS (relation: rationale_for) - confidence is low._