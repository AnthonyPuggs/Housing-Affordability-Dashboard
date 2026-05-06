# National Housing Affordability Score Review Memo

Date: 2026-05-06

## Review Conclusion

The implemented National Housing Affordability Score is economically defensible as a descriptive, historical-relative market-entry monitoring index. It should not be interpreted as an absolute affordability threshold, a welfare-stress measure, an ABS/NHHA statistic, or a lender assessment.

The score is useful because it combines three direct market-entry constraints: mortgage serviceability, rental entry pressure and deposit barriers. A score near 0 or 100 means low or high relative to the score window, not that housing is universally unaffordable or affordable for all household types.

## Main Critiques

- The ownership channel is partly double-counted because mortgage serviceability and deposit barrier both embed dwelling prices. The overlap is acceptable for v1 because they represent different entry constraints: monthly servicing capacity versus upfront deposit accumulation.
- The component bars previously looked like weighted contributions but displayed only raw component scores. The dashboard should show both score and weighted points.
- The chart was visually useful but static. Users need to select historical dates so the headline score and component bars can explain what drove the score at different points in the cycle.
- The score needs visible diagnostics: sample window, component correlations, input missingness, latest contribution points and sensitivity variants.

## Current V1 Diagnostics

- Common score sample: 27 complete dates from October 2012 to October 2025.
- Latest headline score: 22.6 out of 100.
- Latest weighted points: mortgage serviceability 4.6, rental entry 17.5 and deposit barrier 0.5.
- Mortgage serviceability and deposit barrier burden inputs have a correlation of about 0.895, confirming material but not perfect ownership-channel overlap.
- Latest sensitivity variants range from 7.8 when rental entry is omitted to 29.97 when mortgage serviceability is omitted; the geometric default is 12.3. This means the low latest score is robust, but the exact level is sensitive to whether the weakest ownership components are penalised more heavily.

## Required Interpretation Guardrails

- Keep fixed v1 weights at 40 per cent mortgage serviceability, 35 per cent rental entry and 25 per cent deposit barrier.
- Keep price-to-income, real wage growth, real mortgage rate, unemployment, supply and population variables outside the headline score.
- Treat sensitivity variants as diagnostics only; they do not redefine the public headline score.
- Surface instability if equal-weight, ownership-heavy, rental-heavy, leave-one-out or geometric variants materially change the latest interpretation.
