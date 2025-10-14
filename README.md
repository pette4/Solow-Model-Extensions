# Empirical Analysis of the Solow Model

**Course:** Econometrics (30413)  

---

## Overview

This project empirically tests the **Solow growth model** across 168 countries, investigating how **GDP per capita** relates to the **savings rate**, **population growth**, and **trade openness**.  
The goal is to assess whether long-run income differences can be explained by structural economic fundamentals, consistent with the predictions of the Solow framework.

---

## Data

All data were obtained from **The World Bank (2000–2023)**:

| Variable | Description | Indicator Code |
|-----------|--------------|----------------|
| GDP per capita | Current US$ | `NY.GDP.PCAP.CD` |
| Population Growth | Annual % | `SP.POP.GROW` |
| Gross Saving Rate | % of GDP | `NY.GNS.ICTR.ZS` |
| Trade | % of GDP | `NE.TRD.GNFS.ZS` |

---

## Methodology

- Converted datasets to long format and merged by country.  
- Performed **cross-sectional regressions** of log(GDP per capita) on log(saving), log(population growth), and log(trade).  
- Compared different timeframes (2023 only, and 2000–2023 averages).  
- Conducted subgroup analyses:
  - OECD vs. Non-OECD countries  
  - Resource exporters vs. others  
- Applied **Chow tests** for structural breaks and **Breusch–Pagan tests** for heteroskedasticity.

---

## Findings

- The model performs best using **long-run averaged variables**, consistent with the steady-state assumption of the Solow model.  
- **Trade openness** is a significant and robust determinant of GDP per capita.  
- Results are coherent with **Mankiw et al. (1992)**, supporting an implied capital share (α) ≈ **0.33**.  
