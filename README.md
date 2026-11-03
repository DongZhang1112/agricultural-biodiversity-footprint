# Growing biodiversity footprints despite agricultural efficiency gains

**Data and code archive for the Nature Food submission**

This archive contains the 167-country × 21-year (2000–2020) harmonized analytical panel, derived tables for the manuscript figures and supplementary analyses, a four-commodity strictly matched sample, the main-figure plotting script, and additional reproducibility scripts. The overall agricultural output measure excludes aquaculture.

## Contents

- `TFP_BIIFP_analysis_ready.xlsx` — original analysis workbook (unchanged except filename).
- `Strict_commodity_summary.csv` — original commodity summary used by Fig. 5 (unchanged except filename).
- `data/` — 14 analysis and derived-data CSV files, originally provided in `data.zip`. Legacy placeholder column names for the USDA output unit were clarified; **numeric values were not altered**.
- `source_data/WDI_AG.LND.AGRI.K2_2026-07-13.csv` — archived World Bank WDI agricultural-land extract used in the analytical panel. Indicator `AG.LND.AGRI.K2`, in km², updated 13 July 2026. Retaining this historical extract is essential because WDI may revise its data.
- `source_data/USDA_ERS_AgTFPInternational2023_long.csv` — supplied USDA ERS International Agricultural Productivity long-format data. Agricultural output uses `Outcrop_Q + Outanim_Q`, excluding `Outfish_Q`.
- `scripts/01_reproduce_global_country.R` — computes global annual indices, country trends, decoupling, and period-specific BFI trends from `data/01_country_and_year.csv`.
- `scripts/02_reproduce_strict_decomposition.R` — independently recalculates the strictly matched commodity decomposition and leave-one-country-out sensitivities from `data/08_four_commodity_matched_country.csv`.
- `scripts/03_supplementary_figures.R` — renders S1–S3 visual summaries from the archived derived tables. These are *reproductions of the plotted results*, not guaranteed visual matches to the submitted figure layouts.
- `scripts/04_main_figures.R` — original R figure script for Figs. 2–5, updated only to use the standardized workbook filenames and an optional `NF_DATA_DIR` environment variable.
- `documentation/QC_report.txt` — data-level integrity checks and differences from the original analysis scope.
- `documentation/SHA256SUMS.txt` — file fingerprints.

## Units and definitions

| Variable | Definition | Unit |
| --- | --- | --- |
| `Y`, `Output_CL_Q` | USDA ERS `Outcrop_Q + Outanim_Q` | thousands of constant 2015 international dollars, as documented for the dataset series; verify against exact source vintage |
| `A`, `Agricultural_land_km2` | WDI agricultural land, `AG.LND.AGRI.K2` | km² |
| `BF_total_km2eq` | Model-attributed biodiversity intactness loss footprint | km²-equivalent |
| `LI` | `A / Y` | km² / thousand constant 2015 international dollars |
| `BFI` | `BF / Y` | km²-equivalent / thousand constant 2015 international dollars |
| Commodity `Q` | Physical production, FAOSTAT | tonnes |
| Commodity intensity | commodity BF / commodity Q | km²-equivalent / tonne |

Some original workbook columns use alternative scaling such as `LI_ha_per_mUSD` and `BFI_haeq_per_mUSD`. The new reproducibility scripts calculate ratios from unscaled numerators and denominators. Index-based temporal changes are invariant to consistent unit rescaling.

## Quick start (from the extracted archive root)

Use R with `ggplot2` for Supplementary Figures and `readxl`, `dplyr`, `tidyr`, `ggplot2`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `scales`, `patchwork` for main figures.

```sh
Rscript scripts/01_reproduce_global_country.R
Rscript scripts/02_reproduce_strict_decomposition.R
Rscript scripts/03_supplementary_figures.R
Rscript scripts/04_main_figures.R
```

The main-figure script produces PDF figures in `figures/`; other scripts create `reproduced_results/` or `reproduced_figures/`. Scripts should be run from the extracted archive root. To run from elsewhere, set `NF_DATA_DIR` to the archive's absolute path.

## What is and is not reproducible from this archive

**Included and independently recalculable from archived processed inputs:** global annual total/ratio indicators, country trend summaries, decoupling classifications, period BFI changes, strict-match 2000–2020 commodity decomposition, and leave-one-country-out contributions. Fig. 2–5 plotting code and the derived data for the supplementary figures are also supplied.

**Not yet independently recoverable from raw BF input:** rebuilding the original Nguyen et al. (2025) commodity-by-biome biodiversity-footprint dataset, the complete original FAOSTAT commodity country/commodity crosswalk, and the alternative missing-BF-as-zero decomposition from *all* originally unmatched source rows. The archive includes their derived analysis tables, but does **not** include the full source-level BF records and all upstream cleaning scripts. Do not describe this release as a fully end-to-end replication of all source-data processing until those inputs and scripts are added.

The five main figure files and supplementary figure source artwork are not provided here; all scientific numerical data necessary to inspect the underlying analysis and regenerate main quantitative plots are provided. Fig. 1 is a conceptual diagram and does not have an R generator.

## Data source and citation notes

- USDA ERS, *International Agricultural Productivity* (source file supplied with archive). Document the exact download URL and file release metadata when describing the dataset in the manuscript.
- World Bank, *World Development Indicators*, agricultural land in sq. km (`AG.LND.AGRI.K2`), archived extract updated 2026-07-13, based on FAO land-use statistics. Do not substitute a newly downloaded WDI data vintage for exact reproduction.
- Nguyen et al. (2025), biodiversity intactness footprint of agricultural production from 2000 to 2020. The original source BF records are not redistributed here.
- FAOSTAT, Crops and Livestock Products, commodity physical production (tonnes). The strictly matched country-level endpoint values are in `data/08_four_commodity_matched_country.csv`.

## Integrity and provenance cautions

- The `data/` folder contains analysis-level tables. Some tables duplicate information at different aggregation levels by design; do not sum overlapping tables together.
- Original `data.zip` macOS metadata files (`__MACOSX`, `._*`) were omitted.
- Filename typos and placeholder unit header names were cleaned; original CSV numbers were not changed.
- The data's model-attributed BF is not observed biodiversity loss. The decomposition is an accounting identity, not a causal estimator.

**Archive status:** organized submission candidate. Before public release, run the R scripts in the authors' R environment and add the missing source-level BF/FAOSTAT transformation scripts if claiming full source-to-figure reproducibility.
