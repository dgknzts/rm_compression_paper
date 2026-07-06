# rm_compression_paper

Analysis code and data for:

> L-Miao, L.†, Öztaş, D. N.†, Alp, N., & Sayim, B. (2026). *...* († co-first authors)

- **Experiment 1**: single-bar probe, width adjustment (N = 21), folder `exp1/`
- **Experiment 2**: multi-bar probe, width + spacing adjustment (N = 20), folder `exp2/`

## Data

Processed data are included (`exp*/data/processed.csv`, `exp*/data/one_bar_*.csv`), so the analyses run as-is. Raw data and a codebook are available on OSF (link will be added upon publication). To re-run preprocessing, place the raw CSVs in `exp*/data/raw/`.

## Usage

Requires R with `tidyverse`, `lme4`, `emmeans`, `patchwork`, `RColorBrewer`.

From the repository root:

```r
source("run_all.R")
```

or source the scripts in numbered order (`analysis` → `modelling` → `plotting`).

## Structure

```
exp1/, exp2/    analysis, modelling, plotting, data, outputs per experiment
shared/         model functions, plot theme, Figure 1 model panels
model_structure/  likelihood-ratio tests for the random-effects structure
supplementary/  supplementary tables (S1-S3)
```

Figure 2: `exp1/plotting/01_figure.R`. Figure 3: `exp2/plotting/01_figure.R`. Figure 4: `exp2/plotting/02_joint_width_spacing.R`.

## License

Code: MIT. Data: CC-BY 4.0 (OSF).
