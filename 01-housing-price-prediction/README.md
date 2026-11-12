# 01 · Housing price prediction in Cali

**Question.** A property appraiser wants to know how much a home is worth when it reaches the market in Cali, knowing only what the listing says: type, area, bathrooms, and location. How well can that be predicted, and where does the model fail?

**Task:** regression, with price in millions of Colombian pesos (COP).

## Data

- 19,096 for-sale listings of houses and apartments in Cali (2019–2020): 15,277 for training and 3,819 held-out test listings whose prices are hidden.
- Training price: mean COP 391 million, median 290 million, range 50 to 2,500 million. 55% apartments, 45% houses.
- `barrio` is missing in 77% of listings and `habitaciones` in 40%, so location enters through longitude and latitude, and missing values are imputed inside each validation fold to avoid leakage.
- Location features added from public sources: neighborhood, census block and socioeconomic stratum, and distances to parks, schools, health centers, malls, and the MIO transit system.

**Sources.** The listings come from the course dataset `vivienda_cali.rds` (Big Data y Machine Learning para Economía Aplicada, Universidad Icesi), published in the [course repository](https://github.com/eduard-martinez/bdml-applied-economics). A copy is included in `01_data/input/` so the project runs offline and reproducibly; all credit for the dataset goes to its source. The location features in `02_features/input/externos/` come from IDESC, Alcaldía de Cali (CC BY-SA); DANE, Marco Geoestadístico Nacional and Censo 2018; and OpenStreetMap contributors (ODbL).

## Method

1. **Baselines.** The training mean (M0), a line on area (M1), and a regression on area, bathrooms, type, and coordinates (M2).
2. **Validation design.** Random 5-fold cross-validation against **spatial 5-fold cross-validation**, where entire 2.2 km grid cells go to one fold. Random folds let neighboring listings leak across folds; spatial folds measure what an appraiser needs, which is pricing homes in areas the model has not seen. Spatial folds are used for every later decision.
3. **Specification search.** Four linear specifications, from the baseline up to one with interactions, polynomials, stratum, year, and distances.
4. **Regularization.** Ridge, Lasso, and Elastic Net on the 44-column matrix, with the penalty chosen by spatial cross-validation (minimum and one-standard-error rules).
5. **k-NN on the map.** A neighborhood-appraisal benchmark using only coordinates.
6. **Final model.** Chosen by validation error, refit on the full training set, and used once to predict the test listings.

## Results

All figures are RMSE in millions of COP, from out-of-fold predictions on the 15,277 training listings with spatial folds.

| Model | RMSE | R² |
|---|---|---|
| M0 · training mean | 342.9 | −0.02 |
| M1 · line on area | 239.6 | 0.50 |
| M2 · area, bathrooms, type, coordinates | 219.3 | 0.58 |
| E4 · interactions, polynomials, stratum, year, distances (45 coefficients) | 185.4 | 0.70 |
| Elastic Net (α = 0.5) | 186.0 | 0.70 |
| k-NN on the map (k = 100) | 283.3 | 0.30 |
| **E5 · E4 plus stratum × area, modeled in log(price)** | **181.8** | **0.71** |

- **The final model cuts error by 47%** relative to predicting the mean. Its mean absolute error is about COP 99 million.
- **Regularization did not help.** With 15,277 listings and 44 columns there is little variance to remove, so the penalty only adds bias. Ridge, Lasso, and Elastic Net match, but do not beat, the unpenalized specification. Their value would appear with many more columns than listings.
- **Location matters, and the validation scheme shows it.** With spatial folds the error ranges from COP 136 million to 249 million depending on the area held out, a spread that random folds hide.
- **Predictions shrink toward the mean.** In the first validation exercise, M2's slope of predicted on observed price is 0.63. It overpredicts cheap homes and underpredicts expensive ones, and the error in the top price quintile is about four times that of the lowest quintiles.

## Takeaways for the appraiser

A listing supports an automated first appraisal with a typical error of about COP 182 million and explains about 71% of price variation. It is a starting point, not a final appraisal. It will fail most on expensive homes and in areas with few listings or unlike the rest. The listing says nothing about condition, finishes, floor level, or view, which matter most at the high end. Coefficients and selected variables describe prediction, not causal effects on price.

## Project structure

Every stage is a folder with the same three subfolders: `input/` (what the stage reads), `source/` (its scripts), and `output/` (what it writes). `00_run.R` copies each stage's inputs from the previous stages' outputs, so a stage only ever reads its own `input/`.

```
01-housing-price-prediction/
├── 00_run.R           runs the whole project, end to end
├── 01_data/           the listings (input) and their preparation
├── 02_features/       location features: neighborhood, census stratum, distances
├── 03_baselines/      exploration, baselines, and diagnostics
├── 04_validation/     random vs. spatial CV, specification search, Ridge / Lasso / Elastic Net, k-NN
├── 05_final_model/    final model and test predictions
└── 06_report/         report sources and the final PDF (in Spanish)
```

Each stage folder looks like this:

```
03_baselines/
├── input/    files this stage reads (copied by 00_run.R)
├── source/   the stage's scripts
└── output/   files this stage writes
```

## Reproducing the results

From this folder:

```bash
Rscript 00_run.R
```

This runs every stage in order and writes:

- `06_report/output/informe_general.pdf`: the full report.
- `05_final_model/output/predicciones_precio.csv`: predicted price for each test listing.
- `06_report/output/logs/`: one log per stage and `sessionInfo.txt`.

The first run takes several minutes; later runs skip stages that are up to date. It needs R 4.x, plus pandoc and LaTeX to build the PDF.
