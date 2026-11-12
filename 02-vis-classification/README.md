# 02 · Finding social-housing (VIS) listings

**Question.** A housing secretariat wants to find *Vivienda de Interés Social* (VIS) for a subsidy program. Reviewing a listing by hand is cheap, but missing a VIS listing is costly. Which listings should it review?

**Task:** binary classification of a rare class. A listing is VIS when its price is within the legal VIS range (up to 135 minimum monthly wages in the year of publication). The price itself is not used as a predictor.

## Data

Real for-sale listings of houses and apartments in Cali (2019–2020): 15,277 training listings, of which **770 are VIS (5.04%)**, plus 3,819 held-out test listings. Predictors are what the listing reports (type, area, bathrooms) plus location, year, stratum, and distance features from public sources.

**Sources.** The listings come from the course dataset `vivienda_cali.rds` (Big Data y Machine Learning para Economía Aplicada, Universidad Icesi), published in the [course repository](https://github.com/eduard-martinez/bdml-applied-economics). A copy is included in `01_data/input/` so the project runs offline and reproducibly; all credit for the dataset goes to its source. The location features in `02_features/input/externos/` come from IDESC, Alcaldía de Cali (CC BY-SA); DANE, Marco Geoestadístico Nacional and Censo 2018; and OpenStreetMap contributors (ODbL).

## Method

1. **Reference.** Accuracy of the trivial classifier that labels everything "not VIS".
2. **Logistic regression.** ROC curve, AUC, and confusion matrix at the default threshold of 0.5.
3. **Cost-based threshold.** Choose the threshold from the cost of each error, `τ* = c_FP / (c_FP + c_FN)`, and compare it with the precision-recall curve, the right tool for a rare class.
4. **Sensitivity.** Repeat for different cost ratios, because the ratio is an assumption.
5. **Model selection.** Six candidates (three logistic specifications, Lasso, Ridge, Elastic Net) compared by AUC, log-loss, and Brier score with spatial 5-fold cross-validation. The final model is refit on all training data to predict VIS probability for the test listings.

## Results

**Accuracy is misleading here.** Labeling every listing "not VIS" is right 94.96% of the time and finds none of the VIS listings (0% recall).

**The model ranks well; the default cutoff does not.** On a validation split (3,056 listings, 154 VIS) the logistic regression reaches an AUC of 0.963: a random VIS listing gets a higher probability than a random non-VIS one 96% of the time. But at τ = 0.5 it finds only 69 of 154 VIS listings (recall 44.8%, precision 72.6%).

**Choosing the threshold from costs.** With a missed VIS listing assumed to cost 10 times a wasted review, the optimal threshold falls to about 0.09:

| Threshold | VIS found | Missed | Recall | Precision | Listings to review | Total cost |
|---|---|---|---|---|---|---|
| 0.50 (default) | 69 | 85 | 44.8% | 72.6% | 95 | 876 |
| **0.09 (cost-optimal)** | **140** | **14** | **90.9%** | 33.2% | 422 (13.8%) | **422** |

Cost sensitivity shows diminishing returns. Going from a 20:1 to a 50:1 cost ratio finds only 3 more VIS listings and adds 260 reviews.

| Cost ratio (FN : FP) | Threshold | Recall | Precision | Listings to review |
|---|---|---|---|---|
| 2 : 1 | 0.33 | 61.7% | 58.6% | 162 |
| 5 : 1 | 0.17 | 79.2% | 45.2% | 270 |
| 10 : 1 | 0.09 | 90.9% | 33.2% | 422 |
| 20 : 1 | 0.05 | 95.5% | 26.7% | 551 |
| 50 : 1 | 0.02 | 97.4% | 18.5% | 811 |

**Final model (spatial cross-validation, AUC):**

| Model | AUC |
|---|---|
| V0 · no predictors | 0.500 |
| V1 · logit: type, area, bathrooms, stratum | 0.938 |
| V2 · V1 + coordinates, year | 0.940 |
| **V3 · V2 + distances (selected)** | **0.941** |
| Lasso / Ridge / Elastic Net | 0.917 – 0.934 |

- **Spatial folds are the honest test.** Every model scores lower when it must classify zones it has not seen: V2 drops from 0.945 with random folds to 0.940 with spatial folds, and Ridge from 0.934 to 0.917.
- **A plain logistic regression won.** The penalized models, given extra curvature terms, did not beat it.

## Takeaways for the secretariat

The decision is not "is this VIS?" but "how many reviews can we afford?". The threshold should come from the cost of each error, not from the default 0.5, and the cost ratio should be reported as an assumption with its sensitivity. Selected variables and coefficients are predictive, not causal.

## Project structure

Every stage is a folder with the same three subfolders: `input/` (what the stage reads), `source/` (its scripts), and `output/` (what it writes). `00_run.R` copies each stage's inputs from the previous stages' outputs, so a stage only ever reads its own `input/`.

```
02-vis-classification/
├── 00_run.R             runs the whole project, end to end
├── 01_data/             the listings (input) and their preparation
├── 02_features/         location features: neighborhood, census stratum, distances
├── 03_classification/   logistic regression, ROC, and cost-based threshold
├── 04_final_model/      model comparison and final VIS probabilities
└── 05_report/           report sources and the final PDF (in Spanish)
```

Each stage folder looks like this:

```
03_classification/
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

- `05_report/output/informe_general.pdf`: the full report.
- `04_final_model/output/predicciones_vis.csv`: VIS probability for each test listing.
- `05_report/output/logs/`: one log per stage and `sessionInfo.txt`.

The first run takes several minutes; later runs skip stages that are up to date. It needs R 4.x, plus pandoc and LaTeX to build the PDF.
