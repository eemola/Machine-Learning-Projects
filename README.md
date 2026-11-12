# Machine Learning Projects

A collection of applied machine learning projects and exercises. Each project is self-contained and reproducible: it runs from raw data to results with a single command, validates its models honestly, and documents what the results mean.

## Projects

| Project | Task | Methods | Headline result |
|---|---|---|---|
| [**01 · Housing price prediction**](01-housing-price-prediction) | Regression | Linear models, spatial cross-validation, Ridge / Lasso / Elastic Net, k-NN | Out-of-fold RMSE of COP 181.8 million, R² 0.71 |
| [**02 · Social-housing (VIS) classification**](02-vis-classification) | Classification of a rare class (5% of listings) | Logistic regression, ROC / precision-recall, cost-based threshold | AUC 0.94 in spatial CV; 91% recall at the cost-optimal threshold |

More projects will be added over time.

## How the repository is organized

Every project lives in its own folder and follows the same layout, so any of them can be read, run, or reused on its own:

```
Machine-Learning-Projects/
├── 01-housing-price-prediction/
├── 02-vis-classification/
└── ...
```

Inside a project:

```
00_run.R        runs the whole project, end to end
01_data/        the data (input) and its preparation
02_features/    feature construction
03_… 0N_…       numbered modeling stages, in the order they run
0N+1_report/    report sources and the final PDF
README.md       the question, the method, the results
```

Every numbered stage has the same three subfolders: `input/` (what the stage reads), `source/` (its scripts), and `output/` (what it writes).

## Reproducing a project

Each project runs with one command from its own folder:

```bash
Rscript 00_run.R
```

The script runs every stage in order, checks that each one produced its outputs, and builds the report. Stages that are already up to date are skipped. You need R 4.x, plus pandoc and a LaTeX distribution to build the PDF (RStudio bundles pandoc; the script installs TinyTeX if LaTeX is missing). R packages are installed automatically.

Each project includes the input data it needs in `01_data/input/`, so it runs offline. Its README credits the source of the data.
