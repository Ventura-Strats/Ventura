# Feature Selection

## Overview

Feature selection determines which technical indicators (features) each strategy's random forest model uses for training and prediction. Starting from ~2000 candidate features, an iterative elimination process keeps the ~120 most informative ones per strategy.

## Function: `V.removeUselessFeatures(strat_id, starting_features = "")`

**Location**: `Code/Ventura.R:3499`

### Algorithm

1. Starts with all ~2000 features (minus any in `starting_features`)
2. Trains a random forest via `V.testModel(strat_id, YESTERDAY, features_to_ignore, TRUE, nb_trees=2000)`
3. Ranks features by variable importance (least important first)
4. Removes the bottom N features, where N = `floor(1 + 30 * ((n / 120 - 0.8)^2))` (removes more when many features remain, fewer as it approaches the target)
5. Repeats until `n_keep_features = 120` features remain
6. Saves intermediate progress to `Data/Features/features_{strat_id}.RData` (can resume via `starting_features`)

### Output

Returns a list `res` with:

| Field | Description |
|-------|-------------|
| `features_to_ignore` | All features that were eliminated |
| `features_in_model` | The ~120 surviving feature names |
| `dat_feature` | Data frame ready for DB insert: `(strategy_id, feature_id, train, market_configuration)` |
| `sql_delete` | SQL template: `DELETE FROM strategy_feature WHERE strategy_id = %s` |
| `features_old` | Features in the current live model (from DB) |
| `features_common_with_live` | Intersection of old and new feature sets |
| `features_new_not_in_old` | New features not in current model |
| `features_old_not_in_new` | Current features being dropped |

The function does **not** write to the database. It prepares everything and returns it for manual review.

## DB Table: `strategy_feature`

| Column | Description |
|--------|-------------|
| `strategy_id` | Strategy (1-14) |
| `feature_id` | Foreign key to `static_feature` |
| `train` | 1 = used for training, 0 = not used |
| `market_configuration` | 1 = used for market configuration |

## Updating the DB After Running

```r
res <- V.removeUselessFeatures(strat_id)

# Review changes
res$features_new_not_in_old    # features being added
res$features_old_not_in_new    # features being dropped
length(res$features_in_model)  # should be ~120

# Write to DB
D.SQL(sprintf(res$sql_delete, strat_id))
D.replaceDataIntoTable("strategy_feature", res$dat_feature)
```

Repeat for each strategy (1-14). Each strategy is independent.

## How `strategy_feature` Is Consumed

The feature list flows into two downstream steps:

### 1. Loading Training Data: `T.getTechnicals("train", strat_id)`

**Location**: `Code/Technicals.R:5689-5727`

SQL queries against `histo_technicals_int` and `histo_technicals_dbl` filter to only features present in `strategy_feature` for that strategy. This controls which columns appear in the training data.

When loading from file instead of DB (`Technicals.R:5763-5783`), the same filter is applied to the preloaded `Technicals.RData` file.

### 2. Training the Model: `E.trainModel(strat_id)`

**Location**: `Code/Engine.R:533-541`

Reads `strategy_feature` to get the feature list, excludes non-trainable columns (price data, targets, dates), and passes the remaining columns to the random forest.

### End-to-End Flow

```
V.removeUselessFeatures(strat_id)
    -> manual review
    -> write res$dat_feature to strategy_feature table
    -> Train.R runs V.prepareMorningModel()
        -> T.getTechnicals("train", strat_id)  [reads strategy_feature to filter columns]
        -> E.trainModel(strat_id)              [reads strategy_feature for feature list]
        -> saves model to Data/Models/model_{strat_id}.RData
    -> Predict.R loads model and runs predictions
```
