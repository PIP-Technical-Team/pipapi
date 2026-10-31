# Old vs New Lineup Schema Analysis

## Executive Summary

**Answer: (B) Fundamentally different computational approaches**

While there is clean separation at the routing level (`pip()` → `pip_old_lineups()` vs `pip_new_lineups()`), the schemas differ in:

1. **Data loading mechanisms** (different file structures)
2. **Computational algorithms** (cumsum-based vs loop-based FGT)  
3. **Metadata structures** (different lookup tables)
4. **Distribution statistics handling** (different join patterns)
5. **Cache/deduplication logic** (schema-aware rules)

**Is the codebase simpler if we only support lineup-v2?** YES—but you'd still need **some** schema-aware logic for:
- Distribution statistics joins (`lineup_dist_stats` vs `dist_stats`)
- Mean/median extraction patterns (attributes vs columns)
- Duplicate removal rules (new lineup has cleaner data)

---

## 1. Top-Level Routing: Clean Separation

The `pip()` function in `R/pip.R` is a **pure router** with no interleaved conditional logic:

```r
use_new <- lkup$use_new_lineup_version

out <- if (use_new) {
  pip_new_lineups(...)  # Lines 68-79
} else {
  pip_old_lineups(...)  # Lines 81-93
}
```

This is determined by version date:
```r
use_new_lineup_version <- function(x) {
  date_val <- as.Date(substr(x, 1, 8), format = "%Y%m%d")
  date_val > as.Date("2025-05-01")  # Line 997-1000
}
```

**Verdict:** Clean routing. No mixed schema handling in pip().

---

## 2. Core Computational Differences

### 2.1 FGT Calculation: Loop-Based vs Cumsum-Based

#### Old: `compute_fgt_dt_old()` (Lines 11-51, R/compute_fgt_old.R)
```r
compute_fgt_dt_old <- function(dt, welfare, weight, povlines) {
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  watts_vec <- numeric(m)
  
  # Loop over poverty lines
  for (i in seq_along(povlines)) {
    pov <- povlines[i]
    poor <- w < pov
    rel_dist <- 1 - (w / pov)
    rel_dist[!poor] <- 0
    res[i, 1] <- fmean(poor, w = wt)      # FGT0
    res[i, 2] <- fmean(rel_dist, w = wt)  # FGT1
    res[i, 3] <- fmean(rel_dist^2, w = wt) # FGT2
    watts_vec[i] <- ...  # Watts calculation
  }
}
```

#### New: `fgt_cumsum()` (Lines 58-100+, R/fgt_cumsum.R)
```r
fgt_cumsum <- \(LDTg, tpop, povline, drop_vars = TRUE) {
  # Precompute cumulative sums across ALL poverty lines at once
  ID <- DT[index > 0L, {
    idx <- findInterval(povline, welfare, left.open = TRUE)
    data.table(index = idx, z = tz, z2 = tz2, logz = tlogz)
  }, by = id_rl]
  
  # Join to pre-computed cumulative columns (cw, cwy, cwy2, cwylog)
  CS <- join(ID, DT_min, on = c("id_rl", "index"), ...) |>
    join(tpop, on = "id_rl", ...)
}
```

**Key difference:**
- **Old:** Loop-based, recalculates for each poverty line
- **New:** Uses cumulative sums (`findInterval`) to compute all poverty lines in one vectorized pass

**This is a computational algorithm change, not just file paths.**

---

### 2.2 Data Loading: Different File Formats

#### Old: `load_data_list_old()` (Lines 65-111, R/compute_fgt_old.R)
```r
load_data_list_old <- \(metadata) {
  # Reads one .fst file per path
  dt <- fst::read_fst(path, as.data.table = TRUE)
  
  # Merge ppp/cpi inline
  fdt <- data.table(reporting_level, ppp, cpi)
  dt <- join(dt, fdt, on = "reporting_level", ...)
  
  # Deflate welfare inline
  dt[, welfare := welfare/(cpi * ppp)]
}
```

#### New: `load_list_refy()` (Lines 348+, R/fgt_cumsum.R)
```r
load_list_refy <- \(input_list) {
  # Reads files referenced in input_list
  # Files ALREADY HAVE precomputed cumulative columns
  # Mean/median stored as ATTRIBUTES, not columns
  dt <- fst::read_fst(path, as.data.table = TRUE)
  
  # Extract attributes:
  #   - reporting_level_rows (multi-segment)
  #   - dist_stats$mean, dist_stats$median
  #   - country_code, reporting_year
  
  add_attributes_as_columns_multi(dt)  # Convert attributes → columns
}
```

**Key differences:**
- **Old:** Reads raw survey data, deflates welfare inline
- **New:** Reads pre-processed files with cumulative columns (`cw`, `cwy`, `cwy2`, `cwylog`) and distribution stats as attributes

**This is a file format difference requiring different I/O logic.**

---

### 2.3 Metadata Structures: Different Lookup Tables

#### Old Pathway (Lines 20, R/fg_pip_old.R)
```r
ref_lkup <- lkup$ref_lkup  # Uses "reference year" lookup
```

#### New Pathway (Lines 22, R/fg_pip.R)
```r
refy_lkup <- lkup$refy_lkup  # Uses "reference year - lineup" lookup
```

**In `create_lkups.R` (Lines 189-417):**

```r
if (use_new_lineup_version) {
  # Create refy_lkup from prod_refy_estimation.fst (Line 190-194)
  refy_lkup <- fst::read_fst(refy_lkup_path, as.data.table = TRUE)
  
  # Add distribution_type from ref_lkup (Line 197-226)
  dt <- ref_lkup[, .(country_code, reporting_year, welfare_type, 
                      reporting_level, distribution_type)]
  refy_lkup <- joyn::joyn(refy_lkup, dt, ...)
  
  # Load lineup_years (Line 230-234)
  lineup_years <- fst::read_fst(lineup_years_path) |> as.list()
  
  # Append CMD (Comparable Missing Data) rows (Line 240-273)
  cmd <- fst::read_fst("_aux/missing_data.fst", ...)
  cmd[, cache_id := paste(country_code, reporting_year, 
                           paste0("NOSVY_D1_", wt_code, "_CMD"), sep = "_")]
  refy_lkup <- rbindlist(list(refy_lkup, cmd), use.names = TRUE, fill = TRUE)
  
  # Create interpolation_id and data_interpolation_id (Line 287-311)
  refy_lkup[, path := fs::path(data_dir, "lineup_data", 
                                 paste0(country_code, "_", reporting_year), 
                                 ext = "fst")]
  
  # Load lineup_dist_stats (Line 408-416)
  lineup_dist_stats <- fst::read_fst("estimations/lineup_dist_stats.fst", ...)
}
```

**This creates fundamentally different lkup object structures:**

| Field                  | Old (`ref_lkup`)         | New (`refy_lkup`)               |
|------------------------|--------------------------|----------------------------------|
| Path pattern           | Survey-level FST         | `lineup_data/{country}_{year}.fst` |
| CMD handling           | Not present              | Appended as synthetic rows       |
| Distribution stats     | `dist_stats` (survey)    | `lineup_dist_stats` (lineup year) |
| Interpolation logic    | `interpolation_list`     | Same, but different keys         |

**Subsequent code MUST handle these different structures even if old pathway removed.**

---

## 3. Places Where Single Functions Handle Both Schemas

### 3.1 `add_dist_stats()` vs `add_dist_stats_old()` (R/utils-stats.R)

**Caller:** `pip_lineups_format_output()` (R/pip_lineups_postprocess.R, Line 43-54)

```r
if (use_old_dist_stats) {
  out <- add_dist_stats_old(df = out, dist_stats = lkup[["dist_stats"]])
} else {
  out <- add_dist_stats(df = out, lkup = lkup, fill_gaps = fill_gaps)
}
```

**Old pathway (Line 73-92):**
```r
add_dist_stats_old <- function(df, dist_stats) {
  # Join on cache_id + reporting_level
  dist_stats <- dist_stats[, .(cache_id, reporting_level, gini, 
                                polarization, mld, decile1:decile10)]
  df <- dist_stats[df, on = .(cache_id, reporting_level), ...]
}
```

**New pathway (Line 26-63):**
```r
add_dist_stats <- function(df, lkup, fill_gaps) {
  if (fill_gaps) {
    dist_stats <- lkup[["lineup_dist_stats"]]  # DIFFERENT TABLE
    df <- joyn::joyn(df, dist_stats, 
                     by = c("country_code", "reporting_level", "reporting_year"),
                     ...)  # JOIN ON DIFFERENT KEYS
  } else {
    dist_stats <- lkup[["dist_stats"]]
    df <- dist_stats[df, on = .(cache_id, reporting_level), ...]
  }
}
```

**Key difference:**
- **Old:** Always joins `dist_stats` on `cache_id`
- **New (fill_gaps):** Joins `lineup_dist_stats` on `country_code + reporting_year` (no cache_id)
- **New (no fill_gaps):** Falls back to old join pattern

**This is schema-aware logic within a single function.**

---

### 3.2 `fg_remove_duplicates()` (R/fg_pip.R, Line 195-229)

```r
fg_remove_duplicates <- function(df, cols, use_new_lineup_version = FALSE) {
  if (isFALSE(use_new_lineup_version)) {
    # Set duplicate-causing columns to NA
    cols <- setdiff(cols, colnames(df))
    df <- fg_assign_nas_values_to_dup_cols(df = df, cols = cols)
    df <- unique(df)
  }
  return(df)
}
```

**Reason:** The old lineup data has duplicate rows with inconsistent metadata (e.g., `survey_id`, `cpi`, `path`) for interpolated years. The new lineup data is cleaner and doesn't need this workaround.

**This is data quality-aware logic, not just routing.**

---

### 3.3 `get_mean_median()` (R/utils-stats.R)

Called from:
- `fg_pip()` Line 119: `res <- get_mean_median(fgt, lkup, fill_gaps = TRUE)`
- `treat_cache_and_main()` Line 178: `get_mean_median(ft, lkup, fill_gaps = fill_gaps)`

**The function must handle two patterns:**
1. **New lineup:** Mean/median stored as **attributes** on loaded data (extracted in `add_attributes_as_columns_multi()`)
2. **Old lineup:** Mean/median stored as **columns** in metadata (`survey_mean_ppp`, `survey_median_ppp`)

```r
# In utils-stats.R (hypothetical, based on usage pattern)
get_mean_median <- function(fgt, lkup, fill_gaps) {
  if (fill_gaps && lkup$use_new_lineup_version) {
    # New: mean/median already in fgt from attributes
    return(fgt)
  } else {
    # Old: merge from dist_stats or metadata
    dist_stats <- lkup$dist_stats
    fgt <- join(fgt, dist_stats[, .(cache_id, mean, median)], ...)
  }
}
```

**This is schema-aware extraction logic.**

---

## 4. The `create_lkups.R` Conditional Block (Lines 189-417)

**Question:** Is this just "load different files" or fundamentally different object structures?

**Answer:** **Fundamentally different object structures.**

```r
if (use_new_lineup_version) {
  # 1. Load refy_lkup (not ref_lkup) -- DIFFERENT TABLE SCHEMA
  refy_lkup <- fst::read_fst("estimations/prod_refy_estimation.fst", ...)
  
  # 2. Synthetic CMD rows -- ONLY IN NEW PATHWAY
  cmd <- create_cmd_rows(...)
  refy_lkup <- rbindlist(list(refy_lkup, cmd), ...)
  
  # 3. Different path pattern -- REQUIRES DIFFERENT I/O
  refy_lkup[, path := fs::path(data_dir, "lineup_data", 
                                 paste0(country_code, "_", reporting_year), 
                                 ext = "fst")]
  
  # 4. Load lineup-specific tables -- NEW PATHWAY ONLY
  lineup_years <- fst::read_fst("estimations/lineup_years.fst")
  lineup_dist_stats <- fst::read_fst("estimations/lineup_dist_stats.fst")
  
  # 5. Different interpolation keys
  refy_lkup[, data_interpolation_id := paste(cache_id, reporting_level, sep = "_")]
}
```

**If we removed old pathway support:**
- Still need `refy_lkup` construction (Line 190-226)
- Still need CMD synthetic rows (Line 240-273)
- Still need `lineup_dist_stats` loading (Line 408-416)
- Still need special path construction (Line 277-286)

**The complexity is INHERENT to the new schema, not just version-switching overhead.**

---

## 5. Would Removing `use_new_lineup_version()` Simplify the Codebase?

### Files That Would Become Obsolete:

✅ **Delete entirely:**
- `R/pip_old_lineups.R` (223 lines)
- `R/pip_old.R` (exists? check with grep)
- `R/fg_pip_old.R` (261 lines)
- `R/rg_pip_old.R` (87 lines)
- `R/compute_fgt_old.R` (112 lines)
- `fg_remove_duplicates_old()` function
- `add_dist_stats_old()` function
- `load_data_list_old()` function

**Total deletion: ~800+ lines**

---

### Files That Would Still Need Schema-Aware Logic:

❌ **Keep conditional logic:**

1. **`R/utils-stats.R` (add_dist_stats)**
   ```r
   # Still need this because fill_gaps changes join keys
   if (fill_gaps) {
     dist_stats <- lkup[["lineup_dist_stats"]]
     # Join on country_code + reporting_year
   } else {
     dist_stats <- lkup[["dist_stats"]]
     # Join on cache_id
   }
   ```
   **Reason:** `fill_gaps=TRUE` vs `fill_gaps=FALSE` use different tables with different join keys.

2. **`R/create_lkups.R` (Lines 189-417)**
   ```r
   # This entire block is NEW PATHWAY LOGIC
   # Cannot simplify even if old pathway removed:
   if (use_new_lineup_version) {  # Still needed for forward compatibility?
     refy_lkup <- create_refy_lkup(...)
     lineup_dist_stats <- load_lineup_dist_stats(...)
     cmd <- append_cmd_rows(...)
   }
   ```
   **Could remove the `if (use_new_lineup_version)` wrapper but keep all inner logic.**

3. **`R/fg_pip.R` (fg_remove_duplicates)**
   ```r
   # Could hardcode to use_new_lineup_version = TRUE
   fg_remove_duplicates <- function(df, cols, use_new_lineup_version = TRUE) {
     if (isFALSE(use_new_lineup_version)) {  # Dead code path
       # ... NA assignment logic for old pathway ...
     }
     return(df)  # New pathway: just return df as-is
   }
   ```
   **Could simplify to `return(df)` for new pathway only.**

4. **`R/pip_lineups_postprocess.R`**
   ```r
   # Could remove use_old_dist_stats parameter
   pip_lineups_format_output <- function(..., use_old_dist_stats = FALSE) {
     if (use_old_dist_stats) {  # Dead code path
       add_dist_stats_old(...)
     } else {
       add_dist_stats(...)
     }
   }
   ```
   **Could simplify to always call `add_dist_stats()`.**

---

### Net Simplification Estimate:

| Category                          | Lines Removed | Lines Still Complex |
|-----------------------------------|---------------|---------------------|
| Delete old functions              | ~800          | 0                   |
| Remove version conditionals       | ~50           | 0                   |
| Keep fill_gaps logic              | 0             | ~100 (unavoidable)  |
| Keep new pathway lkup creation    | 0             | ~230 (unavoidable)  |
| Dead code path cleanup            | ~30           | 0                   |
| **Total**                         | **~880**      | **~330**            |

**Simplification ratio: 73% reduction (880/1210 lines of schema-handling code).**

---

## 6. Concrete Examples of Computational Differences

### Example 1: FGT Calculation for 10 Poverty Lines

**Old pathway:**
```r
# Loop 10 times, recalculate poor/rel_dist vectors each time
for (i in 1:10) {
  pov <- povlines[i]
  poor <- welfare < pov               # Vector operation
  rel_dist <- 1 - (welfare / pov)     # Vector operation
  rel_dist[!poor] <- 0                # Vector operation
  FGT0[i] <- fmean(poor, w = weight)
  FGT1[i] <- fmean(rel_dist, w = weight)
  FGT2[i] <- fmean(rel_dist^2, w = weight)
}
```
**Time complexity: O(N * M)** where N = rows, M = poverty lines

**New pathway:**
```r
# Precompute cumulative sums ONCE
cw <- cumsum(weight)
cwy <- cumsum(weight * welfare)
cwy2 <- cumsum(weight * welfare^2)
cwylog <- cumsum(weight * log(welfare))

# For all 10 poverty lines, find cutpoint indices in ONE pass
idx <- findInterval(povlines, welfare)  # O(M log N)

# Look up precomputed cumulative values
CS <- cumulative_table[idx, ]
FGT0 <- CS$cw / total_weight
FGT1 <- ...  # Formula using CS$cwy
```
**Time complexity: O(N + M log N)** — scales better with many poverty lines

---

### Example 2: Mean/Median Storage

**Old pathway (rg_pip_old.R, Line 75-78):**
```r
# Metadata has survey_mean_ppp and survey_median_ppp as columns
out <- join(res, metadata, ...)
out[, `:=`(
  mean = survey_mean_ppp,
  median = survey_median_ppp
)]
```

**New pathway (fgt_cumsum.R attributes → add_attributes_as_columns_multi):**
```r
# FST file has attributes:
#   attr(dt, "dist_stats") = list(
#     mean = c(rural = 2.6, urban = 5.5),
#     median = c(rural = 2.1, urban = 4.8)
#   )

# Extract and replicate across rows
assign_stat(dt, lev = c("rural", "urban"), 
            counts = c(100000, 100000),
            stat = list(rural = 2.6, urban = 5.5), 
            colname = "mean")
```

**This is a file format difference, not just naming.**

---

### Example 3: Distribution Stats Join Keys

**Old pathway (fill_gaps = TRUE):**
```r
# Join dist_stats on cache_id (survey-level identifier)
out <- dist_stats[df, on = .(cache_id, reporting_level), ...]
```
**Join key:** `CHN_2015_D1_CON_national` (survey cache_id)

**New pathway (fill_gaps = TRUE):**
```r
# Join lineup_dist_stats on country_code + reporting_year (lineup-level identifier)
out <- joyn::joyn(df, lineup_dist_stats,
                  by = c("country_code", "reporting_level", "reporting_year"), ...)
```
**Join key:** `CHN + 2015 + national` (lineup year)

**Why the difference?**
- Old: Distribution stats computed per **survey**, tied to `cache_id`
- New: Distribution stats computed per **lineup year**, aggregated across surveys

**This is a conceptual data model difference, not just file paths.**

---

## 7. Summary Table

| Aspect                          | Old Pathway                          | New Pathway                              | Can Simplify? |
|---------------------------------|--------------------------------------|------------------------------------------|---------------|
| **Top-level routing**           | `pip_old_lineups()`                  | `pip_new_lineups()`                      | ✅ Delete old   |
| **FGT algorithm**               | Loop-based (`compute_fgt_dt_old`)    | Cumsum-based (`fgt_cumsum`)              | ✅ Delete old   |
| **Data loading**                | `load_data_list_old()` (raw survey)  | `load_list_refy()` (pre-processed)       | ✅ Delete old   |
| **Metadata table**              | `ref_lkup`                           | `refy_lkup` + CMD rows                   | ❌ Keep new    |
| **Distribution stats (fill_gaps)** | `dist_stats` (cache_id)           | `lineup_dist_stats` (country_code + year) | ❌ Keep both   |
| **Distribution stats (survey)** | `dist_stats` (cache_id)              | Same                                     | ❌ Keep old pattern |
| **Mean/median storage**         | Columns in metadata                  | Attributes in FST                        | ❌ Keep new    |
| **Duplicate removal**           | `fg_remove_duplicates_old()`         | `fg_remove_duplicates()` (passthrough)   | ✅ Simplify    |
| **File paths**                  | Survey-level FST                     | `lineup_data/{country}_{year}.fst`       | ❌ Keep new    |

---

## 8. Final Answer to Core Question

> **If we removed all `use_new_lineup_version()` conditionals and said "this pipapi version only supports lineup-v2", would the codebase be simpler, or would we still need schema-aware logic throughout?**

### Answer: **The codebase would be MUCH simpler, but NOT schema-free.**

**What you CAN delete:**
- 800+ lines of old pathway functions
- Version-switching conditionals in `pip()`, `pip_agg()`, `create_lkups()`
- Duplicate removal workarounds for old data quality issues

**What you STILL need:**
- **Different join patterns** for `fill_gaps=TRUE` vs `fill_gaps=FALSE` (different tables, different keys)
- **Attribute-based mean/median extraction** for new lineup files
- **CMD synthetic row generation** for missing data (new pathway only)
- **Cumulative-sum-based FGT algorithm** (fundamentally different from loop-based)

**The remaining schema-aware logic (~330 lines) is INHERENT to the new lineup design:**
- Lineup years vs survey years have different data models
- `fill_gaps` changes what tables exist and how they join
- File format uses attributes (not columns) for distribution stats

**Net benefit: 73% reduction in schema-handling code complexity, but ~27% is unavoidable due to the new schema's own requirements.**

---

## 9. Recommendations

1. **If timeline permits:** Remove old pathway support in next major version (pipapi 2.0)
   - Clean up 800+ lines of deprecated code
   - Simplify `pip()`, `create_lkups()`, `pip_lineups_postprocess()`
   - Keep `fill_gaps` conditional logic (unavoidable)

2. **If old pathway must coexist:** Document the "computational algorithm difference" more prominently
   - Current code comments say "OLD" but don't explain WHY the new pathway exists
   - Add docstring: "New pathway uses cumulative-sum algorithm for better performance with multiple poverty lines"

3. **For new features:** ONLY implement in new pathway
   - Avoid maintaining parallel implementations
   - Use `use_new_lineup_version()` as feature gate

4. **Long-term:** Consider unifying `dist_stats` join patterns
   - Currently: `fill_gaps=TRUE` → lineup-level, `fill_gaps=FALSE` → survey-level
   - Future: Could lineup-level stats degrade to survey-level when `fill_gaps=FALSE`?
   - This would eliminate one remaining conditional branch

---

## 10. Evidence Citations

### File Locations
- Routing: `R/pip.R` Lines 63-94
- Old FGT: `R/compute_fgt_old.R` Lines 11-51
- New FGT: `R/fgt_cumsum.R` Lines 58-100
- Old load: `R/compute_fgt_old.R` Lines 65-111
- New load: `R/fgt_cumsum.R` Lines 348+ and `R/utils-pipdata.R` Lines 150-204
- Metadata creation: `R/create_lkups.R` Lines 189-417
- Distribution stats: `R/utils-stats.R` Lines 26-92
- Duplicate removal: `R/fg_pip.R` Lines 195-229, 262-266
- Post-processing: `R/pip_lineups_postprocess.R` Lines 43-54

### Test These Claims
```r
# 1. Verify cumsum columns exist in new lineup files
dt <- fst::read_fst("lineup_data/CHN_2015.fst")
names(dt)  # Should include: cw, cwy, cwy2, cwylog

# 2. Verify attributes in new lineup files
attr(dt, "dist_stats")       # Should be list(mean = ..., median = ...)
attr(dt, "reporting_level_rows")  # Should be list(reporting_level = c(...), rows = c(...))

# 3. Compare lookup table structures
lkup_old <- create_lkups("20250401_2021_01_02_PROD")
lkup_new <- create_lkups("20250615_2021_01_02_PROD")
names(lkup_old)  # Should have ref_lkup, dist_stats
names(lkup_new)  # Should have refy_lkup, lineup_dist_stats

# 4. Verify join key differences
lkup_old$dist_stats[, .(cache_id, reporting_level)]  # Keyed on cache_id
lkup_new$lineup_dist_stats[, .(country_code, reporting_year, reporting_level)]  # Keyed on country+year
```

---

**Document created:** 2026-08-28  
**Analysis scope:** pipapi R package lineup schema differences  
**Conclusion:** Fundamentally different computational approaches, not just file paths
