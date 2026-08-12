#' Calculate weighted conversion factors for unspecified resources
#'
#' For each "unspecified" resource row in the conversion factor data frame (where
#' convFact is NA), derive a conversion factor AND an lbsToDefault value as the
#' harvest-weighted average of similar (source) species, matched to the SAME unit
#' of measure as the unspecified target.
#'
#' Missing-source detection is harvest-driven: a source that was actually
#' harvested in the target's units but lacks a convFact row is skipped from the
#' weighted average and named in a warning. Un-harvested sources are legitimately
#' absent from the conversion factor table and are not warned about.
#'
#' @param harvData     Household harvest data frame.
#' @param convFactData Conversion factor data frame (NA convFact rows = targets).
#' @param lookupData   Unspecified lookup table mapping targets to sources.
#' @param harvAmtList  Character vector of harvest-amount column names in harvData.
#'
#' @return Updated copy of convFactData with convFact and lbsToDefault filled in
#'   where possible. Inputs unmodified. Diagnostics attached as attr "diagnostics".
calcConvFactor <- function(harvData, convFactData, lookupData, harvAmtList) {

  # ---- 1. Validate required columns -----------------------------------------
  reqHarv   <- c("resource", "units", harvAmtList)
  reqConv   <- c("resource", "units", "convFact", "lbsToDefault", "defaultUnits",
                 "resName")
  reqLookup <- c("resourceTarget", "resNameTarget",
                 "resourceSource", "resNameSource")

  checkCols <- function(df, needed, dfName) {
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) {
      stop(sprintf("`%s` is missing required column(s): %s",
                   dfName, paste(missing, collapse = ", ")), call. = FALSE)
    }
  }
  checkCols(harvData,     reqHarv,   "harvData")
  checkCols(convFactData, reqConv,   "convFactData")
  checkCols(lookupData,   reqLookup, "lookupData")

  # ---- 1b. Coerce join keys to a common type (defensive) --------------------
  harvData     <- harvData     %>% mutate(across(c(resource, units), as.integer))
  convFactData <- convFactData %>% mutate(
    across(c(resource, units, defaultUnits), as.integer),
    across(c(convFact, lbsToDefault), as.numeric)
  )
  lookupData   <- lookupData   %>% mutate(
    across(c(resourceTarget, resourceSource), as.integer)
  )

  # ---- 2. Identify unspecified targets --------------------------------------
  targetData <- convFactData %>%
    filter(is.na(convFact)) %>%
    distinct(resource, units, resName)

  if (nrow(targetData) == 0) {
    message("No unspecified resources (NA convFact) found; nothing to calculate.")
    emptyDiag <- tibble(resource = integer(), units = integer(),
                        resName = character(), convFact_new = double(),
                        lbsToDefault_new = double(), calculated = logical(),
                        nSources = integer(), status = character(),
                        reason = character(), skippedSources = character())
    attr(convFactData, "diagnostics") <- emptyDiag
    return(convFactData)
  }

  # ---- 3. Long harvest table (summed per resource + units) ------------------
  harvLong <- harvData %>%
    select(resource, units, all_of(harvAmtList)) %>%
    pivot_longer(all_of(harvAmtList),
                 names_to = "harvCol", values_to = "harvAmt") %>%
    group_by(resource, units) %>%
    summarise(harvAmt_sum = sum(harvAmt, na.rm = TRUE), .groups = "drop")

  # ---- 4. Known source convFact rows (all units retained) -------------------
  # Sources are matched to a target on BOTH the mapped resource AND the units,
  # so keep every known-convFact row regardless of unit.
  sourceConv <- convFactData %>%
    filter(!is.na(convFact)) %>%
    select(resourceSource    = resource,
           unitsSource        = units,
           convFactSource     = convFact,
           lbsToDefaultSource = lbsToDefault)

  # ---- 5. Helper: compute one target (matched on units) ---------------------
  calcOneTarget <- function(tgtResource, tgtUnits, tgtResName) {

    # Source species this target maps to.
    lookupSources <- lookupData %>%
      filter(resourceTarget == tgtResource) %>%
      distinct(resourceSource, resNameSource)

    if (nrow(lookupSources) == 0) {
      return(list(convFact = NA_real_, lbsToDefault = NA_real_,
                  calculated = FALSE, nSources = 0L, status = "failed",
                  reason = "no matching sources in lookup table",
                  skippedSources = NA_character_))
    }

    # Of the mapped sources, which were ACTUALLY HARVESTED in the target's units?
    # These are the only sources that (a) contribute and (b) *require* a convFact
    # row. Un-harvested sources are legitimately absent and are not warned about.
    harvestedSources <- lookupSources %>%
      inner_join(
        harvLong %>%
          filter(units == tgtUnits, harvAmt_sum > 0) %>%
          select(resourceSource = resource, harvAmt_sum),
        by = "resourceSource"
      )

    # Edge case 3: no mapped source was harvested in the target's units at all.
    if (nrow(harvestedSources) == 0) {
      return(list(convFact = NA_real_, lbsToDefault = NA_real_,
                  calculated = FALSE, nSources = 0L, status = "failed",
                  reason = "no mapped source harvested in the target's units",
                  skippedSources = NA_character_))
    }

    # Attach known convFact rows in the target's units.
    withConv <- harvestedSources %>%
      left_join(sourceConv %>% filter(unitsSource == tgtUnits),
                by = "resourceSource")

    # REQUIRED-but-MISSING: harvested in target's units but no convFact row.
    missingSources <- withConv %>% filter(is.na(convFactSource))
    skippedNames <- if (nrow(missingSources) > 0) {
      paste(missingSources$resNameSource, collapse = "; ")
    } else NA_character_

    # Contributing sources: harvested AND have a convFact row in target's units.
    srcSet <- withConv %>% filter(!is.na(convFactSource))

    if (nrow(srcSet) == 0) {
      return(list(convFact = NA_real_, lbsToDefault = NA_real_,
                  calculated = FALSE, nSources = 0L, status = "failed",
                  reason = "harvested sources lack convFact rows in target's units",
                  skippedSources = skippedNames))
    }

    # Harvest-weighted averages, both in the target's unit of measure.
    convFact_wtd     <- weighted.mean(srcSet$convFactSource,     w = srcSet$harvAmt_sum)
    lbsToDefault_wtd <- weighted.mean(srcSet$lbsToDefaultSource, w = srcSet$harvAmt_sum)

    list(convFact = convFact_wtd, lbsToDefault = lbsToDefault_wtd,
         calculated = TRUE, nSources = n_distinct(srcSet$resourceSource),
         status = "weighted", reason = NA_character_,
         skippedSources = skippedNames)
  }

  # ---- 6. Loop over targets -------------------------------------------------
  results <- targetData %>%
    mutate(calc = pmap(list(resource, units, resName),
                       ~ calcOneTarget(..1, ..2, ..3))) %>%
    mutate(
      convFact_new     = map_dbl(calc, "convFact"),
      lbsToDefault_new = map_dbl(calc, "lbsToDefault"),
      calculated       = map_lgl(calc, "calculated"),
      nSources         = map_int(calc, "nSources"),
      status           = map_chr(calc, "status"),
      reason           = map_chr(calc, "reason"),
      skippedSources   = map_chr(calc, "skippedSources")
    ) %>%
    # Guard: a computed NA is a FAILURE, never a silent success.
    mutate(
      reason     = if_else(calculated & is.na(convFact_new),
                           "computed NA (check source values)", reason),
      status     = if_else(calculated & is.na(convFact_new), "failed", status),
      calculated = calculated & !is.na(convFact_new)
    ) %>%
    select(resource, units, resName, convFact_new, lbsToDefault_new,
           calculated, nSources, status, reason, skippedSources)

  # ---- 7. Report ------------------------------------------------------------
  calculatedData <- results %>% filter(calculated)
  failedData     <- results %>% filter(!calculated)

  if (nrow(calculatedData) > 0) {
    message("Calculated conversion factors for the following unspecified resources:")
    calculatedData %>%
      distinct(resName, units, nSources) %>%
      pwalk(~ message(sprintf("  - %s (units: %s) [sources used: %d]",
                              ..1, ..2, ..3)))
  }

  # Warn about skipped sources (harvested in target's units but no convFact row).
  skippedData <- results %>% filter(!is.na(skippedSources))
  if (nrow(skippedData) > 0) {
    warning(
      "Some source species were harvested in the target's units but lack a ",
      "conversion factor row, and were skipped from the weighted average. Add ",
      "rows to the conversion factor table for these specified resources in the ",
      "matching units:\n",
      paste0(
        skippedData %>%
          distinct(resName, units, skippedSources) %>%
          pmap_chr(~ sprintf("  - %s (units: %s): %s", ..1, ..2, ..3)),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  if (nrow(failedData) > 0) {
    message("Could not calculate the following unspecified resources:")
    failedData %>%
      distinct(resName, units, reason) %>%
      pwalk(~ message(sprintf("  - %s (units: %s) -> %s", ..1, ..2, ..3)))
    message(sprintf(
      paste0("Conversion factor cannot be calculated using weighted average; ",
             "please manually add a conversion factor for %d resource(s) to ",
             "the conversion factor file (convFact_final.csv)."),
      nrow(failedData)))
  }

  # ---- 8. Merge convFact AND lbsToDefault back ------------------------------
  updatedData <- convFactData %>%
    left_join(results %>% select(resource, units, convFact_new, lbsToDefault_new),
              by = c("resource", "units")) %>%
    mutate(
      convFact     = if_else(is.na(convFact) & !is.na(convFact_new),
                             convFact_new, convFact),
      lbsToDefault = if_else(is.na(lbsToDefault) & !is.na(lbsToDefault_new),
                             lbsToDefault_new, lbsToDefault)
    ) %>%
    select(-convFact_new, -lbsToDefault_new)

  attr(updatedData, "diagnostics") <- results
  updatedData
}
