#' Create weighted conversion factor lookup table
#'
#' Generates a lookup table that maps unspecified resource codes to their corresponding
#' specific resource categories, enabling the application of weighted conversion factors
#' for harvest data analysis. This function handles complex matching logic including
#' prefix matching and range-based resource code matching.
#'
#' @details
#' The function filters out unspecified bird and egg resources that have established
#' conversion factors (Naves and Fall 2017) and creates two types of joins:
#' \itemize{
#'   \item Character-based joins that match unspecified resources by prefix
#'   \item Range-based joins that handle unspecified resources requiring multiple
#'         resource code prefixes
#' }
#'
#' Salmon roe (resource codes 118000000-118999999) are explicitly excluded to avoid
#' bias in salmon weight calculations.
#'
#' @param outputFilePath Character string specifying the output file path for the
#'   lookup table. Defaults to "./CSV/00 - Lookup Codes/unspecifiedResourceLookup.csv".
#'
#' @return
#' Invisibly returns `NULL`. Writes a CSV file to `outputFilePath` containing:
#' \describe{
#'   \item{resourceTarget}{Resource code for unspecified category (numeric)}
#'   \item{resNameTarget}{Resource name for unspecified category (character)}
#'   \item{resourceSource}{Specific resource code to use for conversion factor (numeric)}
#'   \item{resNameSource}{Specific resource name (character)}
#'   \item{specList}{Species list identifier (character)}
#'   \item{detailDigit}{Final digit of resource code for matching specificity (integer)}
#' }
#'
#' @references
#' Naves, L. C., & Fall, J. A. (2017). Subsistence harvests of fish and wildlife
#' by Alaska residents, 2015. Alaska Department of Fish and Game, Division of
#' Subsistence Technical Paper No. 422.
#'
#' @note
#' Requires data files:
#' \itemize{
#'   \item "./CSV/00 - Lookup Codes/fullResList_raw.csv" — full resource list
#' }
#' Depends on the \code{fuzzyjoin} package for fuzzy matching operations.
#'
#' @examples
#' \dontrun{
#'   getWtdCFLookup()
#'   getWtdCFLookup(outputFilePath = "./output/custom_lookup.csv")
#' }
#'
#' @export

getWtdCFLookup <- function(outputFilePath = "./CSV/00 - Lookup Codes/unspecifiedResourceLookup.csv") {

  options(scipen = 999) # Very important for parsing resource codes correctly below

  reslist <- read_csv("./CSV/00 - Lookup Codes/fullResList_raw.csv")

  unspecBirdList <- c(431202990,411202990,411202993,411202991,411202992,411202999,
                      411202994,410404990,410404993,410404991,410404992,410404999,
                      410404994,430404990,431204990,411204990,411204993,411204991,
                      411204992,411204999,411204994,411011000,411011003,411011001,
                      411011002,411011004,410210990,410210993,410210991,410210992,
                      410210999,410210994,411208990,411208993,411208991,411208992,
                      411208999,411208994,431802990,421802990,421802993,421802991,
                      421802992,421802999,421802994,411210993,411210991,411210992,
                      411210999,411210994,431216990,411216990,411216993,411216991,
                      411216992,411216999,411216994,430216990,410216990,410216993,
                      410216991,410216992,410216999,410216994,431218990,411218990,
                      411218993,411218991,411218992,411218999,411218994,431006990,
                      411006990,411006993,411006991,411006992,411006999,411006994,
                      431804990,421804990,421804993,421804991,421804992,421804999,
                      421804994,431222990,411222992,411222990,411222993,411222991,
                      411222999,411222994,430226990,410226990,410226993,410226999,
                      410226991,410226992,410226994,431099010,411099010,411099013,
                      411099011,411099012,411099014,430699000,410699000,410699003,
                      410699001,410699002,410699009,410699004,430232990,410232990,
                      410232993,410232991,410232992,410232999,410232994,431226990,
                      411226990,411226993,411226991,411226992,411226999,411226994,
                      411010990,411010993,411010991,411010992,411010999,411010994)

  unspec <- reslist %>%
    filter(str_detect(resName, 'Unspecified')) %>%
    mutate(
      # Extract the digits of the resource code before '99'
      result = case_when(
        str_detect(resource, "99(?=00)") ~ str_extract(resource, ".*(?=99)"),
        str_detect(resource, "00") ~ str_extract(resource, ".*(?=00)"),
        str_ends(resource, "99\\d") ~ str_sub(resource, start = 1, end = 6),
        TRUE ~ NA_character_
      ),

      # Use the final digit to allow for more specific matching to resList
      detailDigit = str_sub(as.character(resource), start = -1),

      # Add some oddball resource codes that don't follow the '99' convention
      result = case_when(resName == "Unspecified fungus" ~ "602046",
                         resName == "Unspecified sandpipers" ~ "41108",
                         resource %in% c(501008990,501008991,501008992,501008993) ~ "501008",
                         between(resource, 119000000, 119999999) ~ "11",
                         between(resource, 117000000, 117999999) ~ "117",
                         TRUE ~ result),

      resultNumStart = case_when(resource %in% c(119000000,119000001,119000002,119000003) ~ 110000000,
                                 resource %in% c(129700000,129700001,129700002,129700003) ~ 120000000,
                                 resource %in% c(129800000,129800001,129800002,129800003) ~ 124600000,
                                 resource %in% c(290000000,290000001,290000002,290000003) ~ 210000000,
                                 TRUE ~ NA_real_),

      resultNumEnd = case_when(resource %in% c(119000000,119000001,119000002,119000003) ~ 116999999,
                               resource %in% c(129700000,129700001,129700002,129700003) ~ 124499999,
                               resource %in% c(129800000,129800001,129800002,129800003) ~ 126412999,
                               resource %in% c(290000000,290000001,290000002,290000003) ~ 229999999,
                               TRUE ~ NA_real_)
    ) %>%

    # Remove bird and egg resources that have conversion factors from Naves and Fall 2017
    filter(!resource %in% unspecBirdList)

  unspecChar <- unspec %>%
    filter(is.na(resultNumStart))

  unspecRange <- unspec %>%
    filter(!is.na(resultNumStart))

  # Uncomment line below to export intermediate table and update codings
  # write_csv(unspec, file = "~/Local Drafts/unspecifiedResourceLookup.csv")

  reslistSpec <- reslist %>%
    # Don't want to match to 'Unspecified' resources on the right-hand table except
    # for all unspecified bird groups that do have conversion factors
    filter(
      !str_detect(resName, "Unspecified") |
        (str_detect(resName, "Unspecified") & resource %in% unspecBirdList)
    ) %>%
    mutate(detailDigit = str_sub(as.character(resource), start = -1))

  # Join based on a single prefix (e.g., 'Unspecified whitefishes' is '1264')
  join1 <- fuzzyjoin::fuzzy_left_join(
    unspecChar, reslistSpec,
    by = c("result" = "resource", "specList" = "specList", "detailDigit" = "detailDigit"),
    match_fun = list(function(x, y) startsWith(as.character(y), x),
                     function(x, y) x == y,
                     function(x, y) x == y)
  ) %>%
    filter(!between(resource.y,118000000,118999999)) # Do not use salmon roe to calculate salmon weights


  # Join based on a range of resource codes; used where multiple code prefixes are needed
  # for a single unspecified resource (e.g., 'Unspecified marine nonsalmon fish')
  join2 <- fuzzyjoin::fuzzy_inner_join(
    unspecRange,
    reslistSpec,
    by = c("resultNumStart" = "resource", "resultNumEnd" = "resource", "specList" = "specList", "detailDigit" = "detailDigit"),
    match_fun = list(`<=`,
                     `>=`,
                     function(x, y) x == y,
                     function(x, y) x == y)
  )

  # Bind joined tables to join to h
  lookupTbl <- bind_rows(join1,join2) %>%
    select(resource.x, resName.x, resource.y, resName.y, specList.x, detailDigit.x) %>%
    rename(resourceTarget = resource.x,
           resNameTarget = resName.x,
           resourceSource = resource.y,
           resNameSource = resName.y,
           specList = specList.x,
           detailDigit = detailDigit.x) %>%
    mutate(detailDigit = as.integer(detailDigit))

  write_csv(lookupTbl, file = outputFilePath)

}
