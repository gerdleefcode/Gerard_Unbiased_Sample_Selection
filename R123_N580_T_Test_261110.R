# ============================================================
# Five demographic tables of independent-samples t-tests
# ============================================================

input_file  <- "Subset_N580_R123_ALL.csv"
output_file <- "independent_t_test_tables.txt"

# FALSE: Welch test; TRUE: pooled-variance t-test
equal_variances <- FALSE

# ------------------------------------------------------------
# 1. Read and validate data
# ------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "Cannot find ", input_file,
    "\nCurrent working directory: ", getwd()
  )
}

dat <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "N/A")
)

if (nrow(dat) == 0) {
  stop("The CSV contains no data rows.")
}

if (anyDuplicated(names(dat))) {
  stop("The CSV contains duplicate column names.")
}

# Table labels and corresponding columns
demographics <- c(
  "Age"          = "Age",
  "Gender"       = "Gender",
  "Education"    = "Edu",
  "Phone System" = "PhoneSystem",
  "Apps Type"    = "AppsType"
)

# Measurement items, in the requested outcome order
items <- list(
  R1TTF = paste0("R1TaskTechnologyFit", c(2, 4, 5, 6)),
  R1HC  = paste0("R1HealthConsciousness", 1:3)
)

required_columns <- unique(c(
  unname(demographics),
  unlist(items, use.names = FALSE)
))

missing_columns <- setdiff(required_columns, names(dat))

if (length(missing_columns) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_columns, collapse = ", "),
    "\nCheck that you are using the updated CSV."
  )
}

# Demographic values must be coded 1 or 2; missing values are allowed.
for (column in unname(demographics)) {
  group <- trimws(as.character(dat[[column]]))
  group[group %in% c("", "NA", "N/A")] <- NA_character_
  
  unexpected <- unique(group[
    !is.na(group) & !(group %in% c("1", "2"))
  ])
  
  if (length(unexpected) > 0) {
    stop(
      "Unexpected code(s) in ", column, ": ",
      paste(unexpected, collapse = ", "),
      ". Expected 1 or 2."
    )
  }
  
  dat[[column]] <- group
}

# Check that item responses are numeric.
for (column in unique(unlist(items, use.names = FALSE))) {
  raw_values <- trimws(as.character(dat[[column]]))
  raw_values[raw_values %in% c("", "NA", "N/A")] <- NA_character_
  
  values <- suppressWarnings(as.numeric(raw_values))
  invalid <- !is.na(raw_values) & !is.finite(values)
  
  if (any(invalid)) {
    stop("Non-numeric or infinite value in item column: ", column)
  }
  
  dat[[column]] <- values
}

# ------------------------------------------------------------
# 2. Calculate participant-level item means
# ------------------------------------------------------------

# All items are required for a composite score.
for (variable in names(items)) {
  dat[[variable]] <- rowMeans(
    dat[, items[[variable]], drop = FALSE],
    na.rm = FALSE
  )
}

# ------------------------------------------------------------
# 3. Calculate one result row
# ------------------------------------------------------------

make_result <- function(variable, demographic_column) {
  scores <- dat[[variable]]
  group  <- dat[[demographic_column]]
  
  x <- scores[!is.na(group) & group == "1" & !is.na(scores)]
  y <- scores[!is.na(group) & group == "2" & !is.na(scores)]
  
  result <- data.frame(
    Measure  = variable,
    N_1      = length(x),
    Mean_1   = if (length(x) > 0) mean(x) else NA_real_,
    SD_1     = if (length(x) > 1) sd(x) else NA_real_,
    N_2      = length(y),
    Mean_2   = if (length(y) > 0) mean(y) else NA_real_,
    SD_2     = if (length(y) > 1) sd(y) else NA_real_,
    Diff1_2 = NA_real_,
    t        = NA_real_,
    df       = NA_real_,
    p        = NA_real_,
    CI_low   = NA_real_,
    CI_high  = NA_real_,
    Note     = "OK",
    stringsAsFactors = FALSE
  )
  
  if (length(x) > 0 && length(y) > 0) {
    result$Diff1_2 <- mean(x) - mean(y)
  }
  
  if (length(x) < 2 || length(y) < 2) {
    result$Note <- "Fewer than 2 scores in at least one group"
    return(result)
  }
  
  test <- tryCatch(
    t.test(
      x,
      y,
      paired = FALSE,
      alternative = "two.sided",
      var.equal = equal_variances,
      conf.level = 0.95
    ),
    error = function(e) e
  )
  
  if (inherits(test, "error")) {
    result$Note <- gsub(
      "[\r\n\t]",
      " ",
      paste("Test unavailable:", conditionMessage(test))
    )
    return(result)
  }
  
  result$t       <- unname(test$statistic)
  result$df      <- unname(test$parameter)
  result$p       <- test$p.value
  result$CI_low  <- unname(test$conf.int[1])
  result$CI_high <- unname(test$conf.int[2])
  
  result
}

# ------------------------------------------------------------
# 4. Create the five tables
# ------------------------------------------------------------

tables <- lapply(unname(demographics), function(column) {
  rows <- lapply(names(items), function(variable) {
    make_result(variable, column)
  })
  
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL
  tab
})

# Format results for the text file.
format_table <- function(tab) {
  decimal_columns <- c(
    "Mean_1", "SD_1", "Mean_2", "SD_2",
    "Diff1_2", "t", "df", "CI_low", "CI_high"
  )
  
  for (column in decimal_columns) {
    values <- tab[[column]]
    tab[[column]] <- ifelse(
      is.na(values),
      NA_character_,
      sprintf("%.3f", values)
    )
  }
  
  p_values <- tab$p
  tab$p <- ifelse(
    is.na(p_values),
    NA_character_,
    ifelse(p_values < 0.001, "<0.001", sprintf("%.3f", p_values))
  )
  
  tab
}

# ------------------------------------------------------------
# 5. Save all tables to one text file
# ------------------------------------------------------------

write_report <- function(path) {
  out <- file(path, open = "wt")
  on.exit(close(out), add = TRUE)
  
  method <- if (equal_variances) "Pooled-variance" else "Welch"
  
  cat(
    "Independent-samples t-tests\n",
    sprintf("Method: %s; two-sided; 95%% confidence intervals.\n", method),
    "P-values are unadjusted.\n",
    "Group 1 and Group 2 refer to demographic codes 1 and 2.\n",
    "Diff1_2 and its confidence interval are Group 1 minus Group 2.\n",
    "Composite scores are item means requiring all items.\n",
    "Missing group values or composite scores are excluded from each test.\n",
    "NA means unavailable or not calculable; see Note.\n",
    file = out,
    sep = ""
  )
  
  for (i in seq_along(tables)) {
    cat(
      sprintf("\nTable %d: %s\n", i, names(demographics)[i]),
      file = out
    )
    
    write.table(
      format_table(tables[[i]]),
      file = out,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      na = "NA"
    )
  }
}

write_report(output_file)

message(
  "Saved results to: ",
  normalizePath(output_file, winslash = "/", mustWork = TRUE)
)