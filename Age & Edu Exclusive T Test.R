# ============================================================
# Eight independent-samples t-test tables
# ============================================================

input_file  <- "Age & Education Exclusive.csv"
output_file <- "age_education_t_tests.txt"

# FALSE = Welch t-test; TRUE = pooled-variance t-test
equal_variances <- FALSE

# ------------------------------------------------------------
# 1. Read and validate the data
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

ttf_items <- paste0("R1TaskTechnologyFit", c(2, 4, 5, 6))
hc_items  <- paste0("R1HealthConsciousness", 1:3)

required_columns <- c("Age", "Edu", ttf_items, hc_items)
missing_columns <- setdiff(required_columns, names(dat))

if (length(missing_columns) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

# Convert required columns to numeric and check for invalid values.
for (column in required_columns) {
  raw_values <- trimws(as.character(dat[[column]]))
  raw_values[raw_values %in% c("", "NA", "N/A")] <- NA_character_
  
  numeric_values <- suppressWarnings(as.numeric(raw_values))
  invalid <- !is.na(raw_values) & !is.finite(numeric_values)
  
  if (any(invalid)) {
    stop("Non-numeric or infinite value in column: ", column)
  }
  
  dat[[column]] <- numeric_values
}

# ------------------------------------------------------------
# 2. Calculate participant-level averages
# ------------------------------------------------------------

# A score is missing if any of its measurement items is missing.
dat$R1TTF <- rowMeans(dat[, ttf_items, drop = FALSE], na.rm = FALSE)
dat$R1HC  <- rowMeans(dat[, hc_items, drop = FALSE], na.rm = FALSE)

# ------------------------------------------------------------
# 3. Define the eight cases exactly as specified in the PDF
# ------------------------------------------------------------

cases <- list(
  list(
    label = "Age - Case 1", column = "Age",
    group1 = 1, group2 = 2:6
  ),
  list(
    label = "Age - Case 2", column = "Age",
    group1 = 1:2, group2 = 3:6
  ),
  list(
    label = "Age - Case 3", column = "Age",
    group1 = 1:3, group2 = 4:6
  ),
  list(
    label = "Age - Case 4", column = "Age",
    group1 = 1:4, group2 = 5:6
  ),
  list(
    label = "Age - Case 5", column = "Age",
    group1 = 1:5, group2 = 6
  ),
  list(
    label = "Education - Case 1", column = "Edu",
    group1 = 1, group2 = 2:4
  ),
  list(
    label = "Education - Case 2", column = "Edu",
    group1 = 1:2, group2 = 3:4
  ),
  list(
    label = "Education - Case 3", column = "Edu",
    group1 = 1:3, group2 = 4
  )
)

# ------------------------------------------------------------
# 4. Calculate one result row
# ------------------------------------------------------------

make_result <- function(measure, in_group1, in_group2) {
  scores <- dat[[measure]]
  
  x <- scores[in_group1 & !is.na(scores)]
  y <- scores[in_group2 & !is.na(scores)]
  
  result <- data.frame(
    Measure  = measure,
    N1       = length(x),
    Mean1    = if (length(x) > 0) mean(x) else NA_real_,
    SD1      = if (length(x) > 1) sd(x) else NA_real_,
    N2       = length(y),
    Mean2    = if (length(y) > 0) mean(y) else NA_real_,
    SD2      = if (length(y) > 1) sd(y) else NA_real_,
    Diff1_2 = if (length(x) > 0 && length(y) > 0) {
      mean(x) - mean(y)
    } else {
      NA_real_
    },
    t        = NA_real_,
    df       = NA_real_,
    p        = NA_real_,
    CI_low   = NA_real_,
    CI_high  = NA_real_,
    Note     = "OK",
    stringsAsFactors = FALSE
  )
  
  if (length(x) < 2 || length(y) < 2) {
    result$Note <- "Fewer than 2 usable scores in a group"
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
    result$Note <- paste(
      "Test unavailable:",
      gsub("[\r\n\t]+", " ", conditionMessage(test))
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
# 5. Format results for the text file
# ------------------------------------------------------------

format_table <- function(tab) {
  decimal_columns <- c(
    "Mean1", "SD1", "Mean2", "SD2", "Diff1_2",
    "t", "df", "CI_low", "CI_high"
  )
  
  for (column in decimal_columns) {
    values <- tab[[column]]
    tab[[column]] <- ifelse(
      is.na(values),
      "NA",
      sprintf("%.3f", values)
    )
  }
  
  p_values <- tab$p
  tab$p <- ifelse(
    is.na(p_values),
    "NA",
    ifelse(
      p_values < 0.001,
      "<0.001",
      sprintf("%.3f", p_values)
    )
  )
  
  tab
}

# ------------------------------------------------------------
# 6. Write all eight tables to one text file
# ------------------------------------------------------------

write_report <- function(path) {
  out <- file(path, open = "wt")
  on.exit(close(out), add = TRUE)
  
  method <- if (equal_variances) "Pooled-variance" else "Welch"
  
  cat(
    "Independent-samples t-tests\n",
    sprintf("Method: %s; two-sided; 95%% confidence intervals.\n", method),
    "Scores are item averages requiring all items.\n",
    "N1 and N2 are the usable score counts for each group.\n",
    "Diff1_2 and its confidence interval are Group 1 minus Group 2.\n",
    "P-values are unadjusted; respondents are reused across cases.\n",
    "Edu code 5 is excluded from the education cases only.\n",
    "NA means unavailable or not calculable; see Note.\n",
    file = out,
    sep = ""
  )
  
  for (i in seq_along(cases)) {
    specification <- cases[[i]]
    demographic <- dat[[specification$column]]
    
    # Explicit membership prevents unlisted codes from being
    # accidentally assigned to either group.
    in_group1 <- !is.na(demographic) &
      (demographic %in% specification$group1)
    
    in_group2 <- !is.na(demographic) &
      (demographic %in% specification$group2)
    
    unassigned <- !(in_group1 | in_group2)
    
    unlisted_codes <- sort(unique(
      demographic[unassigned & !is.na(demographic)]
    ))
    
    results <- do.call(
      rbind,
      lapply(c("R1TTF", "R1HC"), function(measure) {
        make_result(measure, in_group1, in_group2)
      })
    )
    
    rownames(results) <- NULL
    
    cat(
      sprintf("\nTable %d: %s\n", i, specification$label),
      "Group 1 codes: ",
      paste(specification$group1, collapse = ", "),
      "\nGroup 2 codes: ",
      paste(specification$group2, collapse = ", "),
      "\nUnassigned rows, including missing or unlisted codes: ",
      sum(unassigned),
      "\n",
      file = out,
      sep = ""
    )
    
    if (length(unlisted_codes) > 0) {
      cat(
        "Unlisted codes present: ",
        paste(unlisted_codes, collapse = ", "),
        "\n",
        file = out,
        sep = ""
      )
    }
    
    if (specification$column == "Edu") {
      cat(
        "Excluded rows with Edu code 5: ",
        sum(demographic == 5, na.rm = TRUE),
        "\n",
        file = out,
        sep = ""
      )
    }
    
    write.table(
      format_table(results),
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