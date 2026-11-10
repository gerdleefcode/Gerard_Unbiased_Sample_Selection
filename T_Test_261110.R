# ============================================================
# Independent-samples t-tests: Cleaned versus Retained
# Uses base R only; no packages required
# ============================================================

# 1. File locations -------------------------------------------------

input_file <- "Subset_N580_R1_Only.csv"

output_file <- file.path(
  dirname(input_file),
  "R1_EntryStatus_ttests.txt"
)

if (!file.exists(input_file)) {
  stop("CSV not found. Set the working directory or edit input_file.")
}

# 2. Define columns -------------------------------------------------

ttf_items <- c(
  "R1TaskTechnologyFit2",
  "R1TaskTechnologyFit4",
  "R1TaskTechnologyFit5",
  "R1TaskTechnologyFit6"
)

hc_items <- c(
  "R1HealthConsciousness1",
  "R1HealthConsciousness2",
  "R1HealthConsciousness3"
)

demographic_columns <- c(
  "Age", "Gender", "Edu", "PhoneSystem", "AppsType"
)

required_columns <- c(
  "EntryStatus",
  ttf_items,
  hc_items,
  demographic_columns
)

# 3. Import and validate data ---------------------------------------

dat <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "N/A")
)

missing_columns <- setdiff(required_columns, names(dat))

if (length(missing_columns) > 0L) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Keep only variables needed for this task.
# DatasetCategory and UserID are not used.
dat <- dat[, required_columns, drop = FALSE]

numeric_columns <- c(
  ttf_items, hc_items, demographic_columns
)

if (!all(vapply(
  dat[numeric_columns],
  is.numeric,
  logical(1)
))) {
  stop("All item scores and demographic codes must be numeric.")
}

dat$EntryStatus <- trimws(dat$EntryStatus)

if (anyNA(dat$EntryStatus) ||
    any(!dat$EntryStatus %in% c("Cleaned", "Retained"))) {
  stop("EntryStatus must contain only Cleaned and Retained.")
}

dat$EntryStatus <- factor(
  dat$EntryStatus,
  levels = c("Cleaned", "Retained")
)

group_n <- table(dat$EntryStatus)

# The input must contain both groups: 935 rows in total.
if (any(as.integer(group_n) != c(355L, 580L))) {
  stop(sprintf(
    paste0(
      "Group-count mismatch: Cleaned = %d; Retained = %d. ",
      "Expected Cleaned = 355 and Retained = 580. ",
      "Ensure the CSV contains both groups."
    ),
    as.integer(group_n["Cleaned"]),
    as.integer(group_n["Retained"])
  ))
}

# 4. Calculate composite scores ------------------------------------

# Missing-data policy:
# All four TTF items / all three HC items are required.
# An incomplete composite is set to NA, rather than imputed.

dat$R1TTF <- rowMeans(
  dat[, ttf_items, drop = FALSE],
  na.rm = FALSE
)

dat$R1HC <- rowMeans(
  dat[, hc_items, drop = FALSE],
  na.rm = FALSE
)

# Use the requested label in the results.
dat$Education <- dat$Edu

test_variables <- c(
  "R1TTF",
  "R1HC",
  "Age",
  "Gender",
  "Education",
  "PhoneSystem",
  "AppsType"
)

# 5. Run the seven Welch independent-samples t-tests -----------------

run_t_test <- function(variable) {
  
  x <- dat[[variable]][dat$EntryStatus == "Cleaned"]
  y <- dat[[variable]][dat$EntryStatus == "Retained"]
  
  # Exclude missing values separately for each variable/group.
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  if (length(x) < 2L || length(y) < 2L) {
    stop("Insufficient non-missing observations for: ", variable)
  }
  
  test <- t.test(
    x = x,
    y = y,
    alternative = "two.sided",
    paired = FALSE,
    var.equal = FALSE,       # Welch test
    conf.level = 0.95
  )
  
  data.frame(
    Variable = variable,
    Cleaned_n = length(x),
    Cleaned_mean = mean(x),
    Cleaned_SD = sd(x),
    Retained_n = length(y),
    Retained_mean = mean(y),
    Retained_SD = sd(y),
    Mean_difference = mean(x) - mean(y),
    CI_lower = as.numeric(test$conf.int[1]),
    CI_upper = as.numeric(test$conf.int[2]),
    t = as.numeric(test$statistic),
    df = as.numeric(test$parameter),
    p = test$p.value,
    stringsAsFactors = FALSE
  )
}

results <- do.call(
  rbind,
  lapply(test_variables, run_t_test)
)

rownames(results) <- NULL

# Additional, optional multiple-testing adjustment across 7 tests.
# Raw p values remain available in the "p" column.
results$p_Holm <- p.adjust(results$p, method = "holm")

# 6. Format the results table ---------------------------------------

format_p <- function(p) {
  ifelse(p < 0.001, "<.001", sprintf("%.3f", p))
}

results_table <- data.frame(
  Variable = results$Variable,
  
  `Cleaned n` = results$Cleaned_n,
  
  `Cleaned Mean (SD)` = sprintf(
    "%.3f (%.3f)",
    results$Cleaned_mean,
    results$Cleaned_SD
  ),
  
  `Retained n` = results$Retained_n,
  
  `Retained Mean (SD)` = sprintf(
    "%.3f (%.3f)",
    results$Retained_mean,
    results$Retained_SD
  ),
  
  `Diff C-R` = sprintf("%.3f", results$Mean_difference),
  
  `95% CI for difference` = sprintf(
    "[%.3f, %.3f]",
    results$CI_lower,
    results$CI_upper
  ),
  
  t = sprintf("%.3f", results$t),
  df = sprintf("%.2f", results$df),
  p = format_p(results$p),
  p_Holm = format_p(results$p_Holm),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Capture a wide, readable text table without splitting columns.
previous_width <- getOption("width")
options(width = 220)

table_text <- capture.output(
  print(
    results_table,
    row.names = FALSE,
    quote = FALSE,
    right = FALSE
  )
)

options(width = previous_width)

# 7. Build and save the text report ---------------------------------

report <- c(
  "INDEPENDENT-SAMPLES T-TEST REPORT",
  "Comparison: EntryStatus = Cleaned versus Retained",
  sprintf("Input file: %s", basename(input_file)),
  
  sprintf(
    "Group sizes: Cleaned = %d; Retained = %d",
    as.integer(group_n["Cleaned"]),
    as.integer(group_n["Retained"])
  ),
  
  "",
  "Method: two-sided Welch independent-samples t-tests.",
  "R1TTF = average of R1TaskTechnologyFit2, 4, 5 and 6.",
  "R1HC  = average of R1HealthConsciousness1, 2 and 3.",
  "Edu is reported as Education.",
  "DatasetCategory is excluded from the analysis.",
  "",
  table_text,
  "",
  "NOTES",
  "1. SD = standard deviation; CI = confidence interval.",
  "2. All mean differences and CIs are Cleaned minus Retained.",
  "3. A positive difference indicates a higher Cleaned-group mean.",
  "4. p contains the unadjusted, two-sided p value.",
  "5. p_Holm adjusts the seven p values using the Holm method.",
  "6. The 95% confidence intervals are not multiplicity-adjusted.",
  "7. All items are required to calculate each composite score.",
  "8. Missing values are excluded separately for each tested variable.",
  "9. Demographic tests compare numeric category codes.",
  "10. Confirm demographic codes have consistent meanings across records.",
  "11. Categorical tests are generally preferred for demographic distributions."
)

writeLines(report, con = output_file)

# Display the same report in the R console.
cat(report, sep = "\n")

cat(
  "\n\nReport saved to: ",
  normalizePath(output_file, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)

# Optional: open the saved text report for reading.
# file.show(output_file)

# Full-precision numerical results remain available in:
# results