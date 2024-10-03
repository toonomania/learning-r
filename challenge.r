if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

validate_columns <- function(data) {
  required_columns <- c("operation_type", "starttime", "endtime")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Error: Missing columns in CSV file:", paste(missing_columns, collapse = ", ")))
  }
}

validate_row <- function(row) {
  start <- suppressWarnings(as.numeric(row$starttime))
  end <- suppressWarnings(as.numeric(row$endtime))
  
  if (is.na(start) || is.na(end)) {
    return("Starttime and Endtime should be numeric")
  }
  
  if (start >= end) {
    return("Starttime should be less than Endtime")
  }
  
  return(NULL)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Error: Please provide a path to the CSV file as a command-line argument.")
}

csv_file <- args[1]

if (!file.exists(csv_file)) {
  stop(paste("Error: The file", csv_file, "does not exist. Please provide a valid CSV file."))
}

data <- read.csv(csv_file)

validate_columns(data)

cat("CSV columns validated successfully.\n")

invalid_rows <- data.frame()
valid_rows <- data.frame()

for (i in 1:nrow(data)) {
  row <- data[i, ]
  validation_message <- validate_row(row)
  
  if (!is.null(validation_message)) {
    row$error_message <- validation_message
    invalid_rows <- rbind(invalid_rows, row)
  } else {
    valid_rows <- rbind(valid_rows, row)
  }
}

if (nrow(invalid_rows) > 0) {
  cat("Warning: The following rows contain invalid data and will be ignored:\n")
  print(invalid_rows)
}

if (nrow(valid_rows) == 0) {
  stop("Error: No valid rows found in the CSV file. Please check the data and try again.")
}

data <- valid_rows

data$starttime <- as.numeric(data$starttime)
data$endtime <- as.numeric(data$endtime)

data$duration <- data$endtime - data$starttime

operation_summary <- data %>%
  group_by(operation_type) %>%
  summarise(
    Max = max(duration),
    Min = min(duration),
    Mean = mean(duration),
    Median = median(duration)
  )

cat("Operation Duration Summary Statistics:\n")
print(operation_summary)
