# Outputs a CSV file with the correlation matrix for the selected col_correlation_matrix
library(config)

# Load configurations
config_path <- file.path("..", "input", "config.ini")

# Read config file manually (since config.ini format needs custom parsing)
config_lines <- readLines(config_path)

# Parse the config file
parse_config <- function(config_lines) {
  config_list <- list()
  current_section <- NULL
  
  for (line in config_lines) {
    line <- trimws(line)
    if (line == "" || startsWith(line, "#")) next
    
    if (startsWith(line, "[") && endsWith(line, "]")) {
      current_section <- substr(line, 2, nchar(line) - 1)
      config_list[[current_section]] <- list()
    } else if (grepl("=", line) && !is.null(current_section)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      config_list[[current_section]][[key]] <- value
    }
  }
  
  return(config_list)
}

config <- parse_config(config_lines)

input_file <- config$Data$input_file
col_correlation_matrix <- trimws(strsplit(config$Data$col_correlation_matrix, ",")[[1]])

# Load data from the CSV file
input_path <- file.path("..", "input", input_file)
data <- read.csv(input_path)

# Subset the data to only include the columns of interest
data_subset <- data[, col_correlation_matrix, drop = FALSE]

# Calculate correlation coefficients
correlation_matrix <- cor(data_subset, use = "complete.obs")

# Save the correlation matrix to a CSV file
# Define output directory
output_path <- file.path("..", "output")
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

out_file_path <- file.path(output_path, "correlation_matrix.csv")
write.csv(correlation_matrix, out_file_path, row.names = TRUE)