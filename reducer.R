# Function to reduce dataset size while preserving data structure
reduce_dataset <- function(data, target_size = 1000, seed = 123, output_file = "reduced.csv") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (target_size > nrow(data)) {
    warning("Target size is larger than dataset. Returning original dataset.")
    return(data)
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Calculate sampling proportion
  sampling_prop <- target_size / nrow(data)
  
  # Perform random sampling
  reduced_data <- data[sample(nrow(data), 
                              size = target_size, 
                              replace = FALSE), ]
  
  # Reset row names
  rownames(reduced_data) <- NULL
  
  # Save to CSV
  write.csv(reduced_data, file = output_file, row.names = FALSE)
  
  # Print reduction summary
  cat("Original dataset size:", nrow(data), "\n")
  cat("Reduced dataset size:", nrow(reduced_data), "\n")
  cat("Reduction ratio:", round((1 - sampling_prop) * 100, 2), "%\n")
  cat("Data saved to:", output_file, "\n")
  
  return(reduced_data)
}

data <- read.csv("Online_Retail_Clean_Enhanced.csv", header = TRUE, stringsAsFactors = FALSE)

# Example usage:
# Assuming your dataset is called 'your_data'
reduced_data <- reduce_dataset(data)