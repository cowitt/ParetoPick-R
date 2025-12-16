# K-means clustering with PCA and outlier handling in R
library(cluster)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(viridis)

# Function to parse INI config file
parse_config <- function(config_path) {
  config_lines <- readLines(config_path)
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

# Function to convert config values to appropriate types
get_config_value <- function(config, section, key, type = "character") {
  value <- config[[section]][[key]]
  switch(type,
    "integer" = as.integer(value),
    "numeric" = as.numeric(value),
    "logical" = as.logical(value),
    "character" = value,
    value
  )
}

# Function to remove outliers
remove_outliers <- function(data, deviation, count) {
  # Calculate z-scores
  z_scores <- scale(data)
  
  # Find outliers (points where 'count' or more variables are beyond 'deviation' std devs)
  outliers <- abs(z_scores) > deviation
  outliers_count <- rowSums(outliers)
  
  # Keep data points with fewer than 'count' outlying variables
  data_no_outliers <- data[outliers_count < count, ]
  num_outliers <- sum(outliers_count >= count)
  
  return(list(data_no_outliers = data_no_outliers, num_outliers = num_outliers))
}

# Function to perform k-means clustering
kmeans_clustering <- function(pca_data, num_clusters) {
  set.seed(58)
  kmeans_result <- kmeans(pca_data, centers = num_clusters, nstart = 100, algorithm = "Lloyd")
  return(list(labels = kmeans_result$cluster, centers = kmeans_result$centers))
}

# Function to find closest Pareto solution to centroid
find_closest_pareto_solution <- function(centroid, input_data) {
  distances <- apply(input_data, 1, function(x) sqrt(sum((x - centroid)^2)))
  closest_idx <- which.min(distances)
  closest_solution <- input_data[closest_idx, , drop = FALSE]
  return(list(index = rownames(input_data)[closest_idx], solution = closest_solution))
}

# Function to run k-means multiple times and find best solution
run_kmeans_multiple_times <- function(pca_data, min_clusters, max_clusters, input_data, pca_object, fixed_clusters_boolean, fixed_clusters) {
  best_score <- -1
  best_labels <- NULL
  best_centroids <- NULL
  
  if (fixed_clusters_boolean) {
    result <- kmeans_clustering(pca_data, fixed_clusters)
    best_labels <- result$labels
    best_centroids <- result$centers
    best_score <- silhouette(best_labels, dist(input_data))[, 3] %>% mean()
    
    # Calculate Davies-Bouldin Index (simplified version)
    dbi <- sum(cluster.stats(dist(input_data), best_labels)$average.within) / 
           sum(cluster.stats(dist(input_data), best_labels)$average.between)
    
    cat(sprintf("Clusters: %d, Input Data Silhouette Score: %.4f, Davies Bouldin Score: %.4f\n", 
                fixed_clusters, best_score, dbi))
  } else {
    for (num_clusters in min_clusters:max_clusters) {
      result <- kmeans_clustering(pca_data, num_clusters)
      labels <- result$labels
      centroids <- result$centers
      
      sil_score <- silhouette(labels, dist(input_data))[, 3] %>% mean()
      dbi <- sum(cluster.stats(dist(input_data), labels)$average.within) / 
             sum(cluster.stats(dist(input_data), labels)$average.between)
      
      cat(sprintf("Clusters: %d, Input Data Silhouette Score: %.4f, Davies Bouldin Score: %.4f\n", 
                  num_clusters, sil_score, dbi))
      
      if (sil_score > best_score) {
        best_score <- sil_score
        best_labels <- labels
        best_centroids <- centroids
      }
    }
  }
  
  # Find representative solutions
  representative_solutions <- list()
  representative_solutions_index <- c()
  
  # Transform centroids back to original space
  input_axes_centroids <- best_centroids %*% t(pca_object$rotation) + 
                         matrix(rep(pca_object$center, nrow(best_centroids)), 
                               nrow = nrow(best_centroids), byrow = TRUE)
  
  for (i in 1:nrow(input_axes_centroids)) {
    result <- find_closest_pareto_solution(input_axes_centroids[i, ], input_data)
    representative_solutions[[i]] <- result$solution
    representative_solutions_index <- c(representative_solutions_index, result$index)
  }
  
  return(list(
    labels = best_labels,
    rep_solutions_index = representative_solutions_index,
    rep_solutions = representative_solutions,
    score = best_score
  ))
}

# Function to create non-integer sequence
non_integer_range <- function(start, stop, step, precision = 10) {
  seq(start, stop, by = step) %>% round(precision)
}

# Main function
main <- function() {
  # Load Configuration
  config_path <- file.path("..", "input", "config.ini")
  config <- parse_config(config_path)
  
  # Extract configuration values
  input_file <- get_config_value(config, "Data", "input_file")
  columns <- trimws(strsplit(get_config_value(config, "Data", "columns"), ",")[[1]])
  min_clusters <- get_config_value(config, "Clustering", "min_clusters", "integer")
  max_clusters <- get_config_value(config, "Clustering", "max_clusters", "integer")
  fixed_clusters_boolean <- get_config_value(config, "Clustering", "fixed_clusters_boolean", "logical")
  fixed_clusters <- get_config_value(config, "Clustering", "fixed_clusters", "integer")
  handle_outliers_boolean <- get_config_value(config, "Extreme_solutions", "handle_outliers_boolean", "logical")
  deviations_min <- get_config_value(config, "Extreme_solutions", "deviations_min", "numeric")
  deviations_max <- get_config_value(config, "Extreme_solutions", "deviations_max", "numeric")
  deviations_step <- get_config_value(config, "Extreme_solutions", "deviations_step", "numeric")
  count_min <- get_config_value(config, "Extreme_solutions", "count_min", "integer")
  count_max <- get_config_value(config, "Extreme_solutions", "count_max", "integer")
  outlier_to_cluster_ratio <- get_config_value(config, "Extreme_solutions", "outlier_to_cluster_ratio", "numeric")
  min_components <- get_config_value(config, "PCA", "min_components", "integer")
  max_components <- get_config_value(config, "PCA", "max_components", "integer")
  num_variables_to_plot <- get_config_value(config, "Plots", "num_variables_to_plot", "integer")
  var_1 <- get_config_value(config, "Plots", "var_1")
  var_1_label <- get_config_value(config, "Plots", "var_1_label")
  var_2 <- get_config_value(config, "Plots", "var_2")
  var_2_label <- get_config_value(config, "Plots", "var_2_label")
  var_3 <- get_config_value(config, "Plots", "var_3")
  var_3_label <- get_config_value(config, "Plots", "var_3_label")
  var_4 <- get_config_value(config, "Plots", "var_4")
  var_4_label <- get_config_value(config, "Plots", "var_4_label")
  size_min <- get_config_value(config, "Plots", "size_min", "numeric")
  size_max <- get_config_value(config, "Plots", "size_max", "numeric")
  plot_frequency_maps <- get_config_value(config, "Frequency_Plots", "plot_frequency_maps", "logical")
  rscript_package_path <- get_config_value(config, "Frequency_Plots", "rscript_package_path")
  
  # Load input data
  input_path <- file.path("..", "input", input_file)
  raw_data <- read.csv(input_path)
  input_data <- raw_data[, columns, drop = FALSE]
  
  # Create output directory
  output_path <- file.path("..", "output")
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # Initialize final results
  final_score <- -1
  final_labels <- NULL
  final_rep_solutions <- NULL
  final_rep_solutions_index <- NULL
  final_input_data_no_outliers <- NULL
  final_raw_data_no_outliers <- NULL
  final_num_outliers <- NULL
  final_components <- NULL
  
  if (handle_outliers_boolean) {
    # Handle outliers case
    for (num_components in min_components:max_components) {
      cat(sprintf("\nNumber of Principal Components: %d\n", num_components))
      
      # Cache for storing results
      cache <- list()
      
      for (d in non_integer_range(deviations_min, deviations_max, deviations_step)) {
        for (c in count_min:count_max) {
          
          # Remove outliers
          outlier_result <- remove_outliers(input_data, d, c)
          input_data_no_outliers <- outlier_result$data_no_outliers
          num_outliers <- outlier_result$num_outliers
          
          # Create hash key for caching
          data_hash_key <- paste(sort(rownames(input_data_no_outliers)), collapse = "_")
          
          if (data_hash_key %in% names(cache)) {
            cat(sprintf("\nnumber of extreme solutions: %d, deviations: %.2f, count: %d\n", 
                        num_outliers, d, c))
            cached_result <- cache[[data_hash_key]]
            cat(sprintf("this input led to the same outliers produced as for deviations = %.2f and count = %d thus the clustering results are the same.\n", 
                        cached_result$d, cached_result$c))
            cat(sprintf("Best Cluster Count: %d, Best Input Data Silhouette Score: %.4f\n", 
                        cached_result$num_clusters, cached_result$silhouette_score))
            silhouette_score <- cached_result$silhouette_score
          } else {
            # Remove outliers from raw data
            raw_data_no_outliers <- raw_data[rownames(raw_data) %in% rownames(input_data_no_outliers), ]
            
            # Perform PCA
            pca_result <- prcomp(input_data_no_outliers, center = TRUE, scale. = TRUE)
            pca_data <- pca_result$x[, 1:num_components, drop = FALSE]
            
            # Perform clustering
            cat(sprintf("\nnumber of extreme solutions: %d, deviations: %.2f, count: %d\n", 
                        num_outliers, d, c))
            clustering_result <- run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, 
                                                         input_data_no_outliers, pca_result, 
                                                         fixed_clusters_boolean, fixed_clusters)
            
            labels <- clustering_result$labels
            representative_solutions_index <- clustering_result$rep_solutions_index
            representative_solutions <- clustering_result$rep_solutions
            silhouette_score <- clustering_result$score
            
            # Cache results
            cache[[data_hash_key]] <- list(
              num_clusters = max(labels),
              silhouette_score = silhouette_score,
              d = d,
              c = c
            )
          }
          
          # Update final solution if this is better
          if ((silhouette_score > final_score) && 
              (num_outliers < (length(representative_solutions_index) * outlier_to_cluster_ratio))) {
            final_score <- silhouette_score
            final_labels <- labels
            final_rep_solutions <- representative_solutions
            final_rep_solutions_index <- representative_solutions_index
            final_input_data_no_outliers <- input_data_no_outliers
            final_raw_data_no_outliers <- raw_data_no_outliers
            final_num_outliers <- num_outliers
            final_components <- num_components
          }
        }
      }
    }
    
    # Check if solution was found
    if (is.null(final_rep_solutions)) {
      stop("No acceptable representative solution set was found")
    }
    
    cat(sprintf("\nBest Silhouette Score: %.4f, Number of Clusters: %d, Number of Extreme Solutions: %d, Num of PCA: %d\n", 
                final_score, length(final_rep_solutions_index), final_num_outliers, final_components))
    
    # Add cluster labels and representative solution indicators
    final_raw_data_no_outliers$Cluster <- final_labels
    final_raw_data_no_outliers$Representative_Solution <- NA
    final_raw_data_no_outliers$Representative_Solution[rownames(final_raw_data_no_outliers) %in% final_rep_solutions_index] <- 
      final_raw_data_no_outliers$Cluster[rownames(final_raw_data_no_outliers) %in% final_rep_solutions_index]
    
    # Create outliers dataframe
    outliers <- raw_data[!rownames(raw_data) %in% rownames(final_input_data_no_outliers), ]
    outliers$Cluster <- "outlier"
    outliers$Representative_Solution <- "outlier"
    
    # Combine all data
    all_data <- rbind(final_raw_data_no_outliers, outliers)
    
    # Export to CSV
    out_file_path <- file.path(output_path, "kmeans_data_w_clusters_representativesolutions_outliers.csv")
    write.csv(all_data, out_file_path, row.names = FALSE)
    
  } else {
    # No outlier handling case
    for (num_components in min_components:max_components) {
      cat(sprintf("\nNumber of Principal Components: %d\n", num_components))
      
      # Perform PCA
      pca_result <- prcomp(input_data, center = TRUE, scale. = TRUE)
      pca_data <- pca_result$x[, 1:num_components, drop = FALSE]
      
      # Perform clustering
      clustering_result <- run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, 
                                                   input_data, pca_result, 
                                                   fixed_clusters_boolean, fixed_clusters)
      
      labels <- clustering_result$labels
      representative_solutions_index <- clustering_result$rep_solutions_index
      representative_solutions <- clustering_result$rep_solutions
      silhouette_score <- clustering_result$score
      
      if (silhouette_score > final_score) {
        final_score <- silhouette_score
        final_labels <- labels
        final_rep_solutions <- representative_solutions
        final_rep_solutions_index <- representative_solutions_index
        final_components <- num_components
      }
    }
    
    cat(sprintf("\nBest Silhouette Score: %.4f, Number of Clusters: %d, Num of PCA: %d\n", 
                final_score, length(final_rep_solutions_index), final_components))
    
    # Add cluster labels and representative solution indicators
    raw_data$Cluster <- final_labels
    raw_data$Representative_Solution <- NA
    raw_data$Representative_Solution[rownames(raw_data) %in% final_rep_solutions_index] <- 
      raw_data$Cluster[rownames(raw_data) %in% final_rep_solutions_index]
    
    # Export to CSV
    out_file_path <- file.path(output_path, "kmeans_data_w_clusters_representativesolutions.csv")
    write.csv(raw_data, out_file_path, row.names = FALSE)
    
    all_data <- raw_data
  }
  
  # Qualitative Clustering Analysis
  if (num_variables_to_plot == 2) {
    qualitative_clustering_columns <- c(var_1, var_2)
  } else if (num_variables_to_plot == 3) {
    qualitative_clustering_columns <- c(var_1, var_2, var_3)
  } else if (num_variables_to_plot == 4) {
    qualitative_clustering_columns <- c(var_1, var_2, var_3, var_4)
  } else {
    stop("num_variables_to_plot must be between 2 and 4")
  }
  
  # Create percentile analysis
  qualitative_data <- all_data[, c(qualitative_clustering_columns, "Cluster")]
  
  # Function to create binary columns based on percentiles
  create_binary_columns <- function(series, lower_percentile, upper_percentile) {
    lower_threshold <- quantile(series, lower_percentile / 100, na.rm = TRUE)
    upper_threshold <- quantile(series, upper_percentile / 100, na.rm = TRUE)
    
    if (lower_percentile == 0) {
      return(as.numeric(series <= upper_threshold))
    } else {
      return(as.numeric(series > lower_threshold & series <= upper_threshold))
    }
  }
  
  # Define percentile ranges
  percentile_ranges <- list(c(0, 33), c(33, 66), c(66, 100))
  
  # Create binary columns for each variable and percentile range
  for (col in qualitative_clustering_columns) {
    for (range in percentile_ranges) {
      lower <- range[1]
      upper <- range[2]
      percentile_col_name <- paste0(col, "_", lower, "_", upper)
      qualitative_data[[percentile_col_name]] <- create_binary_columns(qualitative_data[[col]], lower, upper)
    }
  }
  
  # Remove original columns and group by cluster
  qualitative_data <- qualitative_data[, !names(qualitative_data) %in% qualitative_clustering_columns]
  df_grouped <- qualitative_data %>% 
    group_by(Cluster) %>% 
    summarise_all(sum, na.rm = TRUE) %>%
    as.data.frame()
  
  # Transpose for table display
  df_transposed <- t(df_grouped[, -1])
  colnames(df_transposed) <- df_grouped$Cluster
  
  # Create and display table (simplified version)
  print("Percentile Distribution of Solutions within Clusters:")
  print(df_transposed)
  
  # Plot Representative Solutions
  rep_data <- all_data[!is.na(all_data$Representative_Solution), ]
  
  if (num_variables_to_plot >= 3) {
    names(rep_data)[names(rep_data) == var_3] <- var_3_label
  }
  if (num_variables_to_plot == 4) {
    names(rep_data)[names(rep_data) == var_4] <- var_4_label
  }
  
  # Create scatter plot
  if (num_variables_to_plot == 2) {
    p <- ggplot(rep_data, aes_string(x = var_1, y = var_2)) +
      geom_point() +
      geom_text(aes(label = Representative_Solution), hjust = 0, vjust = 0) +
      labs(title = "Representative Solutions", x = var_1_label, y = var_2_label) +
      theme_minimal()
  } else if (num_variables_to_plot == 3) {
    p <- ggplot(rep_data, aes_string(x = var_1, y = var_2, color = var_3_label)) +
      geom_point() +
      geom_text(aes(label = Representative_Solution), hjust = 0, vjust = 0) +
      labs(title = "Representative Solutions", x = var_1_label, y = var_2_label) +
      scale_color_viridis_c() +
      theme_minimal()
  } else if (num_variables_to_plot == 4) {
    p <- ggplot(rep_data, aes_string(x = var_1, y = var_2, color = var_3_label, size = var_4_label)) +
      geom_point() +
      geom_text(aes(label = Representative_Solution), hjust = 0, vjust = 0) +
      labs(title = "Representative Solutions", x = var_1_label, y = var_2_label) +
      scale_color_viridis_c() +
      scale_size_continuous(range = c(size_min, size_max)) +
      theme_minimal()
  }
  
  print(p)
  
  # Create violin plots
  create_violin_plots <- function(data, variables, labels, clusters) {
    plots <- list()
    
    for (i in seq_along(variables)) {
      if (i <= length(variables)) {
        # Ensure proper ordering of clusters
        cluster_levels <- unique(data$Cluster)
        if ("outlier" %in% cluster_levels) {
          numeric_clusters <- cluster_levels[cluster_levels != "outlier"]
          numeric_clusters <- as.character(sort(as.numeric(numeric_clusters)))
          cluster_levels <- c(numeric_clusters, "outlier")
        } else {
          cluster_levels <- as.character(sort(as.numeric(cluster_levels)))
        }
        
        data$Cluster <- factor(data$Cluster, levels = cluster_levels)
        
        p <- ggplot(data, aes_string(x = "Cluster", y = variables[i])) +
          geom_violin() +
          labs(title = paste("Distribution of", variables[i], "by Cluster"),
               y = labels[i]) +
          theme_minimal()
        plots[[i]] <- p
      }
    }
    
    return(plots)
  }
  
  # Create violin plots for all variables
  plot_variables <- c(var_1, var_2)
  plot_labels <- c(var_1_label, var_2_label)
  
  if (num_variables_to_plot >= 3) {
    plot_variables <- c(plot_variables, var_3)
    plot_labels <- c(plot_labels, var_3_label)
  }
  if (num_variables_to_plot == 4) {
    plot_variables <- c(plot_variables, var_4)
    plot_labels <- c(plot_labels, var_4_label)
  }
  
  violin_plots <- create_violin_plots(all_data, plot_variables, plot_labels, all_data$Cluster)
  
  # Display violin plots
  if (length(violin_plots) > 0) {
    do.call(grid.arrange, c(violin_plots, ncol = 2))
  }
  
  # Run frequency plots if requested
  if (plot_frequency_maps) {
    r_script_path <- file.path("..", "r_files", "plot_frequency_maps.R")
    system2(rscript_package_path, args = r_script_path)
  }
  
  cat("Analysis completed successfully!\n")
}

# Run main function
if (sys.nframe() == 0) {
  main()
}