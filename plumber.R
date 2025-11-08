###############################################
# Project: Data Analytics Dashboard
# File: plumber.R
# Description: R-based API for data analysis, visualization,
#              and regression modeling. Works with HTML frontend.
# Author: Lokesh Aslekar , Arya Hedau
# Date: 2025
###############################################

# Run the API:
# R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"

library(plumber)
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)

# Get port from environment variable (for cloud deployment)
port <- as.numeric(Sys.getenv('PORT', 8000))

#---------------------------------------------------
# 1️⃣ Enable CORS (required for frontend connection)
#---------------------------------------------------
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, Accept")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#---------------------------------------------------
# 2️⃣ Basic API setup
#---------------------------------------------------
# Ensure 'www' directory exists for saving plots
www_dir <- file.path(getwd(), 'www')
if (!dir.exists(www_dir)) {
  dir.create(www_dir)
  cat("Created www directory for plots\n")
}

# Serve static files from www directory
#* @get /<file:path>
#* @serializer contentType list(type="image/png")
function(file, res) {
  file_path <- file.path("www", file)
  
  if (file.exists(file_path)) {
    file_ext <- tools::file_ext(file)
    content_type <- switch(
      file_ext,
      "png" = "image/png",
      "jpg" = "image/jpeg",
      "jpeg" = "image/jpeg",
      "gif" = "image/gif",
      "svg" = "image/svg+xml",
      "application/octet-stream"
    )
    
    res$setHeader("Content-Type", content_type)
    res$setHeader("Cache-Control", "public, max-age=3600") # Cache for 1 hour
    
    readBin(file_path, "raw", n = file.info(file_path)$size)
  } else {
    res$status <- 404
    return(list(error = "File not found"))
  }
}

#---------------------------------------------
#* @apiTitle Data Analytics & Visualization API
#* @apiDescription REST API for data analysis and regression modeling
#---------------------------------------------

#---------------------------------------------------
# 3️⃣ Health check endpoint
#---------------------------------------------------
#* Health check endpoint
#* @get /ping
#* @serializer json
function() {
  list(
    status = "API running successfully", 
    time = Sys.time(),
    version = "1.0.0",
    environment = ifelse(port == 8000, "development", "production")
  )
}

#---------------------------------------------------
# 4️⃣ API info endpoint
#---------------------------------------------------
#* Get API information
#* @get /info
#* @serializer json
function() {
  list(
    name = "Data Analytics API",
    version = "1.0.0",
    description = "R-based API for data analysis and visualization",
    endpoints = c("/ping", "/info", "/analyze"),
    environment = ifelse(port == 8000, "development", "production"),
    port = port
  )
}

#---------------------------------------------------
# 5️⃣ Upload CSV, clean data, run regression
#---------------------------------------------------
#* Upload CSV, clean data, run regression, return JSON
#* @param file:file The uploaded CSV file
#* @post /analyze
#* @serializer json
function(req, res) {
  tryCatch({
    # Log the request
    cat("Analysis request received at", as.character(Sys.time()), "\n")
    
    # Check if file was uploaded
    if (is.null(req$files) || length(req$files) == 0) {
      res$status <- 400
      return(list(error = "No file uploaded. Please upload a CSV file using form-data with field name 'file'."))
    }
    
    file_info <- req$files[[1]]
    cat("Processing file:", file_info$name, "\n")
    
    # Try reading the CSV file with multiple methods
    df <- NULL
    error_messages <- c()
    
    # Method 1: readr::read_csv
    tryCatch({
      df <- read_csv(file_info$datapath, show_col_types = FALSE)
      cat("Successfully read with read_csv\n")
    }, error = function(e) {
      error_messages <<- c(error_messages, paste("read_csv:", e$message))
    })
    
    # Method 2: base R read.csv
    if (is.null(df)) {
      tryCatch({
        df <- read.csv(file_info$datapath, stringsAsFactors = FALSE)
        cat("Successfully read with read.csv\n")
      }, error = function(e) {
        error_messages <<- c(error_messages, paste("read.csv:", e$message))
      })
    }
    
    # Method 3: Try with different encoding
    if (is.null(df)) {
      tryCatch({
        df <- read.csv(file_info$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        cat("Successfully read with UTF-8 encoding\n")
      }, error = function(e) {
        error_messages <<- c(error_messages, paste("UTF-8 read:", e$message))
      })
    }
    
    if (is.null(df)) {
      res$status <- 400
      return(list(
        error = "Could not read CSV file. Please ensure it is a valid CSV file.",
        details = error_messages
      ))
    }
    
    # Log dataset info
    cat("Dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
    cat("Column names:", paste(names(df), collapse = ", "), "\n")
    
    # Clean data: drop empty columns and handle missing values
    original_cols <- ncol(df)
    df <- df %>% select(where(~!all(is.na(.))))
    removed_cols <- original_cols - ncol(df)
    if (removed_cols > 0) {
      cat("Removed", removed_cols, "empty columns\n")
    }
    
    # Handle missing values for numeric columns
    num_cols <- sapply(df, is.numeric)
    na_counts <- sapply(df[num_cols], function(col) sum(is.na(col)))
    
    if (any(na_counts > 0)) {
      cat("Handling missing values in numeric columns\n")
      df[num_cols] <- lapply(df[num_cols], function(col) {
        if (any(is.na(col))) {
          col_mean <- mean(col, na.rm = TRUE)
          if (!is.na(col_mean)) {
            col[is.na(col)] <- col_mean
            cat("Replaced", sum(is.na(col)), "NA values with mean\n")
          }
        }
        col
      })
    }
    
    # Check for numeric columns
    numeric_names <- names(df)[sapply(df, is.numeric)]
    cat("Numeric columns found:", paste(numeric_names, collapse = ", "), "\n")
    
    if (length(numeric_names) < 2) {
      res$status <- 400
      return(list(
        error = "Dataset must contain at least two numeric columns for regression analysis.",
        numeric_columns_found = numeric_names,
        total_columns = ncol(df)
      ))
    }
    
    # Choose target variable (prefer common targets, else first numeric)
    preferred_targets <- c("mpg", "price", "sales", "revenue", "target", "y", "output")
    target <- NULL
    
    for (pref_target in preferred_targets) {
      if (pref_target %in% numeric_names) {
        target <- pref_target
        break
      }
    }
    
    if (is.null(target)) {
      target <- numeric_names[1]
    }
    
    predictors <- setdiff(numeric_names, target)
    cat("Selected target:", target, "\n")
    cat("Available predictors:", paste(predictors, collapse = ", "), "\n")
    
    # Pick up to 3 predictors with highest variance (most informative)
    if (length(predictors) > 0) {
      pred_variances <- sapply(df[predictors], var, na.rm = TRUE)
      pred_sel <- predictors[order(-pred_variances)]
      pred_sel <- head(pred_sel, 3)
      cat("Selected predictors (by variance):", paste(pred_sel, collapse = ", "), "\n")
    } else {
      res$status <- 400
      return(list(error = "No suitable predictors found after processing."))
    }
    
    # Create model formula dynamically
    formula_text <- paste(target, "~", paste(pred_sel, collapse = " + "))
    cat("Model formula:", formula_text, "\n")
    
    # Build regression model
    model <- lm(as.formula(formula_text), data = df)
    summ <- summary(model)
    
    # Generate unique filenames for plots
    timeid <- as.integer(Sys.time())
    p1file <- paste0("scatter_", timeid, ".png")
    p2file <- paste0("residuals_", timeid, ".png")
    p1path <- file.path("www", p1file)
    p2path <- file.path("www", p2file)
    
    # Generate plots with error handling
    plot_errors <- c()
    
    # Scatter Plot with regression line (using first predictor)
    tryCatch({
      g1 <- ggplot(df, aes_string(x = pred_sel[1], y = target)) +
        geom_point(color = "blue", alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
        ggtitle(paste("Regression: ", target, " vs ", pred_sel[1])) +
        xlab(pred_sel[1]) +
        ylab(target) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      ggsave(filename = p1path, plot = g1, width = 8, height = 6, dpi = 150, bg = "white")
      cat("Generated scatter plot:", p1file, "\n")
    }, error = function(e) {
      plot_errors <<- c(plot_errors, paste("Scatter plot:", e$message))
      p1file <<- NA
    })
    
    # Residual Plot
    tryCatch({
      resid_df <- data.frame(Fitted = fitted(model), Residuals = resid(model))
      g2 <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
        geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
        ggtitle("Residuals vs Fitted Values") +
        xlab("Fitted Values") +
        ylab("Residuals") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      ggsave(filename = p2path, plot = g2, width = 8, height = 6, dpi = 150, bg = "white")
      cat("Generated residual plot:", p2file, "\n")
    }, error = function(e) {
      plot_errors <<- c(plot_errors, paste("Residual plot:", e$message))
      p2file <<- NA
    })
    
    # Compute results
    r_squared <- round(summ$r.squared, 4)
    adj_r_squared <- round(summ$adj.r.squared, 4)
    f_statistic <- round(summ$fstatistic[1], 2)
    
    # Get host information for dynamic URL
    host <- req$HTTP_HOST
    if (is.null(host) || host == "") {
      host <- "localhost:8000"
    }
    
    protocol <- ifelse(grepl("localhost", host), "http", "https")
    base_url <- paste0(protocol, "://", host)
    
    # Build plots list
    plots_list <- list()
    if (!is.na(p1file)) {
      plots_list$scatter <- paste0(base_url, "/", p1file)
    }
    if (!is.na(p2file)) {
      plots_list$residuals <- paste0(base_url, "/", p2file)
    }
    
    # Build comprehensive response
    response <- list(
      success = TRUE,
      formula = formula_text,
      target = target,
      predictors = pred_sel,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      f_statistic = f_statistic,
      coefficients = as.list(round(coef(model), 4)),
      dataset_info = list(
        rows = nrow(df),
        columns = ncol(df),
        numeric_columns = numeric_names,
        removed_empty_columns = removed_cols
      ),
      plots = plots_list,
      summary = capture.output(summ),
      warnings = if (length(plot_errors) > 0) plot_errors else NULL,
      timestamp = as.character(Sys.time())
    )
    
    cat("Analysis completed successfully for", file_info$name, "\n")
    cat("R-squared:", r_squared, "\n")
    
    return(response)
    
  }, error = function(e) {
    # Global error handler
    cat("ERROR in analysis:", e$message, "\n")
    res$status <- 500
    return(list(
      error = "Internal server error during analysis",
      details = e$message,
      timestamp = as.character(Sys.time())
    ))
  })
}

#---------------------------------------------------
# 6️⃣ Simple echo test endpoint
#---------------------------------------------------
#* Test endpoint with message
#* @param msg The message to echo
#* @get /echo
#* @serializer json
function(msg = "hello") {
  list(
    message = paste("Echo:", msg),
    timestamp = Sys.time()
  )
}

#---------------------------------------------------
# 7️⃣ Dataset info endpoint
#---------------------------------------------------
#* Get information about uploaded dataset
#* @param file:file The uploaded CSV file
#* @post /dataset-info
#* @serializer json
function(req, res) {
  file_info <- req$files[[1]]
  
  if (is.null(file_info)) {
    res$status <- 400
    return(list(error = "No file uploaded"))
  }
  
  df <- read.csv(file_info$datapath, stringsAsFactors = FALSE)
  
  list(
    filename = file_info$name,
    size = file_info$size,
    rows = nrow(df),
    columns = ncol(df),
    column_names = names(df),
    column_types = sapply(df, class),
    numeric_columns = names(df)[sapply(df, is.numeric)],
    preview = head(df, 5)
  )
}

#---------------------------------------------------
# 8️⃣ Run the API
#---------------------------------------------------
cat("=============================================\n")
cat("Data Analytics API Starting...\n")
cat("Version: 1.0.0\n")
cat("Port:", port, "\n")
cat("Environment:", ifelse(port == 8000, "development", "production"), "\n")
cat("Working directory:", getwd(), "\n")
cat("WWW directory:", www_dir, "\n")
cat("=============================================\n")

# Create plumber router
pr <- plumber::plumb('plumber.R')

# Add startup message
cat("API endpoints available:\n")
cat("- GET  /ping         Health check\n")
cat("- GET  /info         API information\n")
cat("- GET  /echo?msg=    Echo test\n")
cat("- POST /analyze      Data analysis\n")
cat("- POST /dataset-info Dataset info\n")
cat("- GET  /<filename>   Serve static files\n")
cat("=============================================\n")

# Run the API
pr$run(host = '0.0.0.0', port = port, swagger = FALSE)