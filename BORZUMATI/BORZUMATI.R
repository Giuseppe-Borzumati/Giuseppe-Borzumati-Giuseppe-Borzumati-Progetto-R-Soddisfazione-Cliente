# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cluster)
library(factoextra)
library(ggfortify)

# Set working directory to where your Excel file is located
setwd("C:/Users/foti2/Desktop/TESI DI LAUREA ECONOMIA/PROGETTO TESI DI LAUREA/BORZUMATI 2/BORZUMATI")

# Specify the Excel file name
file_path <- "dati.xlsx"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("Error: File 'dati.xlsx' does not exist at the specified path.")
}

# Read the sheets from the Excel file
sheets <- excel_sheets(file_path)

# Function to convert character columns to numeric
convert_to_numeric <- function(df) {
  numeric_columns <- names(df)[sapply(df, is.character)]
  df[numeric_columns] <- lapply(df[numeric_columns], function(x) as.numeric(as.character(x)))
  return(df)
}

# Iterate through each sheet
for (sheet in sheets) {
  # Read data from the Excel sheet
  data <- read_excel(file_path, sheet = sheet)
  colnames(data) <- gsub(" ", "_", colnames(data))
  
  # Select numerical columns for analysis
  numerical_data <- data %>%
    select(contains("Qualità"), contains("Prezzo"), contains("Servizio_Clienti"),
           contains("Velocità_di_Consegna"), contains("Facilità_d'Uso"),
           contains("Assistenza_Post-Vendita"), contains("Informazioni_Ricevute"),
           contains("Probabilità_di_Raccomandazione"), contains("Esperienza_Complessiva"),
           contains("Probabilità_di_Riacquisto")) %>%
    mutate_all(as.numeric)
  
  # PCA and Regression Analysis if 'Prezzo' column exists
  if ("Prezzo" %in% colnames(numerical_data)) {
    pca <- prcomp(numerical_data, scale. = TRUE)
    
    pca_plot <- autoplot(pca, data = numerical_data, colour = 'Esperienza_Complessiva') +
      ggtitle(paste("PCA of Customer Satisfaction Data - Sheet:", sheet))
    print(pca_plot)
    
    regression_model <- lm(pca$x[,1] ~ numerical_data$Prezzo)
    print(summary(regression_model))
    
    regression_plot <- ggplot(numerical_data, aes(x = Prezzo, y = pca$x[,1])) +
      geom_point() +
      geom_smooth(method = "lm") +
      ggtitle(paste("Regression Analysis: PCA1 vs Price - Sheet:", sheet))
    print(regression_plot)
  } else {
    print(paste("Sheet", sheet, "does not contain the 'Prezzo' column. Skipping regression analysis."))
  }
  
  # Time Series Analysis if 'Data_di_acquisto' column exists
  if ("Data_di_acquisto" %in% colnames(data)) {
    data$Data_di_acquisto <- as.Date(data$Data_di_acquisto)
    monthly_data <- data %>%
      group_by(month = floor_date(Data_di_acquisto, "month")) %>%
      summarise(average_satisfaction = mean(Esperienza_Complessiva, na.rm = TRUE))
    
    time_series_plot <- ggplot(monthly_data, aes(x = month, y = average_satisfaction)) +
      geom_line() +
      ggtitle(paste("Time Series Analysis of Customer Satisfaction - Sheet:", sheet))
    print(time_series_plot)
  } else {
    print(paste("Sheet", sheet, "does not contain the 'Data di acquisto' column. Skipping time series analysis."))
  }
  
  # K-Means Clustering
  set.seed(123)
  kmeans_result <- kmeans(numerical_data, centers = 3)
  
  cluster_plot <- fviz_cluster(kmeans_result, data = numerical_data) +
    ggtitle(paste("Cluster Analysis of Customer Satisfaction - Sheet:", sheet))
  print(cluster_plot)
}
