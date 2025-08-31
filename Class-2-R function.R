# Write a function classify_gene() 
classify_gene <- function (logFC, padj){
  ifelse (logFC>1 & padj < 0.05, "Upregulate",
  ifelse (logFC < -1 & padj < 0.05,"Downregulate","Not significatnt"))
}
# Define input and output folders
input_dir <- "raw_data" 
output_dir <- "Results"


# create output folder if not already exist

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}


# List which files to process
files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv") 


# Prepare empty list to store results in R 
result_list <- list()

for (file_names in files_to_process) {
  cat("\nProcessing:", file_names, "\n")
  
  input_file_path <- file.path(input_dir, file_names)
  
  # Import dataset
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values...\n")
  
  # handling missing values
  
  if("logFC" %in% names(data)){
    missing_count <- sum(is.na(data$logFC))
    
    cat("Missing values in 'logFC':", missing_count, "\n")
    data$logFC[is.na(data$logFC)] <-1
  }
  
  if("padj" %in% names(data)){
    missing_count <- sum(is.na(data$padj))
    
    cat("Missing values in 'padj':", missing_count, "\n")
    data$padj[is.na(data$padj)] <- 1
  }
  # calssify
  data$Status <- classify_gene(data$logFC, data$padj)
  cat("status has been classified successfully.\n")
  # save results in R
  result_list[[file_names]] <- data 
  
  # save results in Results folder
  output_file_path <- file.path(output_dir, paste0("status_results", file_names))
  write.csv(data, output_file_path, row.names = FALSE)
  cat("Results saved to:", output_file_path, "\n")
  
}

# The loop repeats until all files are processed.

results_1 <- result_list[[1]] 

results_2 <- result_list[[2]]


#   - Print summary counts of significant, upregulated, and downregulated genes
#   - Use table() for summaries
data$Status_fac <- factor(data$Status_fac,
                          level= c("Upregulate","Downregulate","Not significatnt")) 
table(data$Status_fac)

# Save the entire R workspace
save.image(file = "MaisKarajeh_class_2_Assigment.RData")
