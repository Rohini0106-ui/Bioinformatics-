# Step 1: Create the data frame from your provided data
genedata <- data.frame(
  Gene = c("GeneA","GeneB","GeneC","GeneD","GeneE","GeneF"),
  Tissue = c("Liver","Heart","Brain","Liver","Heart","Brain"),
  Condition = c("Control","Control","Treatment","Treatment","Control","Treatment"),
  Exp1 = c(5.3, 7.8, 6.1, 4.9, 7.3, 5.5),
  Exp2 = c(4.1, 5.4, 8.5, 3.6, 6.8, 7.9),
  Exp3 = c(6.2, 6.9, 9.0, 5.7, 8.1, 8.7)
)

# Step 2: Save as a CSV file
write.csv(genedata, "genelist.csv", row.names = FALSE)
cat("genelist.csv created successfully\n")

# Load necessary library
if(!require(readr)) install.packages("readr", repos="http://cran.us.r-project.org")
library(readr)

# user input 
input_file <- readline(prompt="Enter input CSV filename (e.g., genelist.csv): ")

#read file 
df <- read_csv(input_file)

cat("file read successfully!\n")
print(head(df))

#add mean expression 
df$MeanExp <- rowMeans(df[, c("Exp1","Exp2","Exp3")], na.rm = TRUE)

#ask user for output file name 
output_file <- readline(prompt="Enter output CSV filename (e.g., results.csv): ")

if(!grepl("\\.csv$", output_file)) {
  output_file <- paste0(output_file, ".csv")
}

#save the csv file 
write_csv(df, output_file)
cat(paste("Data written to", output_file, "\n"))


#install packages 

install.packages("dplyr") 
install.packages("tidyr") 

library(dplyr)
library(tidyr)

filtered <- genedata %>%
  filter(Tissue == "Liver", Condition == "Treatment")

filtered

transformed <- genedata %>%
  mutate(MeanExp = rowMeans(select(., Exp1:Exp3)))

transformed

aggregated <- transformed %>%
  group_by(Tissue, Condition) %>%
  summarise(AvgExp = mean(MeanExp), .groups = "drop")

aggregated

long_format <- genedata %>%
  pivot_longer(cols = starts_with("Exp"),
               names_to = "Experiment",
               values_to = "Expression")

head(long_format)

# Install and load the 'optparse' package for command-line argument parsing
install.packages("optparse")  
library(optparse)  

# 1. Setup parser for command-line arguments
option_list <- list(
  make_option(c("-i", "--input"), type="character", help="Input CSV file"),  
  # Define an option for output CSV file 
  make_option(c("-o", "--output"), type="character", default="results.csv",
              help="Output CSV file [default: %default]")  
)
# Parse the command-line arguments using the options defined above
opt <- parse_args(OptionParser(option_list=option_list))  

# 2. Validate input
if (is.null(opt$input)) stop("Please provide --input <file>")  
# If the input file is not provided, stop execution and show an error

# 3. Read the input CSV file into a DataFrame
df <- read.csv(opt$input)  
# 'opt$input' contains the path of the input CSV file provided by the user

# 4. Transform data: calculate the mean expression if columns Exp1, Exp2, and Exp3 exist
if(all(c("Exp1","Exp2","Exp3") %in% colnames(df))) {  
  # Check if all three columns are present in the DataFrame
  df$MeanExp <- rowMeans(df[, c("Exp1","Exp2","Exp3")])  
  # Calculate the row-wise mean of Exp1, Exp2, and Exp3 , Store the result in a new column "MeanExp"
}

# 5. Save the processed DataFrame to a CSV file
write.csv(df, opt$output, row.names=FALSE)  # row.names=FALSE prevents writing row numbers into the CSV

# Print a confirmation message
cat("Data written to", opt$output, "\n")  # Show the output file path dynamically




















