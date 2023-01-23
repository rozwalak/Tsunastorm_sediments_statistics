library(readxl)

df <- read_excel("input/tsunastorm_grainsize.xlsx", sheet = "Logarithmic_fw57")
colnames(df)[2] <- "Mean"
colnames(df)[8] <- "Sorting"

metadata <- read.csv("input/metadata.csv")
colnames(metadata)[1] <- "Sample Name"
merged <- right_join(metadata, df)
