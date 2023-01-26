library(readxl)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggsci)


df <- read_excel("input/tsunastorm_geochemistry.xlsx")

metadata <- read.csv("input/metadata.csv")

df2PCA <- df %>% column_to_rownames(var="Sample_name")

#Perform PCA
pca <- prcomp(df2PCA, scale=TRUE)

#calculate percent variation per PC
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca.data <- data.frame(Sample_name=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])

merged <- right_join(metadata, pca.data)

PCA_plot <- ggplot(data=merged, aes(x=X, y=Y, shape=Type_general, color = Type_details)) +
  geom_point(size=6) +
  theme_classic(base_size = 18) +
  guides(shape = guide_legend(title = "Type of sediments")) +
  guides(color = guide_legend(title = "Sources")) + 
  xlab(paste("PC 1 (", pca.var.per[1], "% )" )) +
  ylab(paste("PC 2 (", pca.var.per[2], "% )" )) +
  scale_colour_brewer(palette = 'Set2')
PCA_plot

#calculate PCs loadings
PC1_loading_scores <- pca$rotation[,1]
PC2_loading_scores <- pca$rotation[,2]

PC1_loading_scores
PC2_loading_scores

#save plot file
ggsave("geochemistry_PCA.png", plot = PCA_plot, bg = "white", width = 20, height = 10.4, device = "png", path = "output")
