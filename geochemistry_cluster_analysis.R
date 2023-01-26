library(readxl)
library(tidyverse)
library(ggpubr)
library(factoextra)
library(writexl)

df <- read_excel("input/tsunastorm_geochemistry.xlsx")
df <- subset(df, Sample_name != "m21/15_15-16") #remove outlier

df <- na.omit(df) 

metadata <- read.csv("input/metadata.csv")

df2cluster <- df %>% column_to_rownames(var="Sample_name")

# Optimal number of clusters
fviz_nbclust(x = df2cluster, 
             FUNcluster = kmeans, 
             method = "wss") +
  # To draw reference line
  geom_vline(xintercept = 4, 
             linetype = 2) + 
  # Add subtitle "Elbow method"
  labs(subtitle = "Elbow method")

set.seed(123)
res.km <- kmeans(na.omit(df2cluster), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = df2cluster,
             palette = "npg",
             geom = "point",
             ellipse.type = "norm", 
             ggtheme = theme_bw(),
)

#combine with PCA

# Dimension reduction using PCA
res.pca <- prcomp(df2cluster,  scale = FALSE)
# Coordinates of individuals
results <- data.frame(Sample_name=rownames(res.pca$x),
                      PC.1=res.pca$x[,1],
                      PC.2=res.pca$x[,2])
# Add clusters obtained using the K-means algorithm
results$cluster <- factor(res.km$cluster)
# Add metadata
results <- merge(metadata, results)
#save results in excel
write_xlsx(results, "output/Lackoutlier_geochemistry_PCA_clusters.xlsx")

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

#Plot
PCA_and_kmeans_clusters <- ggscatter(
  results, x = "PC.1", y = "PC.2", 
  color = "cluster", palette = "aaas", ellipse = TRUE, ellipse.type = "norm",
  shape = "Type_details", size = 3.5,  legend = "right", ggtheme = theme_bw(base_size = 18),
  xlab = paste0("PC 1 (", variance.percent[1], "% )" ),
  ylab = paste0("PC 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)
PCA_and_kmeans_clusters

#save plot file
ggsave("Lackoutlier_geochemistry_PCA_and_kmeans_clusters.png", plot = PCA_and_kmeans_clusters, bg = "white", width = 20, height = 10.4, device = "png", path = "output")
