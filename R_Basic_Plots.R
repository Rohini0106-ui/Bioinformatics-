#R Basic Plots 

#gene data
set.seed(123)
gene_data <- data.frame(
  condition = rep(c("Control", "Treatment1", "Treatment2"), each =50),
  expression_level = c(
    rnorm(50, mean = 5, sd = 1),
    rnorm(50, mean = 7, sd = 1.3),
    rnorm(50, mean = 8, sd = 1.9)
  )
)

#install the package and load the library 
install.packages("ggplot2") 
library(ggplot2)

ggplot(gene_data, aes(x = condition, y = expression_level, color = condition))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "gene expression",
    x = "condition",
    y = "Expression Level"
  )

#valcono plot 
set.seed(123)
volcano_data <- data.frame(
  logFC = rnorm(1000, mean = 0, sd = 2),
  p_value = runif(1000, min = 0.001, max =0.1)
)
#add significance 
volcano_data$significance <- ifelse (
  volcano_data$p_value < 0.05 & abs(volcano_data$logFC) > 1,
  "significant", "Not significant" 
)
ggplot(volcano_data, aes(x = logFC, y = -log10(p_value))) +
  geom_point(aes(color = significance), size = 2) +
  scale_color_manual(values = c("Not significant" = "yellow", "significant" = "green")) +
  theme_minimal()+
  labs(
    title = "vcp",
    x = " log fold change",
    y = "-log10 P-value"
  )

#otu abundance bar plot 
set.seed(123)
samples <- paste("Sample", 1:10)
taxa <- paste("Taxon", 1:5)

otu_data <- expand.grid (sample = samples, taxon = taxa)
otu_data$abundance <- rpois(nrow(otu_data), lambda = 20)

ggplot(otu_data, aes(x = sample, y = abundance, fill = taxon)) + 
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() + 
  labs(
    title = "MCC",
    x = "sample",
    y = "Abundance"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pca plot 
set.seed(123)
pca_data <- data.frame(
  PC1 = rnorm(100, mean = 0, sd = 5),
  PC2 = rnorm(100, mean = 0, sd = 5),
  group = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE)
)
ggplot(pca_data, aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 3) + 
  theme_minimal() +
  labs(
    title = "PCA",
    x = "PC1",
    y = "PC2"
  )


library(ggplot2)

#example data
data <- data.frame(
  x = 1:10,
  y1 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  y2 = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)
)

#line plot 
ggplot(data, aes(x = x))+
  geom_line(aes(y = y1, color = "Line 1"), linewidth = 1.2)+
  geom_line(aes(y = y2, color = "Line 2"), linewidth = 1.2)+
  labs(titile = "Lines in ggplot",
       x = "x - label", y = "y - label")+
  scale_color_manual(values = c("Line 1" ="red", "Line 2" = "blue"))+
  theme_minimal()

library(ggplot2)

#calculate mean sepal.length for each species 
mean_sepal <- aggregate(Sepal.Length ~ Species, data = iris, mean)

#line plot 
ggplot(mean_sepal, aes(x = Species, y = Sepal.Length, group = 1))+
  geom_line(color = "blue", linewidth = 1.3)+
  geom_point(size = 4, color = "pink")+
  labs(title = "iris data line plot",
       x = "species", y = "mean")+
  theme_minimal()
 
#gene expression level across samples
#stimulate gene expression data 
samples <- paste0("Sample", 1:10)
expression <- c(2.5, 3.2, 4.0, 3.8, 5.1, 6.0, 5.8, 7.2, 6.8, 7.5)

#line plot
plot(expression, type = "o", col = "maroon", lwd = 2,
     xlab = "samples", ylab ="expression",
     main = "gene expression across", xaxt = "n")
axis(1, at = 1:length(samples), labels = samples)


library(ggplot2)

#example data 
data <- data.frame(
  sample = c("S1", "S1", "S2", "S2"),
  Taxa = c("PhylumA", "PhylumB", "PhylumA", "PhylumB"),
  Abundance = c(0.25, 0.55, 0.35, 0.80)
)

#bar plot 
ggplot(data, aes(x = sample, y = Abundance, fill = Taxa))+
  geom_bar(stat = "identity", position = "stack")+
  labs(title = "barplot", x = "sample", y = "Abundance")+
  theme_minimal()

#example data 
data <- data.frame(
  sample = c("S1", "S1", "S2", "S2"),
  Taxa = c("PhylumA", "PhylumB", "PhylumA", "PhylumB"),
  Abundance = c(0.25, 0.55, 0.35, 0.80)
)
#position + dodge to cret a grouped bar plot
#grouped bar plot 
ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "grouped bar plot", x= "sample", y = "Abundance")+
  theme_minimal()

#coor flip to make it horizontal 
#horiontal bar plot
ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "horizontal bar plot", x= "sample", y = "Abundance")+
  theme_minimal()

#costum color  
ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  scale_fill_manual(values = c("phylumA" = "#1f77b4", "Phylum" = "#ff7"))
  labs(title = "horizontal bar plot", x= "sample", y = "Abundance", fill = "Taxa")+
  theme_minimal()
  
#add data lables bar 

  ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
    geom_bar(stat = "identity", position = "stack")+
    geom_text(aes(label = round(Abundance, 2)), position = position_stack(vjust = 0.3))
    labs(title = "bar plot")+
    theme_minimal()

#Adjust bar width 
# use width parameter in geom_bar
    ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
      geom_bar(stat = "identity", position = "stack", width = 0.2)+
      labs(title = "adjusted bar width")+
      theme_minimal()
#dynamic colors for many categories
#use RcolorBrewer for plattes
    install.packages("RColorBrewer")
 library(RColorBrewer)
    library(ggplot2)
    ggplot(data, aes(x= sample, y = Abundance, fill = Taxa))+
      geom_bar(stat = "identity", position = "stack")+
      scale_color_brewer(palette = "Set2")+
      labs(title = "dynamic color")+
      theme_minimal()    
      
      