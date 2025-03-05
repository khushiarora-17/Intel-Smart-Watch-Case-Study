
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")
install.packages("ggplot2")

library(readxl)       # Reading Excel files
library(tidyverse)    # Data manipulation and visualization
library(cluster)      # Clustering methods
library(openxlsx)     # Exporting results to Excel
library(ggplot2)      # Advanced plotting


# Display column names, summary statistics, and structure of the dataset
print(names(SmartWatch_Data_File))
summary(SmartWatch_Data_File)
str(SmartWatch_Data_File)

# DATA PREPARATION FOR CLUSTERING

df_cluster <- SmartWatch_Data_File %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# Standardize the selected data (mean = 0, sd = 1)
df_std <- scale(df_cluster)


# HIERARCHICAL CLUSTERING ANALYSIS

#Euclidean distance between observations
dist_euc <- dist(df_std, method = "euclidean")

# hierarchical clustering using Ward's method
hc <- hclust(dist_euc, method = "ward.D2")

# dendrogram to visualize the clustering structure
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Observations", ylab = "Height")
# rectangles to show 4 clusters (based on the elbow plot below)
rect.hclust(hc, k = 4, border = "red")

# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS (Elbow Method)

sorted_heights <- sort(hc$height, decreasing = TRUE)
# Correctly extracting the number of clusters vs. merge height
elbow_data <- data.frame(
  Clusters = seq_along(hc$height),  # Number of clusters
  Height = rev(hc$height)  # Keep the natural order
)

# Elbow plot using ggplot2
ggplot(elbow_data[1:10, ], aes(x = Clusters, y = Height)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Plot (Hierarchical Clustering)", 
       x = "Number of Clusters", y = "Height") +
  scale_x_continuous(breaks = 1:10) + 
  theme_minimal()

elbow_data <- data.frame(
  Clusters = seq_along(hc$height),  # Generates correct cluster numbers
  Height = rev(hc$height)  # Ensures natural hierarchical order
)

ggplot(elbow_data[1:10, ], aes(x = Clusters, y = Height)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Plot (Hierarchical Clustering)", 
       x = "Number of Clusters", y = "Height") +
  scale_x_continuous(breaks = 1:10) +  # Fix: Properly labels x-axis
  theme_minimal()


# Based on the elbow plot and our analysis, we choose 4 clusters.

# ASSIGN CLUSTERS AND PROFILE SEGMENTS

# CutING dendrogram to form 4 clusters
clusters <- cutree(hc, k = 4)

# Appending cluster assignments back to the original dataset
data_with_clusters <- SmartWatch_Data_File %>% mutate(Cluster = as.factor(clusters))

# Checking the size of each cluster

cluster_proportions <- round(prop.table(table(data_with_clusters$Cluster)) * 100, 2)
print(cluster_proportions)

sum(table(data_with_clusters$Cluster))

table(data_with_clusters$Cluster)

# Profile clusters by computing mean values of the product preference variables
cluster_profiles <- data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "{col}_mean"))
print(cluster_profiles)

# Export the cluster profiles to an Excel file for your report
write.xlsx(cluster_profiles, "Intel_Smartwatch_Cluster_Profiles.xlsx")


# Reshape data to long format for ggplot
data_long <- data_with_clusters %>%
  pivot_longer(cols = c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style),
               names_to = "Variable", values_to = "Value")

# Create faceted boxplot
ggplot(data_long, aes(x = Cluster, y = Value, fill = as.factor(Cluster))) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +  # Creates separate plots for each variable
  labs(title = "Distribution of Clustering Variables by Cluster",
       x = "Cluster", y = "Value") +
  theme_minimal()


# demographic insights, profile demographics per cluster.
demographic_profiles <- data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(
    Female_Percentage = round(mean(Female, na.rm = TRUE) * 100, 2),
    Degree_Holders = round(mean(Degree, na.rm = TRUE) * 100, 2),
    Avg_Income = round(mean(Income, na.rm = TRUE), 2),
    Avg_Age = round(mean(Age, na.rm = TRUE), 2),
    AmznP_Percentage = round(mean(AmznP, na.rm = TRUE) * 100, 2)
  )
 print(demographic_profiles)
    
    
# Summarizing all key variables per cluster
full_cluster_profiles <- data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(
    ConstCom_M = round(mean(ConstCom, na.rm = TRUE), 4),
    TimelyInf_M = round(mean(TimelyInf, na.rm = TRUE), 4),
    TaskMgm_M = round(mean(TaskMgm, na.rm = TRUE), 4),
    DeviceSt_M = round(mean(DeviceSt, na.rm = TRUE), 4),
    Wellness_M = round(mean(Wellness, na.rm = TRUE), 4),
    Athlete_M = round(mean(Athlete, na.rm = TRUE), 4),
    Style_M = round(mean(Style, na.rm = TRUE), 4),
    AmznP_M = round(mean(AmznP, na.rm = TRUE), 4),  # Amazon Prime membership (binary)
    Female_M = round(mean(Female, na.rm = TRUE), 4),  # Female proportion (binary)
    Degree_M = round(mean(Degree, na.rm = TRUE), 4),  # Education level
    Income_M = round(mean(Income, na.rm = TRUE), 4),  # Income level
    Age_M = round(mean(Age, na.rm = TRUE), 4)  # Age
  )



# Print the cluster profile table
print(full_cluster_profiles)

# Export the table to Excel for the report
write.xlsx(full_cluster_profiles, "IntelSmartwatch_Full_Cluster_Profiles.xlsx")

