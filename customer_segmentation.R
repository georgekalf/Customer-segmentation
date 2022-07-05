### --- Group 13 --- ###
### --- 2nd Group Assignment --- ###

# ---+ loading libraries
library(tidyr)
library(gbm)
library(rattle)
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(scatterplot3d)
library(tidyverse)
library(ggpubr)
library(kernlab)
library(lle)
library(PCAmixdata)
library(cowplot)
library(lubridate)
library(treemap)
library(reshape)
library(bestNormalize)
library(tidymodels)
library(recipes)
library(d3Tree)
library(ggpubr)
library(factoextra)
library(devtools)
library(treemapify)
library(ggbiplot)


# ---+ importing the file 
Customer_per = read.csv('customer-personality.csv')

### ---+ determines whether the variable is character, 
### ---+ factor, category, binary, discrete numeric, and continuous numeric, 
### ---+ and prints a concise statistical summary according to each
head(Customer_per)
describe(Customer_per)

# ---+ checking the variables 
ls(Customer_per)

# ---+ checking the types of every column of the dataset
str(Customer_per)

# ---+ checking for duplicates in the dataframe
duplicated(Customer_per)
sum(duplicated(Customer_per))

# ---+ dropping ID 
Customer_per <- subset (Customer_per, select = -ID)

# ---+ Dropping missing values 
sum(is.na(Customer_per))  # ---+ check missing values

Customer_per = drop_na(Customer_per)
sum(is.na(Customer_per))

# ---+ replacing variable names
Customer_per[Customer_per == "2n Cycle"] <- "Master"
Customer_per[Customer_per == "Alone"] <- "Single"

# ---+ delete 2 unknown variables (Marital_Status: YOLO and Absurd)
Customer_per = subset(Customer_per, Marital_Status != "YOLO")
Customer_per = subset(Customer_per, Marital_Status != "Absurd")

### ---+ converting year_birth to age 
Customer_per$Year_Birth = 2022 - Customer_per$Year_Birth
names(Customer_per)[names(Customer_per) == "Year_Birth"] = "Age"

# ---+ grouping by Age
Customer_per[Customer_per$Age <= 20, "age_group"] <- "0-20"
Customer_per[Customer_per$Age > 20 & Customer_per$Age <= 40, "age_group"] <- "20-40"
Customer_per[Customer_per$Age > 40 & Customer_per$Age <= 60, "age_group"] <- "40-60"
Customer_per[Customer_per$Age > 60 & Customer_per$Age <= 80, "age_group"] <- "60-80"
Customer_per[Customer_per$Age > 80, "age_group"] <- "> 80"

### --- Visualizations --- ###

### --- BOXPLOTS ---###

## -- Age -- ##
age = ggplot(Customer_per, aes(x=Age)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  scale_x_continuous(labels = scales::comma)

## -- Income -- ##
income = ggplot(Customer_per, aes(x=Income)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  scale_x_continuous(labels = scales::comma)

## -- Recency -- ##
recency = ggplot(Customer_per, aes(x=Recency)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) 

## -- Wines -- ##
wines = ggplot(Customer_per, aes(x=MntWines)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Wines")

## -- Fruits -- ##
fruits = ggplot(Customer_per, aes(x=MntFruits)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5)+
  labs( x = "Number of Fruits")

## -- Meat Products -- ##
meat = ggplot(Customer_per, aes(x=MntMeatProducts)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Meat")

## -- Fish Products -- ##
fish = ggplot(Customer_per, aes(x=MntFishProducts)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Fish")

## -- Sweet Products -- ##
sweet = ggplot(Customer_per, aes(x=MntSweetProducts)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Sweet Products")

## -- Gold Products -- ##
gold = ggplot(Customer_per, aes(x=MntGoldProds)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5)+
  labs( x = "Number of Gold Products")

## -- Number of Deals Purchases -- ##
Ndeals = ggplot(Customer_per, aes(x=NumDealsPurchases)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Deals Purchases")

## -- Number of Web Purchases -- ##
Nweb = ggplot(Customer_per, aes(x=NumWebPurchases)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Web Purchases")

## -- Number of Catalog Purchases -- ##
Ncatalog = ggplot(Customer_per, aes(x=NumCatalogPurchases)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Catalog Purchases")

## -- Number of Store Purchases -- ##
Nstore = ggplot(Customer_per, aes(x=NumStorePurchases)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Store Purchases")

## -- Number of Web Visits Per Month -- ##
Nwebvisits = ggplot(Customer_per, aes(x=NumWebVisitsMonth)) + 
  geom_boxplot(color="black", fill="lightblue", alpha=0.5) +
  labs( x = "Number of Web Visits Per Month")

### --- Designing the final plot --- ###
tittlebox =  ggdraw() + draw_label("Boxplots of the Dataset", fontface='bold')
boxplots = plot_grid(age, income, recency, wines,fruits,meat, fish, sweet, gold, Ndeals,Nweb, Ncatalog, Nstore, Nwebvisits , ncol = 3 )
plot_grid(tittlebox, boxplots, nrow = 2,
          rel_heights = c(0.2, 1, 1))

# ---+ dropping outliers for 'Income' and 'Age'
Customer_per <- Customer_per %>% filter(Income <= 500000)
Customer_per <- Customer_per %>% filter(Age <= 100)
df_new <- Customer_per

# ---+ converting factor values 
copy_factor <- data.frame(Customer_per)
col_names <- names(copy_factor)
copy_factor[,col_names] <- lapply(copy_factor[,col_names] , factor)

# ---+ converting numerical values 
copy_numeric <- char2numeric(copy_factor)

# ---+  correlation matrix 
cor0 <- round(cor(copy_numeric, use = "pairwise.complete.obs"), 2)

### ---+  visualizing correlation matrix 
corrplot(cor0, method="pie", type = "upper")

# ---+ Continuing with some visualizations of our dataset
# ---+ linear representation - age distribution
ggplot(df_new, aes(x = Age)) +
  geom_density(fill = "lightblue") + 
  labs(title = "Customers by age")

# ---+ proportion of customers against age distribution 
ggplot(df_new, aes(x = Age)) +
  geom_dotplot(fill = "yellow", 
               color = "black",
               binwidth = 67/100,) +
  labs(title = "Customers by age",
       y = "Proportion",
       x = "Age")

# ---+ Lollipop Education vs Income
lollipop = aggregate(Income ~ Education , data = df_new, FUN = mean)

ggplot(lollipop, aes(x=Education, y=Income )) + 
  geom_segment( aes(x=Education, xend=Education, y=0, yend=Income), color="blue", linetype="dotdash") +
  geom_point( color="orange", size=4, alpha=0.6)  +
  coord_flip() +
  theme(axis.text = element_text(face="bold"),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma)

# ---+ TreeMap Marital status vs Income 
tree_map_plot = aggregate(Income ~ Marital_Status , data = df_new, FUN = sum)

ggplot(tree_map_plot,
            aes(x = Marital_Status, fill = Income,
                area = Income,
                label = Marital_Status)) + 
              geom_treemap()+
              geom_treemap_text(color = "White",
                                place = "center") +
  scale_fill_continuous(labels = scales::comma) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

### ---  Barplots --- ###

#1
Wines <- ggplot(df_new, aes(x=MntWines, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on wine in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE) +    # ---+ to remove legend for fill aesthetic
  scale_x_continuous(labels = scales::comma)
#2
Fruits <- ggplot(df_new, aes(x=MntFruits, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on fruits in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE) + 
  scale_x_continuous(labels = scales::comma)
#3
Meat <- ggplot(df_new, aes(x=MntMeatProducts, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on meat in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE) + 
  scale_x_continuous(labels = scales::comma)
#4
Fish = ggplot(df_new, aes(x=MntFishProducts, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on fish in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE) + 
  scale_x_continuous(labels = scales::comma)
#5
Sweets = ggplot(df_new, aes(x=MntSweetProducts, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on sweets in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE) + 
  scale_x_continuous(labels = scales::comma)
#6
Gold = ggplot(df_new, aes(x=MntGoldProds, y=Education, fill = Education)) + 
  geom_bar(stat = "identity") + xlab("Amount spent on gold in last 2 years") +
  scale_fill_manual(values = c("orange", "wheat", "thistle", "lightblue")) +
  guides( fill = FALSE)   +  
  scale_x_continuous(labels = scales::comma)

# ---+ Designing the final plot 
title <- ggdraw() + draw_label("Education vs Amount spend on Products", fontface='bold')
bottom_row <- plot_grid(Wines, Fruits, Meat, Fish, Sweets, Gold, ncol = 1 )
plot_grid(title, bottom_row, nrow = 2,
          rel_heights = c(0.2, 1, 1))

#### --- Age group vs Purchases --- ###

# ---+ Creating a new data set for the plots 
data = aggregate(cbind(NumWebPurchases,NumCatalogPurchases,NumDealsPurchases,NumStorePurchases) ~ age_group , data = df_new, FUN = sum)

### --- Number of Web Purchases by Age group --- ###
NumWebPurchases <- ggplot(data) +
  # Making custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:6) * 1000),
    color = "lightgrey"
  ) +
  # Adding bars to represent the Number of Web Purchases 
  geom_col(aes(
      x = reorder(str_wrap(age_group, 5), NumWebPurchases),
      y = NumWebPurchases,
      fill = NumWebPurchases
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  # Adding dots to represent the Number of Web Purchases 
  geom_point(aes(
      x = reorder(str_wrap(age_group, 5),NumWebPurchases),
      y = NumWebPurchases
    ),
    size = 3,
    color = "gray12"
  ) +
  # Lollipop shaft for the sum of Number of Web Purchases per age group
  geom_segment(aes(
      x = reorder(str_wrap(age_group, 5), NumWebPurchases),
      y = 0,
      xend = reorder(str_wrap(age_group, 5), NumWebPurchases),
      yend = 5000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar() +
  # Adding labels
  labs(title = "Web Purchases by age group") +
  theme( panel.background = element_rect(fill = "white", color = "white"),panel.grid = element_blank(), 
         panel.grid.major.x = element_blank(),
         # Removing axis ticks and text
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_text(color = "gray12", size = 12),
         # Moving the legend to the left
         legend.position = "left")

### --- Number of Catalog Purchases by Age group --- ###

NumCatalogPurchases <- ggplot(data) +
  # Making custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:5) * 1000),
    color = "lightgrey"
  ) +
  # Adding bars to represent the Number of Catalog Purchases 
  geom_col(aes(
      x = reorder(str_wrap(age_group, 5), NumCatalogPurchases),
      y = NumCatalogPurchases,
      fill = NumCatalogPurchases
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Adding dots to represent the sum of Number of Catalog Purchases 
  geom_point(aes(
      x = reorder(str_wrap(age_group, 5),NumCatalogPurchases),
      y = NumCatalogPurchases
    ),
    size = 3,
    color = "gray12"
  ) +
  # Lollipop shaft for the sum of Number of Web Purchases per age group
  geom_segment(aes(
      x = reorder(str_wrap(age_group, 5), NumCatalogPurchases),
      y = 0,
      xend = reorder(str_wrap(age_group, 5), NumCatalogPurchases),
      yend = 2500
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar() +
  # Adding labels
  labs(
    title = "Catalog Purchases by age group") +
  theme( panel.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), 
         panel.grid.major.x = element_blank(),
         # Removing axis ticks and text
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_text(color = "gray12", size = 12),
         # Moving the legend to the left
         legend.position = "left")

### --- Number of Deals Purchases by Age group --- ###

NumDealsPurchases <- ggplot(data) +
  # ---+ Making custom panel grid
  geom_hline(aes(yintercept = y), 
    data.frame(y = c(0:5) * 1000),
    color = "lightgrey"
  ) +
  # Adding bars to represent the Number of Deals Purchases 
  geom_col(aes(
      x = reorder(str_wrap(age_group, 5), NumDealsPurchases),
      y = NumDealsPurchases,
      fill = NumDealsPurchases
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  # Adding dots to represent the sum of the  Deals of Web Purchases 
  geom_point(aes(
      x = reorder(str_wrap(age_group, 5),NumDealsPurchases),
      y = NumDealsPurchases
    ),
    size = 3,
    color = "gray12"
  ) +
  # Lollipop shaft for the sum of Number of Deals Purchases per age group
  geom_segment(aes(
      x = reorder(str_wrap(age_group, 5), NumDealsPurchases),
      y = 0,
      xend = reorder(str_wrap(age_group, 5), NumDealsPurchases),
      yend = 3000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar()  +
  # Adding labels
  labs(
    title = "Deals Purchases by age group") +
  theme( panel.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), 
         panel.grid.major.x = element_blank(),
         # Removing axis ticks and text
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_text(color = "gray12", size = 12),
         # Moving the legend to the left
         legend.position = "left"
  )

### --- Number of Store Purchases by Age group --- ###

NumStorePurchases <- ggplot(data) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:7) * 1000),
    color = "lightgrey"
  ) +
  # Adding bars to represent the sum of the Number of Store Purchases 
  geom_col(
    aes(
      x = reorder(str_wrap(age_group, 5), NumStorePurchases),
      y = NumStorePurchases,
      fill = NumStorePurchases
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  # Adding dots to represent the sum of the  Deals of Web Purchases 
  geom_point(
    aes(
      x = reorder(str_wrap(age_group, 5),NumStorePurchases),
      y = NumStorePurchases
    ),
    size = 3,
    color = "gray12"
  ) +
  # Lollipop shaft for the sum of Store Purchases per age group
  geom_segment(
    aes(
      x = reorder(str_wrap(age_group, 5), NumStorePurchases),
      y = 0,
      xend = reorder(str_wrap(age_group, 5), NumStorePurchases),
      yend = 6000
    ),
    linetype = "dashed",
    color = "black"
  ) +
  coord_polar()  +
  # Adding labels
  labs(
    title = "Store Purchases by age group") +
  theme( panel.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), 
         panel.grid.major.x = element_blank(),
         # Removing axis ticks and text
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_text(color = "gray12", size = 12),
         # Moving the legend to the left
         legend.position = "left"
  )

# ---+ Designing the final plot 
title_Circular <- ggdraw() + draw_label("Number of Purchases by age", fontface='bold')
bottom_Circular <- plot_grid(NumWebPurchases, NumCatalogPurchases, NumStorePurchases,NumDealsPurchases, ncol = 2 )
plot_grid(title_Circular, bottom_Circular, nrow = 2,
          rel_heights = c(0.2, 1, 1))

################################################################################
# ---+ Continue with some pre-processing
head(df_new$Dt_Customer, 30)

# --+ extracting the value of the year and convert it into factor variable
df_new$Dt_Customer <- dmy(df_new$Dt_Customer)
df_new$Dt_Customer <- year(df_new$Dt_Customer)
df_new$Dt_Customer <- as.factor(df_new$Dt_Customer)

df_new$Dt_Customer <- fct_collapse(df_new$Dt_Customer,
                                   '3' = '2014',
                                   '2' = '2013',
                                   '1' = '2012')

df_new$Dt_Customer <- as.numeric(levels(df_new$Dt_Customer))[df_new$Dt_Customer]

# ---+ dropping age_group column
df_new <- subset(df_new, select = -age_group)

# ---+ checking types of the features
str(df_new)

# ---+ converting characters into numeric
df_new$Education <- fct_collapse(df_new$Education,
                                       "4" = "PhD",
                                       "3" = "Master",
                                       "2" = "Graduation",
                                       "1" = "Basic")
df_new$Education <- as.numeric(levels(df_new$Education))[df_new$Education]

df_new$Marital_Status <- fct_collapse(df_new$Marital_Status,
                                            "1" = "Married",
                                            "2" = "Together",
                                            "3" = "Divorced",
                                            "4" = "Widow",
                                            "5" = "Single")

df_new$Marital_Status <- as.numeric(levels(df_new$Marital_Status))[df_new$Marital_Status]

# ---+ Factor Analysis 
copy_fa <- data.frame(df_new)

# ---+ visualize correlation matrix to get an overview
fa.cor = cor(copy_fa)
corrplot(fa.cor, method="pie", type = "upper")

# ---+ determine the number of factors of FA
fa.eigen = eigen(fa.cor)

fa.eigen$values # check the eigenvalues (the first 5 eigenvalues are greater than 1)
cumsum(fa.eigen$values)/sum(fa.eigen$values) # the percentage of variance explained by factors

# ---+  use the scree plot
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 5 based on elbow method

# ---+ factor analysis
fa.res = factanal(x = copy_fa, factors = 5, rotation = "none", lower = 0.001)
fa.res

# ---+ factor rotation with promax
fa.res = factanal(x = copy_fa, factors = 5, rotation = "promax", lower = 0.001)
print(fa.res, cut = 0.3)

# ---+ factor scores
fa.res = factanal(x = copy_fa, factors = 5, rotation = "promax", scores = "Bartlett", lower = 0.001)
head(fa.res$scores)

# ---+ kmeans without PCA 
# ---+ scaling the dataset
df_kmeans <- scale(df_new)
wss_nopca <- function(k){
  kmeans(df_kmeans, k, nstart = 200)$tot.withinss
}
fviz_nbclust(df_kmeans, kmeans, method = "wss")
# ---+ slope decreases after 4 clusters

# ---+ calculating kmeans for 4 clusters
set.seed(732)
res_kmeans_nopca <- kmeans(df_kmeans, centers = 4, nstart = 25)
print(res_kmeans_nopca)
res_kmeans_nopca$cluster
head(res_kmeans_nopca$cluster, 10)

# ---+ mean of each variable by clusters using the df_kmeans data
aggregate(df_kmeans, by = list(cluster = res_kmeans_nopca$cluster), mean)

# ---+ size of the clusters
res_kmeans_nopca$size
res_kmeans_nopca$centers # ---+ mean

# ---+ visualization of clusters
dev.off()
fviz_cluster(res_kmeans_nopca, data = df_kmeans, geom = "point",
             ellipse.type = "convex", ggtheme = theme_bw())

# ---+ trying PCA (unsupervised learning, reducing noise)
df_pca <- recipe(~., data = df_new)

# ---+  ensuring symmetric distribution
df_pca <- df_pca %>% step_orderNorm(all_numeric_predictors())

# ---+ centering and scaling predictors (mean -> 0, st. deviation -> 1)
df_pca <- df_pca %>% step_normalize(all_numeric_predictors())

# ---+ applying PCA (checking how many PCs explain most of the variance)
check_pca <- df_pca %>% prep() %>% bake(new_data = NULL) %>% prcomp()
tidy(check_pca, matrix = "eigenvalues")

# ---+ up to 19 components explain 100% of the variance
df_pca <- df_pca %>% step_pca(all_numeric_predictors(), num_comp = 19) %>% prep %>% bake(new_data = NULL)

# ---+ visualization the dimensions
fviz_eig(check_pca)

# ---+ Graph of variables. Positive correlated variables point to the same side of the plot. 
# ---+ Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(check_pca,
             col.var = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# ---+ hierarchical clustering
distance <- dist(df_pca, method = "euclidean")
hplot <- hclust(distance, method = "complete")
plot(hplot, hang = -1, cex = 0.6)
# ---+ 2 groups are emerging

# ---+ KMEANS after PCA
wss <- function(k){
  kmeans(df_pca, k, nstart = 200)$tot.withinss
}
fviz_nbclust(df_pca, kmeans, method = "wss")
# ---+ slope decreases after 2 clusters

# ---+ calculating kmeans for 2 clusters
set.seed(732)
res_kmeans <- kmeans(df_pca, centers = 2, nstart = 25)
print(res_kmeans)
res_kmeans$cluster
head(res_kmeans$cluster, 10)

# ---+ mean of each variable by clusters using the df_pca data
aggregate(df_pca, by = list(cluster = res_kmeans$cluster), mean)

# ---+ size of the clusters
res_kmeans$size
res_kmeans$centers # ---+ mean

# ---+ visualization of clusters
dev.off()
fviz_cluster(res_kmeans, data = df_pca, geom = "point",
             ellipse.type = "convex", ggtheme = theme_bw())

# ---+ Trying mixed PCA (categorical & numerical variables)

# ---+ splitting the dataset
# ---+ dropping age_group column
Customer_per <- subset(Customer_per, select = -age_group)
split = splitmix(Customer_per) # ---+ columns of class "integer" are considered quantitative
X1 = split$X.quanti
X2 = split$X.quali

res.pcamix = PCAmix(X.quanti = X1, X.quali = X2, rename.level = TRUE, graph = FALSE)
res.pcamix$eig

# ---+ have a look at the results
par(mfrow=c(2,2))
plot(res.pcamix, choice = "ind", coloring.ind = X2$Education, label = FALSE,
     posleg = "bottomright", main ="(a) Observations")
plot(res.pcamix, choice = "levels", xlim = c(-1.5,2.5), main = "(b) Levels")
plot(res.pcamix, choice = "cor", main = "(c) Numerical variables")
plot(res.pcamix, choice = "sqload", coloring.var = T, leg = TRUE,
     posleg = "topright", main = "(d) All variables")
sort(res.pcamix$ind$coord[,2])[1:10]

summary(res.pcamix)

# ---+ kernel PCA (might improve the noise in comparison with pca, mixpca)
df_new_scaled <- data.frame(scale(df_new))

par(mfrow = c(1, 4))
##### sigma = 0.01
kpc = kpca(~.,data=df_new_scaled,kernel="rbfdot",
         kpar=list(sigma=0.01),features=2)

plot(rotated(kpc),col="red",pch=16,cex=1.5,
     xlab = "KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main = "sigma=0.01")

##### sigma = 0.1
kpc1 = kpca(~.,data=df_new_scaled,kernel="rbfdot",
           kpar=list(sigma=0.1),features=2)

plot(rotated(kpc1),col="red",pch=16,cex=1.5,
     xlab = "KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main = "sigma=0.1")

##### sigma = 0.5
kpc2 = kpca(~.,data=df_new_scaled,kernel="rbfdot",
            kpar=list(sigma=0.5),features=2)

plot(rotated(kpc2),col="red",pch=16,cex=1.5,
     xlab = "KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main = "sigma=0.5")

##### sigma = 1
kpc3 = kpca(~.,data=df_new_scaled,kernel="rbfdot",
          kpar=list(sigma=1),features=2)

plot(rotated(kpc3), col = "red", 
     pch = 16, cex = 1.5,
     xlab = "KPC1",ylab = "KPC2", 
     xlim = c(-8.80,10.06), ylim = c(-7.0,10.70),
     main = "sigma = 1")

### Running Kmeans after Kpca (sigma = 0.01) --- ###
# ---+ converting to dataframe 
kpca.dataframe <- as.data.frame(rotated(kpc))

wss <- function(k){
  kmeans(kpca.dataframe, k, nstart = 200)$tot.withinss
}
fviz_nbclust(kpca.dataframe, kmeans, method = "wss")
# ---+ slope decreases after 4 clusters

# ---+ calculating kmeans for 4 clusters
set.seed(732)
res_kmeans_kpca <- kmeans(kpca.dataframe, centers = 4, nstart = 25)
print(res_kmeans_kpca)
res_kmeans_kpca$cluster
head(res_kmeans_kpca$cluster, 10)

# ---+ mean of each variable by clusters using the kpca.dataframe data
aggregate(kpca.dataframe, by = list(cluster = res_kmeans_kpca$cluster), mean)

# ---+ size of the clusters
res_kmeans_kpca$size
res_kmeans_kpca$centers # ---+ mean

# ---+ visualization of clusters
dev.off()
fviz_cluster(res_kmeans_kpca, data = kpca.dataframe, geom = "point",
             ellipse.type = "convex", ggtheme = theme_bw())

### --- Comparing the results of our analysis ---# 
print(res_kmeans_nopca) # ---+ in the initial dataset
print(res_kmeans) # ---+ after use of PCA
print(res_kmeans_kpca) # ---+ after implementing KPCA

# ---+ KPCA and then kmeans seems to be the most appropriate method to do our segmentation

# ---+ Cluster Analysis

targeted_clustered <- df_new %>% mutate(cluster = res_kmeans_kpca$cluster)

# ---+ transforming our dataset to do the clustering
explore <- targeted_clustered
explore$Education <- as.factor(explore$Education)
explore$cluster <- as.factor(explore$cluster)
ggplot(data = explore) + geom_jitter(mapping = aes(x = cluster, y = Education)) +
  scale_y_discrete(labels=c("1" = "Basic", "2" = "Graduation",
                            "3" = "Master", "4" = "PhD")) +
  ggtitle("Clusters based on the Education")

# ---+ checking clustering with Marital Status
explore <- targeted_clustered
explore$Marital_Status <- as.factor(explore$Marital_Status)
explore$cluster <- as.factor(explore$cluster)
ggplot(data = explore) + geom_jitter(mapping = aes(x = cluster, y = Marital_Status)) +
  scale_y_discrete(labels = c("1" = "Married",
                              "2" = "Together",
                              "3" = "Divorced",
                              "4" = "Widow",
                              "5" = "Single")) +
  ggtitle("Clusters based on the Marital Status")

# ---+ Based on Income
ggplot(explore) + 
  geom_histogram(mapping = aes(x = Income, fill = cluster), bins = 50) + 
  facet_wrap(~cluster) + ggtitle("Histograms of the clusters based on Income")

# ---+ Based on Number of Purchases
targeted_clustered %>%
  dplyr::select(cluster, NumWebPurchases, NumCatalogPurchases, NumStorePurchases, NumDealsPurchases, NumWebVisitsMonth)%>%
  melt(id='cluster')%>%
  ggplot(aes(as_factor(cluster), fill = cluster,value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol = 5) +
  labs( x = "Clusters") + 
  ggtitle("Boxplots representing the clusters and the number of purchases and visits on web")

# ---+ Based on the amount of money spent
targeted_clustered%>%
  dplyr::select(cluster, MntWines, MntFruits, MntMeatProducts, MntSweetProducts, MntGoldProds)%>%
  melt(id='cluster')%>%
  ggplot(aes(as_factor(cluster), fill = cluster, value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol=5) +
  labs( x = "Clusters") +
  ggtitle("Boxplots representing the clusters and the amount spent on different products")

# ---+ Based on the Age
targeted_clustered %>%
  ggplot(aes(as_factor(cluster), fill = cluster,Age))+
  geom_boxplot() +
  labs( x = "Clusters") +
  ggtitle("Boxplots representing clusters against the age distribution")

# ---+ Based on the number of Teens at home
targeted_clustered$Teenhome <- as.factor(targeted_clustered$Teenhome)
ggplot(data = targeted_clustered) + 
  geom_jitter(mapping = aes(x=cluster,y=Teenhome)) +
  labs( x = "Clusters") +
  ggtitle("Clusters representing the number of teens at home")

# ---+ Based on the number of Kids at home 
targeted_clustered$Kidhome <- as.factor(targeted_clustered$Kidhome)
ggplot(data = targeted_clustered) + geom_jitter(mapping = aes(x=cluster,y=Kidhome)) +
  labs( x = "Clusters") +
  ggtitle("Clusters representing the number of kids at home")

# ---+ Based on the date of registration of the customers in the company
targeted_clustered$Dt_Customer <- as.factor(targeted_clustered$Dt_Customer)
ggplot(data = targeted_clustered) + geom_jitter(mapping = aes(x=cluster,y=Dt_Customer)) +
  labs( x = "Clusters") + scale_y_discrete(labels = c('3' = '2014',
                                                      '2' = '2013',
                                                      '1' = '2012')) +
  ggtitle("Representation of the clusters against the enrollment date with the company")

