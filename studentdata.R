library(tidyr, dplyr)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(cluster)
library(useful)
library(pvclust)
library(mclust)
library(fpc)
library(corrplot)
library(ggmap) 


D1<- read.table("student-data.csv", sep = ",", header = TRUE)
D2 <- dplyr::select(D1,G1,G2,G3)
D3 <- na.omit(D2)
D3 <- scale(D3)

mydata <-D3

fit <- Mclust(mydata)
plot(fit)
summary(fit)

#----------------------------------------------------
#  Gaussian finite mixture model fitted by EM algorithm 
#----------------------------------------------------
  
#  Mclust EEV (ellipsoidal, equal volume and shape) model with 8 components:
  
#  log.likelihood    n df       BIC       ICL
#-116.7782 1000 58 -634.2062 -700.6705

#Clustering table:
#  1   2   3   4   5   6   7   8 
#7 108   7 319  11 438  61  49 

#from the Mclust Models, it suggests that the BIC model with 8 clusters are the best models for clustering

fit1 <- kmeans(mydata, 8)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph

clusplot(mydata, fit1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions

plotcluster(mydata, fit1$cluster)

fit2 <- kmeans(mydata, 4)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph

clusplot(mydata, fit2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions

plotcluster(mydata, fit2$cluster)

#analysis the cluster models, I think the first model with 8 clusters are better to group the data

D2 <- data.frame(D2, fit1$cluster)
#D2$fit1.cluster <- as.factor(D2$fit1.cluster)

# have PCA on the variables
write.csv(D1, file = "D1.csv", row.names = FALSE)

#select the numeric data only to construct the data frame
D4 <- dplyr::select(D1,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)

COR <- cor(D4)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
#from this correlation plot, we can see there is two cluster of the variables the grades"G1,2,3", study behavior "time in section, time for task, levels completion, studytime, health"
#the grade are negatively correlated with the failures and avatar request(playful)

D10 <- data.frame(D4, fit1$cluster)
COR <- cor(D10)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

D5 <- scale(D4, center = TRUE)

pca <- prcomp(D5, scale = TRUE)

#23  variables and  1000 observations.

#plot the Principle components
pca$sdev

#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue

pca$sdev^2

#A summary of our pca will give us the proportion of variance accounted for by each component

summary(pca)
#Importance of components:
#  PC1    PC2     PC3     PC4    PC5     PC6     PC7     PC8     PC9    PC10    PC11
#Standard deviation     2.012 1.6788 1.42620 1.30119 1.1570 1.07292 1.03906 0.99080 0.94454 0.94193 0.91209
#Proportion of Variance 0.176 0.1225 0.08844 0.07361 0.0582 0.05005 0.04694 0.04268 0.03879 0.03858 0.03617
#Cumulative Proportion  0.176 0.2986 0.38701 0.46062 0.5188 0.56887 0.61581 0.65850 0.69728 0.73586 0.77203
#PC12    PC13    PC14    PC15    PC16   PC17    PC18    PC19    PC20    PC21    PC22
#Standard deviation     0.88601 0.86604 0.79809 0.77231 0.73977 0.6917 0.63143 0.56845 0.55699 0.42844 0.39105
#Proportion of Variance 0.03413 0.03261 0.02769 0.02593 0.02379 0.0208 0.01734 0.01405 0.01349 0.00798 0.00665
#Cumulative Proportion  0.80616 0.83877 0.86646 0.89240 0.91619 0.9370 0.95433 0.96838 0.98187 0.98985 0.99650
#PC23
#Standard deviation     0.2838
#Proportion of Variance 0.0035
#Cumulative Proportion  1.0000

plot(pca, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data

#Now, create a data frame of the transformed data from your pca.
D6 <- as.data.frame(pca$x)
D7 <- dplyr::select(D6,1:10)

#Attach the variable "mean_correct" from your original data frame to D3.

D8 <- cbind(D7, D2)
#Now re-run your scatterplots and correlations between the transformed data and mean_correct. If you had dropped some components would you have lost important infomation about mean_correct?

COR2 <- cor(D8)
corrplot(COR2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#from the correlation plot, we can see that the PC1 is the only components correlated with grades G1 G2 and G3 and so is the cluster indicator: fit1.cluster

#take a close look at the PC1
pca$rotation

#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component

loadings <- abs(pca$rotation) #abs() will make all eigenvectors positive

loadings2 <- sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()? * group_by () %>% summarise (n=n()) %>% mutate (rel.freq = n / total)*)


