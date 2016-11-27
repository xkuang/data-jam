library(tidyr, dplyr)
library(ggplot2)
library(cluster)
library(mclust)
library(corrplot)
library(rpart)
library(pvclust)
library(fpc)
#Thw target goal of this project is to predict G3 final grade

#first construct clusters based on G1 and G2
D1<- read.table("student-data.csv", sep = ",", header = TRUE)
E1<- read.table("extra-activity.csv", sep = ",", header = TRUE)

D1$scorechange <- D1$G3 - D1$G1

#assume that we do not know G3, select the numeric data + G1 G2 only to construct the data frame
D <- dplyr::select(D1,G1,G2,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)
D2 <- na.omit(D)
D2 <- scale(D2)
mydata <-D2

# Ward Hierarchical Clustering with Bootstrapped p values
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

fit <- kmeans(mydata, 3)
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
plotcluster(mydata, fit$cluster)
#looking at the cluster models, I think the first model with 5 clusters is better to group the data based on grades
D3 <- cbind(D, fit$cluster)
COR <- cor(D3)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
#from this correlation plot, we can see there is a cluster related to the grades"G1,2,teacher request, fit", 
#the grade are negatively correlated with the failures and avatar request(playful)

#build prediction model with decision tree
C <- dplyr::select(D1,G1,G2,G3,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)

#sampling
#split the data based on clustering
C1 <- dplyr::filter(C,fit$cluster == 1)
C1p <- dplyr::select(C1,4:23)

C2 <- dplyr::filter(C,fit$cluster == 2)
C2p <- dplyr::select(C2,4:23)

C3 <- dplyr::filter(C,fit$cluster == 3)
C3p <- dplyr::select(C3,4:23)

# have PCA on the variables
#for C1
C1p <- scale(C1p, center = TRUE)
pca <- prcomp(C1p, scale = TRUE)
#plot the Principle components
pca$sdev
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue
pca$sdev^2
#A summary of our pca will give us the proportion of variance accounted for by each component
summary(pca)
plot(pca, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data
#Now, create a data frame of the transformed data from your pca.
C1a <- as.data.frame(pca$x)
C1b <- dplyr::select(C1a,1:10)
C1b <- cbind(C1b, C1$G1,C1$G2)
CORC1 <- cor(C1b)
corrplot(CORC1, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#from the correlation plot, we can see that the PC7 is the best components (=40/38) correlated with grades G1 G2
#take a close look at the PC7
pca$rotation
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component
loadingsC1 <- abs(pca$rotation) #abs() will make all eigenvectors positive
loadingsC1b <- sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()? * group_by () %>% summarise (n=n()) %>% mutate (rel.freq = n / total)*)
#from the PC9, we can select 5 factors > 0.09.

#for C2
C2p <- scale(C2p, center = TRUE)
pca <- prcomp(C2p, scale = TRUE)
#plot the Principle components
pca$sdev
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue
pca$sdev^2
#A summary of our pca will give us the proportion of variance accounted for by each component
summary(pca)
plot(pca, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data
C2a <- as.data.frame(pca$x)
C2b <- dplyr::select(C2a,1:10)
C2b <- cbind(C2b, C2$G1,C2$G2)
CORC2 <- cor(C2b)
corrplot(CORC2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#from the correlation plot, we can see that the PC4 is the best components (=-28/-17) correlated with grades G1 G2
#take a close look at the PC4
pca$rotation
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component
loadings <- abs(pca$rotation) #abs() will make all eigenvectors positive
loadings2 <- sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()? * group_by () %>% summarise (n=n()) %>% mutate (rel.freq = n / total)*)
#from the PC2, we can select 5 factors > 0.08

#for C3
C3p <- scale(C3p, center = TRUE)
pca <- prcomp(C3p, scale = TRUE)
#plot the Principle components
pca$sdev
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue
pca$sdev^2
#A summary of our pca will give us the proportion of variance accounted for by each component
summary(pca)
plot(pca, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data
C3a <- as.data.frame(pca$x)
C3b <- dplyr::select(C3a,1:10)
C3b <- cbind(C3b, C3$G1,C3$G2)
CORC3 <- cor(C3b)
corrplot(CORC3, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#from the correlation plot, we can see that the PC9 is the best components (=-29/-30) correlated with grades G1 G2
#take a close look at the PC5
pca$rotation
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component
loadings <- abs(pca$rotation) #abs() will make all eigenvectors positive
loadings2 <- sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()? * group_by () %>% summarise (n=n()) %>% mutate (rel.freq = n / total)*)
#from the PC5 and PC2, we can select the factors > 0.08

#in each cluster, take 50% data to build prediction tree
C1s <- dplyr::sample_frac(C1, 0.5, replace = TRUE)
C2s <- dplyr::sample_frac(C2, 0.5, replace = TRUE)
C3s <- dplyr::sample_frac(C3, 0.5, replace = TRUE)

#build decision tree based on cluster (> 0.09)
c.tree1 <- rpart(scorechange ~ G1+ G2 +teacher.requests+customize.character + avatar.requests+famrel+age+freetime, method="class", data=C1s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree1)
post(c.tree1, file = "tree1.ps", title = "C1 tree1")
C1$predict1 <- predict(c.tree1, C1, type = "class")
mismatch1 <- dplyr::filter(C1, scorechange != predict1)

c.tree2 <- rpart(G3 ~ G1+ G2 + failures+age+avatar.requests+teacher.requests, method="class", data=C2s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree2)
post(c.tree2, file = "tree2.ps", title = "C2 tree2")

c.tree3 <- rpart(G3 ~ G1+ G2 + customize.character+avatar.requests+traveltime+teacher.requests, method="class", data=C3s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree3)
#Plot your tree
post(c.tree3, file = "tree3.ps", title = "C3 tree3")

C2$predict1 <- predict(c.tree2, C2, type = "class")
C3$predict1 <- predict(c.tree3, C3, type = "class")

#find the mismatch predictions
mismatch2 <- dplyr::filter(C2, G3 != predict1)
mismatch3 <- dplyr::filter(C3, G3 != predict1)
#getting all the mismatch dataframe together

#build decision tree based on cluster (> 0.09)
c.tree1 <- rpart(G3 ~ G1+ G2 +teacher.requests+studytime+failures+Fedu+Dalc+avatar.requests+famrel, method="class", data=C1s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree1)
post(c.tree1, file = "tree1.ps", title = "C1 tree1")
C1$predict1 <- predict(c.tree1, C1, type = "class")
mismatch1 <- dplyr::filter(C1, G3 != predict1)

c.tree2 <- rpart(G3 ~ G1+ G2 + failures+avatar.requests+teacher.requests+age+freetime+Fedu+Walc+studytime, method="class", data=C2s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree2)
post(c.tree2, file = "tree2.ps", title = "C2 tree2")
C2$predict1 <- predict(c.tree2, C2, type = "class")
mismatch2 <- dplyr::filter(C2, G3 != predict1)

c.tree3 <- rpart(G3 ~ G1+ G2+avatar.requests+teacher.requests+failures+Medu+customize.character, method="class", data=C3s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.tree3)
#Plot your tree
post(c.tree3, file = "tree3.ps", title = "C3 tree3")
C3$predict1 <- predict(c.tree3, C3, type = "class")
#find the mismatch predictions
mismatch3 <- dplyr::filter(C3, G3 != predict1)
#getting all the mismatch dataframe together
error <- 83+ 146 +169

error

