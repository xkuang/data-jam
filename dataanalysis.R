library(tidyr, dplyr)
library(rpart)
library(pvclust)
library(corrplot)
library(mclust)
library(cluster)
library(ggplot2)
library(fpc)

D1<- read.table("student-data.csv", sep = ",", header = TRUE)
D2<- read.table("recode.csv", sep = ",", header = TRUE)
E1<- read.table("extra-activity.csv", sep = ",", header = TRUE)
DE <- dplyr::full_join(D2, E1, by = "id")

DE$scorechange1 <- DE$G2 - DE$G1
DE$scorechange2 <- DE$G3 - DE$G2
DE$scorechange3 <- DE$scorechange1 + DE$scorechange2
DE$scorechange3b <- DE$G3 - DE$G1
DE$diffscorechange <- DE$scorechange2 - DE$scorechange1

DE1 <- dplyr::filter(DE, G3 != 0)
DE1 <- dplyr::filter(DE1, G3 != 1)
DE1 <- dplyr::filter(DE1, G2 != 0)
DE1 <- dplyr::filter(DE1, G1 != 0)

S1 <- dplyr::filter(DE1, school != "MS")
S2 <- dplyr::filter(DE1, school != "GP")
T <- dplyr::filter(DE1, Pstatus != "A")
A <- dplyr::filter(DE1, Pstatus != "T")
G <- dplyr::filter(DE1, famsize != "LE3")
L <- dplyr::filter(DE1, famsize != "GT3")
F <- dplyr::filter(DE1, sex != "M")
M <- dplyr::filter(DE1, sex != "F")

plot(DE1$address,DE1$G3)
plot(DE1$address,DE1$scorechange3)

plot(DE1$sex,DE1$G3)
plot(DE1$sex,DE1$scorechange3)

plot(DE1$schoolsup,DE1$G3)
plot(DE1$schoolsup,DE1$G1)
plot(DE1$schoolsup,DE1$scorechange3)

plot(DE1$famsize,DE1$scorechange3)

plot(DE1$school,DE1$G3)
plot(DE1$school,DE1$scorechange3)

plot(DE1$sex,DE1$avatar.requests)
plot(DE1$sex,DE1$customize.character)

DE2 <- dplyr::select(DE1,G1,G2,G3,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task,school.1,gender,citystatus,famsize.1,Pstatus.1,Mjob.1,Fjob.1,schoolsup.1,famsup.1)
mydata <- DE2

# Model Based Clustering
fit <- Mclust(mydata)
#plot(fit) # plot results 
summary(fit)

#> summary(fit)
#----------------------------------------------------
# Gaussian finite mixture model fitted by EM algorithm 
#----------------------------------------------------

# Mclust EEV (ellipsoidal, equal volume and shape) model with 8 components:

#  log.likelihood    n df       BIC       ICL
#-116.7782        1000 58   -634.2062 -700.6705

#Clustering table:
#  1   2   3   4   5   6   7   8 
#  7 108   7 319  11 438  61  49 

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=8) # cut tree into 8 clusters
# draw dendogram with red borders around the 8 clusters 
rect.hclust(fit, k=8, border="red")
# Ward Hierarchical Clustering with Bootstrapped p values
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

COR <- cor(mydata)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

# Multiple Linear Regression Example 
fit1 <- lm(G3 ~ age, data=mydata)
fit2 <- lm(G3 ~ G2+age, data=mydata)
fit3 <- lm(G3 ~ G1+G2+age, data=mydata)
fit4 <- lm(G3 ~ G1+G2+age+failures, data=mydata)
fit5 <- lm(G3 ~ G1+G2+age+failures+absences, data=mydata)
fit6 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime, data=mydata)
fit7 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime+health, data=mydata)
fit8 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime+health+schoolsup.1, data=mydata)
fit9 <- lm(G3 ~ G1+G2+failures+absences+Walc+avatar.requests+schoolsup.1+studytime+Fedu+teacher.requests, data=mydata)
anova(fit1, fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9)
#Analysis of Variance Table

#Model 1: G3 ~ age
#Model 2: G3 ~ G2 + age
#Model 3: G3 ~ G1 + G2 + age
#Model 4: G3 ~ G1 + G2 + age + failures
#Model 5: G3 ~ G1 + G2 + age + failures + absences
#Model 6: G3 ~ G1 + G2 + age + failures + absences + traveltime
#Model 7: G3 ~ G1 + G2 + age + failures + absences + traveltime + health
#Model 8: G3 ~ G1 + G2 + age + failures + absences + traveltime + health + schoolsup.1
#Model 9: G3 ~ G1 + G2 + failures + absences + Walc + avatar.requests +  schoolsup.1 + studytime + Fedu + teacher.requests
#Res.Df    RSS Df Sum of Sq         F    Pr(>F)    
#1    947 7821.3                                     
#2    946  739.6  1    7081.7 9828.6963 < 2.2e-16 ***
#  3    945  691.2  1      48.4   67.2215 7.929e-16 ***
#  4    944  687.1  1       4.1    5.6419   0.01774 *  
#  5    943  675.1  1      12.0   16.6877 4.783e-05 ***
#  6    942  672.0  1       3.0    4.2318   0.03995 *  
#  7    941  668.7  1       3.4    4.6986   0.03044 *  
#  8    940  666.4  1       2.3    3.1272   0.07732 .  
#9    938  675.8  2      -9.4                        
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Sample <- dplyr::sample_frac(DE1, 0.5, replace = TRUE)
#based on regression
c.treeR <- rpart(G3 ~ G1 + G2 + age + failures + absences + traveltime + health + schoolsup.1, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeR, file = "tree.ps", title = "tree")
DE1$predict1 <- predict(c.treeR, DE1, type = "class")
mismatchR <- dplyr::filter(DE1, G3 != predict1)

#based on correlation
c.treeC <- rpart(G3 ~ G1 + G2 + failures + absences + Walc + avatar.requests +  schoolsup.1 + studytime + Fedu + teacher.requests, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeC, file = "tree.ps", title = "tree")
DE1$predict2 <- predict(c.treeC, DE1, type = "class")
mismatchC <- dplyr::filter(DE1, G3 != predict2)

#based on clustering
c.treeK <- rpart(G3 ~ G1 + G2 + age+ failures + absences + forum.posts + customize.character + Walc+ Fjob.1+ schoolsup.1 + studytime + Fedu + teacher.requests, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeK, file = "tree.ps", title = "tree")
DE1$predict3 <- predict(c.treeK, DE1, type = "class")
mismatchK <- dplyr::filter(DE1, G3 != predict3)

#based on clustering and PCA
DE4 <- dplyr::select(DE1,G1,G2,scorechange1,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task,school.1,gender,citystatus,famsize.1,Pstatus.1,Mjob.1,Fjob.1,schoolsup.1,famsup.1)

#fit1 <- kmeans(mydata, 3)
#clusplot(mydata, fit1$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
#plotcluster(mydata, fit1$cluster)
#DE5 <- cbind(DE4, fit1$cluster)

fit2 <- kmeans(mydata, 2)
clusplot(mydata, fit2$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
plotcluster(mydata, fit2$cluster)
DE5 <- cbind(DE4, fit2$cluster)

C1 <- dplyr::filter(DE5,fit2$cluster == 1)
C2 <- dplyr::filter(DE5,fit2$cluster == 2)

#F1 <- dplyr::filter(DE3,fit1$cluster == 1)
#F2 <- dplyr::filter(DE3,fit1$cluster == 2)
#F3 <- dplyr::filter(DE3,fit1$cluster == 3)

# have PCA on the variables
#for C1
C1p <- dplyr::select(C1, 1:32)
C1p <- scale(C1p, center = TRUE)
pca1 <- prcomp(C1p, scale = TRUE)
#plot the Principle components
pca1$sdev
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue
pca1$sdev^2
#A summary of our pca will give us the proportion of variance accounted for by each component
summary(pca1)
plot(pca1, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data
#Now, create a data frame of the transformed data from your pca.
C1a <- as.data.frame(pca1$x)
C1b <- dplyr::select(C1a,1:10)
C1b <- cbind(C1b, C1$G1,C1$G2)
CORC1 <- cor(C1b)
corrplot(CORC1, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
pca1$rotation
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component
loadingsC1 <- abs(pca1$rotation) #abs() will make all eigenvectors positive

#in each cluster, take 50% data to build prediction tree
C1ss <- dplyr::filter(DE3,fit2$cluster == 1)
C2ss <- dplyr::filter(DE3,fit2$cluster == 2)
C1s <- dplyr::sample_frac(C1ss, 0.5, replace = TRUE)
C2s <- dplyr::sample_frac(C2ss, 0.5, replace = TRUE)

#build decision tree based on cluster (> 0.1)
c.treeP1 <- rpart(G3 ~ G1+ G2+failures+Medu+teacher.requests+Fedu+avatar.requests+school.1+traveltime+Walc+Dalc+studytime+Mjob.1+citystatus+age+goout+forum.posts, method="class", data=C1s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.treeP1)
post(c.treeP1, file = "tree1.ps", title = "C1 tree1")
C1ss$predict1 <- predict(c.treeP1, C1ss, type = "class")
mismatch1 <- dplyr::filter(C1ss, G3 != predict1)

#for C2
C2p <- dplyr::select(C2, 1:32)
C2p <- scale(C2p, center = TRUE)
pca2 <- prcomp(C2p, scale = TRUE)
#plot the Principle components
pca2$sdev
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue
pca2$sdev^2
#A summary of our pca will give us the proportion of variance accounted for by each component
summary(pca2)
plot(pca2, type = "lines")
#from this pca plot, it suggests that we can only keep 10 principle components for data
C2a <- as.data.frame(pca2$x)
C2b <- dplyr::select(C2a,1:10)
C2b <- cbind(C2b, C2$G1,C2$G2)
CORC2 <- cor(C2b)
corrplot(CORC2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

pca2$rotation
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component
loadingsC2 <- abs(pca2$rotation) #abs() will make all eigenvectors positive

c.treeP2 <- rpart(G3 ~ G1+ G2+teacher.requests+Medu+Fedu+Mjob.1+failures+famsup.1+customize.character+avatar.requests+levels.complete+absences+Dalc+Walc+schoolsup.1+health+studytime+time.in.session+traveltime, method="class", data=C2s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.treeP2)
post(c.treeP2, file = "tree2.ps", title = "C2 tree2")
C2ss$predict1 <- predict(c.treeP2, C2ss, type = "class")
mismatch2 <- dplyr::filter(C2ss, G3 != predict1)

#=======================#

Sample <- dplyr::sample_frac(DE1, 0.5, replace = TRUE)
#based on regression
c.treeR <- rpart(G3 ~ G1 + G2 + age + failures + absences + traveltime + health + schoolsup.1, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeR, file = "treeR.ps", title = "tree")
DE1$predict1 <- predict(c.treeR, DE1, type = "class")
mismatchR <- dplyr::filter(DE1, G3 != predict1)

#based on correlation
c.treeC <- rpart(G3 ~ G1 + G2 + failures + absences + Walc + avatar.requests +  schoolsup.1 + studytime + Fedu + teacher.requests, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeC, file = "treeC.ps", title = "tree")
DE1$predict2 <- predict(c.treeC, DE1, type = "class")
mismatchC <- dplyr::filter(DE1, G3 != predict2)

#based on clustering
c.treeK <- rpart(G3 ~ G1 + G2 + age+ failures + absences + forum.posts + customize.character + Walc+ Fjob.1+ schoolsup.1 + studytime + Fedu + teacher.requests, method="class", data=Sample,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
post(c.treeK, file = "treeK.ps", title = "tree")
DE1$predict3 <- predict(c.treeK, DE1, type = "class")
mismatchK <- dplyr::filter(DE1, G3 != predict3)

#based on clustering-PCA-correlation
c.treeP1 <- rpart(G3 ~ G1+ G2+failures+Medu+teacher.requests+Fedu+avatar.requests+school.1+traveltime+Walc+Dalc+studytime+Mjob.1+citystatus+age+goout+forum.posts, method="class", data=C1s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.treeP1)
post(c.treeP1, file = "treeP1.ps", title = "C1 tree1")
C1ss$predict1 <- predict(c.treeP1, C1ss, type = "class")
mismatch1 <- dplyr::filter(C1ss, G3 != predict1)

c.treeP2 <- rpart(G3 ~ G1+ G2+teacher.requests+Medu+Fedu+Mjob.1+failures+famsup.1+customize.character+avatar.requests+levels.complete+absences+Dalc+Walc+schoolsup.1+health+studytime+time.in.session+traveltime, method="class", data=C2s, control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.0001))
printcp(c.treeP2)
post(c.treeP2, file = "treeP2.ps", title = "C2 tree2")
C2ss$predict1 <- predict(c.treeP2, C2ss, type = "class")
mismatch2 <- dplyr::filter(C2ss, G3 != predict1)
