library(tidyr, dplyr)
library(rpart)
library(pvclust)
library(corrplot)
library(mclust)

D1<- read.table("student-data.csv", sep = ",", header = TRUE)
E1<- read.table("extra-activity.csv", sep = ",", header = TRUE)
DE <- dplyr::full_join(D1, E1, by = "id")

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

plot(DE1$famsize,DE1$scorechange3)

plot(DE1$school,DE1$G3)
plot(DE1$school,DE1$scorechange3)

plot(DE1$sex,DE1$avatar.requests)
plot(DE1$sex,DE1$customize.character)

DE2 <- dplyr::select(DE1,G1,G2,G3,scorechange1, scorechange2,scorechange3,diffscorechange,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)
mydata <- DE2

# Model Based Clustering
fit <- Mclust(mydata)
plot(fit) # plot results 
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
groups <- cutree(fit, k=8) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
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
         sig.level=0.050, insig = "blank")

# Multiple Linear Regression Example 
fit1 <- lm(G3 ~ age, data=mydata)
fit2 <- lm(G3 ~ G2+age, data=mydata)
fit3 <- lm(G3 ~ G1+G2+age, data=mydata)
fit4 <- lm(G3 ~ G1+G2+age+failures, data=mydata)
fit5 <- lm(G3 ~ G1+G2+age+failures+absences, data=mydata)
fit6 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime, data=mydata)
fit7 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime+health, data=mydata)
fit8 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime+health+teacher.requests, data=mydata)
fit9 <- lm(G3 ~ G1+G2+age+failures+absences+traveltime+health+Walc+time.in.session, data=mydata)
anova(fit1, fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9)
#Analysis of Variance Table

#Model 1: G3 ~ age
#Model 2: G3 ~ G2 + age
#Model 3: G3 ~ G1 + G2 + age
#Model 4: G3 ~ G1 + G2 + age + failures
#Model 5: G3 ~ G1 + G2 + age + failures + absences
#Model 6: G3 ~ G1 + G2 + age + failures + absences + traveltime
#Model 7: G3 ~ G1 + G2 + age + failures + absences + traveltime + health
#Model 8: G3 ~ G1 + G2 + age + failures + absences + traveltime + health +  teacher.requests
#Model 9: G3 ~ G1 + G2 + age + failures + absences + traveltime + health +  Walc + time.in.session
#Res.Df    RSS Df Sum of Sq         F    Pr(>F)    
#1    947 7821.3                                     
#2    946  739.6  1    7081.7 9966.6810 < 2.2e-16 ***
#3    945  691.2  1      48.4   68.1652 5.068e-16 ***
#4    944  687.1  1       4.1    5.7211   0.01696 *  
#5    943  675.1  1      12.0   16.9220 4.236e-05 ***
#6    942  672.0  1       3.0    4.2912   0.03858 *  
#7    941  668.7  1       3.4    4.7646   0.02930 *  
#8    940  668.5  1       0.2    0.2849   0.59361    
#9    939  667.2  1       1.3    1.7713   0.18354    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1  

Sample <- dplyr::sample_frac(DE1, 0.1, replace = TRUE)
c.tree <- rpart(G3 ~ G1 + G2 + age + failures + absences + traveltime + health, method="class", data=Sample)
post(c.tree, file = "tree.ps", title = "tree")
DE1$predict1 <- predict(c.tree, DE1, type = "class")
mismatch <- dplyr::filter(DE1, G3 != predict1)





#assume that we do not know G3, select the numeric data + G1 G2 only to construct the data frame
S1a <- dplyr::select(S1,G1,G2,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)
S1b <- dplyr::select(S2,G1,G2,age,Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,forum.posts,levels.complete,avatar.requests,teacher.requests,customize.character,time.in.session,av.seconds.per.task)

S1aa <- na.omit(S1a)
S1aa <- scale(S1aa)
mydata <-S1aa

COR <- cor(mydata)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")


