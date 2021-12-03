library(psych)
library(psychTools)
library(dplyr)
library(factoextra)
library(devtools)
library(tidyverse)
library(FactoMineR)
library(readr)
library(fpca)
library(fdapace)

#Part 1 PCA

PAQ_Ocean <- read.csv("C:\\Users\\18086\\Desktop\\PAQ_Ocean.txt", sep="")
view(PAQ_Ocean)
data_wide <- spread(PAQ_Ocean, var, value)
View(data_wide)
PAQ_1<-data_wide
View(PAQ_1)
summary(PAQ_1)
describe(PAQ_1)

colSums(is.na(PAQ_1))
PAQ2 <-PAQ_1

for(i in 1:ncol(PAQ_1)) 
{PAQ2[ , i][is.na(PAQ2[ , i])] <- mean(PAQ2[ , i], na.rm = TRUE)}

summary(PAQ2)
PAQ<-PAQ2
View(PAQ)
summary(PAQ)
colSums(is.na(PAQ))


PAQO <- subset(PAQ, select = -c(id, age, sex)) 
colSums(is.na(PAQO))
str(PAQO)

PAQ.cov <- cov(PAQO) #load covariance matrix
summary(PAQ.cov)
PAQ_final <- princomp(covmat = PAQ.cov) 
summary(PAQ_final, loading=T)

#outputs the standard deviation of variables
PAQ_final$scale

#standard deviation of each principal component
std_dev <- PAQ_final$sdev
std_dev
#variance
pr_var <- std_dev^2
pr_var
#variance of first 9 components
pr_var[1:9]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:9]

#scree plot 1
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


screeplot(PAQ_final)

# Part 2 Multidimensional Scaling

library(MASS)
library(smacof)
library(vegan)

nations<-read.delim("C:\\Users\\18086\\Downloads\\Nations.txt")

view(nations)
describe(nations)

country <- data.matrix(nations, rownames.force = NA) 
rownames(country) <- c("Brazil", "Congo"   ,   "Cuba",      "Egypt",    "France" ,  "India"  ,
                       "Israel"   ,"Japan",   "China"  ,"UdSSR"  ,"USA"  , "Yugoslavia")

natdiss <- sim2diss(country, method = 7, to.dist = TRUE)

nat_res_one<-isoMDS(natdiss, k=1)
nat_res_two<-isoMDS(natdiss, k=3)

nat_res_one
nat_res_two

stress = c(nat_res_one$stress, nat_res_two$stress)
dimensions = 1:2
plot(dimensions, stress, type = "b", xlab = "Number of Dimensions", ylab = "Stress")

x <- nat_res_one$points[,1] 
y <- nat_res_two$points[,3]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(country), cex=.7)

stressplot(nat_res_two,natdiss)

