install.packages("psych")
library(psych)
install.packages("corrplot")
library(corrplot)
install.packages("polycor")
library(polycor)
install.packages("mirt")
library(mirt)
install.packages("ltm")
library(ltm)
install.packages("difR")
library(difR)
install.packages("lordif")
library(lordif)


# Load data set
CET <- read.csv('~/desktop/CET.csv', header = TRUE)
CET.A.Q <- CET[, 8:16]             # CET-A items
CET.B.Q <- CET[, 17:25]            # CET-B items
CET.A.Qtrue <- na.omit(CET.A.Q)    # remove NA
length(CET.A.Qtrue[,1])            # number of respondents complete CET-A
CET.B.Qtrue <- na.omit(CET.B.Q)    # remove NA
length(CET.B.Qtrue[,1])            # number of respondents complete CET-B


table(CET$Gender)  # 1(male): 199; 2(female):180


summary(CET$Age)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.00   46.50   58.00   56.16   68.00   88.00 
sd(CET$Age)


summary(CET$Years.of.Edu)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  5.00   11.00   13.00   13.22   16.00   21.00       7
sd(na.omit(CET$Years.of.Edu))



### Correlations---------------------------------------------------------------
# Polychoric correlations of CET-A items
corre.A <- polychoric(CET.A.Qtrue)
corre.A$rho
corrplot(corr = corre.A$rho,method='color',order = 'original',
         addCoef.col = 'grey')

# KMO test of CET-A items
KMO(CET.A.Qtrue)
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = corre.A$rho)
# Overall MSA =  0.61
# MSA for each item = 
# CET.A.Q1 CET.A.Q2 CET.A.Q3 CET.A.Q4 CET.A.Q5 CET.A.Q6 CET.A.Q7 CET.A.Q8 CET.A.Q9 
#     0.64     0.60     0.66     0.70     0.58     0.61     0.48     0.61     0.57 

# Bartlett's test of CET-A items
cortest.bartlett(CET.A.Qtrue)
# $chisq
# [1] 85.35044

# $p.value
# [1] 6.903429e-06

# $df
# [1] 36


# Polychoric correlations of CET-B items
corre.B <- polychoric(CET.B.Qtrue)
corre.B$rho
corrplot(corr = corre.B$rho,method='color',order = 'original',
         addCoef.col = 'grey')

# KMO test of CET-B items
KMO(CET.B.Qtrue)
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = CET.B.Qtrue)
# Overall MSA =  0.52
# MSA for each item = 
# CET.B.Q1 CET.B.Q2 CET.B.Q3 CET.B.Q4 CET.B.Q5 CET.B.Q6 CET.B.Q7 CET.B.Q8 CET.B.Q9 
#     0.56     0.67     0.52     0.49     0.42     0.47     0.49     0.61     0.57 

# Bartlett's test of CET-B items
cortest.bartlett(CET.B.Qtrue)
# $chisq
# [1] 76.781

# $p.value
# [1] 8.874331e-05

# $df
# [1] 36



### Parallel Analysis----------------------------------------------------------
# Find number of components and factors of CET-A items
fa.parallel(CET.A.Qtrue,fm='minres',fa='both',n.iter = 100, 
            cor='poly',show.legend = TRUE)
# Parallel analysis suggests that the number of factors =  1  
# and the number of components =  1


# Find number of components and factors of CET-B items
fa.parallel(CET.B.Qtrue,fm='minres',fa='both',n.iter = 100, 
            cor='poly',show.legend = TRUE)
# Parallel analysis suggests that the number of factors =  2  
# and the number of components =  2



### Principal Component  Analysis----------------------------------------------
# PCA of CET-A
pc.A <- principal(corre.A$rho,nfactors = 1,rotate = 'none',scores = TRUE)
pc.A
pc.A$weights
pc.A$scores
rc.A <- principal(corre.A$rho,nfactors = 1,rotate = 'varimax',scores = TRUE)
rc.A
rc.A$weights
rc.A$r.scores
rc.A$loadings


# PCA of CET-B
pc.B <- principal(corre.B$rho,nfactors = 2,rotate = 'none',scores = TRUE)
pc.B
pc.B$weights
rc.B <- principal(corre.B$rho,nfactors = 2,rotate = 'varimax',scores = TRUE)
rc.B
rc.B$weights
rc.B$r.scores
rc.B$loadings



### Factor Analysis------------------------------------------------------------
# FA of CET-A items
fapoly.A <- fa.poly(CET.A.Qtrue,nfactors = 1,rotate = 'promax')
factor.plot(fapoly.A$fa)
fa.diagram(fapoly.A,simple=TRUE ,digits = 3)


# FA of CET-B items
fapoly.B <- fa.poly(CET.B.Qtrue,nfactors = 2,rotate = 'promax')
factor.plot(fapoly.B$fa)
fa.diagram(fapoly.B,simple=TRUE ,digits = 3)



### Correlations between CET items and Executive Ability variables-------------
Stroop <- CET$Stroop
Fluency <- CET$Fluency
Animals <- CET$Animals
Exe.ability <- data.frame(Stroop,Fluency,Animals)  # three executive measures

corre_exe.A <- matrix(0,9,3)   # matrix of correlations between CET-A items and three variables
corre_exe.A.p <- matrix(0,9,3) # matrix of p-values
for (i in 1:9) {
  for (j in 1:3) {
    corre_exe.A[i, j] <- corr.test(x = CET.A.Q[,i], y = Exe.ability[,j],
                            use = 'pairwise.complete.obs', method = 'spearman')$r
    corre_exe.A.p[i, j] <- corr.test(x = CET.A.Q[,i], y = Exe.ability[,j],
                              use = 'pairwise.complete.obs', method = 'spearman')$p
  }
}
corre_exe.A
corre_exe.A.p
# item 9 is postively correlated with Stroop 


corre_exe.B <- matrix(0,9,3)   # matrix of correlations between CET-B items and three variables
corre_exe.B.p <- matrix(0,9,3) # matrix of p-values
for (i in 1:9) {
  for (j in 1:3) {
    corre_exe.B[i, j] <- corr.test(x = CET.B.Q[,i], y = Exe.ability[,j],
                                   use = 'pairwise.complete.obs', method = 'spearman')$r
    corre_exe.B.p[i, j] <- corr.test(x = CET.B.Q[,i], y = Exe.ability[,j],
                                     use = 'pairwise.complete.obs', method = 'spearman')$p
  }
}
corre_exe.B
corre_exe.B.p
# remove items 1, 2 and 4



### Item Response Theory ------------------------------------------------------        
# set scores 0&1 == 0 and 2&3 == 1
CET.A.Qadj <- CET.A.Qtrue
for (i in 1:9) {
  CET.A.Qadj[,i][CET.A.Qadj[,i]==1] <- 0
  CET.A.Qadj[,i][CET.A.Qadj[,i]==2] <- 1
  CET.A.Qadj[,i][CET.A.Qadj[,i]==3] <- 1
}

CET.B.Qadj <- CET.B.Qtrue
for (i in 1:9) {
  CET.B.Qadj[,i][CET.B.Qadj[,i]==1] <- 0
  CET.B.Qadj[,i][CET.B.Qadj[,i]==2] <- 1
  CET.B.Qadj[,i][CET.B.Qadj[,i]==3] <- 1
}



## 2-PL Model------------------------------------------------------------------
# Difficulty=b, theta=ability
# Discrimination=a=how good the question is at figuring a person out.
IRTmodel.A <- ltm(CET.A.Qadj~z1,IRT.param = TRUE)
summary(IRTmodel.A)
coef(IRTmodel.A)
plot(IRTmodel.A,type = 'ICC')
plot(IRTmodel.A,type = 'IIC')
item.fit(IRTmodel.A)
# remain all 9 items


IRTmodel.B <- ltm(CET.B.Qadj[,-c(1,2,4)]~z1,IRT.param = TRUE)
summary(IRTmodel.B)
coef(IRTmodel.B)
plot(IRTmodel.B,type = 'ICC')
plot(IRTmodel.B,type = 'IIC')
item.fit(IRTmodel.B)
# remove item 5



### GRM------------------------------------------------------------------------
# Graded Response Model of CET-A
model.grm.A <- 'CET.A.item = 1-9'   # use all 9 items
results.grm.A <- mirt(data = CET.A.Qtrue, model = model.grm.A,
                       itemtype = 'graded', SE = TRUE, verbose = FALSE)
coef.grm.A <- coef(results.grm.A, IRTpars = TRUE, simplify = TRUE)
items.grm.A <- as.data.frame(coef.grm.A$items)
items.grm.A
anova(results.grm.A)   # test model fitness
# plot OCC (option characteristic curve)
plot(results.grm.A, type = 'trace', which.items = c(1:9),
     main = '', auto.key=list(points=FALSE, lines=TRUE,columns=4))
# plot IIF (item information function)
plot(results.grm.A,type='infotrace',which.items=c(1:9),
     main='',par.settings=simpleTheme(lwd=2))
# plot TIF (total amount of information)
plot(results.grm.A,type='info',theta_lim=c(-4,4),lwd=2)
# plot cSEM (conditional standard error of measurement)
plot(results.grm.A,type='SE', theta_lim=c(-4,4),lwd=2)
# remove item 7


# Graded Response Model of CET-B
model.grm.B <- 'CET.B.item = 1-5'
results.grm.B <- mirt(data = CET.B.Qtrue[,-c(1,2,4,5)], model = model.grm.B,
                      itemtype = 'graded', SE = TRUE, verbose = FALSE)
coef.grm.B <- coef(results.grm.B, IRTpars = TRUE, simplify = TRUE)
items.grm.B <- as.data.frame(coef.grm.B$items)
items.grm.B
anova(results.grm.B)
# plot OCC (option characteristic curve)
plot(results.grm.B, type = 'trace', which.items = c(1:5),
     main = '', auto.key=list(points=FALSE, lines=TRUE,columns=4))
# plot IIF (item information function)
plot(results.grm.B,type='infotrace',which.items=c(1:5),
     main='',par.settings=simpleTheme(lwd=2))
# plot TIF (total amount of information)
plot(results.grm.B,type='info',theta_lim=c(-4,4),lwd=2)
# plot cSEM (conditional standard error of measurement)
plot(results.grm.B,type='SE', theta_lim=c(-4,4),lwd=2)
# remove item 6


### DIF analysis---------------------------------------------------------------
CET.adj <- CET

# median(na.omit(CET.adj$Age))=58
# separate patients by median age
CET.adj$Age[CET.adj$Age <= median(na.omit(CET.adj$Age))] <- 0
CET.adj$Age[CET.adj$Age > median(na.omit(CET.adj$Age))] <- 1

# separate patients by median years of education
# median(na.omit(CET.adj$Years.of.Edu)) = 13
CET.adj$Years.of.Edu[CET.adj$Years.of.Edu <= median(na.omit(CET.adj$Years.of.Edu))] <- 0
CET.adj$Years.of.Edu[CET.adj$Years.of.Edu > median(na.omit(CET.adj$Years.of.Edu))] <- 1

# males == 0, females == 1
CET.adj$Gender[CET.adj$Gender == 1] <- 0
CET.adj$Gender[CET.adj$Gender == 2] <- 1


# categorise scores 0 & 1 == 0 and 2 & 3 == 1
for (i in 8:16) {
  CET.adj[,i][CET.adj[,i] == 1] <- 0
  CET.adj[,i][CET.adj[,i] == 2] <- 1
  CET.adj[,i][CET.adj[,i] == 3] <- 1
}
for (i in 17:25) {
  CET.adj[,i][CET.adj[,i] == 1] <- 0
  CET.adj[,i][CET.adj[,i] == 2] <- 1
  CET.adj[,i][CET.adj[,i] == 3] <- 1
}



## set 'Age' as the group argument-------------------------
CET.A.Age <- na.omit(CET.adj[, c(2,8:16)])
model.Age.A <- 'CET.A.items=1-9'    # use all nine items
Age.A <- as.factor(CET.A.Age$Age)
DIF.Age.A <- multipleGroup(data = CET.A.Age[,c(2:10)],
                       model = model.Age.A, SE=TRUE,
                       group = Age.A)
plot(DIF.Age.A, type='trace',which.items=c(1:9))
plot(DIF.Age.A, type='infotrace',which.items=c(1:9))
DIF(DIF.Age.A, c('a1','d'),simplify = TRUE, plotdif = TRUE)


CET.B.Age <- na.omit(CET.adj[, c(2,17:25)])
model.Age.B <- 'CET.B.items=1-9'     # use all nine items
Age.B <- as.factor(CET.B.Age$Age)
DIF.Age.B <- multipleGroup(data = CET.B.Age[,c(2:10)],
                           model = model.Age.B, SE=TRUE,
                           group = Age.B)
plot(DIF.Age.B, type='trace',which.items=c(1:9))
plot(DIF.Age.B, type='infotrace',which.items=c(1:9))
DIF(DIF.Age.B, c('a1','d'),simplify = TRUE, plotdif = TRUE)



## set 'Years.of.Edu' as the group argument----------------
CET.A.Edu <- na.omit(CET.adj[, c(3,8:16)])
model.Edu.A <- 'CET.A.items=1-9'
Edu.A <- as.factor(CET.A.Edu$Years.of.Edu)
DIF.Edu.A <- multipleGroup(data = CET.A.Edu[,c(2:10)],
                           model = model.Edu.A, SE=TRUE,
                           group = Edu.A)
plot(DIF.Edu.A, type='trace',which.items=c(1:9))
plot(DIF.Edu.A, type='infotrace',which.items=c(1:9))
DIF(DIF.Edu.A, c('a1','d'),simplify = TRUE, plotdif = TRUE)


CET.B.Edu <- na.omit(CET.adj[, c(3,17:25)])
model.Edu.B <- 'CET.B.items=1-9'
Edu.B <- as.factor(CET.B.Edu$Years.of.Edu)
DIF.Edu.B <- multipleGroup(data = CET.B.Edu[,c(2:10)],
                           model = model.Edu.B, SE=TRUE,
                           group = Edu.B)
plot(DIF.Edu.B, type='trace',which.items=c(1:9))
plot(DIF.Edu.B, type='infotrace',which.items=c(1:9))
DIF(DIF.Edu.B, c('a1','d'),simplify = TRUE, plotdif = TRUE)



# set 'Gender' as the group argument-----------------------
CET.A.Gender <- na.omit(CET.adj[, c(4,8:16)])
model.Gender.A <- 'CET.A.items=1-9'
Gender.A <- as.factor(CET.A.Gender$Gender)
DIF.Gender.A <- multipleGroup(data = CET.A.Gender[,c(2:10)],
                           model = model.Gender.A, SE=TRUE,
                           group = Gender.A)
plot(DIF.Gender.A, type='trace',which.items=c(1:9))
plot(DIF.Gender.A, type='infotrace',which.items=c(1:9))
DIF(DIF.Gender.A, c('a1','d'),simplify = TRUE, plotdif = TRUE)


CET.B.Gender <- na.omit(CET.adj[, c(4,17:25)])
model.Gender.B <- 'CET.B.items=1-9'
Gender.B <- as.factor(CET.B.Gender$Gender)
DIF.Gender.B <- multipleGroup(data = CET.B.Gender[,c(2:10)],
                              model = model.Gender.B, SE=TRUE,
                              group = Gender.B)
plot(DIF.Gender.B, type='trace',which.items=c(1:9))
plot(DIF.Gender.B, type='infotrace',which.items=c(1:9))
DIF(DIF.Gender.B, c('a1','d'),simplify = TRUE, plotdif = TRUE)













