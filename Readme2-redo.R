##{r setup, include=FALSE}, meaning the results will be ran again everytime and not saved
##Plots situation, tools, global options, r-markdown, What does it mean?

options(htmltools.dir.version = FALSE)

knitr::opts_chunk$set(eval = TRUE,
                      echo = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      cache = FALSE)

htmltools::tagList(rmarkdown::html_dependency_font_awesome())

library(ggplot2)
library(kableExtra)

theme_set(theme_gray(15))

install.packages("RefManageR")
library(RefManageR)
BibOptions(check.entries = FALSE, 
           bib.style = "numeric", 
           cite.style = "authoryear", 
           style = "markdown",
           hyperlink = FALSE, 
           dashed = FALSE)
bib <- ReadBib("../refs/references.bib", check = FALSE) ##Could be useful later when using R, using references, but not really now. Cant create a bibTex file

##FIRST PART: UNCERTAINTY
setwd("~/Master/Data analysis")
CEO_Diary <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysissurvey_response_data.csv")

View(CEO_Diary)

CEO_Diary[1:15,c(1:5,37, 39, 40)] 
CEO_Diary[1:25,c(2:6,25, 30, 35, 40)]

apply(CEO_Diary,2,class)

apply(CEO_Diary,3,class) #Error, probably because working with only rows and columns
apply(CEO_Diary,1,class) #only got "Character" because of looking at rows, not columns as in 2

nrow(CEO_Diary)

summary(CEO_Diary[1:5])
summary(CEO_Diary[6:10])

png(file="figs/CEOTypes.png", width=800, height=300)#Got error, trying different route
png(file="C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/Assignment 3/CEOTypes.png", width=800, height=300)


par(mar=c(9, 3 ,1,1))
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off()
##Worked and got a plot
knitr::include_graphics("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/Assignment 3/CEOTypes.png")

barplot(prop.table(table(CEO_Diary$type)));

table(CEO_Diary$type);

prop.table(table(CEO_Diary$type));

#All worked and got plot and tables

fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)

View(fit)

glm(strategy ~ consultants + ins, data=CEO_Diary); summary

##Interesting to see how the coefficients change when replacing politicians with insiders. Even the consultants one differs quite a lot.

##END OF FIRST PART: INTRODUCTION

##START OF SECOND PART: UNCERTAINTY


knitr::include_graphics("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/Assignment 3/FreqSampDist.png")
##Got error, needed to add to the code
png(file = "C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/Assignment 3/FreqSampDist.png", width = 800, height = 600)
knitr::include_graphics("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/Assignment 3/FreqSampDist.png")
##Now it worked, png first to create a file to store it in.(logically)

browser <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/web-browsers.csv")
dim(browser)
head(browser)

head(browser, n=10)
##Shows the number of rows in the browser data set

mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)

View(browser)
mean(browser$anychildren); var(browser$anychildren)/1e4; sqrt(var(browser$anychildren)/1e4)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 2000
mub <- c()
for (b in 1:2000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 100
mub <- c()
for (b in 1:100){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 10000
mub <- c()
for (b in 1:10000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
print()
hist(mub, main = "Histogram with Fitted Density Function", col = "lightblue", border = "black")

plot(h, main = "Histogram with Fitted Density Function", col = "lightblue", border = "black", xlim = range(xfit))

y = rnorm(100,0,1)
x = rnorm(100,0,1)
plot(y,x)

#can you explain why we need each term in the last expression? 
lines(xfit, yfit, col = "black", lwd = 2)
##Couldnt get the histogram plotted

B <- 1000
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)

cov(betas[,"broadband"], betas[,"anychildren"])


B <- 100
betas <- c()
for (b in 1:100){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)

cov(betas[,"broadband"], betas[,"anychildren"])
##Changing the amount in bootstrap also changes the covariance, which is interesting. I thought these would not impact each other.

spendy <- glm(log(spend) ~ . -id, data=browser)
round(summary(spendy)$coef,2)
pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank <- rank(pval)
reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
png(file="C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/BHAlgoExample.png",
    width=600, height=350)
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank, (0.1/9)*pvalrank)
dev.off()
##Worked and checked out

SC <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/semiconductor.csv")
dim(SC)
full <- glm(FAIL ~ ., data=SC, family=binomial)
pvals <- summary(full)$coef[-1,4] #-1 to drop the intercept

hist(pvals, xlab="p-value", main="", col="lightblue") #did not work again, see email I sent
fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
       ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha)
}

fdr_cut(pvals)
##I get 0.01217043 think this is correct, or at least nearing
##END OF UNCERTAINTY: SECTION 2

## START OF SECTION 3: REGRESSION
oj <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/oj.csv")
head(oj, n=5)    
tail(oj, n=5)    
glm(log(sales) ~ brand + log(price), data=oj)

glm(log(sales) ~ brand + log(price), data=oj)
x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)

oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)
oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)
oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

glm(log(sales) ~ log(price)*brand*feat, data=oj)

email <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/spam.csv")
dim(email)
colnames(email)

glm(spam ~ ., data=email, family='binomial')

spammy <- glm(spam ~ ., data=email, family='binomial')

coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])
coef(spammy)["word_order"]; exp(coef(spammy)["word_order"])
coef(spammy)["word_make"]; exp(coef(spammy)["word_make"])
coef(spammy)["word_george"]; exp(coef(spammy)["word_george"]); 1/exp(coef(spammy)["word_george"])
coef(spammy)["word_order"]; exp(coef(spammy)["word_order"]); 1/exp(coef(spammy)["word_order"])

predict(spammy, newdata = email[c(1,4000),], type="response")

summary(spammy)$deviance
summary(spammy)$null.deviance

summary(spammy)
D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2

##END OF SECTION 3: REGRESSION

##START OF SECTION 4: REGULARIZATION
SC <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
Out <- data.frame(full=rep(NA,K)) 
for(k in 1:K){ 
  train <- which(foldid!=k)} # train on all but fold `k'
rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
predfull <- predict(rfull, newdata=SC[-train,], type="response")
Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
cat(k, " ")

n <- nrow(SC) # the number of observations
K <- 25 # the number of `folds'
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
Out <- data.frame(full=rep(NA,K)) 
for(k in 1:K){ 
  train <- which(foldid!=k)} # train on all but fold `k'
rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
predfull <- predict(rfull, newdata=SC[-train,], type="response")
Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
cat(k, " ")



boxplot(Out, col="plum", ylab="R2")
colMeans(Out) 

null <- glm(FAIL~1, data=SC)
fwd <- step(null, scope=formula(full), dir="forward")
length(coef(fwd))

install.packages("gamlr")
library(gamlr)

## Browsing History. 
## web has 3 colums: [machine] id, site [id], [# of] visits
web <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/browser-domains.csv")

## Read in actual website names and relabel site factor
sitenames <- scan("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)

## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))

## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]

## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

# what sites did household 1 visit?
#head(xweb[1, xweb[1,]!=0])
## now read in the spending data 
yspend <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
plot(spender) ## path plot

cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)

betamin = coef(cv.spender, select="min"); betamin
head(AIC(spender))

##END OF SECTION 4: REGULARIZATION

##START OF SECTION 5: CLASSIFICATION
install.packages("MASS")
library(MASS)
data(fgl)
dim(fgl)
head(fgl, n = 2)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)

x <- scale(fgl[,1:9]) # column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) # apply function sd to columns of x

library(class) #has knn function 
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)

#### ******* German Credit Data ******* ####
credit <- read.csv("../Data/credit.csv")
## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## a few others
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]
head(credit)
dim(credit)

library(gamlr)
source("naref.R")
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)

default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

rule <- 1/5 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 1/5 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 1/5 rule

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

source("roc.R")
png(file="ROCCurve.png", width=600, height=350)
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
       y=mean((pred>.2)[default==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
       y=mean((pred>.5)[default==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
roc(p=predoos, y=defaultoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
       y=mean((predoos>.2)[defaultoos==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
       y=mean((predoos>.5)[defaultoos==1]), 
       cex=1.5, pch=20, col='blue') 
dev.off()

knitr::include_graphics("ROCCurve.png")

par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") 

library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variables and all their interactions with refractive index (RI).
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit

plot(glassfit)
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) 
plot(glassfit$glm, xvar="lambda")

B  <- coef(glassfit, select="min"); B ## extract coefficients
B <- do.call(cbind, B) 
colnames(B) <- levels(gtype) # column names dropped in previous command. This command adds them back.

DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg; #B is a matrix. Fixed Row. Vary Columns. k is Mg, a is WinNF, b is WinF. 
exp(DeltaBMg);
1 - exp(DeltaBMg)

probfgl <- predict(glassfit, xfgl, type="response"); dim(probfgl); head(probfgl,n=2); tail(probfgl,n=2)
#gives in-sample probabilities. Note: this is nXKX1 array. Need nXK array. To convert: 
probfgl <- drop(probfgl); #use dim(probfgl) to check dim is 214 by 6
n <- nrow(xfgl)
trueclassprobs <- probfgl[cbind(1:n, gtype)]; head(trueclassprobs,n=3); tail(trueclassprobs,n=3) 
#for each obs there is one probability that corresponds to realized shard for that obs. Last command extracts those probabilities. 
#Note use of a matrix to index a matrix.

plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
     xlab="glass type", ylab="prob( true class )") 

##END OF SECTION 5: CLASSIFICATION

##START OF SECTION 6: CONTROLS
oj <- read.csv("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/oj.csv")
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)
##No other experiments possible because of the values of the other variables.

brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)

pricereg <- lm(log(sales) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj) 
presid <- log(oj$price) - phat
residfit <- lm(log(sales) ~ presid, data=oj)
coef(basefit)

data <- read.table("C:/Users/Gebruiker/OneDrive - Erasmus University Rotterdam/Documents/Master/Data analysis/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]

dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]

exp(dcoef) - 1

######
cell <- read.csv("../Data/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985

par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")


phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1

t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]

library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)
## naive lasso regression
naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",] 


treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)


par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3)) 
## little to resemble an experiment here...

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 indicates how much independent signal you have for estimating 
coef(summary( glm( y ~ d + dhat) ))
# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] 
# AICc says abortion rate has no causal effect on crime.


library(gamlr)
data(hockey)
head(goal, n=2)
player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice. 
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.


x <- cbind(config,team,player)
y <- goal$homegoal
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)
selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )


summary(nhlmle)


x[1,x[1,]!=0] #check first observation for players on the ice
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
CI = fit + c(-2,2)*se.fit

########PART TWO
library(Matrix)
data <- read.table("../Data/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd
cell <- read.csv("../Data/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
phone <- cellrate[ t + 1 ]
t <- factor(t)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]


library(AER)
library(gamlr)

dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }

orthoLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
  # randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, 
                    times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- rep(NA, nobs)
  # run OOS orthogonalizations
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
    dhat <- predict(dfit, x[I[[b]],], type="response")
    yhat <- predict(yfit, x[I[[b]],], type="response")
    dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
    ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
    cat(b," ")
  }
  rfit <- lm(ytil ~ dtil)
  gam <- coef(rfit)[2]
  se <- sqrt(vcovHC(rfit)[2,2])
  cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))
  
  return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}

# OrthoML and effect of abortion access on crime

resids <- orthoLTE( x=x, d=d, y=y, 
                    dreg=dreg, yreg=yreg, nfold=5) 
head(resids$dtil)
head(resids$ytil)
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime

library(foreign)

descr <- read.dta("../Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("../Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("../Data/oregonhie_survey12m_vars.dta")

# nicely organized, one row per person
all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)

P <- descr[,c("person_id","household_id", "numhh_list")]
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
levels(P$numhh_list) <- c("1","2","3+")

# 12 month is the survey that really matters
# need to control for household size interacted with survey return time
Y <- s12[,c("weight_12m",
            "doc_any_12m","doc_num_mod_12m",
            "er_any_12m","er_num_mod_12m",
            "hosp_any_12m","hosp_num_mod_12m")]
Y$doc_any_12m <- as.numeric(Y$doc_any_12m=="Yes")
Y$er_any_12m <- as.numeric(Y$er_any_12m=="Yes")
Y$hosp_any_12m <- as.numeric(Y$hosp_any_12m=="Yes")

# smk_ever_12m - num19_12m are sources of heterogeneity, plus descr
X <- s12[,121:147]
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]

sapply(Y,function(y) sum(is.na(y)))
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]

# pull out the weights and attach doc_any to P
weights <- Y[,1]
Y <- Y[,-1]

# replace some ridiculous values in survey and drop num19
X$hhsize_12m[X$hhsize_12m>10] <- 10
X$num19_12m <- NULL

# organize to make it pretty for text
P$doc_any_12m <- Y$doc_any_12m # you can explore other responses if you want
P <- P[,c(1,2,6,5,4,3)]
names(P)[6] <- "numhh"


# data has been cleaned in the background
head(P,n=3)
dim(P)
table(P$selected)

ybar <- tapply(P$doc_any_12m, P$selected, mean)
( ATE = ybar['1'] - ybar['0'] )

nsel <- table(P[,c("selected")])
yvar <- tapply(P$doc_any_12m, P$selected, var)
( seATE = sqrt(sum(yvar/nsel)) )

ATE + c(-2,2)*seATE

lin <- glm(doc_any_12m ~ selected + numhh, data=P);
round( summary(lin)$coef["selected",],4) # 6-7% increase in prob


levels(X$edu_12m)
source("naref.R")
levels(naref(X$edu_12m))
X <- naref(X) #makes NA the base group

xnum <- X[,sapply(X,class)%in%c("numeric","integer")]
xnum[66:70,]
colSums(is.na(xnum))
# flag missing
xnumna <- apply(is.na(xnum), 2, as.numeric)
xnumna[66:70,]

mzimpute <- function(v){ 
  if(mean(v==0,na.rm=TRUE) > 0.5) impt <- 0
  else impt <- mean(v, na.rm=TRUE)
  v[is.na(v)] <- impt
  return(v) }
xnum <- apply(xnum, 2,  mzimpute)
xnum[66:70,]



# replace/add the variables in new data frame 
for(v in colnames(xnum)){
  X[,v] <- xnum[,v]
  X[,paste(v,"NA", sep=".")] <- xnumna[,v] }
X[144:147,]

xhte <- sparse.model.matrix(~., data=cbind(numhh=P$numhh, X))[,-1]
xhte[1:2,1:4]
dim(xhte)

dxhte <- P$selected*xhte
colnames(dxhte) <- paste("d",colnames(xhte), sep=".")
htedesign <- cbind(xhte,d=P$selected,dxhte)
# include the numhh controls and baseline treatment without penalty 
htefit <- gamlr(x=htedesign, y=P$doc_any_12m, free=c("numhh2","numhh3+","d"))
gam <- coef(htefit)[-(1:(ncol(xhte)+1)), ]
round(sort(gam)[1:6],4)
round(sort(gam, decreasing=TRUE)[1:6],4)

load("../Data/dominicks-beer.rda")
head(wber)
wber = wber[sample(nrow(wber), 100000), ]
head(upc)
dim(upc)
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) #ln price per 12 ounces

coef( margfit <- lm(log(MOVE) ~ lp, data=wber[,]) )
#10% increase in price decreases  quantity sold by 6%
#ATE

wber$s <- factor(wber$STORE); wber$u <- factor(wber$UPC); wber$w <- factor(wber$WEEK)
xs <- sparse.model.matrix( ~ s-1, data=wber); xu <- sparse.model.matrix( ~ u-1, data=wber); xw <- sparse.model.matrix( ~ w-1, data=wber)
# parse the item description text as a bag o' words
library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
descr <- DocumentTermMatrix(descr)
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), # convert from stm to Matrix format
                      dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))
descr[1:5,1:6]
descr[287,descr[287,]!=0]
controls <- cbind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)

# naive lasso
naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[,], y=log(wber$MOVE), free=1, standardize=FALSE)
print( coef(naivefit)[1:2,] )
# orthogonal ML 
resids <- orthoLTE( x=controls, d=wber$lp, y=log(wber$MOVE), dreg=dreg, yreg=yreg, nfold=5)

# interact items and text with price
#lpxu <- xu*wber$lp
#colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cbind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")

eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)
# fullhte
lnwberMOVE <- log(wber[['MOVE']])
fullhte <- gamlr(x=cbind(d,controls), y=lnwberMOVE, lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
coef(fullhte)

hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)

















