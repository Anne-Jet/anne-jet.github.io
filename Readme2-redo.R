##{r setup, include=FALSE}, meaning the results will be ran again everytime and not saved
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
setwd("~/Master/Data analysis/Assignment 3/")
CEO_Diary <- read.csv("survey_response_data.csv")

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
print(plot(h, main = "Histogram with Fitted Density Function", col = "lightblue", border = "black", xlim = range(xfit)))
hist(mub, main = "Histogram with Fitted Density Function", col = "lightblue", border = "black")

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










