# anne-jet.github.io

data <- read.csv("C://Users//Gebruiker//OneDrive - Erasmus University Rotterdam//Documents//Master//Data analysis//survey_response_data.csv")

CEO_Diary <- read.csv("C:\Users\Gebruiker\OneDrive - Erasmus University Rotterdam\Documents\Master\Data analysis\survey_response_data.csv")

View(CEO_Diary)

library(knitr)        # for including graphics
install.packages("knitr")
library(knitr)        # for including graphics
include_graphics("figs/ViewCEODiary.png")
#error created, because of lack of file
CEO_Diary[1:15,c(1:5,37, 39, 40)] 

```{r}
apply(CEO_Diary,2,class)
```

nrow(CEO_Diary)

summary(CEO_Diary[1:5])

  png(file="figs/CEOTypes.png", width=800, height=300)
  par(mar=c(9, 3 ,1,1))
  barplot(prop.table(table(CEO_Diary$type)), las=2)
  dev.off()
#Error because of lack of file

fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)

PrintBibliography(bib, start = 1, end = 6)


#02 Uncertainty
output:
  xaringan::moon_reader:
    css: [default, "style/middlebury.css", "style/middlebury-fonts.css"]
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
      ratio: "16:9"
      
#Errors

knitr::opts_chunk$set(eval = TRUE,
               echo = TRUE,
               warning = FALSE,
               message = FALSE,
               cache = FALSE)
               
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

library(htmltools)

install.packages('htmltools')
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

install.packages('rmarkdown')
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
library(ggplot2)
library(kableExtra)

theme_set(theme_gray(15))

install.packages('ggplot2')
install.packages('kableExtra')

library(ggplot2)
library(kableExtra)

theme_set(theme_gray(15))

library(RefManageR)
BibOptions(check.entries = FALSE, 
           bib.style = "numeric", 
           cite.style = "authoryear", 
           style = "markdown",
           hyperlink = FALSE, 
           dashed = FALSE)
bib <- ReadBib("../refs/references.bib", check = FALSE)

]
.pull-right[
```{r, echo=FALSE, out.width="100%"}
  knitr::include_graphics("figs/BootSampDist.png")
```
]

.pull-right
```{r, echo=FALSE, out.width="100%"}
knitr::include_graphics("figs/BootSampDist.png")



mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)

browser <- read.csv('C:\\Users\\Gebruiker\\OneDrive - Erasmus University Rotterdam\\Documents\\Master\\Data analysis\\web-browsers.csv')

mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)


B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)
  
 h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  
 h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40)
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4))

 h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  
lines(xfit, yfit, col = "black", lwd = 2)

 B <- 1000
  betas <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
    betas <- rbind(betas, coef(reg_b))
  }; head(betas, n=3)
  
  cov(betas[,"broadband"], betas[,"anychildren"])


browser <- read.csv('C:\\Users\\Gebruiker\\OneDrive - Erasmus University Rotterdam\\Documents\\Master\\Data analysis\\web-browsers.csv')
  spendy <- glm(log(spend) ~ . -id, data=browser)
  round(summary(spendy)$coef,2)
  
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
  pvalrank <- rank(pval)
  reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 

  plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
  lines(pvalrank, (0.1/9)*pvalrank)
  dev.off()
  
  
SC <- read.csv('C:\\Users\\Gebruiker\\OneDrive - Erasmus University Rotterdam\\Documents\\Master\\Data analysis\\semiconductor.csv')
dim(SC)

full <- glm(FAIL ~ ., data=SC, family=binomial)
pvals <- summary(full)$coef[-1,4]

hist(pvals, xlab="p-value", main="", col="pink")#looks like we have some 

