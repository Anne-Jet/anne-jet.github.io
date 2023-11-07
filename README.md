# anne-jet.github.io


data <- read.csv("C:\\Users\\Gebruiker\\OneDrive - Erasmus University Rotterdam\\Documents\\Master\\Data analysis\\survey_response_data.csv")

CEO_Diary <- read.csv("C:\\Users\\Gebruiker\\OneDrive - Erasmus University Rotterdam\\Documents\\Master\\Data analysis\\survey_response_data.csv")

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
