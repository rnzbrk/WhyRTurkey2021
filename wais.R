


library(psych)
library(cNORM)
library(tidyverse)
library(data.table)
library(ggplot2)
library(hrbrthemes)



wais <- read_csv("https://raw.githubusercontent.com/rnzbrk/R-files-/master/wais_tibble.csv")


head(wais)

 waisIQ <- matrix(ncol=ncol(wais), nrow=nrow(wais))
 
 for(i in 3:ncol(wais)){
 
  waisIQ[,i] <-rankByGroup(wais, group = "age", raw=wais[[i]], scale="IQ")$normValue
  waisIQ <- as.data.frame(waisIQ)
   waisIQ[,c(1,2)] <- wais [,c(1,2)]
   colnames(waisIQ) <- colnames(wais)
 }

head(waisIQ)
 

describeBy(waisIQ, group="age")



waisIQ$Verbal <- rowSums(wais[,c(3:6)], na.rm=TRUE) 
waisIQ$Performance <- rowSums(wais[,c(7:10)], na.rm=TRUE) 
waisIQDT <- data.table(waisIQ)

head(waisIQDT)

waisIQDT[, Z_Verbal := scale(Verbal), by = age]
waisIQDT[, Z_Performance := scale(Performance), by = age]


head(waisIQDT)

waisIQDT[, IQ_Verbal := (Z_Verbal*15)+100, by = age]
waisIQDT[, IQ_Performance := (Z_Performance*15)+100, by = age]


head(waisIQDT)



waisIQDT$IQComp <- rowSums(waisIQDT[,c(15:16)], na.rm=TRUE) 
waisIQDT[, Z_IQComp := scale(IQComp), by = age]
waisIQDT[, FSIQ := (Z_IQComp*15)+100, by = age]


head(waisIQDT)


waisIQDT[, Statnine := round((Z_IQComp*2)+5,0), by = age]
waisIQDT[, percrank := round(rank(FSIQ)/length(FSIQ)*100,0), by = age]

waisIQDT[1:100, ]


theme_set(theme_classic())


g <- ggplot(waisIQDT, aes(FSIQ))
g + geom_density(aes(fill=factor(age)), alpha=0.8) + 
    labs(title="Density plot", 
         subtitle="FSIQ by Age Groups",
         x="FSIQ Score",
         fill="Age Groups - Recoded")
		 
		 
		 
ggplot(data=waisIQDT, aes(x=FSIQ, group=age, fill=age)) +
    geom_density(adjust=1.5) +
    theme_ipsum() +
    facet_wrap(~age) +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      axis.ticks.x=element_blank()
    )