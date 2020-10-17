# make an interactive figure

library(plotly)
library(htmlwidgets)
library(ggplot2)
library(dplyr)
library(readr)

oo<-read_csv("processed_data/severity_data_and_traits1.csv")
z<-ggplot(oo,aes(x=median.sev,y=-1*Estimate,label=COMMON.NAME,))+
  geom_point(aes(col=feeding_specialisation))+geom_smooth(method="lm")+ylab("Modelled Response to Fire")+
  geom_hline(yintercept = 0,linetype="dashed")+xlab("Median fire severity in post-fire observations")+
  theme_classic()

z_bits <- ggplot_build(z)$oo

z_p <- ggplotly(z, width=900, height=850)

setwd("docs")
saveWidget(widget=z_p, file="interactive_figure.html")
setwd("..")

ggplotly(z)

summary(lm(median.sev ~ Estimate, oo))
