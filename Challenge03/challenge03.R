###packages
rm(list=ls())   #to clean
setwd("~/Desktop/DataViz Challenges/Du Bois/Challenge03/") #set folder
library(ggplot2)
library(showtext)
library(dplyr)
library(scales)

###specifications
col_red='#a9133aff'
col_background='#dad2c5ff'
font_add_google(name='Odibee Sans', family='Odibee Sans')
font_add_google(name='Tomorrow', family='Tomorrow') 
myfont='Tomorrow'

###import data set
data=read.table('data.csv', header = T, sep=',')

###modify data set
data$Date=factor(data$Date, levels=sort(data$Date, decreasing = T))

###plot
pl=ggplot(data, aes(x=Date, y=Land)) + geom_col(width=.55, fill=col_red) + coord_flip(clip="off") +
  geom_text(data=data[c(1,nrow(data)),], aes(x=Date, y=Land/2, label=scales::comma(Land)), family=myfont, fontface='bold') +
  geom_text(aes(x=Date, y=-50000, label=Date), family=myfont) +
  ggtitle("ACRES OF LAND OWNED BY NEGROES\nIN GEORGIA.") +
  annotate('text', x=-.5 , y=max(data$Land)/2, label='#DuBoisChallenge2024 - 03 | Giulia Mezzadri | giuliamezzadri.com', family=myfont, hjust=.5, size=3.5) +
  theme(
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.title = element_text(family=myfont, face='bold', hjust=.5, vjust=1.5, size=13.5),
    plot.margin = unit(c(1, .5, 1, .5), "cm"),
    plot.background = element_rect(fill = col_background), 
    panel.background = element_rect(fill = col_background)
  )
print(pl)
width=6.2
ggsave(file='challenge03.pdf', plot=pl, width=width, height=1.3*width)


