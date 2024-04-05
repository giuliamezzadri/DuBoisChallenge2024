###packages
rm(list=ls())   #to clean
setwd("~/Desktop/DataViz Challenges/Du Bois/Challenge07/") #set folder
library(ggplot2)
library(showtext)
library(dplyr)
showtext_auto()

###import data set
data=read.table('data.csv', header = F, sep=',')
data$V1[which(data$V1=='Suede')]='Suéde'
col_red='#a9133aff'
col_green='#36683dff'
col_background='#dad2c5ff'
font_add_google(name = "Bellefair", family = "Bellefair")
myfont='Bellefair'

###plot
pl=ggplot(data, aes(x=reorder(V1,V2), y=V2, fill=V1)) + geom_col(width=.55) + coord_flip(xlim=c(1,11),clip="off") +
  scale_fill_manual(values=c(rep(col_green,5), col_red, rep(col_green,4))) +
  scale_y_continuous(expand=c(0,.5), limits=c(0,max(data$V2))) +
  ggtitle("Illiteracy of the American Negroes compared with that of other nations.", subtitle="Proportion d' illettrés parmi les Nègres Americains comparée à celle des autres nations.") +
  guides(fill='none') +
  labs(caption='#DuBoisChallenge2024 - 07 | Giulia Mezzadri | giuliamezzadri.com') +
  theme(
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_text(hjust=0, family=myfont, color='black'), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.title = element_text(family=myfont, hjust=.5, vjust=1.5, size=14),
    plot.subtitle = element_text(family=myfont, hjust=.5, vjust=-.5, size=11),
    plot.caption = element_text(family=myfont, hjust=.5, size=9),
    plot.margin = unit(c(1, 1.5, .5, .5), "cm"),
    plot.background = element_rect(fill = col_background), 
    panel.background = element_rect(fill = col_background)
    ) +
  annotate('text', x=11.25 , y=max(data$V2)/2, label='Done by Atlanta University', family=myfont, size=3)
print(pl)
width=6.5
ggsave(file='challenge07.pdf', plot=pl, width=width, height=1.3*width)

  
