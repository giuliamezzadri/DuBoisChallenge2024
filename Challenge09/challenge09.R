###packages
rm(list=ls())   #to clean
library(ggplot2)
library(showtext)
library(dplyr)
showtext_auto()

###fonts
font_add_google(name = "Squada One", family = "squada")
font_add_google(name='Barlow Condensed', family='Barlow Condensed')
font_add_google(name='Tajawal', family='Tajawal')
font_add_google(name='Tomorrow', family='Tomorrow') 
myfont='Barlow Condensed'

###import data set
data=read.table('data.csv', header = TRUE, sep=',')

###colors
col_green='#36683dff'
col_black='#151612ff'
col_background='#dad2c5ff'
col_dark_green='#004024ff' #'#344935ff'

###data new format
data_new_format=data.frame(rbind(cbind(Year=data$Year, Perc=data$Slave, Type='Slave'), cbind(Year=data$Year, Perc=data$Free, Type='Free')))
cols=c('Year','Perc')
data_new_format[,cols]=sapply(data_new_format[,cols], as.numeric)
data_new_format=cbind(data_new_format, label=data_new_format$Perc)
data_new_format$Perc[11]=12
  
###plot
pl=ggplot(data_new_format, aes(x=Year, y=Perc, fill=Type)) +
  geom_area() + coord_cartesian(clip="off") +
  geom_segment(data=data_new_format[which(data_new_format$Type=='Free' & !(data_new_format$Perc %in% c(8,100,14))),], aes(x=Year,xend=Year,y=99.9,yend=100-Perc+4), size=.3, color=col_dark_green) +
  geom_segment(data=data_new_format[which(data_new_format$Type=='Free' & data_new_format$Perc==14),], aes(x=Year,xend=Year,y=99.9,yend=98.5), size=.3, color=col_dark_green) +
  geom_segment(data=data_new_format[which(data_new_format$Type=='Free' & data_new_format$Perc==14),], aes(x=Year,xend=Year,y=93.5,yend=100-Perc+4), size=.3, color=col_dark_green) +
  geom_text(data=data_new_format[which(data_new_format$Type=='Free'),], aes(x=Year, y=103, label=Year), color='black', family=myfont, fontface='bold', size=5.5) +
  geom_text(data=data_new_format[which(data_new_format$Type=='Free' & data_new_format$Perc!=100),], color='black', aes(x=Year, y=100-Perc+2, label=paste(label,'%',sep='')), family=myfont, fontface='bold') +
  geom_text(data=data_new_format[which(data_new_format$Type=='Free' & data_new_format$Perc==100),], color='black', aes(x=Year, y=100-11+2, label=paste(label,'%',sep='')), family=myfont, fontface='bold') +
  annotate('text', x=mean(data$Year), y=96, size=6, label=toupper('Free - Libres'), family=myfont, fontface='bold') +
  annotate('text', x=mean(data$Year), y=55, size=8, label=toupper('Slaves\nEsclaves'), family=myfont, fontface='bold', color='white', lineheight=.8) +
  scale_fill_manual(values=c(col_green, col_black), guide='none') +
  ggtitle(toupper("Proportion of freemen and slaves among american negroes.\nProportion des nègres libres et des esclaves en amérique.")) +
  annotate('text', x=mean(data$Year), y=117, size=3, label='DONE BY ATLANTA UNIVERSITY.', family=myfont, fontface='bold') +
  labs(caption='#DuBoisChallenge2024 - 09 | Giulia Mezzadri | giuliamezzadri.com') +
  theme(
    plot.title = element_text(family=myfont, hjust=.5, face='bold', size=13.5, lineheight=2),
    plot.caption = element_text(family=myfont, hjust=.5, size=10),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.margin = unit(c(.8, .5, .5, .5), "cm"),
    plot.background = element_rect(fill = col_background), 
    panel.background = element_rect(fill = col_background)
  )

print(pl)
width=6
ggsave(file='challenge09.pdf', plot=pl, width=width, height=1.3*width)


