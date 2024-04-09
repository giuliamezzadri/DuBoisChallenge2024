###packages
rm(list=ls())   #to clean
library(ggplot2)
library(showtext)
showtext_auto()
options(digits=10)

###Specifications
col_red='#a9133aff'
col_black='#151612ff'
col_background='#dad2c5ff'
font_add_google(name='Barlow Condensed', family='Barlow Condensed')
font_add_google(name='Tajawal', family='Tajawal')
myfont='Tajawal'

###import data sets
data=read.table('data.csv', header=T, sep=',')

###modify data set
new_data=data.frame(rbind(cbind(Year=data$Year, Percentage=data$Slave, Type='Slave'), cbind(Year=data$Year, Percentage=data$Free, Type='Free')))
cols=c('Year', 'Percentage')
new_data[,cols]=sapply(new_data[,cols], as.numeric)
new_data$Type=factor(new_data$Type, levels=c('Free','Slave'))
new_data=cbind(new_data, label=new_data$Percentage)
new_data$label[which(new_data$Year==1790 | new_data$Year==1870)]=paste(new_data$label[which(new_data$Year==1790 | new_data$Year==1870)], "%", sep='')
new_data$Percentage[which(new_data$Percentage==100)]=10
new_data$Percentage[which(new_data$Percentage==0)]=90

###to create the small waves at the top of the graph
M=800
dt=1/60000
x=seq(min(data$Year), max(data$Year), length.out=M)
set.seed(10)
y=97 + cumsum(rnorm(M)) * sqrt(dt)
tab=data.frame(Year=x, Percentage=y, Type='Other')

###plot
pl=ggplot(new_data, aes(x=Year, y=Percentage, group=Type, fill=Type)) + geom_area() +
  geom_area(data=tab, aes(x=Year, y=Percentage, fill=Type), color=col_black) +
  coord_flip(ylim=c(97,101)) + scale_x_reverse(breaks=data$Year) + geom_vline(aes(xintercept = Year), color=col_background) +
  geom_line(data=new_data[which(new_data$Type=='Slave'),], aes(x=Year, y=Percentage), color=col_background) +
  scale_fill_manual(values=c(col_red, col_background, col_black)) +
  geom_segment(aes(x=1790,xend=1790-.8,y=99,yend=99), size=.2) +
  geom_segment(aes(x=1790,xend=1790-.8,y=98,yend=98), size=.2) +
  geom_segment(aes(x=1790,xend=1790-.8,y=97,yend=97), size=.2) +
  annotate('text', x=1790-2 , y=99, label='1%', family=myfont, size=3.5) +
  annotate('text', x=1790-2 , y=98, label='2%', family=myfont, size=3.5) +
  annotate('text', x=1790-2 , y=97, label='3%', family=myfont, size=3.5) +
  annotate('text', x=1790-5 , y=100.33, label='PERCENT\nOF\nFREE NEGROES', family=myfont, size=2.5, lineheight=.8) +
  annotate('text', x=1870+11 , y=98.5, label='#DuBoisChallenge2024 - 02\nGiulia Mezzadri | giuliamezzadri.com', family=myfont, hjust=.5, size=3.5) +
  ggtitle('SLAVES AND FREE NEGROES.') +
  guides(fill='none') +
  geom_text(aes(x=Year, label=label), data=new_data[which(new_data$Type=='Free'),], y=100.2, hjust=0, family=myfont) +
  theme(
    plot.title = element_text(family=myfont, hjust=-.9, face='bold', size=16),
    aspect.ratio = 2.5,
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_text(family=myfont, color='black', size=11.5), axis.ticks.y=element_blank(),
    text = element_text(size = 11.5),
    panel.grid=element_blank(),
    plot.margin = unit(c(1, 0, 0, 1), "cm"),
    axis.line = element_line(color=col_background),
    plot.background = element_rect(fill = col_background, color=col_background),
    panel.background = element_rect(fill = col_background, color=col_background)
  )
width=6.2
ggsave(file='challenge02.pdf', plot=pl, width=width, height=1.3*width)
