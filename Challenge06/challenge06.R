###packages
rm(list=ls())   #to clean
library(ggplot2)
library(scales)
library(dplyr)
library(showtext)
library(shadowtext)
showtext_auto()
options(digits=10)

###Specifications
###colors
col_black='black'
col_BB='#302418ff' 
col_brown='#66482cff'
col_yellow='#e0aa01ff'
col_YW='#e0bd60ff'
col_background='#d7cdc1ff' #'#dad2c5ff'
col_beige='#bba178ff'


###fonts
font_add_google(name='Barlow Condensed', family='Barlow Condensed')
font_add_google(name = "Bellefair", family = "Bellefair")
myfont='Barlow Condensed'
myfont2='Bellefair'

###import data sets
data=read.table('data.csv', header=T, sep=',')

###data for labels
data_labs=data.frame(rbind(cbind(Year=data$Year, Number=data$Negroes, Type='Negroes', Draw=data$Negroes, Perc=data$Negroes/(data$Negroes+data$Mulattoes)), cbind(Year=data$Year, Number=data$Mulattoes, Type='Mulattoes', Draw=data$Mulattoes/2, Perc=data$Mulattoes/(data$Negroes+data$Mulattoes))))
cols=c('Number','Draw','Perc')
data_labs[,cols]=sapply(data_labs[,cols], as.numeric)
data_labs <- data_labs %>%
  group_by(Year) %>%
  mutate(Label_y = rev(cumsum(rev(Draw)) - 0.5 * rev(Draw)), Upper=cumsum(Draw))
data_labs$Label_y[which(data_labs$Type=='Mulattoes')]=0
data_labs=rbind(data_labs, data.frame(Year="1860", Number=as.numeric(NaN), Type='White', Draw=as.numeric(NaN), Perc=as.numeric(NaN), Label_y=as.numeric(-data_labs$Label_y[which(data_labs$Type=='Negroes' & data_labs$Year==1860)]), Upper=as.numeric(NaN)))
data_labs=cbind(data_labs, label=data_labs$Type)
data_labs$label[which(data_labs$Type=='White')]='Whites'

###modify data set
new_data=data.frame(rbind(cbind(Year=data$Year, Number=data$Negroes, Type='Negroes', Draw=data$Negroes-data$Mulattoes/2), cbind(Year=data$Year, Number=NaN, Type='BB', Draw=data$Mulattoes/2), cbind(Year=data$Year, Number=data$Mulattoes, Type='Mulattoes', Draw=data$Mulattoes/2)))
cols=c('Number','Draw')
new_data[,cols]=sapply(new_data[,cols], as.numeric)
opposite_data=new_data
opposite_data$Draw=-opposite_data$Draw
opposite_data$Number=NaN
opposite_data$Type[which(opposite_data$Type=='Negroes')]='White'
opposite_data$Type[which(opposite_data$Type=='BB')]='YW'
opposite_data$Type[which(opposite_data$Type=='Mulattoes')]='Yellow'
new_data=rbind(new_data, opposite_data)
new_data$Type=factor(new_data$Type, levels=c('Negroes','BB','Mulattoes','White','YW','Yellow'))

###additional years
new_data=rbind(new_data, cbind(Year=1840, Number=NaN, Type=as.character(new_data$Type[which(new_data$Year==1860)]), Draw=as.numeric(new_data$Draw[which(new_data$Year==1860)])/2))
new_data=rbind(new_data, cbind(Year=1800, Number=NaN, Type=c('Negroes','BB','Mulattoes'), Draw=.001))
new_data=rbind(new_data, cbind(Year=1800, Number=NaN, Type=c('White','YW','Yellow'), Draw=-.001))
new_data$Draw[which(new_data$Type=='White' & new_data$Year!=1890)]=-6*10^6
new_data$Draw[which(new_data$Year==1840 & new_data$Type=='Negroes')]=as.numeric(new_data$Draw[which(new_data$Year==1860 & new_data$Type=='Negroes')])/3*2.2
new_data$Draw[which(new_data$Year==1800 & new_data$Type=='Negroes')]=as.numeric(new_data$Draw[which(new_data$Year==1840 & new_data$Type=='Negroes')])/2
new_data$Year=factor(new_data$Year, levels=c(1890,1860,1840,1800))
cols=c('Number','Draw')
new_data[,cols]=sapply(new_data[,cols], as.numeric)

tab=new_data[which(new_data$Type %in% c('Negroes','BB','Mulattoes')),] %>%
  group_by(Year) %>%
  mutate(Label_y = rev(cumsum(rev(Draw)) - 0.5 * rev(Draw)), Upper=rev(cumsum(rev(Draw))))
new_data=rbind(tab, cbind(new_data[which(new_data$Type %in% c('White','YW','Yellow')),], Label_y=NaN, Upper=NaN))

###plot
pl=ggplot(new_data, aes(x=Year, y=Draw, group=Type, fill=Type)) +
  geom_area(aes(color=Type)) +
  geom_line(data=new_data[which(new_data$Type=='Mulattoes'),], aes(x=Year, y=Draw)) +
  geom_line(data=new_data[which(new_data$Type=='Yellow'),], aes(x=Year, y=Draw)) +
  coord_flip(ylim=c(6.8*10^6, -2.3*10^6), xlim=c(3.8,1.4)) + scale_y_reverse() +
  scale_fill_manual(values=c(col_black, col_BB, col_brown, col_background, col_YW, col_yellow)) +
  scale_color_manual(values=c(col_black, col_BB, col_brown, col_background, col_YW, col_yellow)) +
  geom_vline(data=new_data[which(new_data$Year %in% c(1860,1840,1800)),], aes(xintercept = Year), color=col_background) +
  geom_shadowtext(data=data_labs[which(data_labs$Type!='White'),], aes(x=Year, y=Label_y, label=scales::comma(round(as.numeric(Number)))), family=myfont, fontface='bold', vjust=-.8, color=c('white','white','black','black'), bg.colour=c('black','black','white','white'), size=4.5) +
  geom_shadowtext(data=data_labs[which(data_labs$Type!='White'),], aes(x=Year, y=Label_y, label=paste(round(as.numeric(Perc)*100),'%',sep='')), family=myfont, fontface='bold', vjust=1.8, color=c('black','white','black','black'), bg.color=c(col_background,'black',col_background,'white'), size=3.8) +
  geom_shadowtext(data=data_labs[which(data_labs$Year==1860),], aes(x=Year, y=Label_y, label=toupper(label)), family=myfont, fontface='bold', vjust=6.5, color=c('white','black','black'), bg.color=c('black','white',col_background), size=4.5) +
  geom_shadowtext(data=data_labs[which(data_labs$Year==1860 & data_labs$Type=='Mulattoes'),], aes(x=Year, y=Label_y, label=toupper('Mulatres')), family=myfont, fontface='bold', vjust=9.5, color='black', bg.color='white', size=3.8) +
  geom_text(data=new_data[which(new_data$Type=='Negroes' & new_data$Year!=1890),], aes(x=Year, y=Upper, label=Year), family=myfont, fontface='bold', hjust=1.5, color='black', size=3.8) +
  geom_text(data=new_data[which(new_data$Type=='Negroes' & new_data$Year==1890),], aes(x=Year, y=Upper, label=Year), family=myfont, fontface='bold', vjust=1.5, color='black', size=3.8) +
  geom_segment(aes(x = Year, y = 0, xend = Year, yend = Draw), data = new_data[which(new_data$Type=='White'),], color=col_beige) +
  geom_segment(aes(x = Year, y = 0, xend = Year, yend = Upper), data = new_data[which(new_data$Type=='Negroes' & new_data$Year %in% c(1890)),], color=col_black) +
  geom_segment(aes(x = Year, y = 0, xend = Year, yend = Upper+10^5), data = new_data[which(new_data$Type=='Negroes' & new_data$Year %in% c(1800)),], color=col_background) +
  ggtitle('The Amalgamation of the White and Black elements of the population\nin the Unites States.', subtitle="Amalgamation des elements blancs et noirs parmi la population Americaine.") +
  labs(caption='#DuBoisChallenge2024 - 06 | Giulia Mezzadri | giuliamezzadri.com') +
  guides(fill='none', color='none') +
  annotate('text', x=4.28 , y=2.25*10^6, label='Done by Atlanta University.', family=myfont, size=3) +
  theme(
    plot.title = element_text(family=myfont, hjust=.5, face='bold', size=14, lineheight=1),
    plot.subtitle = element_text(family=myfont, hjust=.5, size=11, vjust=-.8),
    plot.caption = element_text(family=myfont, hjust=.5, size=9),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.margin = unit(c(.8, 0, .5, 0), "cm"),
    plot.background = element_rect(fill = col_background, color=col_background),
    panel.background = element_rect(fill = col_background, color=col_background)
  )
width=6.2
print(pl)

###
tab2=data.frame(Year=new_data$Year[which(new_data$Type %in% c('BB'))], Number=NaN, Type='BB1', Draw=new_data$Draw[which(new_data$Type %in% c('BB'))]/(5/4), Label_y=NaN, Upper=NaN, order=1)
tab2=rbind(tab2, data.frame(Year=new_data$Year[which(new_data$Type %in% c('BB'))], Number=NaN, Type='BB2', Draw=new_data$Draw[which(new_data$Type %in% c('BB'))]/(5/3), Label_y=NaN, Upper=NaN, order=2))
ggplot(tab2, aes(x=Year, y=Draw, group=factor(Type), fill=factor(Type))) +
  geom_area(aes(color=Type)) +
  coord_flip(ylim=c(6.8*10^6, -2.3*10^6), xlim=c(3.8,1.4)) + scale_y_reverse()

###to create the small waves at the top of the graph
M=800
dt=1/100000
x=seq(0, 100, length.out=M)
set.seed(7)
y=cumsum(rnorm(M)) * sqrt(dt)
tab=data.frame(x=x, y=y)
tab=tab[10:M,]
tab=rbind(tab, c(x=107, y=min(tab$y)))
pl2=ggplot(data=tab, aes(x=x, y=y)) + geom_line(size=.01, color='grey20') + geom_area(data=tab[1:264,], aes(y=y), fill = col_black) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = NULL, y = NULL) + theme_void()
pl3=pl + annotation_custom(ggplotGrob(pl2), xmin = 4.004, xmax = 4.1, ymin=-new_data$Draw[which(new_data$Type=='Negroes' & new_data$Year==1800)]-10^4, ymax = 2.75*10^6)
ggsave(file='challenge06.pdf', plot=pl3, width=width, height=1.2*width)

