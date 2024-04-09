###packages
rm(list=ls())   #to clean
library(ggplot2)
library(showtext)
library(dplyr)
showtext_auto()

###fonts
font_add_google(name = "Squada One", family = "Squada One")
font_add_google(name='Barlow Condensed', family='Barlow Condensed')

###import data set
data=read.table('data.csv', header = TRUE, sep=',')
col_red='#920122ff'
col_green='#1f541fff'
col_black='#151612ff'
col_background='#dad2c5ff'

###
data_new_format=data.frame(Year=c(rep(1860,3),rep(1890,3)), Type=c('A','Slave','Free','Owners','Tenants','Z'), Percentage=c(NA,89,11,19,81,NA), Number=c(145,89,16,38,162,50), Label=c(NA,'SLAVES\nESCLAVES','FREE LABORERS\nOUVRIER LIBRES','PAYSANT PROPRIETORS\nPAYSANTS PROPRIETAIRES','TENANTS\nMÉTAYERS',NA))
data_new_format = arrange(data_new_format, Year, -xtfrm(Type))

# Calculate y position, placing it in the middle
data_new_format <- data_new_format %>%
  group_by(Year) %>%
  mutate(Label_y = cumsum(Number) - 0.5 * Number, Upper=cumsum(Number))

pl=ggplot(data_new_format, aes(x=factor(Year), y=Number, fill=Type)) +
  geom_col(width=.6) +
  geom_text(data=data_new_format[c(2,6),], aes(label=Year, y=Upper+8), family='Squada One', fontface='bold', size=8) +
  geom_text(data=data_new_format[c(1,5,6),], aes(label=Label, y=Label_y-4.5), color=c(col_red, rep('black',2)), family='Barlow Condensed', fontface='bold', size=4.6, lineheight=.8) +
  geom_text(data=data_new_format[c(1,5,6),], aes(label=paste(Percentage,'%',sep=''), y=Label_y+8), color=c(col_red, rep('black',2)), family='Squada One', fontface='bold', size=8.5) +
  geom_text(data=data_new_format[c(2),], aes(label=Label, y=Label_y), hjust=.3, color='black', family='Barlow Condensed', fontface='bold', size=4.6, lineheight=.8) +
  geom_text(data=data_new_format[c(2),], aes(label=paste(Percentage,'%',sep=''), y=Label_y), hjust=2, color='black', family='Squada One', fontface='bold', size=8.5) +
  geom_line(data=data_new_format[c(2,5),], aes(x=as.numeric(factor(Year))+c(.3,-.3), y=Upper), stat='identity', group=1, linetype='dashed', color='grey60') +
  geom_line(data=data_new_format[c(2,5),], aes(x=as.numeric(factor(Year))+c(.3,-.3), y=Upper/c(1,5/4)), stat='identity', group=1, linetype='dashed', color='grey60') +
  geom_line(data=data_new_format[c(2,5),], aes(x=as.numeric(factor(Year))+c(.3,-.3), y=Label_y), stat='identity', group=1, linetype='dashed', color='grey60') +
  geom_line(data=data_new_format[c(2,5),], aes(x=as.numeric(factor(Year))+c(.3,-.3), y=Label_y-c(0,(131-40)/2)), stat='identity', group=1, linetype='dashed', color='grey60') +
  geom_line(data=data_new_format[c(1,4),], aes(x=as.numeric(factor(Year))+c(.3,-.3), y=Upper), stat='identity', group=1, linetype='dashed', color='grey60') +
  scale_fill_manual(values=c(col_background, col_green, col_red, col_black, col_green, col_background), guide='none') +
  annotate('text', x=1.5 , y=max(data_new_format$Upper)+50, size=4, label='DONE BY ATLANTA UNIVERSITY.', family='Barlow Condensed', fontface='bold') +
  annotate('text', x=.55 , y=max(data_new_format$Upper)-30, hjust=0, size=3.5, label='IN 1890 NEARLY ONE FIFTH OF THEM OWNED THEIR OWN HOMES AND FARMS.\nTHIS ADVANCE WAS ACCOMPLISHED ENTIRELY WITHOUT STATE AID, AND IN THE\nFACE OF PROSCRIPTIVE LAWS.\n\nEN 1890 ENVIRON UN CINQUIÈME ÉTAIENT PROPRIÉTAIRES DE LEURS HAB-\nITATIONS ET DE LEURS FERMES. CE PROGRÈS S\'EST ACCOMPLI SANS\nSECOURS AUCUN DE L\'ETAT ET EN PRÉSENCE DE LOIS DÉFAVORABLES.', family='Barlow Condensed') +
  annotate('text', x=.65 , y=max(data_new_format$Upper)-90, lineheight=1.5, hjust=0, size=3.5, label='IN 1860 NEARLY 90% OF THE BLACKS WERE SLAVES.\nEN 1860 ENVIRON 90% DES NÈGRES ÉTAIENT ESCLAVES.', family='Barlow Condensed') +
  theme(
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    plot.caption = element_text(family='Barlow Condensed', hjust=.5, size=11),
    plot.title = element_text(family='Barlow Condensed', face='bold', size=16, hjust=.5, lineheight=2),
    panel.grid=element_blank(),
    plot.margin = unit(c(1, 0, .5, 0), "cm"),
    plot.background = element_rect(fill = col_background), 
    panel.background = element_rect(fill = col_background)
  ) +
  labs(caption='#DuBoisChallenge2024 - 08 | Giulia Mezzadri | giuliamezzadri.com') +
  ggtitle('THE RISE OF THE NEGROES FROM SLAVERY TO FREEDOM IN ONE GENERATION. \nPROGRÈS GRADUEL DES NÈGRES DE L\`ESCLAVAGE À LA LIBERTÉ EN UNE GÉNÉRATION.')
print(pl)
ggsave(file='challenge08.pdf', plot=pl, width=8, height=10)


