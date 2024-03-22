###packages
rm(list=ls())   #to clean
library(ggplot2)
library(showtext)
library(dplyr)
library(ggmap)
library(stringi)
library(ggpubr)
showtext_auto()
options(digits=10)

###Specifications
col_blue='#2a2c5fff'
col_brown='#6f4f2fff'
col_beige='#bba178ff'
col_red='#a9133aff'
col_pink='#e89793ff'
col_yellow='#dbaf3cff'
col_green='#36683dff'
col_background='#dad2c5ff'
font_add_google(name='Barlow Condensed', family='Barlow Condensed')
font_add_google(name='Tajawal', family='Tajawal')
myfont='mono'
myfont2='Tajawal'

###import data sets
data1870=read.table('data1870.csv', header=T, sep=',')
data1880=read.table('data1880.csv', header=T, sep=',')

###import coordinates
files_list=list.files(path = "County Coordinates", full.names = TRUE)
data_coord=read.csv(file = files_list[1], header = F, sep = ',')[1,]
data_coord=cbind(data_coord, Group=NaN, Order=NaN, id=NA)
for(i in 1:length(files_list)){
  new_raw_data=read.csv(file = files_list[i], header = F, sep = ',')
  county=substr(files_list[i], 20, stri_length(files_list[i]))
  county=sub("0.*", "", county)
  new_raw_data=cbind(new_raw_data, Group=i, Order=1:nrow(new_raw_data), id=county)
  data_coord=rbind(data_coord,new_raw_data)
}
data_coord=data_coord[-1,]

###rename columns
names(data_coord)[1:2]=c('long','lat')
names(data1870)[1]='id'
names(data1880)[1]='id'

###merge data sets
data_coord$long=-data_coord$long
map1870=merge(data_coord, data1870, by='id')
map1880=merge(data_coord, data1880, by='id')

###reorder data
vect_pop=c("20000 - 30000", "15000 - 20000", "10000 - 15000", "5000 - 10000", "2500 - 5000", "1000 - 2500", "> 1000", "")
vect_pop_labs=c('BETWEEN 20,000 AND 30,000', '15,000 TO 20,000', '10,000 TO 15,000', '5,000 TO 10,000', '2,500 TO 5,000', '1,000 TO 2,500', 'UNDER 1,000', '')
names(vect_pop_labs)=vect_pop
map1870$Population=factor(map1870$Population, levels=vect_pop)
map1880$Population=factor(map1880$Population, levels=vect_pop)
fake_points=data.frame(long=rep(mean(data_coord$long), 3), lat=rep(mean(data_coord$lat), 3), Population=vect_pop[1:3])
fake_points$Population=factor(fake_points$Population, levels=vect_pop)
fake_points2=data.frame(long=rep(mean(data_coord$long), 4), lat=rep(mean(data_coord$lat), 4), Population=vect_pop[4:7])
fake_points2$Population=factor(fake_points2$Population, levels=vect_pop)

###plot 1870
pl1870=ggplot(map1870, aes(long, lat, fill=Population)) +
  geom_map(map=map1870, aes(map_id=id), color="grey20") + coord_quickmap() +
  geom_point(data = fake_points, aes(x=long, y=lat, color=Population), alpha = 0) +
  scale_color_manual(values=c(col_blue, col_brown, col_beige), name='', labels=vect_pop_labs) +
  scale_fill_manual(values=c(col_blue, col_brown, col_beige, col_red, col_pink, col_yellow, col_green, col_background)) +
  annotate('text', x=mean(data_coord$long)-1 , y=max(data_coord$lat)+.12, label='1870', family=myfont2, fontface='bold', size=7) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size=10, byrow = TRUE)),
    fill='none'
    ) +
  theme(
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    legend.text = element_text(family=myfont, size=15, margin = margin(t = 1, b = 1, unit = "cm")),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.spacing.x = unit(.3, 'cm'),
    legend.margin=margin(b=3, l=1.5, unit='cm'),
    plot.margin = unit(c(0, 0, -.6, 0), "cm"),
    plot.background = element_blank(),
    panel.background = element_blank()
  )

###plot 1880
pl1880=ggplot(map1880, aes(long, lat, fill=Population)) +
  geom_map(map=map1880, aes(map_id=id), color="grey20") + coord_quickmap() +
  geom_point(data = fake_points2, aes(x=long, y=lat, color=Population), alpha = 0) +
  scale_color_manual(values=c(col_red, col_pink, col_yellow, col_green), name='', labels=vect_pop_labs) +
  scale_fill_manual(values=c(col_blue, col_brown, col_beige, col_red, col_pink, col_yellow, col_green, col_background)) +
  annotate('text', x=mean(data_coord$long)-1 , y=max(data_coord$lat)+.12, label='1880', family=myfont2, fontface='bold', size=7) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size=10, byrow = TRUE)),
    fill='none'
  ) +
  theme(
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    legend.text = element_text(family=myfont, size=15, margin = margin(t = 1, b = 1, unit = "cm")),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.spacing.x = unit(.3, 'cm'),
    legend.margin=margin(l=2, r = 3.5, unit='cm'),
    plot.margin = unit(c(-.6, 0, 0, 0), "cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = 'left'
  )

###plot title
pl_title=ggplot() + 
   labs(title = "NEGRO POPULATION OF GEORGIA BY COUNTIES.") +
   theme(plot.margin = unit(c(1, 0,.5, 0), "cm"), plot.background = element_blank(), panel.background = element_blank(), plot.title = element_text(family=myfont2, face='bold', hjust=.5, size=22))

###signature
pl_signature=ggplot() +
  labs(title = "#DuBoisChallenge2024 - 01 | Giulia Mezzadri | giuliamezzadri.com") +
  theme(plot.margin = unit(c(.5, 0, 1, 0), "cm"), plot.background = element_blank(), panel.background = element_blank(), plot.title = element_text(family=myfont2, hjust=.5, size=15))

###combine plots
width=10
pl=ggarrange(pl_title, pl1870, pl1880, pl_signature, ncol=1, nrow=4, heights=c(.7,10,10,.5)) + bgcolor(col_background) 
ggsave(file='challenge01.pdf', plot=pl, width=width, height=1.2*width)
