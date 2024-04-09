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
myfont='Tomorrow'

###import data set
data=read.table('data.csv', header = TRUE, sep=',')

###colors
col_red='#920122ff'
col_yellow='#e8b601ff'
col_brown='#2b1100ff' 
col_black='black'
col_background='#dad2c5ff'

# Calculate y position, placing it in the middle
data_new_format <- data %>%
  mutate(Label_y = rev(cumsum(rev(Percentage)) - 0.5 * rev(Percentage)), Upper=rev(cumsum(rev(Percentage))))
data_new_format=cbind(data_new_format, text=c("i.e. full-blooded\nnegroes.","i.e. persons with\nsome white blood\nor descendants\nof light colored\nafricans.","i.e. persons with\nmore white than\nnegro blood."))

###plot
pl=ggplot(data_new_format, aes(x=0, y=Percentage, fill=Category, color=Category)) +
  geom_col(width=.4) + coord_cartesian(clip="off") +
  geom_text(aes(x=0, y=Label_y, label=paste(Percentage,'%',sep='')), color=c("white",col_red,"black"), family=myfont, fontface='bold') +
  geom_text(aes(x=-.5, y=Upper-6, label=toupper(paste(Category,'.',sep=''))), color='black', family=myfont, fontface='bold', vjust=1, size=4.5) +
  geom_text(aes(x=-.5, y=Upper-10, label=toupper(text)), family=myfont, hjust=0, vjust=1, lineheight=1, size=2.8, color='grey10') +
  scale_fill_manual(values=c(col_black, col_brown, col_yellow), guide='none') +
  scale_color_manual(values=c(col_black, col_brown, col_yellow), guide='none') +
  ggtitle(toupper("Race amalgamation in georgia."), subtitle=toupper("Based on a study of 40,000 individuals of negro descent.")) +
  labs(caption='#DuBoisChallenge2024 - 05 | Giulia Mezzadri | giuliamezzadri.com') +
  theme(
    plot.title = element_text(family=myfont, hjust=.5, face='bold', size=13, lineheight=1.6),
    plot.subtitle = element_text(family=myfont, hjust=.5, size=9),
    plot.caption = element_text(family=myfont, hjust=.5, size=8),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.margin = unit(c(.8, 2, .5, 2.2), "cm"),
    plot.background = element_rect(fill = col_background), 
    panel.background = element_rect(fill = col_background)
  )

print(pl)
width=6
ggsave(file='challenge05.pdf', plot=pl, width=width, height=1.3*width)


