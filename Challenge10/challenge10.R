###packages
rm(list=ls())   #to clean
setwd("~/Desktop/DataViz Challenges/Du Bois/Challenge10/") #set folder
library(ggplot2)
library(maps)
library(scales)
library(dplyr)
library(ggpubr)
library(showtext)
library(shadowtext)
library(ggstar)
library(cowplot)
library(extrafont) 
loadfonts(device = "win")
showtext_auto()
options(digits=10)

###colors
col_dark_blue='#2a2c5fff'
col_red='#c4304fff'# '#bb2f49ff'
col_blue='#878db4ff'
col_beige='#b1957fff'
col_pink='#ebc6b2ff' #'#e5cbbbff' # '#e89793ff'
col_gray='#958e7eff'
col_yellow='#f1bb28ff'
col_brown='#52382eff'
col_beige2='#d5bfadff'
col_background='#e9decdff'
red='#a9133aff'
vect_col=c(col_red, col_blue, col_pink, col_beige, col_gray , col_yellow)
col_states=c(col_red, col_blue, col_gray, col_red, col_red, col_yellow, col_red, 'white', col_gray, 'black', col_pink, col_beige2,
             col_pink, col_gray, col_beige2, col_brown, col_brown, col_gray, col_brown, col_red, col_red, col_red, col_yellow, col_dark_blue,
             col_blue, col_blue, col_red, col_beige2, col_yellow, col_beige2, col_yellow, col_blue, col_gray, col_beige2, col_beige2, col_beige2,
             col_beige2, col_beige2, col_beige2, col_beige2, col_pink, col_blue, col_dark_blue, col_gray, col_dark_blue, col_dark_blue, col_yellow,
             col_brown, col_dark_blue, col_yellow, col_brown, col_blue, col_blue, col_blue, col_blue, col_yellow, col_yellow, col_yellow, col_yellow, col_yellow,
             col_pink, col_yellow, col_dark_blue)

###fonts
font_add_google(name='PT Sans Narrow', family='PT Sans Narrow')
font_add_google(name='Saira Condensed', family='Saira Condensed')
font_add_google(name='Gemunu Libre', family='Gemunu Libre')
font_add_google(name='Dosis', family='Dosis')
font_add_google(name='Odibee Sans', family='Odibee Sans')
myfont='Saira Condensed'
myfont2='Odibee Sans'

###import us map
map = map_data("state")

###import data sets
data=read.table('data.csv', header=T, sep=',')
occ_labs=c('Teachers','Ministers','Business','Government Service','Other Professions','House Wives')
data$Occupation=factor(data$Occupation, levels=occ_labs)

# Compute the position of labels
data <- data %>% 
  arrange(rev(Occupation)) %>%
  mutate(ypos = cumsum(Percentage)- 0.5*Percentage )

###plot map
pl_map=ggplot() + 
  geom_polygon(data=map, aes(x=long, y=lat, group=group, fill=factor(group))) +
  coord_map("conic", lat0=30) +
  guides(fill='none') +
  scale_fill_manual(values=col_states) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = NULL, y = NULL) + theme_void()

###plot top
pl_top=ggplot() + coord_cartesian(xlim=c(-1.9,1.9), ylim=c(-2.85,.95)) +
  ggtitle(toupper("        A series of statistical charts, illustrating the condition of the de-\nscendants of former african slaves now resident in the United States\nof America."),
          subtitle=toupper("        Une série de cartes et diagrammes statistiques montrant la condition pré-\nsente des descendants des anciens esclaves africains actuellment établis dans\nles etats Unis d'amérique.")) +
  annotate('text', x=-1.52 , y=0, label=toupper('Prepared and executed by\nnegro students under the\ndirection of\nAtlanta University,\nAtlanta, GA,\nUnited States of America.'), family=myfont, size=5, hjust=.5, lineheight=.8) +
  annotate('text', x=1.2 , y=0, label=toupper('Préparées et executées par\ndes étudiants nègres sous\nla direction de l\'université\n d\'atlanta,\netat de georgie,\netats unis d\'amérique.'), family=myfont, size=5, hjust=.5, lineheight=.8, color=col_red) +
  annotation_custom(ggplotGrob(pl_map), xmin = -.67, xmax = .35, ymin=-1, ymax = 1) +
  geom_rect(aes(xmin=-.7, xmax=-.57, ymin=-1.4, ymax=-1), fill="black") +
  geom_point(aes(x=-.635, y=-1.1), color=col_background, size=1.5) +
  geom_star(aes(x=-.635, y=-1.29), color=col_background, fill=col_background, size=1.5) +
  annotate('text', x=-.5 , y=-1.2, label=toupper('Center of negro population\nAtlanta University'), family=myfont, size=3, hjust=0, lineheight=1) +
  annotate('text', x=-.16 , y=-1.8, label=toupper('The university was founded in 1867. It has instructed 6000 negro students.'), family=myfont, size=5, hjust=.5, lineheight=.8) +
  annotate('text', x=-.16 , y=-2.15, label=toupper('L\'université a été fondée en 1867. Elle a donné l\'instruction à 6000 étudiants negrès.'), family=myfont, size=5, hjust=.5, lineheight=.8, color=col_red) +
  annotate('text', x=-.16 , y=-2.5, label=toupper('It has graduated 330 negroes among whom are:'), family=myfont, size=5, hjust=.5, lineheight=.8) +
  annotate('text', x=-.16 , y=-2.85, label=toupper('Elle a délivré des diplomes à 330 nègres dont :'), family=myfont, size=5, hjust=.5, lineheight=.8, color=col_red) +
  theme(
    plot.margin = margin(30, 0, 0, 32),
    plot.title = element_text(family=myfont, face='bold', hjust=0, size=18, margin = margin(b = 12)),
    plot.subtitle = element_text(family=myfont, hjust=0, size=16, color=col_red),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  )


###main plot
pl=ggplot(data, aes(x="", y=Percentage, fill=Occupation)) + geom_col(position='stack', width=1) +
  coord_polar("y", start=pi/2, direction=-1) +
  geom_text(aes(x=1, y = ypos, label = paste(Percentage,'%',sep='')), data=data[which(data$Occupation %in% c('House Wives')),], size=6, family=myfont, fontface='bold') +
  geom_text(aes(x=1, y = ypos, label = paste(Percentage,'%',sep='')), data=data[which(data$Occupation %in% c('Teachers')),], size=6, family=myfont, fontface='bold') +
  geom_text(aes(x=1.4, y = ypos, label = paste(Percentage,'%',sep='')), data=data[which(data$Occupation %in% c('Business','Other Professions')),], size=4.5, family=myfont, fontface='bold') +
  geom_text(aes(x=1.2, y = ypos, label = paste(Percentage,'%',sep='')), data=data[which(data$Occupation %in% c('Ministers','Government Service')),], size=4.5, family=myfont, fontface='bold') +
  scale_fill_manual(values=vect_col, breaks=c('Teachers','Ministers','Government Service','Business','Other Professions','House Wives')) +
  guides(
    fill='none'
  ) +
  theme(
    plot.margin = margin(-15, 0, -20, 5),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  )

###legend left
pl_legend_l=ggplot(data, aes(x=Percentage, y=Percentage, color=Occupation)) + geom_point() +
  scale_color_manual(values=vect_col, name='', labels=toupper(occ_labs)) +
  guides(color = guide_legend(override.aes = list(size=7, byrow = TRUE))) +
  theme(
    legend.margin = margin(0, 0, 0, 70),
    legend.position = 'left',
    legend.text = element_text(family=myfont, size=10),
    legend.key = element_blank(),
    legend.background = element_blank()
    )
legend_l=cowplot::get_legend(pl_legend_l)

###legend right
fr_occ_labs=c('mères de famille','medicins, advocats, et étudiants','marchands','employés du gouvernment','ministres de l\'evangile','professeurs et instituteurs')
data=cbind(data, FR_occ=fr_occ_labs)
pl_legend_r=ggplot(data, aes(x=Percentage, y=Percentage, color=FR_occ)) + geom_point() +
  scale_color_manual(values=vect_col, name='', labels=toupper(rev(fr_occ_labs))) +
  guides(
    color = guide_legend(override.aes = list(size=7), label.position = "left")
    ) +
  theme(
    legend.position = 'left',
    legend.text.align = 1,
    legend.margin = margin(0, 10, 0, -80),
    legend.text = element_text(family=myfont, size=10, hjust=1),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
legend_r=cowplot::get_legend(pl_legend_r)

###plot bottom text
pl_bottom=ggplot() + coord_cartesian(xlim=c(-.4,5.8), ylim=c(-.5,.3)) +
  annotate('text', x=0, y=0, label=toupper("     The university has 20 professors and instructors and 250 students at present. It has five build-\nings, 60 acres of campus, and a library of 11,000 volumes. It aims to raise and civilize the sons of\nthe freedmen by training their more capable members in the liberal arts according to the best\nstandards of the day. The proper accomplishment of this work demands an endowment fund of\n$500,000.\n     L'université a actuellement 20 professeurs et instructeurs et 250 étudiants. Elle est composée\nde cinc bâtiments, 60 acres (environ 26 hectares) de terrain servant de cour et de champ de récré-\nation, et d'une bibliotèque contenant 11,000 volumes. Son but est d'élever et de civiliser les fils\ndes nègres affranchis en donnant aux mieux doués une éducation dans les arts libéraux en ac-\ncord avec les idées les plus progressistes de l'époque. L'accomplissement de cette œuvre demande\nune donation de $500,000 (2,500,000 Francs)."), family=myfont, size=4.5, hjust=0, lineheight=.8) +
  annotate('text', x=2.5 , y=-.37, label="#DuBoisChallenge2024 - 10 | Giulia Mezzadri | giuliamezzadri.com", family=myfont, size=4, hjust=.5, lineheight=.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = NULL, y = NULL) + theme_void()


###arrange
pl_final=ggarrange(pl_top ,ggarrange(legend_l, pl, legend_r, ncol=3, nrow=1, widths = c(.7,1.6,1)), pl_bottom, nrow=3, heights=c(1.1,.8,.7)) + bgcolor(col_background) 
width=8
ggsave(file='challenge10.pdf', plot=pl_final, width=width, height=1.3*width)
