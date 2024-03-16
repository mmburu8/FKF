### PREREQUISITES AND PACKAGES ARE FOUND IN THE R SCRIPT kpl viz.R
# Visualization inspired by Bundesliga 2022/2023 and lessons learnt from failure in Premier league 2022/2023
back_to_earth = c('Bandari' = 'blue3', 'Bidco United' = 'blue2', 'Gor Mahia' = 'green4', 'Homeboyz' = 'gold2', 'Kariobangi Sharks' = 'yellowgreen',
                  'KCB' = 'green3', 'Leopards' = 'blue3', 'Mathare United' = 'seagreen', 'Nairobi City Stars' = 'purple2', 'Nzoia Sugar' = 'seagreen4',
                  'Police' = 'firebrick2', 'Posta Rangers' = 'red3', 'Sofapaka' = 'steelblue2', 'Talanta' = 'red2', 'Tusker' = 'grey9',
                  'Ulinzi Stars' = 'red1', 'Vihiga Bullets' = 'darkgreen', 'Wazito' = 'gold2')

# FINAL GAMEDAY
final_gd <- kpl_2223[kpl_2223$matchweek == 34,]
print(nrow(final_gd))
# Add text column for points and goal difference
output_ptsgd <- vector('character', nrow(final_gd))
for (i in 1:nrow(final_gd)){
  output_ptsgd[i] <- paste(as.character(final_gd[i, 'points']), ' (', final_gd[i, 'gd'], ')', sep='')
}
final_gd$pts_gd <- output_ptsgd

# background image 
back_img <- readPNG('trois.png')

# logo dataframe
logoDF <- data.frame(x=67, y=5, image='kpl-removebg-preview.png')
# add text for outcome position

txtDF <- data.frame(x=c(62, 66, 65), y=c(3,2,1), txt=c('1ST = CAF PLAYOFF', '16TH = RELEGATION PLAYOFF', '17TH - 18TH = RELEGATION'),
                    col=c('gold', 'orchid4', 'red4'))
print(txtin)
final_gd%>%
  mutate(teams = fct_reorder(teams, desc(position)))%>%
  ggplot(aes(x=points, y=teams))+
  background_image(back_img)+
  #annotation_custom(rasterGrob(back_img, width=unit(1, 'npc'), height=unit(1, 'npc')), -Inf, Inf, -Inf, Inf)+
  geom_bar(aes(fill=teams, color=teams), alpha=0.7, width=0.85, stat='identity')+
  geom_text(aes(x=points+4, y=teams, color=teams, label=pts_gd), family='roboto')+
  geom_text(aes(x=-8,y=teams,color=teams,label=teams), family='roboto')+
  geom_image(aes(x=points-2, y=teams, image=image), size=0.055)+
  geom_image(logoDF, mapping=aes(x=x, y=y,image=image), size=0.17)+
  geom_text(txtDF, mapping=aes(x=x, y=y,label=txt),family='roboto', size=3.5)+
  labs(x='', y='')+
  theme(legend.position='none',
        plot.background=element_rect(fill='lavenderblush1', color='lavenderblush1'),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype='dashed',color='snow'),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face='plain', family='carter', size=10, colour=c(rep('red4', 2), 'orchid4', rep('grey8', 14), 'gold')),
        axis.text.x = element_text(face='plain', family='carter', size=9),
        axis.ticks.y = element_blank())+
  scale_color_manual(values=back_to_earth)+
  scale_fill_manual(values=back_to_earth)+
  scale_x_continuous(position='top', limits=c(-11,75), breaks=seq(0, 70, 5))+
  scale_y_discrete(labels=c('18TH', '17TH', '16TH', '15TH', '14TH', '13TH', '12TH', 
                            '11TH', '10TH', '9TH', '8TH', '7TH', '6TH', '5TH', '4TH',
                            '3RD', '2ND', '1ST'))
  
#animation viz
kpl_sn <- kpl_2223 %>%
  group_by(matchweek) %>%
  arrange(matchweek, desc(position)) %>%
  mutate(ranking = row_number())

dybala <- kpl_sn %>%
  ggplot(aes(x=points, y=as.factor(ranking)))+
  background_image(back_img)+
  geom_bar(aes(fill=teams, color=teams), alpha=0.7, width=0.85, stat='identity')+
  geom_text(aes(x=points, y=as.factor(ranking), label=as.character(points), color=teams), hjust=-0.1,size=4, family='roboto')+
  geom_text(aes(x=-9,y=as.factor(ranking),color=teams,label=teams), family='roboto', size=4.2)+
  geom_image(aes(x=points-2, y=as.factor(ranking), image=image), size=0.055)+
  geom_image(logoDF, mapping=aes(x=x, y=y,image=image), size=0.17)+
  geom_text(txtDF, mapping=aes(x=x, y=y,label=txt),family='roboto', size=3.8)+  
  labs(x='', y='', title='2022/2023 FKF PREMIER LEAGUE  MW{closest_state}')+
  theme(legend.position='none',
        plot.background=element_rect(fill='lavenderblush1', color='lavenderblush1'),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype='dashed',color='snow'),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face='plain', family='carter', size=12, colour=c(rep('red4', 2), 'orchid4', rep('grey8', 14), 'gold')),
        axis.text.x = element_text(face='plain', family='carter', size=11, colour='grey8'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face='plain', family='lilita', size=17, hjust=0.5))+
  scale_color_manual(values=back_to_earth)+
  scale_fill_manual(values=back_to_earth)+
  scale_x_continuous(position='top', limits=c(-13,75), breaks=seq(0, 70, 5))+
  scale_y_discrete(labels=c('18TH', '17TH', '16TH', '15TH', '14TH', '13TH', '12TH', 
                            '11TH', '10TH', '9TH', '8TH', '7TH', '6TH', '5TH', '4TH',
                            '3RD', '2ND', '1ST'))+
  transition_states(matchweek, transition_length = 3, state_length=1)+
  ease_aes('exponential-in')
mygif <- animate(dybala, height=580, width=900, nframes=600, renderer=gifski_renderer(loop=FALSE))
mygif
anim_save(filename='kpl.gif', mygif)
