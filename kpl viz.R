#importing libraries
library(gt)
library(gtExtras)
library(ggpubr)
library(tidyverse)
library(showtext)
library(ggtext)
library(hrbrthemes)
library(ggbump)
library(ggimage)
library(grid)
library(png)
library(jpeg)
library(patchwork)
library(gganimate)
# read csv file
kpl_rank <- read.csv("KPL Ranks.csv")
kpl_table <- read.csv('KPL Table.csv')
kpl_points <- read.csv('KPL Points.csv')
kpl_gd <- read.csv('KPL GD.csv')
kpl_2223 <- read.csv('kpl_summary.csv')
# add fonts
font_add_google('Merriweather', 'merri')
font_add_google('Carter One', 'carter')
font_add_google('Roboto Slab', 'roboto')
font_add_google('Lilita One', 'lilita')
showtext_auto()
# TRANSFORM DATA
# empty vectors
position <- c()
matchweek <- c()
teams <- c()
points <- c()
gd <- c()
for (i in 1:31){
  # add matchweeks
  mw <- c(rep(i, 18))
  # add positions
  pos <- kpl_rank[, paste('X', as.character(i), sep='')]
  pts <- kpl_points[, paste('X', as.character(i), sep='')]
  g <- kpl_gd[, paste('X', as.character(i), sep='')]
  # concatenate
  matchweek <- c(matchweek, mw)
  position <- c(position, pos)
  points <- c(points, pts)
  gd <- c(gd, g)
  teams <- c(teams, kpl_rank[,'Teams'])
}
# transform dataframe
kpl_2223 <- data.frame(
  matchweek = matchweek,
  teams = teams,
  position = position,
  points = points,
  gd = gd
)

# Add text column for points and goal difference
output_ptsgd <- vector('character', nrow(kpl_2223))
for (i in 1:nrow(kpl_2223)){
  if (kpl_2223[i, 'gd'] > 0){
  output_ptsgd[i] <- paste(as.character(kpl_2223[i, 'points']), '<+', kpl_2223[i, 'gd'], '>', sep='')
  }else{
    output_ptsgd[i] <- paste(as.character(kpl_2223[i, 'points']), '<+', kpl_2223[i, 'gd'], '>', sep='')
  }
}
kpl_2223$pts_gd <- as.character(output_ptsgd)
# team names
team_names <- data.frame(
  xt = c(rep(1, 18)),   
  yt = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  teams = kpl_rank$Teams
)
# add images to dataframe
timu <- c(kpl_rank[,'Teams'])
kpl_2223$image <- 'police.png'
team_names$image <- 'police.png'
# loop through kpl teams
for (i in 1:length(timu)){
  kpl_2223[kpl_2223$teams == timu[i], 'image'] <- paste(timu[i], 'png', sep='.')
  team_names[team_names$teams == timu[i], 'image'] <- paste(timu[i], 'png', sep='.')
  }
# kpl logo 
my_image <- readPNG('kpl-removebg-preview.png', native=TRUE)
# named vectors for scale_color_manual
back_to_earth = c('Bandari' = 'blue3', 'Bidco United' = 'blue2', 'Gor Mahia' = 'green4', 'Homeboyz' = 'yellow', 'Kariobangi Sharks' = 'yellowgreen',
                   'KCB' = 'green3', 'Leopards' = 'blue3', 'Mathare United' = 'seagreen', 'Nairobi City Stars' = 'purple2', 'Nzoia Sugar' = 'seagreen4',
                   'Police' = 'firebrick2', 'Posta Rangers' = 'red3', 'Sofapaka' = 'steelblue2', 'Talanta' = 'red2', 'Tusker' = 'grey9',
                   'Ulinzi Stars' = 'red1', 'Vihiga Bullets' = 'darkgreen', 'Wazito' = 'yellow1')
kpl_2223$matchweek <- as.numeric(kpl_2223$matchweek)


# VISUALIZE TABLE
kpl_strong <- kpl_table %>%
  select(P, Teams, MP, W, D, L, GD, Points)%>%
  gt() %>%
  tab_header(
    title = 'FKF PREMIER LEAGUE TABLE') %>%
  cols_label(P='', MP='MP',W='W', D='D',L='L', GD='GD',Points='Points')%>%
  gt_highlight_rows(rows=1, fill='gold1',font_weight='normal') %>%
  gt_highlight_rows(rows=16, fill='orchid2',font_weight='normal') %>%
  gt_highlight_rows(rows=c(17,18), fill='red2',font_weight='normal') %>%
  tab_style(locations=cells_title(), 
            style=list(cell_text(weight='bold', size=24, font='roboto'))) %>% 
  tab_style(locations=cells_body(), 
            style=list(cell_text(weight='normal', size=21, font='roboto'))) %>%
tab_style(locations=cells_column_labels(), 
          style=list(cell_text(weight='bold', size=22, font='roboto'))) %>%
  tab_options(table.background.color='lavenderblush1', table.width = pct(80))
kpl_strong
# save
gtsave(kpl_strong, 'kpl strong 1.png')


# africa's talking
b3cf8404cff3ed7b32b48cd84542def16f108075f4e8bc5443fe379b11dfec04