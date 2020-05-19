library(tidyverse)

lb = read.csv("results/Leaderboard.csv")[,-1];head(lb)
n = lb %>% select(Player)
d = lb %>% select(-Player)

