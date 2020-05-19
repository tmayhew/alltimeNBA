library(rvest)
library(tidyverse)
library(ggthemes)
library(Rfast)

g1 = read.csv("finalRegSeason/group1.csv")[,-1]
summary(g1$Yr)

names = g1 %>% select(Player, Pos, Age, Tm, Yr)
dat1 = g1 %>% select(-Player, -Pos, -Age, -Tm, -Yr, -GS, -X3P, -X3PA, -X3P., -ORB, 
                     -DRB, -STL, -BLK, -TOV, -X3PAr, -ORB., -DRB., -STL., -BLK., 
                     -TOV., -USG., -OBPM, -DBPM, -BPM, -VORP)
pr.out = prcomp(dat1, scale = T)
PC1 = -1*(pr.out$x[,1])
g1p = cbind.data.frame(g1, PC1)

g2 = read.csv("finalRegSeason/group2.csv")[,-1]
summary(g2$Yr)
names = g2 %>% select(Player, Pos, Age, Tm, Yr)
dat1 = g2 %>% select(-Player, -Pos, -Age, -Tm, -Yr, -GS, -X3P, -X3PA, -X3P., -X3PAr)
pr.out = prcomp(dat1, scale = T)
PC1 = -1*(pr.out$x[,1])
g2p = cbind.data.frame(g2, PC1)

rbind.data.frame(g1p, g2p) %>% arrange(desc(PC1)) %>% select(Player, Yr, PC1)

g3 = read.csv("finalRegSeason/group3.csv")[,-1]
summary(g3$Yr)
names = g3 %>% select(Player, Pos, Age, Tm, Yr)
dat1 = g3 %>% select(-Player, -Pos, -Age, -Tm, -Yr, -GS, -X3P, -X3PA, -X3P., -X3PAr)
pr.out = prcomp(dat1, scale = T)
PC1 = -1*(pr.out$x[,1])
g3p = cbind.data.frame(g3, PC1)

reg = rbind.data.frame(g1p, g2p, g3p) %>% arrange(desc(PC1));nrow(reg)
reg = reg %>% filter(MP > 100);nrow(reg)
reg %>% head(20)

#####################################################################################################################

prime.df = reg %>% group_by(Player) %>% summarise(
    p1 = max(PC1),
    p2 = nth(PC1, 2, descending = T),
    p3 = nth(PC1, 3, descending = T),
    p4 = nth(PC1, 4, descending = T),
    p5 = nth(PC1, 5, descending = T),
  ) %>% arrange(desc(p1))

prime.df$p1 = ifelse(abs(prime.df$p1) < 0.001, 0, prime.df$p1)
prime.df$p2 = ifelse(abs(prime.df$p2) < 0.001, 0, prime.df$p2)
prime.df$p3 = ifelse(abs(prime.df$p3) < 0.001, 0, prime.df$p3)
prime.df$p4 = ifelse(abs(prime.df$p4) < 0.001, 0, prime.df$p4)
prime.df$p5 = ifelse(abs(prime.df$p5) < 0.001, 0, prime.df$p5)

prime.df = prime.df %>% drop_na() %>% head(500)
names = prime.df %>% select(Player)
dat2 = prime.df %>% select(-Player, -p4, -p5)
dat3 = prime.df %>% select(-Player)

prime.df$PC1 = prime.df$p1
pr.out1 = prcomp(dat2, scale = T)
coef. = -(pr.out1$rotation[,1])
prime.df$PC2 = prime.df$p1*(coef.[1]) + prime.df$p2*(coef.[2]) + prime.df$p3*(coef.[3])

pr.out2 = prcomp(dat3, scale = T)
coef2. = -(pr.out2$rotation[,1])
prime.df$PC3 = prime.df$p1*(coef2.[1]) + prime.df$p2*(coef2.[2]) + prime.df$p3*(coef2.[3]) + prime.df$p4*(coef2.[4]) + prime.df$p5*(coef2.[5])

#################################################################################################

primesc = prime.df %>% select(Player, PC1, PC2, PC3)
head(primesc)
n = primesc %>% select(Player)
d = primesc %>% select(-Player)
pr.out = prcomp(d, scale. = T)
primesc$score = -1*(pr.out$x[,1])
primesc$adj = primesc$score + abs(min(primesc$score))
primesc$f = primesc$adj/max(primesc$adj)

p = primesc %>% arrange(desc(f)) %>% select(Player, psc = f);p
#p %>% write.csv("results/primeleaderboard.csv")
p = p %>% arrange((psc)) %>% tail(25)
p$Player = factor(as.character(p$Player), levels = p$Player)
p %>% ggplot(aes(x = Player, y = psc)) + geom_bar(stat = "identity", width = I(1/2)) + coord_flip() + theme_bw() + scale_y_continuous("Prime Score")





