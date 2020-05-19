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

t1cut = round(nrow(reg)/5,0);t1cut
t2cut = round(nrow(reg)/5,0)*2;t2cut
t3cut = round(nrow(reg)/5,0)*3;t3cut
t4cut = round(nrow(reg)/5,0)*4;t4cut
t1Pcut = reg$PC1[t1cut]
t2Pcut = reg$PC1[t2cut]
t3Pcut = reg$PC1[t3cut]
t4Pcut = reg$PC1[t4cut]

reg$tier = as.factor(ifelse(reg$PC1 >= t1Pcut, 1, ifelse(reg$PC1 >= t2Pcut, 2, ifelse(reg$PC1 >= t3Pcut, 3, ifelse(reg$PC1 >= t4Pcut, 4, 5)))))
reg %>% ggplot(aes(x = tier, y = PC1)) + geom_boxplot()

all.nbadf = read.csv("allnba.csv")[,-1] %>% select(Year, Player, all.nba)
all.nbadf$firstteam = ifelse(all.nbadf$all.nba == 1, 1, 0)
all.nbadf$secthird = ifelse(all.nbadf$all.nba == 1, 0, 1)

reg$Player = as.character(reg$Player)
for (i in 1:nrow(reg)){
  sp = strsplit(x = reg$Player[i], split = "")[[1]]
  if (sp[length(sp)] == "*"){
    reg$Player[i] = paste(sp[1:(length(sp)-1)], collapse = "")
  } else{
    reg$Player[i] = paste(sp[1:(length(sp))], collapse = "")
  }
}
names(all.nbadf)[1] = c("Yr")
allstardf = read.csv("allstardf.csv")[,-1]

head(all.nbadf)
head(allstardf)
head(reg)

allstardf = read.csv("allstardf.csv")[,-1]
allstardf %>% filter(Player == "Reggie Miller")

cdf = full_join(all.nbadf, allstardf, by = c("Player", "Yr"))
for (i in 1:nrow(cdf)){
  for (j in 1:ncol(cdf)){
    if(is.na(cdf[i,j])){
      cdf[i,j] = 0
    }else{
      cdf[i,j] = cdf[i,j]
    }
  }
}

careerdf = full_join(reg, cdf, by = c("Player", "Yr"))

for (i in 1:nrow(careerdf)){
  for (j in 1:ncol(careerdf)){
    if(is.na(careerdf[i,j])){
      careerdf[i,j] = 0
    }else{
      careerdf[i,j] = careerdf[i,j]
    }
  }
}

head(careerdf)
write.csv(careerdf, "careerdf.csv")

