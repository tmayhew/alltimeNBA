library(rvest)
library(tidyverse)
library(ggthemes)
library(Rfast)

careerdf = read.csv("careerdf.csv")[,-1]
careerdf = careerdf %>% select(Player, Yr, everything())

for (i in 1:nrow(careerdf)){
  careerdf$gp[i] = sample(1:5, size = 1, replace = T, prob = c(0.20, 0.20, 0.20, 0.20, 0.20))
}
table(careerdf$gp, careerdf$tier)

head(careerdf)

dat.numeric = careerdf[,c(6:50, 54)];head(dat.numeric)
draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"firstteam"]))
colnames(draft.cor1) <- "cor"
draft.cor1$var <- rownames(draft.cor1)
mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 1, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with First-Team All-NBA Selections"); pl1

scaleselected = scale(careerdf[,c(6:50)])
gps = careerdf %>% select(Player, Yr, gp, allstar, firstteam, secthird)
selected = cbind.data.frame(gps, scaleselected)

train = selected %>% filter(gp != 1)
test = selected %>% filter(gp == 1)
glm.model = glm(firstteam ~ . - Player - Yr - gp -  allstar - WS - FTr - STL. - X3PAr - BPM - X2P - X2PA - X2P. - GS - DRB. - X3P. - STL - PTS - gp, data = selected)
summary(glm.model)


dat.numeric = careerdf[,c(6:50, 56)]
draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"allstar"]))
colnames(draft.cor1) <- "cor"
draft.cor1$var <- rownames(draft.cor1)
mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 1, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with All Star Appearences"); pl1

dat.numeric = careerdf[,c(6:50, 55)]
draft.cor1 <- data.frame(abs(cor(dat.numeric)[,"secthird"]))
colnames(draft.cor1) <- "cor"
draft.cor1$var <- rownames(draft.cor1)
mm.cor1 <- select(arrange(draft.cor1, desc(cor)), var, cor)
mm.cor1 <- mm.cor1[2:nrow(mm.cor1),]
pl1 = ggplot(data=mm.cor1, aes(x=reorder(var, cor), y=cor)) + geom_bar(stat = "identity", width = I(1/8), position = position_dodge(width=0.2)) + coord_flip() + scale_x_discrete("Variable") + theme_clean() + scale_y_continuous("Absolute Value of Correlation", breaks = seq(0, 1, by = 0.10)) + ggtitle("Variable Correlation", subtitle = "Correlation with Second or Third-Team All-NBA Selections"); pl1

