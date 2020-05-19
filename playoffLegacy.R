library(tidyverse)
library(ggthemes)
library(rvest)
library(ggpubr)

s2019 = read.csv("playoffstats/2019.csv")[,-1]
s2018 = read.csv("playoffstats/2018.csv")[,-1]
s2017 = read.csv("playoffstats/2017.csv")[,-1]
s2016 = read.csv("playoffstats/2016.csv")[,-1]
s2015 = read.csv("playoffstats/2015.csv")[,-1]
s2014 = read.csv("playoffstats/2014.csv")[,-1]
s2013 = read.csv("playoffstats/2013.csv")[,-1]
s2012 = read.csv("playoffstats/2012.csv")[,-1]
s2011 = read.csv("playoffstats/2011.csv")[,-1]
s2010 = read.csv("playoffstats/2010.csv")[,-1]
s2009 = read.csv("playoffstats/2009.csv")[,-1]
s2008 = read.csv("playoffstats/2008.csv")[,-1]
s2007 = read.csv("playoffstats/2007.csv")[,-1]
s2006 = read.csv("playoffstats/2006.csv")[,-1]
s2005 = read.csv("playoffstats/2005.csv")[,-1]
s2004 = read.csv("playoffstats/2004.csv")[,-1]
s2003 = read.csv("playoffstats/2003.csv")[,-1]
s2002 = read.csv("playoffstats/2002.csv")[,-1]
s2001 = read.csv("playoffstats/2001.csv")[,-1]
s2000 = read.csv("playoffstats/2000.csv")[,-1]
s1999 = read.csv("playoffstats/1999.csv")[,-1]
s1998 = read.csv("playoffstats/1998.csv")[,-1]
s1997 = read.csv("playoffstats/1997.csv")[,-1]
s1996 = read.csv("playoffstats/1996.csv")[,-1]
s1995 = read.csv("playoffstats/1995.csv")[,-1]
s1994 = read.csv("playoffstats/1994.csv")[,-1]
s1993 = read.csv("playoffstats/1993.csv")[,-1]
s1992 = read.csv("playoffstats/1992.csv")[,-1]
s1991 = read.csv("playoffstats/1991.csv")[,-1]
s1990 = read.csv("playoffstats/1990.csv")[,-1]
s1989 = read.csv("playoffstats/1989.csv")[,-1]
s1988 = read.csv("playoffstats/1988.csv")[,-1]
s1987 = read.csv("playoffstats/1987.csv")[,-1]
s1986 = read.csv("playoffstats/1986.csv")[,-1]
s1985 = read.csv("playoffstats/1985.csv")[,-1]
s1984 = read.csv("playoffstats/1984.csv")[,-1]
s1983 = read.csv("playoffstats/1983.csv")[,-1]
s1982 = read.csv("playoffstats/1982.csv")[,-1]
s1981 = read.csv("playoffstats/1981.csv")[,-1]
s1980 = read.csv("playoffstats/1980.csv")[,-1]
s1979 = read.csv("playoffstats/1979.csv")[,-1]
s1978 = read.csv("playoffstats/1978.csv")[,-1]
s1977 = read.csv("playoffstats/1977.csv")[,-1]
s1976 = read.csv("playoffstats/1976.csv")[,-1]
s1975 = read.csv("playoffstats/1975.csv")[,-1]
s1974 = read.csv("playoffstats/1974.csv")[,-1]
s1973 = read.csv("playoffstats/1973.csv")[,-1]
s1972 = read.csv("playoffstats/1972.csv")[,-1]
s1971 = read.csv("playoffstats/1971.csv")[,-1]
s1970 = read.csv("playoffstats/1970.csv")[,-1]
s1969 = read.csv("playoffstats/1969.csv")[,-1]
s1968 = read.csv("playoffstats/1968.csv")[,-1]
s1967 = read.csv("playoffstats/1967.csv")[,-1]
s1966 = read.csv("playoffstats/1966.csv")[,-1]
s1965 = read.csv("playoffstats/1965.csv")[,-1]
s1964 = read.csv("playoffstats/1964.csv")[,-1]
s1963 = read.csv("playoffstats/1963.csv")[,-1]
s1962 = read.csv("playoffstats/1962.csv")[,-1]
s1961 = read.csv("playoffstats/1961.csv")[,-1]
s1960 = read.csv("playoffstats/1960.csv")[,-1]
s1959 = read.csv("playoffstats/1959.csv")[,-1]
s1958 = read.csv("playoffstats/1958.csv")[,-1]
s1957 = read.csv("playoffstats/1957.csv")[,-1]
s1956 = read.csv("playoffstats/1956.csv")[,-1]
s1955 = read.csv("playoffstats/1955.csv")[,-1]
s1954 = read.csv("playoffstats/1954.csv")[,-1]
s1953 = read.csv("playoffstats/1953.csv")[,-1]
s1952 = read.csv("playoffstats/1952.csv")[,-1]
s1951 = read.csv("playoffstats/1951.csv")[,-1]
s1950 = read.csv("playoffstats/1950.csv")[,-1]

ivec = c()
newf <- function(dataframe){
  for (i in 5:ncol(dataframe)){
    if (min(dataframe[,i]) == 0 & max(dataframe[,i]) == 0){
      ivec = c(ivec, i)
    }
  }
  return(ivec)
}
names(s1973)[newf(s1973)];names(s1974)[newf(s1974)]

group1 = rbind.data.frame(s1950, s1951, s1952, s1953, s1954,
                          s1955, s1956, s1957, s1958, s1959,
                          s1960, s1961, s1962, s1963, s1964,
                          s1965, s1966, s1967, s1968, s1969,
                          s1970, s1971, s1972, s1973)
mn = newf(group1)
g1 = group1[,-c(mn)]
names = g1 %>% select(Player, Pos, Age, Tm, Yr, W)
dat1 = g1 %>% select(-Player, -Pos, -Age, -Tm, -Yr, -W)

pr.out = prcomp(dat1, scale = T)
pr.out$center
pr.out$scale
pr.out$rotation[,1:3]
g1$PC1 = pr.out$x[,1]
g1$PC1a = (g1$PC1+abs(min(g1$PC1)))*g1$W
g1$PC1n = g1$PC1a*80/(max(g1$PC1a))
g1p = g1 %>% arrange(desc(PC1n)) %>% 
       select(Player, Tm, Yr, W, PTS, TRB, AST, PC1a)


g2 = rbind.data.frame(s1974, s1975, s1976, s1977, s1978, s1979,
                          s1980, s1981, s1982, s1983, s1984,
                          s1985, s1986, s1987, s1988, s1989,
                          s1990, s1991, s1992, s1993, s1994,
                          s1995, s1996, s1997, s1998, s1999,
                          s2000, s2001, s2002, s2003, s2004,
                          s2005, s2006, s2007, s2008, s2009,
                          s2010, s2011, s2012, s2013, s2014,
                          s2015, s2016, s2017, s2018, s2019)
names = g2 %>% select(Player, Pos, Age, Tm, Yr, W)
dat1 = g2 %>% select(-Player, -Pos, -Age, -Tm, -Yr, -W)

pr.out = prcomp(dat1, scale = T)
pr.out$center
pr.out$scale
pr.out$rotation[,1:3]

g2$PC1 = -(pr.out$x[,1])
g2$PC1a = (g2$PC1+abs(min(g2$PC1)))*(g2$W)
g2$PC1n = g2$PC1a*100/(max(g2$PC1a))
g2p = g2 %>% arrange(desc(PC1n)) %>% 
       select(Player, Tm, Yr, W, PTS, TRB, AST, PC1a)

plLeg = rbind.data.frame(g1p, g2p) %>% arrange(desc(PC1a))
champw = plLeg %>% group_by(Yr) %>% 
  summarise(maxW = max(W))

PLL = full_join(plLeg, champw, by = "Yr")
PLL$PC1b = PLL$PC1a/PLL$maxW

PLL$PC1n = PLL$PC1b*1/(max(PLL$PC1b))
PLL = PLL %>% arrange(desc(PC1n))

PLL.leaderboard = PLL %>% group_by(Player) %>% summarise(
  totalPL = sum(PC1n)
  ) %>% arrange(desc(totalPL))
PLL.leaderboard$rk = 1:nrow(PLL.leaderboard)
PLL.leaderboard = PLL.leaderboard %>% select(rk, everything())
PLL.leaderboard %>% write.csv("results/playofflegacyleaderboard.csv")

PLdf = PLL.leaderboard %>% arrange((totalPL)) %>% tail(25)
PLdf$Player = factor(as.character(PLdf$Player), levels = PLdf$Player)
PLdf %>% ggplot(aes(x = Player, y = totalPL)) + geom_bar(stat = "identity", width = I(1/2)) + coord_flip() + theme_bw() + scale_y_continuous("Playoff Legacy Score")

PLL %>% filter(Tm == "GSW", Yr == "2017")
PLL %>% filter(Tm == "CHI", Yr == "1996")

##########################################################################################

players = c("Chris Paul", "Tony Parker")

PLL.leaderboard %>% filter(Player %in% players)
p = PLL %>% filter(Player %in% players) 
p %>% ggplot(aes(x = Yr, y = PC1n)) + geom_point(aes(col = Player)) +
  geom_line(data = subset(PLL, Player == players[1]), aes(col = Player)) + 
  geom_line(data = subset(PLL, Player == players[2]), aes(col = Player)) +
  geom_line(data = subset(PLL, Player == players[3]), aes(col = Player)) +
  scale_x_continuous("", breaks = seq(min(p$Yr), max(p$Yr), by = 2)) + 
  scale_y_continuous("Playoff Run Score", limits = c(0, 1)) + theme_clean();p

#PLL %>% select(-maxW, -PC1b, -PC1a) %>% filter(PC1n > 0.80)
PLL %>% group_by(Tm) %>% 
  summarise(
    PLt = sum(PC1n)
  ) %>% arrange(desc(PLt)) %>% head(20)

team = "UTA"
PLL %>% filter(Tm == team) %>% group_by(Player) %>% summarise(totPC = sum(PC1n)) %>% arrange(desc(totPC))
pl = PLL %>% filter(Tm == team) %>% select(Player) %>%  distinct() %>% head(3)
players = pl[1:3,1]
p = PLL %>% filter(Player %in% players) 
p %>% ggplot(aes(x = Yr, y = PC1n)) + geom_point(aes(col = Player)) +
  geom_line(data = subset(PLL, Player == players[1]), aes(col = Player)) + 
  geom_line(data = subset(PLL, Player == players[2]), aes(col = Player)) +
  geom_line(data = subset(PLL, Player == players[3]), aes(col = Player)) +
  scale_x_continuous("", breaks = seq(min(p$Yr), max(p$Yr), by = 4)) + 
  scale_y_continuous("Playoff Run Score", limits = c(0, 1)) + theme_clean();p

bteam = PLL %>% group_by(Tm, Yr) %>% summarise(
  totPL = sum(PC1n)
) %>% arrange(desc(totPL))

plotteam1 <- function(Yr, Tm){
  year = Yr
  team = Tm
  data = PLL %>% filter(Yr == year, Tm == team) 
  data = data %>% arrange((PC1n))
  data$Player = factor(as.character(data$Player), levels = data$Player)
  
  pl = data %>% ggplot(aes(x = Player, y = PC1n)) + 
    geom_bar(stat = "identity", width = I(1/4)) +
    coord_flip() + ggtitle(paste(Yr, Tm)) + scale_x_discrete("") +
    scale_y_continuous("", limits = c(0, 1)) + theme_bw()
  return(pl)
}
plotteam2 <- function(Yr, Tm){
  year = Yr
  team = Tm
  data = PLL %>% filter(Yr == year, Tm == team) 
  data = data %>% arrange((PC1n))
  data$Player = factor(as.character(data$Player), levels = data$Player)
  
  pl = data %>% ggplot(aes(x = Player, y = PC1n)) + 
    geom_bar(stat = "identity", width = I(1/4)) +
    coord_flip() + ggtitle(paste(Yr, Tm)) + scale_x_discrete("") +
    scale_y_continuous("Playoff Run Score", limits = c(0, 1)) + theme_bw()
  return(pl)
}

ggarrange(plotteam1(Yr = 2017, Tm = "GSW"), 
          plotteam2(Yr = 1996, Tm = "CHI"), 
          nrow = 2)

#PLL %>% filter(Yr == 2019)

  