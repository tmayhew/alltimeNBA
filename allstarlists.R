library(rvest)
library(tidyverse)

as.list = read.csv("as.list.csv")[,-1] # pulled from wikipedia
names(as.list) = c("Player", "NumSel", "Yrs")
as.list$Yrs = as.character(as.list$Yrs)
as.list %>% head()

df = cbind.data.frame(Player = "Player", Yr = 2020)
for (i in 1:nrow(as.list)){
  playername = as.list$Player[i]
  sp = strsplit(as.list$Yrs[i], split = "; ")[[1]]
  list = NULL
  for (j in 1:length(sp)){
    yrsp = strsplit(sp[j], split = "–")[[1]]
    if (length(yrsp) == 1){
      sq = yrsp[1]
    } else{
      sq = seq(yrsp[1], yrsp[2], by = 1)
    }
    list = c(list, sq)
  }
  bind = cbind.data.frame(Player = rep(playername, length(list)), Yr = list)
  df = rbind.data.frame(df, bind)
}

df = df[-1,]
rownames(df) = NULL
df$allstar = 1
allstardf = df

