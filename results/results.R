library(tidyverse)
pr = read.csv("results/primeleaderboard.csv")[,-1]
pr$Player = as.character(pr$Player)
for (i in 1:nrow(pr)){
  sp = strsplit(x = pr$Player[i], split = "")[[1]]
  if (sp[length(sp)] == "*"){
    pr$Player[i] = paste(sp[1:(length(sp)-1)], collapse = "")
  } else{
    pr$Player[i] = paste(sp[1:(length(sp))], collapse = "")
  }
  if (sp[1] == "M" & sp[2] == "a" & sp[3] == "n" & sp[4] == "u" & sp[5] == " "){
    pr$Player[i] = "Manu Ginobili"
  }
  if (sp[1] == "N" & sp[2] == "e" & sp[3] == "n"){
    pr$Player[i] = "Nene Hilario"
  }
  if (pr$Player[i] == "Hedo T<fc>rkoglu"){
    pr$Player[i] = "Hedo Türkoglu"
  }
  if (pr$Player[i] == "Dra<U+009E>en Petrovic"){
    pr$Player[i] = "Dražen Petrović"
  }
  if (pr$Player[i] == "Luka Doncic"){
    pr$Player[i] = "Luka Dončić"
  }
}
pl = read.csv("results/playofflegacyleaderboard.csv")[,-c(1, 2)]
pl$Player = as.character(pl$Player)
for (i in 1:nrow(pl)){
  sp = strsplit(x = pl$Player[i], split = "")[[1]]
  if (sp[length(sp)] == "*"){
    pl$Player[i] = paste(sp[1:(length(sp)-1)], collapse = "")
  } else{
    pl$Player[i] = paste(sp[1:(length(sp))], collapse = "")
  }
  if (sp[1] == "M" & sp[2] == "a" & sp[3] == "n" & sp[4] == "u" & sp[5] == " "){
    pl$Player[i] = "Manu Ginobili"
  }
  if (sp[1] == "N" & sp[2] == "e" & sp[3] == "n" & sp[4] == "ê"){
    pl$Player[i] = "Nene Hilario"
  }
}


write.csv(full_join(pr, pl, by = "Player"), "results/Leaderboard.csv")

