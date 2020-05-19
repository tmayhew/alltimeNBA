library(rvest)
library(tidyverse)
library(ggthemes)

year = 2019
link = paste0("https://www.basketball-reference.com/playoffs/NBA_", year, "_games.html")
page <- read_html(link)
data.raw <- html_table(page, fill=TRUE)[[1]]
data.raw = as.data.frame(data.raw)
dr = data.raw[,2:6]
names(dr) = c("Tm1", "PTS1", "Tm2", "PTS2")
dr$w = ifelse(dr$PTS1 > dr$PTS2, dr$Tm1, dr$Tm2)
wdf = as.data.frame(table(dr$w));
names(wdf) = c("Tm", "W");
wdf = wdf %>% arrange(desc(W))
write.csv(wdf, paste0("teamwins/", year, ".csv"))
head(wdf, 2)
