library(rvest)
library(tidyverse)
library(ggthemes)

year = 2019
link1 = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_poss.html")

page1 <- read_html(link1)
dr <- as.data.frame(html_table(page1, fill=TRUE)[[1]])[,-1]
keep.cols <- names(dr) %in% c("")
dr <- dr[! keep.cols] 
for (i in 1:nrow(dr)){
  for (j in 5:ncol(dr)){
    if (dr[i, j] == ""){
      dr[i, j] = 0
    }
    dr[i, j] = as.numeric(dr[i, j])
  }
}
dr = na.omit(dr)




