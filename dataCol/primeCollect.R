library(rvest)
library(tidyverse)
library(ggthemes)
library(brnn)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Totals:
#for (i in c(1950:2019)){#year = i#link1 = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")#page1 <- read_html(link1)#dr <- as.data.frame(html_table(page1, fill=TRUE)[[1]])[,-1]#keep.cols <- names(dr) %in% c("")#dr <- dr[! keep.cols]#for (i in 1:nrow(dr)){#  for (j in 5:ncol(dr)){#    if (dr[i, j] == ""){#      dr[i, j] = 0#    }#    dr[i, j] = as.numeric(dr[i, j])#  }#}#dr = na.omit(dr)#dr$Yr = year#dr = dr %>% distinct(Player, .keep_all = T)#}
# Advanced:
#for (i in c(2011:2019)){#year = i#link1 = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html")#page1 <- read_html(link1)#dr <- as.data.frame(html_table(page1, fill=TRUE)[[1]])[,-1]#keep.cols <- names(dr) %in% c(".1", "")#dr <- dr[! keep.cols]#for (row in 1:nrow(dr)){#  for (col in 5:ncol(dr)){#    if (dr[row, col] == ""){#      dr[row, col] = 0#    }#    dr[row, col] = as.numeric(dr[row, col])#  }#}#dr = dr %>% filter(Player != "Player")#head(dr)#dr = na.omit(dr)#dr$Yr = year#dr = dr %>% distinct(Player, .keep_all = T)#}
# Merge Total/Adv Stats
#for (i in c(1950:2019)){#year = i#a = read_csv(paste0("totals/", year, ".csv"))[,-1]#b = read_csv(paste0("advanced/", year, ".csv"))[,-1]; b = b %>% select(-Pos, -Age, -Tm, -G, -MP, -Yr)#s = full_join(a, b, by = "Player")#}

s2019 = read.csv("regseason/2019.csv")[,-1];s2018 = read.csv("regseason/2018.csv")[,-1];s2017 = read.csv("regseason/2017.csv")[,-1];s2016 = read.csv("regseason/2016.csv")[,-1];s2015 = read.csv("regseason/2015.csv")[,-1];
s2014 = read.csv("regseason/2014.csv")[,-1];s2013 = read.csv("regseason/2013.csv")[,-1];s2012 = read.csv("regseason/2012.csv")[,-1];s2011 = read.csv("regseason/2011.csv")[,-1];s2010 = read.csv("regseason/2010.csv")[,-1];
s2009 = read.csv("regseason/2009.csv")[,-1];s2008 = read.csv("regseason/2008.csv")[,-1];s2007 = read.csv("regseason/2007.csv")[,-1];s2006 = read.csv("regseason/2006.csv")[,-1];s2005 = read.csv("regseason/2005.csv")[,-1];
s2004 = read.csv("regseason/2004.csv")[,-1];s2003 = read.csv("regseason/2003.csv")[,-1];s2002 = read.csv("regseason/2002.csv")[,-1];s2001 = read.csv("regseason/2001.csv")[,-1];s2000 = read.csv("regseason/2000.csv")[,-1];
s1999 = read.csv("regseason/1999.csv")[,-1];s1998 = read.csv("regseason/1998.csv")[,-1];s1997 = read.csv("regseason/1997.csv")[,-1];s1996 = read.csv("regseason/1996.csv")[,-1];s1995 = read.csv("regseason/1995.csv")[,-1];
s1994 = read.csv("regseason/1994.csv")[,-1];s1993 = read.csv("regseason/1993.csv")[,-1];s1992 = read.csv("regseason/1992.csv")[,-1];s1991 = read.csv("regseason/1991.csv")[,-1];s1990 = read.csv("regseason/1990.csv")[,-1];
s1989 = read.csv("regseason/1989.csv")[,-1];s1988 = read.csv("regseason/1988.csv")[,-1];s1987 = read.csv("regseason/1987.csv")[,-1];s1986 = read.csv("regseason/1986.csv")[,-1];s1985 = read.csv("regseason/1985.csv")[,-1];
s1984 = read.csv("regseason/1984.csv")[,-1];s1983 = read.csv("regseason/1983.csv")[,-1];s1982 = read.csv("regseason/1982.csv")[,-1];s1981 = read.csv("regseason/1981.csv")[,-1];s1980 = read.csv("regseason/1980.csv")[,-1];
s1979 = read.csv("regseason/1979.csv")[,-1];s1978 = read.csv("regseason/1978.csv")[,-1];s1977 = read.csv("regseason/1977.csv")[,-1];s1976 = read.csv("regseason/1976.csv")[,-1];s1975 = read.csv("regseason/1975.csv")[,-1];
s1974 = read.csv("regseason/1974.csv")[,-1];s1973 = read.csv("regseason/1973.csv")[,-1];s1972 = read.csv("regseason/1972.csv")[,-1];s1971 = read.csv("regseason/1971.csv")[,-1];s1970 = read.csv("regseason/1970.csv")[,-1];
s1969 = read.csv("regseason/1969.csv")[,-1];s1968 = read.csv("regseason/1968.csv")[,-1];s1967 = read.csv("regseason/1967.csv")[,-1];s1966 = read.csv("regseason/1966.csv")[,-1];s1965 = read.csv("regseason/1965.csv")[,-1];
s1964 = read.csv("regseason/1964.csv")[,-1];s1963 = read.csv("regseason/1963.csv")[,-1];s1962 = read.csv("regseason/1962.csv")[,-1];s1961 = read.csv("regseason/1961.csv")[,-1];s1960 = read.csv("regseason/1960.csv")[,-1];
s1959 = read.csv("regseason/1959.csv")[,-1];s1958 = read.csv("regseason/1958.csv")[,-1];s1957 = read.csv("regseason/1957.csv")[,-1];s1956 = read.csv("regseason/1956.csv")[,-1];s1955 = read.csv("regseason/1955.csv")[,-1];
s1954 = read.csv("regseason/1954.csv")[,-1];s1953 = read.csv("regseason/1953.csv")[,-1];s1952 = read.csv("regseason/1952.csv")[,-1];s1951 = read.csv("regseason/1951.csv")[,-1];s1950 = read.csv("regseason/1950.csv")[,-1];

ivec = c()
newf <- function(dataframe){
  for (i in 5:ncol(dataframe)){
    if (min(dataframe[,i]) == 0 & max(dataframe[,i]) == 0){
      ivec = c(ivec, i)
    }
  }
  return(ivec)
}

paste("Added 1951 season:", names(s1950)[newf(s1950)][which(names(s1950)[newf(s1950)] %!in% names(s1951)[newf(s1951)])])
paste("Added 1952 season:", names(s1951)[newf(s1951)][which(names(s1951)[newf(s1951)] %!in% names(s1952)[newf(s1952)])])
paste("Added 1953 season:", names(s1952)[newf(s1952)][which(names(s1952)[newf(s1952)] %!in% names(s1953)[newf(s1953)])])
paste("Added 1954 season:", names(s1953)[newf(s1953)][which(names(s1953)[newf(s1953)] %!in% names(s1954)[newf(s1954)])])
paste("Added 1955 season:", names(s1954)[newf(s1954)][which(names(s1954)[newf(s1954)] %!in% names(s1955)[newf(s1955)])])

data = rbind.data.frame(s1959, s1958, s1957, s1956, s1955, 
                        s1954, s1953, s1952, s1951, s1950)

train = data %>% filter(Yr >= 1952)
test = data %>% filter(Yr < 1952)

model1 <- brnn(MP ~ (Age + G + FG + FGA + FG. + X2P + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + TRB + AST + PF + PTS + TS. + FTr + OWS + DWS + WS), neurons = 5, data = train)
read.csv("predMP.csv")[,-1] %>% ggplot(aes(x = MP, y = pred.MP)) + geom_point(alpha = I(1/2)) + geom_abline(slope = 1, intercept = 0) + ggtitle("1952-1959 NBA Seasons", subtitle = "Actual MP. vs. Predicted MP.") + theme_bw()
pred.MP = predict(model1, newdata = test)
pred.MP = ifelse(pred.MP < 0, 0, pred.MP)
test$MP = round(pred.MP)
test$WS.48 = ifelse(test$MP != 0, (test$WS)*48/(test$MP), 0)
model2 <- brnn(PER ~ (Age + G + FG + FGA + FG. + X2P + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + TRB + AST + PF + PTS + TS. + FTr + OWS + DWS + WS + MP + WS.48), neurons = 5, data = train)
read.csv("predPER.csv")[,-1] %>% ggplot(aes(x = PER, y = pred.PER)) + geom_point(alpha = I(1/2)) + geom_abline(slope = 1, intercept = 0) + ggtitle("1952-1959 NBA Seasons", subtitle = "Actual PER vs. Predicted PER") + theme_bw()
pred.PER = predict(model2, newdata = test)
test$PER = pred.PER

s1950 = test %>% filter(Yr == 1950)
s1951 = test %>% filter(Yr == 1951)

paste("1951 season:", names(s1950)[newf(s1950)][which(names(s1950)[newf(s1950)] %!in% names(s1951)[newf(s1951)])])
paste("1964 season:", names(s1950)[newf(s1950)][which(names(s1950)[newf(s1950)] %!in% names(s1964)[newf(s1964)])])

paste("1965 season:", names(s1964)[newf(s1964)][which(names(s1964)[newf(s1964)] %!in% names(s1965)[newf(s1965)])])
paste("1970 season:", names(s1965)[newf(s1965)][which(names(s1965)[newf(s1965)] %!in% names(s1970)[newf(s1970)])])

data = rbind.data.frame(s1950, s1951, s1952, s1953, s1954, 
                        s1955, s1956, s1957, s1958, s1959,
                        s1960, s1961, s1962, s1963, s1964, 
                        s1965, s1966, s1967, s1968, s1969,
                        s1970, s1971, s1972, s1973, s1974)
train = data %>% filter(Yr >= 1965)
test = data %>% filter(Yr < 1965)
model3 <- brnn(AST. ~ G + MP + FG + FGA + FG. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + TRB + AST + PF + PTS + PER + TS. + FTr + OWS + DWS + WS + WS.48, neurons = 5, data = train)
read.csv("predASTp.csv")[,-1] %>% ggplot(aes(x = AST., y = predAST.)) + geom_point(alpha = I(1/2)) + geom_abline(slope = 1, intercept = 0) + ggtitle("1965-1974 NBA Seasons", subtitle = "Actual AST. vs. Predicted AST.") + theme_bw()
test$AST. = predict(model3, newdata = test)
test$AST. = ifelse(test$AST. < 0, 0, test$AST.)
summary(train$AST.);summary(test$AST.)

s1950 = test %>% filter(Yr == 1950)
s1951 = test %>% filter(Yr == 1951)
s1952 = test %>% filter(Yr == 1952)
s1953 = test %>% filter(Yr == 1953)
s1954 = test %>% filter(Yr == 1954)
s1955 = test %>% filter(Yr == 1955)
s1956 = test %>% filter(Yr == 1956)
s1957 = test %>% filter(Yr == 1957)
s1958 = test %>% filter(Yr == 1958)
s1959 = test %>% filter(Yr == 1959)
s1960 = test %>% filter(Yr == 1960)
s1961 = test %>% filter(Yr == 1961)
s1962 = test %>% filter(Yr == 1962)
s1963 = test %>% filter(Yr == 1963)
s1964 = test %>% filter(Yr == 1964)


paste("1971 season:", names(s1970)[newf(s1970)][which(names(s1970)[newf(s1970)] %!in% names(s1971)[newf(s1971)])])
paste("1973 season:", names(s1971)[newf(s1971)][which(names(s1971)[newf(s1971)] %!in% names(s1973)[newf(s1973)])])

data = rbind.data.frame(s1950, s1951, s1952, s1953, s1954, 
                        s1955, s1956, s1957, s1958, s1959,
                        s1960, s1961, s1962, s1963, s1964, 
                        s1965, s1966, s1967, s1968, s1969,
                        s1970, s1971, s1972, s1973, s1974,
                        s1975, s1976, s1977, s1978, s1979)

train = data %>% filter(Yr >= 1971)
test = data %>% filter(Yr < 1971)
model4 <- brnn(TRB. ~ G + MP + FG + FGA + FG. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + TRB + AST + PF + PTS + PER + TS. + FTr + OWS + DWS + WS + WS.48 + AST., neurons = 5, data = train)
read.csv("predTRBp.csv")[,-1] %>% ggplot(aes(x = TRB., y = predict.model4.)) + geom_point(alpha = I(1/2)) + geom_abline(slope = 1, intercept = 0) + ggtitle("1965-1974 NBA Seasons", subtitle = "Actual TRB. vs. Predicted TRB.") + theme_bw()
test$TRB. = predict(model4, newdata = test)
test$TRB. = ifelse(test$TRB. < 0, 0, test$TRB.)
summary(train$TRB.);summary(test$TRB.)

s1950 = test %>% filter(Yr == 1950)
s1951 = test %>% filter(Yr == 1951)
s1952 = test %>% filter(Yr == 1952)
s1953 = test %>% filter(Yr == 1953)
s1954 = test %>% filter(Yr == 1954)
s1955 = test %>% filter(Yr == 1955)
s1956 = test %>% filter(Yr == 1956)
s1957 = test %>% filter(Yr == 1957)
s1958 = test %>% filter(Yr == 1958)
s1959 = test %>% filter(Yr == 1959)
s1960 = test %>% filter(Yr == 1960)
s1961 = test %>% filter(Yr == 1961)
s1962 = test %>% filter(Yr == 1962)
s1963 = test %>% filter(Yr == 1963)
s1964 = test %>% filter(Yr == 1964)
s1965 = test %>% filter(Yr == 1965)
s1966 = test %>% filter(Yr == 1966)
s1967 = test %>% filter(Yr == 1967)
s1968 = test %>% filter(Yr == 1968)
s1969 = test %>% filter(Yr == 1969)
s1970 = test %>% filter(Yr == 1970)
s1971 = train %>% filter(Yr == 1971)
s1972 = train %>% filter(Yr == 1972)
s1973 = train %>% filter(Yr == 1973)
group1 = rbind.data.frame(s1950, s1951, s1952, s1953, s1954, 
                          s1955, s1956, s1957, s1958, s1959,
                          s1960, s1961, s1962, s1963, s1964, 
                          s1965, s1966, s1967, s1968, s1969,
                          s1970, s1971, s1972, s1973)
write.csv(group1, "finalRegSeason/group1.csv")

# BIG GAP #########################################################################################################

paste("1974 season:", names(s1973)[newf(s1973)][which(names(s1973)[newf(s1973)] %!in% names(s1974)[newf(s1974)])])
paste("1977 season:", names(s1974)[newf(s1974)][which(names(s1974)[newf(s1974)] %!in% names(s1977)[newf(s1977)])])

paste("1978 season:", names(s1977)[newf(s1977)][which(names(s1977)[newf(s1977)] %!in% names(s1978)[newf(s1978)])])
paste("1979 season:", names(s1978)[newf(s1978)][which(names(s1978)[newf(s1978)] %!in% names(s1979)[newf(s1979)])])

data = rbind.data.frame(s1974, s1975, 
                        s1976, s1977, s1978, s1979, s1980,
                        s1981, s1982, s1983, s1984, s1985,
                        s1986, s1987, s1988)
train = data %>% filter(Yr >= 1978)
test = data %>% filter(Yr < 1978)
model = brnn(TOV ~ G + MP + FG + FGA + FG. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + 
               BLK + PF + PTS + PER + TS. + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + OWS + DWS + WS + WS.48 +
               OBPM + DBPM + BPM + VORP, neurons = 5, data = train)
read.csv("predTOV.csv")[,-1] %>% ggplot(aes(x = train.TOV, y = predict.model.)) + geom_point(alpha = I(1/2)) + geom_abline(slope = 1, intercept = 0) + ggtitle("1978-1988 NBA Seasons", subtitle = "Actual TOV vs. Predicted TOV") + theme_bw()
test$TOV = predict(model, newdata = test)
test$TOV = ifelse(test$TOV < 0, 0, round(test$TOV))

model2 = brnn(TOV. ~ G + MP + FG + FGA + FG. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + 
               BLK + PF + PTS + PER + TS. + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + OWS + DWS + WS + WS.48 +
               OBPM + DBPM + BPM + VORP + TOV, neurons = 5, data = train)
test$TOV. = predict(model2, newdata = test)
test$TOV. = ifelse(test$TOV. < 0, 0, test$TOV.)

model3 = brnn(USG. ~ G + MP + FG + FGA + FG. + X2P + X2PA + X2P. + eFG. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + 
                BLK + PF + PTS + PER + TS. + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + OWS + DWS + WS + WS.48 +
                OBPM + DBPM + BPM + VORP + TOV + TOV., neurons = 5, data = train)
test$USG. = predict(model3, newdata = test)
test$USG. = ifelse(test$USG. < 0, 0, test$USG.)

s1974 = test %>% filter(Yr == 1974)
s1975 = test %>% filter(Yr == 1975)
s1976 = test %>% filter(Yr == 1976)
s1977 = test %>% filter(Yr == 1977)

group2 = rbind.data.frame(s1974, s1975, s1976, s1977, s1978, s1979)
write.csv(group2, "finalRegSeason/group2.csv")

###################################################################################################################

paste("1980 season:", names(s1979)[newf(s1979)][which(names(s1979)[newf(s1979)] %!in% names(s1980)[newf(s1980)])])
paste("2019 season:", names(s1980)[newf(s1980)][which(names(s1980)[newf(s1980)] %!in% names(s2019)[newf(s2019)])])

group3 = rbind.data.frame(s1980, s1981, s1982, s1983, s1984,
                          s1985, s1986, s1987, s1988, s1989,
                          s1990, s1991, s1992, s1993, s1994,
                          s1995, s1996, s1997, s1998, s1999,
                          s2000, s2001, s2002, s2003, s2004,
                          s2005, s2006, s2007, s2008, s2009,
                          s2010, s2011, s2012, s2013, s2014,
                          s2015, s2016, s2017, s2018, s2019)
write.csv(group3, "finalRegSeason/group3.csv")
