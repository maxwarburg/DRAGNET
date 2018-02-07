########################################### DRAGNET - LIPSYNC DATASETS
########################################### Max Otis Warburg
########################################### LIBRARIES

library(rvest)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)

########################################### DATA CAPTURE

seasons <- c("https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_1)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_2)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_3)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_4)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_5)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_6)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_7)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_8)",
             "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_9)")

as <- c("https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_All_Stars_(season_1)",
        "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_All_Stars_(season_2)")

all <- append(seasons,as)

count <- 1
ls <- data.frame()

for(i in seasons){
  bin <- i %>% read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[4]') %>% html_table(fill=T)
  bin <- bin[[1]]
  bin$Origin.Season = count
  ls <- rbind(ls,bin)
  count = count + 1
}

count <- 1
as.ls <- data.frame()

for(i in as){
  bin <- i %>% read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[4]') %>% html_table(fill=T)
  bin <- bin[[1]]
  bin$AS.Season = count
  as.ls <- rbind.fill(as.ls,bin)
  count = count + 1
}

rm(all,as,count,i,seasons,bin)

########################################### TIDYING

colnames(ls)[2] <- "Contestant1"
colnames(ls)[3] <- "vs"
colnames(ls)[4] <- "Contestant2"

ls <- select(ls, -vs)

ls$Song <- gsub('\\"',"",x=ls$Song)

ls$Artist <- str_extract(ls$Song,"\\(([^\\)]+)\\)$")

ls$Song <- gsub("\\(([^\\)]+)\\)$","",x=ls$Song)

ls$Artist <- gsub("(\\(|\\))","",x=ls$Artist)

ls$Champion <- ifelse(ls$Eliminated == ls$Contestant1, 
                      ls$Champion <- ls$Contestant2,
                      ls$Champion <- ls$Contestant1) 

colnames(ls)

ls <- ls[c(6,1,2,3,5,8,4,7)]

########################################### WRITING
write.csv(x = ls,file = "/Users/maxwarburg/Documents/R Content/Projects/DRAGNET/ls.csv",row.names = FALSE)
