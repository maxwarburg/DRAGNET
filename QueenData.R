########################################### DRAGNET - QUEEN DATASETS
########################################### Max Otis Warburg
########################################### LIBRARIES

library(rvest)
library(dplyr)
library(plyr)
library(tidyr)

########################################### SEASONS

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

###################### QUEENS

count <- 1
queens <- data.frame()

for(i in seasons){
  bin <- i %>% html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
  bin <- bin[[1]]
  bin$Origin.Season = count
  queens <- rbind(queens,bin)
  count = count + 1
}

as.queens <- data.frame()
count <- 1
for(i in as){
  bin <- i %>% html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
  bin <- bin[[1]]
  bin$AS.Season = count
  as.queens <- rbind(as.queens,bin)
  count = count + 1
}

rm(all,as,count,i,seasons,bin)

########################################### TIDYING

queens$Origin.Season <- as.factor(queens$Origin.Season)
as.queens$AS.Season <- as.factor(as.queens$AS.Season)

queens <- separate(queens, Hometown, into=c("City","State.Country"),sep=",",remove=TRUE)
as.queens <- separate(as.queens, Hometown, into=c("City","State.Country"),sep=",",remove=TRUE)

as.queens <- select(as.queens, -`Original placement`,-`Original season`)

queens$Contestant <- gsub("\\[(.*?)\\]","",queens$Contestant)
queens$Name <- gsub("\\[(.*?)\\]","",queens$Name)
queens$Outcome <- gsub("\\[(.*?)\\]","",queens$Outcome)

as.queens$Contestant <- gsub("\\[(.*?)\\]","",as.queens$Contestant)
as.queens$Name <- gsub("\\[(.*?)\\]","",as.queens$Name)
as.queens$Outcome <- gsub("\\[(.*?)\\]","",as.queens$Outcome)

queens <- queens[c(7,1,2,3,4,5,6)]
as.queens <- as.queens[c(7,1,2,3,4,5,6)]

########################################### WRITING
write.csv(x = queens,file = "/Users/maxwarburg/Documents/R Content/Projects/DRAGNET/queens.csv",row.names = FALSE)
