########################################### DRAGNET
########################################### Max Otis Warburg
############################################################ 
########################################### LIBRARIES

library(rvest)
library(dplyr)
library(plyr)
library(tidyr)

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

###################### QUEENS
count1 <- 1
count2 <- 2
queens <- data.frame()
for(i in seasons){
  bin <- i %>% html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
  bin <- bin[[1]]
  bin$Placement <- seq(from=1,to=nrow(bin),by=1)
  bin$Origin.Season = count1
  queens <- rbind(queens,bin)
  count1 = count1 + 1
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

###################### PROGRESS
progress <- data.frame()
for(i in all){
  bin <- i %>% read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=T)
  bin <- bin[[1]]
  bin$Season
  progress <- bind_rows(progress,bin)
  count = count + 1
}

###################### LIPSYNCS
ls <- data.frame()
for(i in seasons){
  bin <- i %>% read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[4]') %>% html_table(fill=T)
  bin <- bin[[1]]
  ls <- rbind(ls,bin)
}

as.ls <- data.frame()
for(i in as){
  bin <- i %>% read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[4]') %>% html_table(fill=T)
  bin <- bin[[1]]
  as1_ls
  as2_ls
  
}
########################################### TIDYING

queens$Origin.Season <- as.factor(queens$Origin.Season)

queens <- ddply(queens,
               c("Contestant","Name","Outcome"),
               transform,
               Contestant=gsub("\\[(.*?)\\]","",Contestant),
               Name=gsub("\\[(.*?)\\]","",Name),
               Outcome=gsub("\\[(.*?)\\]","",Outcome))

agg <- group_by(queens,Origin.Season)

agg <- aggregate(queens, by=list(Origin.Season), FUN=count)

queens <- ddply(queens, 
                c("Outcome"), 
                summarise,
                Cohort.Size = length(),
                

queens <- separate(queens, Hometown, into=c("City","State.Country"),sep=",",remove=TRUE)

queens$Placement <- switch(queens$Outcome,
                           )

queens$Placement.Percentile <- outcome / count

queens$Outcome <- ifelse(grep(queens$Outcome,"/([1-9][0-9]*)/"),)

########################################### TODO

# Data sets are queen data, progress data, lipsyncs data

#groupby season and assign numerical placements 

#summarize placement data and attatch to queens

