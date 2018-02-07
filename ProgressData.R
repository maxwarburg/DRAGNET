########################################### DRAGNET - PROGRESS DATASETS
########################################### Max Otis Warburg
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

progress <- data.frame()
count <- 1

# for every season
for(season in seasons){
  
  bin <- season %>% 
    read_html() %>%  
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% 
    html_table(fill=T)
  bin <- bin[[1]]
 
  for (row in 1:nrow(bin)){
    
    Contestant <- bin[row,1]
    Season <- count
    Episodes <- sum(is.na(bin[row,]) == FALSE) 
    
    Safe <- 0
    Win <- 0
    BTM2 <- 0
    Elim <- 0
    EpElim <- 0
    
    Winner <- 0
    Runnerup <- 0
    MissC <- 0
    Disq <- 0
    Out <- 0
    
    for (column in 1:ncol(bin)){
      
      ifelse(bin[row,column]  == "SAFE" | bin[row,column]  == "LOW" | bin[row,column]  == "HIGH",Safe <- Safe + 1,
      ifelse(bin[row,column]  == "WIN",Win <- Win + 1,
      ifelse(bin[row,column]  == "BTM2",BTM2 <- BTM2 + 1,
      ifelse(bin[row,column]  == "ELIM" | bin[row,column]  == "Eliminated",{Elim <- Elim + 1;EpElim <- column - 1},
      ifelse(bin[row,column]  == "Winner",Winner <- Winner + 1,
      ifelse(bin[row,column]  == "Runner-up" | bin[row,column]  == "Runner-Up",Runnerup <- Runnerup + 1,
      ifelse(bin[row,column]  == "Miss C",MissC <- MissC + 1,
      ifelse(bin[row,column]  == "DISQ", Disq <- Disq + 1,
      ifelse(bin[row,column]  == "OUT",Out <- Out + 1,print("ERROR"))))))))))
      
      # bin[row,column] <- na.omit(bin[row,column])
      # if (bin[row,column]  == "ELIM" | bin[row,column]  == "Eliminated"){EpElim <- column -1}

    }

    bin2 <- data.frame(Contestant,Season,Episodes,Safe,Win,BTM2,Elim,EpElim,Winner,Runnerup,MissC,Disq,Out)
    progress <- rbind(progress,bin2)
      
  }
  
  count <- count + 1
  
}

########################################### THE METRIC

progress$Performance <- (progress$Safe*5 + 
                         progress$Win*20 +
                         progress$BTM2*0 +
                         progress$Elim*-5 + 
                         ifelse(progress$EpElim == 0,progress$EpElim + 100,progress$EpElim*5) +
                         progress$Runnerup*60 +
                         progress$Winner*100 +
                         progress$MissC*30) 

progress <- progress[c(1,14,2,3,4,5,6,7,8,9,10,11,12,13)]

top <- progress[order(progress$Performance),]

rm(all,as,count,i,seasons,bin,bin2,Contestant,Season,Episodes,
   Safe,Win,BTM2,Elim,EpElim,Winner,Runnerup,MissC,Disq,Out,column,row,season)

########################################### WRITING
write.csv(x = progress,file = "/Users/maxwarburg/Documents/R Content/Projects/DRAGNET/progress.csv",row.names = FALSE)
