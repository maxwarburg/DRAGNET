########################################### DRAGNET - PROGRESS DATASETS
########################################### Max Otis Warburg
########################################### LIBRARIES

library(rvest)
library(dplyr)
library(plyr)
library(tidyr)

########################################### DATA CAPTURE

seasons <- c("http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_1)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_2)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_3)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_4)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_5)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_6)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_7)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_8)",
             "http://rupaulsdragrace.wikia.com/wiki/RuPaul%27s_Drag_Race_(Season_9)")

###################### QUEENS

progress <- data.frame()
count <- 1

# for every season
for(season in seasons){
  
  bin <- season %>% 
    read_html() %>%  
    html_nodes(xpath='//*[@id="mw-content-text"]/table[3]') %>% 
    html_table(fill=T)
  bin <- bin[[1]]
  
  for (row in 1:nrow(bin)){

    Contestant <- bin[row,"Contestant"]
    Season <- count
    Episodes <- sum(is.na(bin[row,]) == FALSE) - 6

    Win <- 0
    High <- 0 
    Safe <- 0
    Low <- 0
    Btm2 <- 0
    Elim <- 0
    
    Winner <- 0
    Top <- 0
    RunUp <- 0
    MsCon <- 0
    
    Disq <- 0
    Dept <- 0
    Out <- 0
    
    for (column in 1:ncol(bin)){
      
      ifelse(grepl("WINNER", bin[row,column]), Winner <- Winner + 1,
      ifelse(grepl("DISQ",bin[row,column]), Disq <- Disq + 1,
      ifelse(grepl("DEPT",bin[row,column]), Dept <- Dept + 1,
      ifelse(grepl("OUT",bin[row,column]), Out <- Out + 1,
      ifelse(grepl("RUNUP", bin[row,column]), RunUp <- RunUp + 1,
      ifelse(grepl("MS.CON", bin[row,column]), MsCon <- MsCon + 1,
      ifelse(grepl("TOP", bin[row,column]), Top <- Top + 1,
                    
                                         
      ifelse(grepl("WIN",bin[row,column]), Win <- Win + 1,
      ifelse(grepl("HIGH",bin[row,column]), High <- High + 1,
      ifelse(grepl("SAFE", bin[row,column]), Safe <- Safe + 1,
      ifelse(grepl("LOW" , bin[row,column]) , Low <- Low + 1,
      ifelse(grepl("BTM2" , bin[row,column]), Btm2 <- Btm2 + 1,
      ifelse(grepl("ELIM", bin[row,column]), Elim <- Elim + 1,
      print('er'))))))))))))))

    }
    
    bin2 <- data.frame(Contestant,Season,Episodes,
                       Win,High,Safe,Low,Btm2,Elim,
                       Winner,Top,RunUp,MsCon,
                       Disq,Dept,Out)
    
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

