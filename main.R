library(dplyr)
library(stringr)
mentalHealth <- read.csv("mxmh_survey_results.csv")
mentalHealth <- na.omit(mentalHealth)
spotifyData <- read.csv("Spotify_data.csv")
spotifyData <- na.omit(spotifyData)

filtered_spofityData <- filter(spotifyData, str_detect(Age, "6-12") == FALSE)
filtered_Spotify <- filter(mentalHealth, str_detect(Primary.streaming.service, "Spotify"),)

age_group <- function(age){
  if(is.na(age) || age < 6){
    return(NULL)
  } 
  
  if(age > 60){
    return("60+")
  } else if (age >= 35){
    return("35-60")
  } else if (age >= 20){
    return("20-35")
  } else if (age >= 12){
    return("12-20")
  } else if (age >= 6){
    return("6-12")
  } 
}

filtered_Spotify$Age.group <-mapply(age_group, filtered_Spotify$Age)

summarized_filtered_Spotify <- aggregate(cbind(Anxiety, Depression, OCD, Hours.per.day) ~ Age.group, data = filtered_Spotify, mean)

getMode <- function(v){
  uniquevar <- unique(v)
  uniquevar[which.max(tabulate(match(v, uniquevar)))]
}

summarized_spotifyData <- aggregate(cbind(music_time_slot, spotify_listening_device) ~ Age, data = spotifyData, getMode)


age_group_spotify_summary <- left_join(summarized_filtered_Spotify, summarized_spotifyData, by = c("Age.group" = "Age"))

age_group_spotify_summary$Num_Responses <- 0

age_counts_mentalh <- table(filtered_Spotify$Age.group) 
age_counts_spotify <- table(filtered_spofityData$Age)

for(i in 1:length(age_counts_mentalh)){
  age_group_spotify_summary[i,"Num_Responses"] <- as.numeric(age_counts_mentalh[i]) + as.numeric(age_counts_spotify[i])
}


write.csv(age_group_spotify_summary, "C:/repo/final-projects-samguern/age_group_spotify_summary.csv", row.names=FALSE)
