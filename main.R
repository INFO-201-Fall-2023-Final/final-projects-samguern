library(dplyr)
library(stringr)
mentalHealth <- read.csv("mxmh_survey_results.csv")
mentalHealth <- na.omit(mentalHealth)
spotifyData <- read.csv("Spotify_data.csv")
spotifyData <- na.omit(spotifyData)

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
fav_music_genre_spotify <- left_join(filtered_Spotify, spotifyData, by = c("Fav.genre" = "fav_music_genre"))

write.csv(age_group_spotify_summary, "C:/repo/final-projects-samguern/age_group_spotify_summary.csv", row.names=FALSE)
