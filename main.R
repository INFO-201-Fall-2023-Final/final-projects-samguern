library(dplyr)
library(stringr)
mentalHealth <- read.csv("mxmh_survey_results.csv")
mentalHealth <- na.omit(mentalHealth)
spotifyData <- read.csv("Spotify_data.csv")
spotifyData <- na.omit(spotifyData)
#test
unique(spotifyData$Age)

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

mean_anxiety_by_age_group <- aggregate(Anxiety ~ Age.group, data = filtered_Spotify, mean)
mean_depression_by_age_group <- aggregate(Depression ~ Age.group, data = filtered_Spotify, mean)
mean_ocd_by_age_group <- aggregate(OCD ~ Age.group, data = filtered_Spotify, mean)
mean_hours_by_age_group <- aggregate(Hours.per.day ~ Age.group, data = filtered_Spotify, mean)

getMode <- function(v){
  uniquevar <- unique(v)
  uniquevar[which.max(tabulate(match(v, uniquevar)))]
}

most_common_time_slot_by_age_group <- aggregate(music_time_slot ~ Age, data = spotifyData, getMode)

age_group_spotify_summary <- data.frame(Age.group = unique(filtered_Spotify$Age.group))

age_group_spotify_summary <- left_join(age_group_spotify_summary ,mean_anxiety_by_age_group)
age_group_spotify_summary <- left_join(age_group_spotify_summary ,mean_depression_by_age_group)
age_group_spotify_summary <- left_join(age_group_spotify_summary ,mean_ocd_by_age_group)
age_group_spotify_summary <- left_join(age_group_spotify_summary ,mean_hours_by_age_group)
age_group_spotify_summary <- left_join(age_group_spotify_summary ,most_common_time_slot_by_age_group, by = c("Age.group"= "Age"))
