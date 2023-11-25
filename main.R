mentalHealth <- read.csv("mxmh_survey_results.csv")
spotifyData <- read.csv("Spotify_data.csv")

unique(spotifyData$Age)
filtered_Spotify <- filter(mentalHeatlh, Primary.streaming.service = "Spotify")

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

mentalHealth$Age.group <-mapply(age_group, mentalHealth$Age)

