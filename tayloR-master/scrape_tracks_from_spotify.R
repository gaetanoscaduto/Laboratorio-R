
# loading required packages
packages <- c("spotifyr", "plyr", "tidyverse", "httr", "rvest", 
              "stringr", "ggthemes", "tidytext", "wordcloud", 
              "ggridges", "wesanderson", "yarrr", "knitr", 
              "kableExtra", "radarchart", "openxlsx", "quanteda")

# Install packages
#install.packages(packages)

# Load the packages to confirm installation
lapply(packages, library, character.only = TRUE)

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Didattica/Lab 2024/tayloR-master/")
# set up Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = '4b8c4a038c2f4d2cbd7f0237f7eb539a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4054b75adf2049ec9061350e94ad9f02')


#using spoitfyr

artist_names = c("sfera ebbasta", "pinguini tattici nucleari", "gazzelle",
                 "calcutta", "ultimo", "angelina mango", "i cani", "mahmood", 
                 "rkomi", "ghali", "gli psicologi", "fabri fibra", "annalisa",
                 "tedua", "elodie", "max pezzali", "jovanotti", "cesare cremonini",
                 "fabrizio de andrè", "francesco de gregori", "francesco guccini",
                 "elisa", "laura pausini", "club dogo", "la rappresentante di lista",
                 "marco mengoni", "francesco gabbani", "måneskin", "diodato", 
                 "fiorella mannoia", "elio e le storie tese")

data=data.frame()
for(artist_name in artist_names)
{
  #artist_name = "sfera ebbasta"
artist <- get_artist_audio_features(artist_name)


# The Spotify data for Taylor Swift changed a little in the week between my pulling it and posting this code.



# Getting artist ID on Genius
token <- 'lsOrFYBdwbZwfIhej4nTqdd0CCdQQw_bTsRiu1IbkBXMKYR2uja_Jn3TRtq-mvPP'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL)%>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}


aus=data.frame(id="id", album_name="album_name")

albums = list()

artist$artist_id = NULL
artist$album_id = NULL
artist$album_type = NULL
artist$album_release_date = NULL
artist$album_release_date_precision = NULL
artist$track_id = NULL
artist$external_urls.spotify = NULL
artist$track_uri = NULL
artist$track_number = NULL
artist$album_images = NULL
artist$analysis_url = NULL
artist$artists = NULL
artist$available_markets = NULL
artist$track_href = NULL
artist$is_local = NULL
artist$track_preview_url = NULL
artist$type = NULL
artist$disc_number = NULL


data = rbind(data, artist)

}

write.xlsx(data, "spotify.xlsx")

