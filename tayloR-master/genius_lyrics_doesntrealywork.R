# loading required packages
packages <- c("spotifyr", "plyr", "tidyverse", "httr", "rvest", 
              "stringr", "ggthemes", "tidytext", "wordcloud", 
              "ggridges", "wesanderson", "yarrr", "knitr", 
              "kableExtra", "radarchart", "openxlsx", "quanteda")

# Install packages
#install.packages(packages)

# Load the packages to confirm installation
lapply(packages, library, character.only = TRUE)

#my access token for genius
token <- 'lsOrFYBdwbZwfIhej4nTqdd0CCdQQw_bTsRiu1IbkBXMKYR2uja_Jn3TRtq-mvPP'


#define function to get artist information

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
    map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}
  

#### parte per mettere le lyrics (da mettere in un ciclo)
# Getting track urls
baseURL <- 'https://api.genius.com/artists/' 

artist = read.xlsx("spotify.xlsx")

songs_with_lyrics = data.frame()
artist_names = unique(artist$artist_name)
songs_with_lyrics = data.frame()
for(artist_name in artist_names)
{

#artist_name="ghali"
genius_artists <- genius_get_artists(artist_name)


requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()

i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}



# Filtering to get urls only for tracks on which Taylor Swift is the primary artist
filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
  filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
  index <- append(index, i)
}


artist_lyrics <- data.frame(filtered_track_lyric_urls, filtered_track_lyric_titles)
artist_lyrics <- artist_lyrics[filtered_track_lyric_titles %in% artist$track_name, ]

artist_lyrics$filtered_track_lyric_urls <- as.character(artist_lyrics$filtered_track_lyric_urls)
artist_lyrics$filtered_track_lyric_titles <- as.character(artist_lyrics$filtered_track_lyric_titles)

# Webscraping lyrics using rvest 
lyric_text <- rep(NA, nrow(artist_lyrics))
for (i in 1:nrow(artist_lyrics)) {
  
  lyric = read_html(artist_lyrics$filtered_track_lyric_urls[i]) %>% 
    html_nodes(".Lyrics__Container-sc-1ynbvzw-1") %>% 
    html_text()
  lyric_text[i] <- ifelse(length(lyric)>0, lyric, "")
}



# Cleaning and standardizing lyrics
for (i in 1:length(lyric_text)) {
  lyric_text[i] <- gsub("\\[.*?\\]", "", lyric_text[i])
  lyric_text[i] <- gsub("/n", " ", lyric_text[i])
  lyric_text[i] <- gsub("//[.*?//]", " ", lyric_text[i])
  lyric_text[i] <- gsub("[ [:punct:] ]", " ", lyric_text[i])
  lyric_text[i] <- gsub(" {2,}", " ", lyric_text[i])
  lyric_text[i] <- gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text[i])
  lyric_text[i] <- tolower(lyric_text[i])
}

# lyric_text = lyric_text %>%  
#   tokens(remove_punct = TRUE, remove_symbols = TRUE,
#          remove_numbers = TRUE, remove_url = TRUE) %>%
#   tokens_remove(stopwords("it", source = "nltk"))
# 
#####
# ## trying text analysis
# 
# lyric_text =corpus(lyric_text)
# 
# lyric_tok = lyric_text
# 
#   lyric_tok = lyric_tok %>%
#     tokens(remove_punct = TRUE, remove_symbols = TRUE,
#            remove_numbers = TRUE, remove_url = TRUE) %>%
#     tokens_remove(stopwords("it", source = "nltk"))
# 
#   dfm_lyrics <- dfm(lyric_tok)
# 
#   dfm_lyrics <- dfm_lookup(dfm_lyrics, dictionary = extendeddict_it)
#   
# 
# #####

genius_data <- data.frame(artist = artist_name, track_name = artist_lyrics$filtered_track_lyric_titles, lyrics = lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)


songs_with_lyrics = rbind(songs_with_lyrics, genius_data)
}




# joining Spotify and Genius data
spotify_genius <- merge(genius_data, artist, by = c("track_name", "lyrics"), all.y = T)

# spotify_genius = spotify_genius %>%
#   distinct(paste0(track_name, artist_name), .keep_all = TRUE)

artist=spotify_genius