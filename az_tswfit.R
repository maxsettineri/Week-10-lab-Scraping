library(tidyverse)
library(rvest)


#  library(spotifyr)
# 
#  Sys.setenv(SPOTIFY_CLIENT_ID = 'xxx')
#  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxx')
# 
# get_spotify_access_token()
# 
# ts_albums<-c("Midnights (3am Edition)","Red (Taylor's Version)","Fearless (Taylor's Version)","evermore (deluxe version)","folklore (deluxe version)","Lover","Repuation","1989 (Deluxe Edition)","Speak Now (Deluxe Edition)","Taylor Swift")
# 
# ts<-get_artist_audio_features('taylor swift') %>%  # API call
#   select(artist_name,album_name,album_release_year,track_name) %>%  #Keep this order
#   rename(artist=artist_name,album=album_name,year=album_release_year,song=track_name) %>% 
#   filter(album %in% ts_albums)



alltoowell_lyric<-read_html("https://www.azlyrics.com/lyrics/taylorswift/alltoowell10minuteversiontaylorsversionfromthevault.html") %>% 
  html_nodes("div") %>%  #If we were looking for a table, replace with html_table(fill=TRUE)
  pluck(23) %>% 
  html_text() %>% 
  str_replace_all(c("\n"=" ","\r"=" ")) %>% 
  str_remove_all(pattern = "[[:punct:]]") %>%   #Remove all the punctuation
  str_to_lower() %>% 
  str_split(" ") %>% 
  as.data.frame()
  
colnames(alltoowell_lyric)[1]<-"word"

lyric<-filter(alltoowell_lyric,word != "")

#Make a function for purr

get_lyrics<-function(artist,album,year,song){
  
  #Create url base
  base1<-c("https://azlyrics.com/lyrics/")
  
  base2<-c(".html")
  
  #Clean the artist name and song name to match the url
  artist_url<-str_replace_all(artist,pattern = "(?!\\!)[[:punct:]]",replacement = " ") %>% 
    str_replace_all(pattern = " ",replacement = "") %>%
    str_to_lower() %>% 
    str_squish()  # Just a precaution
  
  song_url<- str_remove_all(song,pattern = "(?![!'])[[:punct:]]") %>%   #The (?!\\[!']) tells R to ignore all punct except ! and '
    str_replace_all(pattern="'",replacement = " ") %>%   #This is a little thing I noticed specific to the website in how they handle apostrophes
    str_replace_all(pattern = " ",replacement = "") %>%
    str_to_lower() %>% 
    str_squish() 
  
  url<-paste(base1,artist_url,"/",song_url,base2,sep="")
  

  
  #Get the data from the website and clean it up
  
  extract<-read_html(url) %>% 
    html_nodes("div") %>%  #If we were looking for a table, replace with html_table(fill=TRUE)
    pluck(23) %>% 
    html_text() %>% 
    str_replace_all(c("\n"=" ","\r"=" ")) %>% 
    str_remove_all(pattern = "[[:punct:]]") %>%   #Remove all the punctuation
    str_to_lower() %>% 
    str_split(" ") %>% 
    as.data.frame() %>% 
    mutate(song=song,artist=artist,album=album,year=year) #Add other names
  
  
  colnames(extract)[1]<-"word"  #Use word here so it matches with stop_words
  
  extract_clean<-extract %>% 
    filter(word != "") %>% 
    anti_join(stop_words,by="word")
  
  Sys.sleep(2)
  
  return(extract_clean)
}


safe_get_ly<-safely(get_lyrics)

tictoc::tic()
song_lyrics<-ts %>% 
  filter(album=="Red (Taylor's Version)") %>% 
  pmap(.,safe_get_ly,.progress = TRUE) %>% 
  transpose()

tictoc::toc()

any_errors_lyrics<-compact(song_lyrics$error)


#Extract the data from the lists  
lyrics<-compact(song_lyrics$result)  %>% 
  as_tibble_col(column_name = "word") %>% 
  unnest()
