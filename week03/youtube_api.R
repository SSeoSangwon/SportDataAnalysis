# install.packages("httr")
# install.packages("jsonlite")
# install.packages("purrr")
# install.packages("stringr")
# install.packages("dplyr")

library(httr)
library(jsonlite)
library(purrr)
library(stringr)
library(dplyr)

key <- Sys.getenv("YOUTUBE_API_KEY")
if (key == "") stop("Set YOUTUBE_API_KEY before running this script.")
vid <- "IYO2Qlpw3tI"
base <- "https://www.googleapis.com/youtube/v3/"


api_video <- 
  paste(paste0("key=", key), 
        paste0("id=", vid), 
        "part=snippet%2Cstatistics",
        sep = "&")

api_video_call <- paste0(base,"videos?", api_video)


vdinfo <- GET(api_video_call)

vdinfo1 <- content(vdinfo, "text", encoding="UTF-8")
vdinfo2 <- fromJSON(vdinfo1, flatten = T)
vdinfo3 <- as.data.frame(vdinfo2)

plid <- "PLRdw3IjKY2glDAvL9kW6yON_Bz7wG3ZdT"

api_pl <- 
  paste(paste0("key=", key), 
        paste0("playlistId=", plid), 
        "part=snippet%2CcontentDetails",
        "maxResults=50",
        sep = "&")


api_pl_call <- paste0(base,"playlistItems?", api_pl)
plifo <- GET(api_pl_call)
plifo1 <- content(plifo, "text", encoding="UTF-8")
plifo2 <- fromJSON(plifo1, flatten = T)
plifo3 <- as.data.frame(plifo2)


df <- map(plifo3$items.contentDetails.videoId, function(.x){
  api_video <- 
    paste(paste0("key=", key), 
          paste0("id=", .x), 
          "part=snippet%2Cstatistics",
          sep = "&")
  
  api_video_call <- paste0(base,"videos?", api_video)
  vdinfo <- GET(api_video_call)
  vdinfo1 <- content(vdinfo, "text", encoding="UTF-8")
  vdinfo2 <- fromJSON(vdinfo1, flatten = T)
  df <- as.data.frame(vdinfo2)
  do.call(rbind, df )
})

df <- data.frame (df)

df <- as.data.frame(t(df))              

rownames(df) <- NULL

df$items.snippet.localized.title[1]

# Bears vs. Rams Week 1 Highlights | NFL 2021

df$awayteam <- str_extract(df$items.snippet.localized.title, ".*(?=\\svs\\.)")
df$hometeam <- str_extract(df$items.snippet.localized.title, "(?<=vs\\.\\s).*(?=\\sWeek)")

df$items.snippet.publishedAt[1]
df$year <- as.numeric(str_extract(df$items.snippet.publishedAt, "\\d\\d\\d\\d(?=\\-)"))
df$month <- as.numeric(str_extract(df$items.snippet.publishedAt, "(?<=\\d\\d\\d\\d\\-)\\d\\d(?=\\-)"))
df$day <- as.numeric(str_extract(df$items.snippet.publishedAt, "(?<=\\d\\d\\d\\d\\-\\d\\d\\-)\\d\\d(?=T)"))

# ------------------------------------------------------------------------------

ex3_key <- key
ex3_base <- "https://www.googleapis.com/youtube/v3/"
ex3_plid <- "PL2S1q3ojLFZJJai1ChUIeCoYhM5pF3A-c"

all_items <- data.frame()

page_token <- ""
has_more <- TRUE

while (has_more) {
  ex3_api_pl <- paste(paste0("key=", ex3_key),
                      paste0("playlistId=", ex3_plid),
                      "part=snippet%2CcontentDetails",
                      "maxResults=50",
                      ifelse(page_token != "", paste0("pageToken=", page_token), ""),
                      sep = "&")
  
  ex3_api_pl_call <- paste0(ex3_base, "playlistItems?", ex3_api_pl)
  
  ex3_plifo <- GET(ex3_api_pl_call)
  ex3_plifo1 <- content(ex3_plifo, "text", encoding="UTF-8")
  ex3_plifo2 <- fromJSON(ex3_plifo1, flatten = TRUE)
  
  ex3_plifo3 <- as.data.frame(ex3_plifo2$items)
  
  all_items <- bind_rows(all_items, ex3_plifo3)
  
  if (!is.null(ex3_plifo2$nextPageToken)) {
    page_token <- ex3_plifo2$nextPageToken
  } else {
    has_more <- FALSE
  }
}

ex3_df <- map(all_items$contentDetails.videoId, function(.x){
  ex3_api_video <- paste(paste0("key=", ex3_key), 
                         paste0("id=", .x), 
                         "part=snippet%2Cstatistics",
                         sep = "&")
  
  ex3_api_video_call <- paste0(ex3_base,"videos?", ex3_api_video)
  ex3_vdinfo <- GET(ex3_api_video_call)
  ex3_vdinfo1 <- content(ex3_vdinfo, "text", encoding="UTF-8")
  ex3_vdinfo2 <- fromJSON(ex3_vdinfo1, flatten = TRUE)
  ex3_df <- as.data.frame(ex3_vdinfo2)
  return(ex3_df)
})

ex3_df <- do.call(rbind, ex3_df)
rownames(ex3_df) <- NULL

title_cleaned <- str_replace_all(ex3_df$items.snippet.localized.title, "\\s*vs\\s*", " vs ")

ex3_df$hometeam <- str_extract(title_cleaned, "\\S+(?= vs )")

ex3_df$awayteam <- str_extract(title_cleaned, "(?<= vs )\\S+")

ex3_df <- ex3_df %>%
  mutate(
    hometeam = case_when(
      hometeam == "강원" ~ "Gangwon",	
      hometeam == "제주" ~ "Jeju Utd",
      hometeam == "포항" ~ "Pohang",
      hometeam == "성남" ~ "Seongnam",
      hometeam == "전북" ~ "Jeonbuk",
      hometeam == "수원" ~ "Suwon Bluewings",
      hometeam == "인천" ~ "Incheon",
      hometeam == "김천" ~ "Gimcheon Sangmu",
      hometeam == "서울" ~ "Seoul",
      hometeam == "대구" ~ "Daegu",
      hometeam == "수원FC" ~ "Suwon FC",
      hometeam == "울산" ~ "Ulsan HD",
      TRUE ~ hometeam
    ),
    awayteam = case_when(
      awayteam == "강원" ~ "Gangwon",	
      awayteam == "제주" ~ "Jeju Utd",
      awayteam == "포항" ~ "Pohang",
      awayteam == "성남" ~ "Seongnam",
      awayteam == "전북" ~ "Jeonbuk",
      awayteam == "수원" ~ "Suwon Bluewings",
      awayteam == "인천" ~ "Incheon",
      awayteam == "김천" ~ "Gimcheon Sangmu",
      awayteam == "서울" ~ "Seoul",
      awayteam == "대구" ~ "Daegu",
      awayteam == "수원FC" ~ "Suwon FC",
      awayteam == "울산" ~ "Ulsan HD",
      TRUE ~ awayteam
    )
  )

ex3_df <- ex3_df %>%
  mutate(
    wk = str_extract(items.snippet.localized.title, "(?<=R)\\d+")
  )

ex3_df$wk <- as.numeric(ex3_df$wk)
# ex3$wk <- as.numeric(ex3$wk)

# CSV 파일 읽어오기
klgfull <- read.csv("klgfull.csv")

klg2022 <- klgfull %>%
  filter(year == 2022)

ex3 <- merge(klg2022, ex3_df, by = c("hometeam", "awayteam"))

ex3 <- ex3 %>%
  select(
    round, hometeam, awayteam, homescore, awayscore, homeodds, drawodds, awayodds, year, month, day, dow, time, stadium, attendance,
    everything()
  )

print(ex3)

