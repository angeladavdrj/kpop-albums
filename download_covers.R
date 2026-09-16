library(tidyverse)
library(jsonlite)

# Create covers directory
dir.create("covers", showWarnings = FALSE)

# Artists to search
artists <- c("BTS", "BLACKPINK", "NewJeans", "Stray Kids", "CORTIS")

# Function to fetch album info, cover image, and tracklist
fetch_album <- \(artist) {
  # 1. Search for top album
  search_url <- str_c(
    "https://itunes.apple.com/search?term=",
    URLencode(artist),
    "&entity=album&limit=1"
  )
  results <- fromJSON(search_url)$results
  
  if (is.null(results) || nrow(results) == 0) return(NULL)
  
  col_id <- results$collectionId[1]
  album_name <- results$collectionName[1]
  
  # 2. Download high-resolution cover
  img_url <- results$artworkUrl100[1] |>
    str_replace("100x100bb", "600x600bb")
  
  clean_artist <- artist |> str_replace_all("[^a-zA-Z0-9]", "_")
  clean_album <- album_name |> str_replace_all("[^a-zA-Z0-9]", "_")
  cover_filename <- str_c(clean_artist, "_", clean_album, ".jpg")
  cover_path <- file.path("covers", cover_filename)
  
  download.file(img_url, destfile = cover_path, mode = "wb", quiet = TRUE)
  
  # 3. Lookup tracklist
  tracks_url <- str_c("https://itunes.apple.com/lookup?id=", col_id, "&entity=song")
  track_results <- fromJSON(tracks_url)$results
  
  song_titles <- track_results |>
    as_tibble() |>
    filter(wrapperType == "track") |>
    pull(trackName)
  
  tibble(
    artist = artist,
    album = album_name,
    cover_file = cover_path,
    tracks = list(song_titles)
  )
}

albums_data <- map(artists, fetch_album) |> list_rbind()

# Save structured data to json for our website
write_json(albums_data, "albums_data.json", pretty = TRUE)
message("Saved album data and covers for ", nrow(albums_data), " artists.")
