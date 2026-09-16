library(tidyverse)
library(jsonlite)

# Create covers directory
dir.create("covers", showWarnings = FALSE)

# Artists to search
artists <- c("BTS", "BLACKPINK", "NewJeans", "Stray Kids", "CORTIS")

# Function to fetch album info, cover image, tracklist, and audio previews
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
  
  if (!file.exists(cover_path)) {
    download.file(img_url, destfile = cover_path, mode = "wb", quiet = TRUE)
  }
  
  # 3. Lookup tracklist with audio previews
  tracks_url <- str_c("https://itunes.apple.com/lookup?id=", col_id, "&entity=song")
  track_results <- fromJSON(tracks_url)$results
  
  tracks_df <- track_results |>
    as_tibble() |>
    filter(wrapperType == "track")
  
  # 4. Find the most popular track for this album from iTunes song search
  pop_search_url <- str_c(
    "https://itunes.apple.com/search?term=",
    URLencode(str_c(artist, " ", album_name)),
    "&entity=song&limit=1"
  )
  pop_results <- fromJSON(pop_search_url)$results
  top_track_name <- if (!is.null(pop_results) && nrow(pop_results) > 0) pop_results$trackName[1] else ""
  
  tracks_list <- tracks_df |>
    transmute(
      track_number = trackNumber,
      track_name = trackName,
      duration_sec = round(trackTimeMillis / 1000),
      preview_url = previewUrl,
      is_favorite = (trackName == top_track_name)
    )
  
  tibble(
    artist = artist,
    album = album_name,
    release_date = results$releaseDate[1],
    genre = results$primaryGenreName[1],
    cover_file = cover_path,
    tracks = list(tracks_list)
  )
}

albums_data <- map(artists, fetch_album) |> list_rbind()

# Save enriched data
write_json(albums_data, "albums_data.json", pretty = TRUE)
message("Updated albums_data.json with audio previews and favorite tracks!")
