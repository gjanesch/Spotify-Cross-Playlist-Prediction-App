library(glue)
library(httr)

## Function for retrieving ONLY the playlist name.  Not from spotifyr, and separate from the main
## get_playlist function for usability purposes.
get_playlist_name <- function(playlist_id, authorization=get_spotify_access_token()){
    name_response <- query_playlist(str_glue('https://api.spotify.com/v1/playlists/{playlist_id}'),
                                    params=list(access_token=authorization, fields=c("name")))
    return(name_response$name)
}


## Correctly format the tracks' artists.  Not from spotifyr.
get_artists <- function(artists){
    return(artists %>% lapply(function(x){paste(x$name, collapse=", ")}) %>% unlist)
}

## Gets an access token for API access.  Must be refreshed after 60 minutes.
## This function is copied from the spotifyr package.
get_spotify_access_token <- function(client_id, client_secret) {
    post <- RETRY('POST', 'https://accounts.spotify.com/api/token',
                  accept_json(), authenticate(client_id, client_secret),
                  body = list(grant_type = 'client_credentials'),
                  encode = 'form', httr::config(http_version = 2)) %>% content
    if (!is.null(post$error)) {
        stop(str_glue('Could not authenticate with given Spotify credentials:\n\t{post$error_description}'))
    }
    access_token <- post$access_token
    return(access_token)
}


## Gets information on a playlist and the tracks that comprise it, including audio features of the
## latter.
## This is mostly a copy from spotifyr, with added code to get track info and filter columns that
## aren't wanted.
get_playlist <- function(playlist_id, fields=NULL, market=NULL, 
                         authorization=get_spotify_access_token()) {
    url <- str_glue('https://api.spotify.com/v1/playlists/{playlist_id}')
    params <- list(
        fields = paste(fields, collapse = ','),
        market = market,
        access_token = authorization
    )
    
    # stopping is built into query_playlist()
    init_query <- get_first_batch(url, params)
    
    n_tracks <- pluck(init_query, "tracks", "total")
    
    if (!is.null(fields)) {
        return(init_query)
    } else if (n_tracks > 100) {
        # identify how many pages there are
        n_pages <- ceiling(pluck(init_query, "tracks", "total")/100) - 1
        # identify pagination offsets
        offsets <- seq(from = 1, to = n_pages) * 100
        # create page urls
        page_urls <- str_glue("{url}/tracks?offset={offsets}&limit=100")
        # query api
        other_pages <- map(page_urls, get_playlist_batch, params)
        # merge the song track results
        all_items <- bind_rows(
            pluck(init_query, "tracks", "items"),
            map_dfr(other_pages, pluck, "items")
        )
        # overwrite init_query item results
        init_query[["tracks"]][["items"]] <- all_items
    }
    
    # make the query object into something nicer
    struct <- structure(init_query, class = c("playlist", "list"))
    
    playlist <- struct$tracks$items %>%
        select(track.name, track.artists, track.album.name, track.duration_ms, acousticness:tempo) %>%
        mutate(track.artists = get_artists(track.artists)) %>% na.omit %>%
        mutate(SongInfo = glue("\"{track.name}\"<br>{track.artists}<br><i>{track.album.name}</i>"))
    
    return(playlist)
}

## Gets the data for the (up to) first 100 tracks from a Spotify playlist.
get_first_batch <- function(url, playlist_params){
    first_batch <- query_playlist(url, playlist_params)
    ids <- first_batch$tracks$items$track.id
    audio_features <- get_track_audio_features(ids, authorization = playlist_params$access_token)
    audio_features <- audio_features %>% select(acousticness, danceability, energy, valence, tempo)
    first_batch$tracks$items <- cbind(first_batch$tracks$items, audio_features)
    return(first_batch)
}

## Gets the data for any tracks beyond the first 100 from a Spotify playlist.  This was splist from
## get_first_batch() due to differences in the structure of what the API's playlist query returns.
get_playlist_batch <- function(url, playlist_params){
    playlist_batch <- query_playlist(url, playlist_params)
    ids <- playlist_batch$items$track.id
    audio_features <- get_track_audio_features(ids, authorization = playlist_params$access_token)
    audio_features <- audio_features %>% select(acousticness, danceability, energy, valence, tempo)
    playlist_batch$items <- cbind(playlist_batch$items, audio_features)
    return(playlist_batch)
}

## Function for submitting a general query to the Spotify API, despite the function name.
## Copied from the spotifyr package.
query_playlist <- function(url, params) {
    res <- RETRY('GET', url, query = params, encode = 'json')
    stop_for_status(res)
    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)
    res
}

## Gets the audio features of up to 100 tracks.
## Copied from the spotifyr package.
get_track_audio_features <- function(ids, authorization = get_spotify_access_token()) {
    stopifnot(length(ids) <= 100)
    base_url <- 'https://api.spotify.com/v1/audio-features'
    params <- list(
        access_token = authorization,
        ids = paste0(ids, collapse = ',')
    )
    res <- RETRY('GET', base_url, query = params, encode = 'json')
    stop_for_status(res)
    res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE) %>%
        .$audio_features %>%
        as_tibble()
    return(res)
}