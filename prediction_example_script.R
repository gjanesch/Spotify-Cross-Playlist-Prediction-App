## Gregory Janesch, last modified 2020-07-26
## Description: Script designed to go through the full process of downloading playlist data
## and analyzing it.  This is meant to be used as a basis for the Shiny app.


library(glue)
library(jsonlite)
library(tidyverse)
library(plotly)

source("spotify_functions.R")
source("client_info.R")
source("playlist_functions.R")

CHILL = "37i9dQZF1DX4WYpdgoIcn6"
DRAGONFORCE = "4Mo1fC3coSORc25t2PIbGK"
COLTRANE = "37i9dQZF1DWVx3vT1QCKCV"
BILLYJOEL = "37i9dQZF1DWY7QTKRRDgA5"
EVANS = "37i9dQZF1DZ06evO2wKKgo"

access_token <- get_spotify_access_token(CLIENT_ID, CLIENT_SECRET)
reference_playlist <- get_playlist(EVANS, authorization=access_token)
target_playlist <- get_playlist(DRAGONFORCE, authorization=access_token)

pca_list <- cross_playlist_pca(reference_playlist, target_playlist)
reference_pcs <- pca_list$Reference
target_pcs <- pca_list$Target
two_component_variance(pca_list$PCA)

## From here, things split into two directions - one for generating the plot from the principal
## components and one for calculating and returning the predictions.

## PART 1: The predictions
closest_target_indices <- calculate_closest_songs(reference_pcs, target_pcs)
recommended_songs <- target_playlist[closest_target_indices,]

## PART 2a: Plotting the principal components' makeup
p <- principal_components_ggplot(pca_list$PCA$rotation)

## Part 2b: Plotting the data
reference_pcs <- reference_pcs %>%
    mutate(SongInfo = reference_playlist$SongInfo, Playlist="Reference")

target_pcs <- target_pcs %>%
    mutate(SongInfo = target_playlist$SongInfo,
           Recommended = row_number() %in% closest_target_indices,
           Playlist = ifelse(Recommended, "Target (Recommendation)", "Target (Non-Recommendation)")) %>%
    select(-Recommended)

dataset <- rbind(reference_pcs, target_pcs)
g <- create_tracks_ggplot(dataset)
ggplotly(g, tooltip="text")
