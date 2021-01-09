## Gregory Janesch, last modified 2020-07-26
## Description: Server code for the Spotify Shiny app.


library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)

source("spotify_functions.R")
source("playlist_functions.R")
source("client_info.R")

ABOUT_STRING <- HTML("<p>This web app is designed to acquire data on two Spotify playlists - a 
                     reference and a target - and make predictions on what which songs from the 
                     target playlist the user would like based on the contents of the reference 
                     playlist.
                     <br><br>
                     It works by performing a principal components analysis (PCA) on several 
                     attributes of the reference playlist's tracks, and then uses the principal 
                     components to project where the tracks on the target playlist would go. 
                     Recommendations are made by calculating the Euclidean distances from each 
                     target song to each reference song, and then using the sum of the smallest 
                     distances for each target songs as an indication of which target songs are 
                     'closest'.
                     <br><br>
                     Notes: <br>
                     - The playlist IDs are the part of the URL after '/playlist/'. <br>
                     - Both playlists need to have at least five songs present in order for the app
                     to work. <br>
                     - The app may take several seconds to complete its recommendations.  Please be
                     patient.")


server <- function(input, output, session) {
    
    ## Handles getting the data and running PCA
    workhorse <- function(access_token){
        reference_playlist <- get_playlist(input$reference_id, authorization=access_token)
        target_playlist <- get_playlist(input$target_id, authorization=access_token)
        pca_list <- cross_playlist_pca(reference_playlist, target_playlist)
        return(list(pca_list, reference_playlist, target_playlist))
    }
    
    ## Perform the predictions and update all of the display elements when the "Predict" button is
    ## used.
    prediction_operation <- observeEvent(input$predict_button, {
        access_token <- get_spotify_access_token(CLIENT_ID, CLIENT_SECRET)
        workhorse_data <- workhorse(access_token)
        pca_list <- workhorse_data[[1]]
        reference_playlist <- workhorse_data[[2]]
        target_playlist <- workhorse_data[[3]]
        closest_target_indices <- calculate_closest_songs(pca_list$Reference, pca_list$Target)
        
        ## Plotly-rendered scatterplot of the track info
        output$tracks_plot <- renderPlotly({
            reference_pcs <- pca_list$Reference %>%
                mutate(SongInfo = reference_playlist$SongInfo, Playlist="Reference")
            
            target_pcs <- pca_list$Target %>%
                mutate(SongInfo = target_playlist$SongInfo,
                       Recommended = row_number() %in% closest_target_indices,
                       Playlist = ifelse(Recommended, "Target (Recommendation)",
                                         "Target (Non-Recommendation)")) %>%
                select(-Recommended)
            
            dataset <- rbind(reference_pcs, target_pcs)
            suppressWarnings(ggplotly(create_tracks_ggplot(dataset), tooltip="text") %>%
                                 layout(legend=list(orientation="h", x=0, y=-0.2), height=400))
        })
        
        ## Data table for the recommended songs.
        output$recommendations <- DT::renderDT(rownames=NULL, options=list(dom="t"), {
            recommended_songs <- target_playlist[closest_target_indices, c("track.name",
                                                                           "track.artists",
                                                                           "track.album.name")]
            recommended_songs
        })
        
        ## Text element mentioning the percentage of the variance accounted for by the first two
        ## principal components
        output$explained_variance <- renderUI({
            two_pcs <- pca_list$PCA$rotation[,1:2]
            pc_diffs <- sort(abs(two_pcs[,1] - two_pcs[,2]), decreasing=TRUE)
            two_biggest_diffs <- names(pc_diffs)[1:2]
            variance <- two_component_variance(pca_list$PCA)
            variance <- round(100*variance, 1)
            HTML(str_glue("The first two principal components explain {variance}% of the total 
                          variance in the reference playlist. <br>
                          The biggest differences between these two components are in the 
                          <b>{two_biggest_diffs[1]}</b> and the <b>{two_biggest_diffs[2]}</b> 
                          features."))
        })
        
        ## Principal components plot.
        output$pc_plot <- renderPlot({
            principal_components_ggplot(pca_list$PCA$rotation)
        })
        
        ## Text element showing the reference playlist name.
        output$reference_playlist_name <- renderUI({
            playlist_name <- get_playlist_name(input$reference_id, access_token)
            HTML(str_glue("<b>Reference Playlist: </b> {playlist_name}<br>"))
        })
        
        ## Text element showing the target playlist name.
        output$target_playlist_name <- renderUI({
            playlist_name <- get_playlist_name(input$target_id, access_token)
            HTML(str_glue("<b>Target Playlist: </b> {playlist_name}<br>"))
        })
    
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    ## Code for bringing up the "About This App" model dialog
    observeEvent(input$about, {
        showModal(modalDialog(ABOUT_STRING, title="About This App", easyClose=TRUE))
    })
}
