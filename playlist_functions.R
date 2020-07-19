## Takes dataframes containing the playlists' data, performs PCA on the reference playlist, then
## uses it to predict how the target playlist looks.
cross_playlist_pca <- function(reference, target){
    reference <- reference %>% select(track.duration_ms:tempo)
    pca <- prcomp(reference, scale=TRUE)
    reference_predictions <- pca$x %>% as.data.frame %>% select(PC1, PC2)
    target_predictions <- predict(pca, target) %>% as.data.frame %>% select(PC1, PC2)
    return(list(Reference=reference_predictions, Target=target_predictions, PCA=pca))
}


## Takes the principal components from the reference and target playlists and
## returns the indices for the n_target songs from the target playlist that are
## closest to the reference songs.  Closeness is determined by the sum of the
## Euclidean distances to the nearest n_reference tracks.
calculate_closest_songs <- function(reference_pcs, target_pcs, n_reference=5, n_target=5){
    indices <- expand.grid(RefIndex=1:nrow(reference_pcs), TargetIndex=1:nrow(target_pcs))
    distances <- indices %>% mutate(RefPC1 = reference_pcs[RefIndex, "PC1"],
                                    RefPC2 = reference_pcs[RefIndex, "PC2"],
                                    TargetPC1 = target_pcs[TargetIndex, "PC1"],
                                    TargetPC2 = target_pcs[TargetIndex, "PC2"],
                                    Distance = sqrt((RefPC1-TargetPC1)^2 + (RefPC2-TargetPC2)^2))
    top_n_per_target <- distances %>% group_by(TargetIndex) %>%
        arrange(Distance, .by_group=TRUE) %>% top_n(n_reference, -Distance)
    closest_target_indices <- top_n_per_target %>%
        summarize(TotalDistance = sum(Distance)) %>%
        arrange(TotalDistance) %>% head(n_target) %>% .$TargetIndex
    return(closest_target_indices)
}


## Calculates the variance captured by PCA's first two principal components.
two_component_variance <- function(pca){
    cumulative_variances <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
    return(cumulative_variances[2])
}


## Takes the principal components predictions from both playlists and plots them on a scatter
## plot for visualization.  The output can be used for Plotly plots, but should have additional
## formatting to make it match this.
create_tracks_ggplot <- function(dataset){
    max_PC1 <- ceiling(max(abs(dataset$PC1)))
    max_PC2 <- ceiling(max(abs(dataset$PC2)))
    g <- ggplot(data=dataset, aes(x=PC1, y=PC2, color=Playlist)) + 
        geom_point(aes(text=SongInfo)) + scale_color_manual(values=c("#FF4444", "lightgreen", "darkgreen")) +
        theme(legend.position = "bottom", legend.title=element_blank()) + 
        xlim(-max_PC1, max_PC1) + ylim(-max_PC2, max_PC2)
    return(g)
}

## Takes the rotation matrix from the principal components output and creates a faceted ggplot for
## display.
principal_components_ggplot <- function(rotation_matrix){
    rotation_df <- rotation_matrix %>% as.data.frame %>% select(PC1, PC2)
    rotation_plotting <- rotation_df %>% mutate(Trait=row.names(.)) %>% pivot_longer(cols=c(PC1,PC2))
    g <- ggplot(rotation_plotting, aes(x=Trait, y=value)) + geom_col() + coord_flip() + facet_wrap(~name)
    return(g)
}