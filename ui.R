library(shinydashboard)
library(plotly)
library(DT)

### Info for the prediction tab ###################################################################

playlist_box = box(width=7,
                   HTML("<h3>Scatterplot of Track Principal Components</h3>"),
                   uiOutput("reference_playlist_name"),
                   uiOutput("target_playlist_name"),
                   plotlyOutput("tracks_plot", width="auto"),
                   HTML("<br><h3>Recommended Tracks</h3>"),
                   DTOutput("recommendations"))

pc_box = box(width=5,
             HTML("<h3>Breakdown of Principal Components</h3>"),
             plotOutput("pc_plot", width = "auto"),
             uiOutput("explained_variance"))

###################################################################################################


# Define UI for application that draws a histogram
dashboardPage(
    title="Spotify Cross-Playlist Predictions",
    skin="green",
    dashboardHeader(title = "Playlist Predictions"),
    dashboardSidebar(
        textInput("reference_id", "Reference Playlist ID", value="37i9dQZF1DWVx3vT1QCKCV"),
        textInput("target_id", "Target Playlist ID", value="37i9dQZF1DZ06evO2wKKgo"),
        actionButton("predict_button", "Make Recommendations"),
        HTML("<br><br><br><br>"),
        actionButton("about", "About This App")
    ),
    dashboardBody(playlist_box, pc_box)
)