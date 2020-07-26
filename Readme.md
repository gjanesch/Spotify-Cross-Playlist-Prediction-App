This contains code for an app designed to make song recommendations from a Spotify playlist based on the contents of another, plus the Dockerfile needed to build it into a Docker container.  Writeups going into more detail can be found [here](https://data-and-the-world.onrender.com/posts/spotify-cross-playlist-predictions-one/) and [here](https://data-and-the-world.onrender.com/posts/spotify-cross-playlist-predictions-two/).

Requires the tidyverse and Plotly.  Does not rely on `spotifyr`, as it was temporarily unavailable while I coded this up, though it does copy several functions from it.

The app can be viewed [here](http://3.85.216.75:3838/).

NOTE: The file "client_info.R" contains the API keys for accessing Spotify, and as such is not included in this repository; you'll need to generate your own.