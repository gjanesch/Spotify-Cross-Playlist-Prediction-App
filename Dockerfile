FROM rocker/shiny-verse:3.6.3

MAINTAINER Gregory Janesch "gjanesch@gmail.com"

RUN apt-get -y update
RUN apt-get -y install apt-utils
RUN apt-get -y install libcurl4-openssl-dev
RUN R -e "install.packages(c('plotly', 'shinydashboard', 'shinyBS', 'DT'))"

COPY *.R /root/app/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app/', port = 3838, host = '0.0.0.0')"]