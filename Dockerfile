FROM rocker/shiny-verse:4.2.3

RUN apt-get update
RUN apt-get install -y libglpk-dev
RUN apt-get install -y libgmp3-dev
RUN apt-get install -y libxml2-dev

RUN R -e "install.packages('colourpicker')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('ggplot2')"
# RUN R -e "install.packages('htmltools')"
RUN R -e "install.packages('igraph')"
# RUN R -e "install.packages('shiny')"
# RUN R -e "install.packages('shinyBS')"
RUN R -e "install.packages('shinycssloaders')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinydashboardPlus')"
RUN R -e "install.packages('shinyhelper')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('reactable')"
# RUN R -e "install.packages('readr')"
# RUN R -e "install.packages('reshape2')"
RUN R -e "install.packages('rintrojs')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('visNetwork')"
