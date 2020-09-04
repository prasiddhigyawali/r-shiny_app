FROM rocker/shiny-verse:latest

RUN R -e "install.packages(c('dplyr', 'tibble', 'anchors', 'plyr', 'reshape2', 'janitor'))"

RUN apt-get update && apt-get install -y git

RUN cd /srv/shiny-server && git clone https://github.com/prasiddhigyawali/r-shiny_app.git

# change permission of the shiny folder where the app resides
RUN chmod -R 777 /srv/shiny-server

WORKDIR /srv/shiny-server/r-shiny_app/

# Add shiny user to docker group to allow writing back onto host
RUN groupadd docker && usermod -aG docker shiny

# Start the server
CMD ["/usr/bin/shiny-server.sh"]
