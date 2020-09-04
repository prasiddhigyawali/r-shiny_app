FROM cyversevice/shiny-verse
RUN R -e "install.packages(c(‘tidyverse’, ‘dplyr’, ‘tibble’, ‘anchors’, ‘plyr’, ‘reshape2’, ‘janitor’))”
RUN cd /srv/shiny-server && git clone https://github.com/prasiddhigyawali/r-shiny_app.git
# change permission of the shiny folder where the app resides
RUN chmod -R 777 /srv/shiny-server
WORKDIR /srv/shiny-server/r-shiny_app/
# Add shiny user to docker group to allow writing back onto host
RUN groupadd docker && usermod -aG docker shiny
# Start the server
CMD ["/usr/bin/shiny-server.sh"]
