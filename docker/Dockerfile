#-- Build the image--#
# docker build -t kamermanpr/pain-sites:v0.1 .

#-- Push the image to Docker Hub --#
# docker push kamermanpr/pain-sites:v0.1

#-- Pull the latest image from Docker Hub --#
# docker pull kamermanpr/pain-sites

#-- Run command --#
# docker run --name pain -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/pain-sites

#-- Login to RStudio --#
# In your browser, navigate to: localhost:8787
# Username: user
# Password: password

#-- Get the verse rocker image --#

FROM rocker/verse:3.6.3

MAINTAINER Peter Kamerman <peter.kamerman@gmail.com>

#-- Install extra packages --#

RUN Rscript -e "install.packages(c('lme4' , 'patchwork', 'skimr'), dependencies = TRUE)"

#-- Make sure TinyTex is installed --#
RUN Rscript -e "tidytext::install_tinytex(force = TRUE))"