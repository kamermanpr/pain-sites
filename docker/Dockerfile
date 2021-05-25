#-- Pull the latest image from Docker Hub --#
# docker pull kamermanpr/pain-sites:v1.1

#-- Run command --#
# docker run --name pain_sites -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/pain-sites:v1.1

#-- Login to RStudio --#
# In your browser, navigate to: localhost:8787
# Username: user
# Password: password

#-- Get the verse rocker image --#

FROM rocker/verse:4.0.4

MAINTAINER Peter Kamerman <peter.kamerman@gmail.com>

#-- Install extra packages --#

RUN Rscript -e "install.packages(c('patchwork', 'skimr'), dependencies = TRUE)"