#!/bin/bash

export MSYS_NO_PATHCONV=1

# Build the Docker image
docker build . -t sports_betting

# Stop old container if it exists
docker rm -f sports_betting 2>/dev/null

# Run the new container
docker run -d --name sports_betting \
  -e PASSWORD=password \
  -p 8787:8787 \
  -v //c/Users/drdre/BIOS611/sports_betting:/home/rstudio/project \
  sports_betting
