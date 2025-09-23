export MSYS_NO_PATHCONV=1

docker build . -t bios611-r

docker run -d --name bios611-r \
-e PASSWORD=password \
-v "/c/Users/drdre/OneDrive/BIOS611:/home/rstudio/project" \
-v "/c/Users/drdre/OneDrive/BIOS611/.ssh:/home/rstudio/.ssh" \
-v "/c/Users/drdre/OneDrive/BIOS611/.gitconfig:/home/rstudio/.gitconfig" \
-p 8787:8787 \
rocker/verse
