export MSYS_NO_PATHCONV=1

docker build . -t bios611

docker run -d --name bios611 \
-e PASSWORD=password \
<<<<<<< HEAD
-v //c/Users/drdre/BIOS611:/home/rstudio/project \
-v //c/Users/drdre/BIOS611/.ssh:/home/rstudio/.ssh \
-v //c/Users/drdre/BIOS611/.gitconfig:/home/rstudio/.gitconfig \
=======
-v "/c/Users/drdre/BIOS611:/home/rstudio/project" \
-v "/c/Users/drdre/BIOS611/.ssh:/home/rstudio/.ssh" \
-v "/c/Users/drdre/BIOS611/.gitconfig:/home/rstudio/.gitconfig" \
>>>>>>> 6ef7fed38d4fe8b60c93072008dd7ba0bdafc164
-p 8787:8787 \
rocker/verse
