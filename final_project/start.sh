export MSYS_NO_PATHCONV=1

docker build -t finalproject .

docker run -d --rm \
  -p 8787:8787 \
  -v "$(pwd)":/home/rstudio/project \
  --name finalproject \
  finalproject