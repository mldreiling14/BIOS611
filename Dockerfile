FROM rocker/verse

RUN apt-update && apt install -y man-db 
RUN yes | unminimize 

