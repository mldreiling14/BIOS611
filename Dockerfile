FROM rocker/verse:latest

# stay root so s6 init can do its thing
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y --no-install-recommends man-db manpages manpages-dev less groff-base \
 && yes | unminimize \
 && rm -rf /var/lib/apt/lists/*

# optional: build the man index so `man` is fast on first use
RUN mandb -q || true
