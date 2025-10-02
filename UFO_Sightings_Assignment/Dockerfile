FROM rocker/verse: latest

RUN R -e "install.packages(c(\
	'tidytext',\
  	'stopwords', \
  	'wordcloud', \
  	'ggrepel', \
  	'RColorBrewer' \
), repos='https://cloud.r-project.org')"

RUN mkdir -p /work/derived_data /work/images

WORKDIR /work
