# BIOS611 Clustering Assignment — Makefile

RSCRIPT = Rscript --vanilla

.PHONY: all clean

all: figures/task1_gap_results.png figures/task2_spectral_results.png

figures:
	mkdir -p figures

figures/task1_gap_results.png: task1.R figures
	RSCRIPT task1.R

figures/task2_spectral_results.png: task2.R figures
	RSCRIPT task2.R
	
clustering_report.html: clustering_report.Rmd figs/task1_gapstat.png figs/task2_spectral_shells.png
	Rscript -e "rmarkdown::render('clustering_report.Rmd', output_format='html_document')"
