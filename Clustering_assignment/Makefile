# BIOS611 Clustering Assignment — Makefile

RSCRIPT = Rscript --vanilla

.PHONY: all clean

all: figures/task1_gap_results.png figures/task2_spectral_shells.png

figures:
	mkdir -p figures

figures/task1_gap_results.png: task1.R figures
	Rscript task1.R

figures/task2_spectral_shells.png: task2.R figures
	Rscript task2.R

clustering_assignment.html: clustering_assignment.Rmd figures/task1_gap_results.png figures/task2_spectral_shells.png
	Rscript -e "rmarkdown::render('clustering_assignment.Rmd', output_file = 'clustering_assignment.html')
	
clean:
	rm -rf figures/

