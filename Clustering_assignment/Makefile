# BIOS611 Clustering Assignment — Makefile

Rscript = Rscript --vanilla

.PHONY: all clean

all: figures/task1_gap_results.png figures/task2_spectral_shells.png


figures:
	mkdir -p figures

figures/task1_gap_results.png: Task1.R figures
	Rscript Task1.R

figures/task2_spectral_shells.png: Task2.R figures
	Rscript Task2.R

clean:
	rm -rf figures/