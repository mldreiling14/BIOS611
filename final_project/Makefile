.PHONY: all clean

all:	derived_data/chr_clean_full.csv \
			figures/ypll_pca.png \
			figures/ypll_pca_boxplot.png


figures: 
	mkdir -p figures
	
derived_data:
	mkdir -p derived_data
  
derived_data/chr_clean_full.csv: cleaned_chr_data.R source_data/county_health_rankings_2025.csv derived_data
	RSCRIPT cleaned_chr_data.R

figures/ypll_pca.png: ypll_pca.R derived_data/chr_clean_full.csv figures
	RSCRIPT ypll_pca.R
	
figures/ypll_pca_boxplot.png: ypll_pca.R derived_data/chr_clean_full.csv figures
	RSCRIPT ypll_pca.R
	
figures/correlated_data.png: correlated_data.R figures
	RSCRIPT correlated_data.R

report.html: report.Rmd figures/x.png
  R -e "rmardown::render('report,Rmd', output_format = 'html_document)"
  
clean:
	rm -rf figures
	rm -rf derived_data  