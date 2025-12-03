RSCRIPT = Rscript --vanilla

.PHONY: all clean

all:	derived_data/chr_clean_full.csv \
			figures/missing_data.png \
			figures/corr_heatmap_small.png \
			figures/correlated_bar.png \
			figures/clusGAP.png \
			figures/le_pca_boxplot.png \
			figures/le_pca.png \
			figures/pca_life_expectancy.png \
			figures/state_cluster_label_distribution_pct.png \
			derived_data/county_clusters_all.csv \
			report.html
		
figures: 
	mkdir -p figures
	
data: derived_data/chr_clean_full.csv derived_data/county_clusters_all.csv
	
derived_data:
	mkdir -p derived_data
  
derived_data/chr_clean_full.csv: cleaned_chr_data.R source_data/county_health_rankings_2025.csv | derived_data
	$(RSCRIPT) cleaned_chr_data.R
	
figures/missing_data.png: missing_data.R derived_data/chr_clean_full.csv | figures
	$(RSCRIPT) missing_data.R
	
figures/corr_heatmap_small.png figures/correlated_bar.png: correlated_data.R derived_data/chr_clean_full.csv | figures
	$(RSCRIPT) correlated_data.R
	
figures/clusGAP.png: kmeans_pca.R derived_data/chr_clean_full.csv | figures
	$(RSCRIPT) kmeans_pca.R

figures/le_pca.png figures/le_pca_boxplot.png figures/pca_life_expectancy.png: le_pca.R derived_data/chr_clean_full.csv | figures
	$(RSCRIPT) le_pca.R
	
figures/le_tsne.png derived_data/county_clusters_all.csv: tsne_plot.R derived_data/chr_clean_full.csv | figures derived_data
	$(RSCRIPT) tsne_plot.R

figures/state_cluster_label_distribution_pct.png: state_cluster_distribution.R derived_data/county_clusters_all.csv | figures
	$(RSCRIPT) state_cluster_distribution.R

report.html: report.Rmd \
             figures/missing_data.png \
             figures/corr_heatmap_small.png \
             figures/correlated_bar.png \
             figures/clusGAP.png \
             figures/le_pca.png \
             figures/le_pca_boxplot.png \
             figures/pca_life_expectancy.png \
             figures/le_tsne.png \
             figures/state_cluster_label_distribution_pct.png
	R -e "rmarkdown::render('report.Rmd', output_format = 'html_document')"


clean:
	rm -rf figures
	rm -rf derived_data
	rm -f report.html