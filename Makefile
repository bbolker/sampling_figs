figs_dat.rda: Results4x50.xlsx setup.R funs.R
	R CMD BATCH --vanilla setup.R

figures.Rout: figures.R funs.R figs_dat.rda
	mkdir -p figs
	R CMD BATCH --vanilla figures.R
	cd figs; for i in 1 2; do ../compress_tiff intjepi_fig_$i; done
