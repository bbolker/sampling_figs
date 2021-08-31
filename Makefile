figs_dat.rda: Results4x50.xlsx setup.R funs.R
	R CMD BATCH --vanilla setup.R

figures.Rout: figures.R funs.R figs_dat.rda
	R CMD BATCH --vanilla figures.R
