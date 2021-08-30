library(tidyverse)
source("funs.R")
## source_file <- "run1and2REV.xlsx"  (n_table=8)
source_file <- "Results4x50.xlsx"
out_file <- "figs_dat.rda"
n_tab <- 4

new_MSE <- get_all_MSEtab(fn=source_file,
                          n_table=n_tab, debug=TRUE,
                          sub_names=FALSE)

new_MSE_r <- get_all_MSEtab(fn=source_file,
                          n_table=n_tab, debug=TRUE,
                          sub_names=FALSE,
                          resamp_data=TRUE)

all_MSE <- (bind_rows(list("FALSE"=new_MSE,"TRUE"=new_MSE_r),
                      .id="resample")
    %>% mutate_at("resample", as.logical)
)


RR_vec <- unique(new_MSE$RR)
## debug(get_partabs)
gg <- (map_dfr(c(TRUE,FALSE),
              function(resamp) {
                  map_dfr(setNames(RR_vec,RR_vec),
                          ~get_partabs(npops=53,
                                       fn=source_file,
                                       sub_names=FALSE,
                                       n_table=n_tab,
                                       resamp_data=resamp,
                                       RR=.),
                          .id="RR")
              },
              .id="resamp")
    %>% mutate_at("resamp",~.=="1")
    %>% mutate_at("RR",as.numeric)
    %>% rename(p_ratio="Prob-ratio")
    %>% mutate_at("n",factor)
    %>% mutate_at("pocket_scaling", ~suppressWarnings(as.numeric(.)))  ## drop 'n/a' - should do upstream
)

## checking code: we do seem to have vanilla1 here ..
## gg %>% filter(RR==1,population=="vanilla-1", resamp==FALSE, n==7) %>% select(method,mse,p_ratio)

gg2 <- (gg
    %>% filter(!grepl("vanilla", population),   ## exclude vanilla pops: only non-resampled results
               !resamp)
    %>% select(RR, method, n, true_mean, mean, population)  ## choose variables to keep
    %>% group_by(RR, n, method)
    ## -> 3000 rows (RR(4) x n(3) x method(5) x population(50)
    %>% summarise(mse = mean((mean-true_mean)^2), .groups = "drop_last") ## compute MSE across populations
    %>% mutate(mse_ratio = mse/mse[method=="Random"])
    %>% filter(method != "Random")
)
gg2 %>% filter(RR==1, n== 7)


gg0 <- gg %>% filter(RR==1) %>%
    mutate(err = mean-true_mean,
           abs_err = abs(err),
           method = reorder(factor(method), X=abs_err, FUN = function(x) mean(x, na.rm=TRUE)))

gg0 %>% group_by(method) %>% summarise(m = mean(abs_err))
ggplot(gg0,
       aes(x = method, y = err)) + facet_wrap(~n) + geom_boxplot()

save("all_MSE","gg", file=out_file)
