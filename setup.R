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
    ## exclude vanilla pops: only non-resampled results
    %>% filter(!grepl("vanilla", population))
    ## choose variables to keep
    %>% select(RR, method, n, mse, mse.rr, resamp, population)  
)

gg_sum <- (gg2
    %>% group_by(RR, n, population, resamp)
    %>% transmute( ## get rid of mse/mse.rr !
            method = method,
            rmse = sqrt(mse)/sqrt(mse[method == "Random"]),
            rmse.rr = sqrt(mse.rr)/sqrt(mse.rr[method == "Random"]))
    ##
    ## gg3 %>% filter(resamp==FALSE, n==7, RR==1, population == 0)
    ##
    %>% filter(method != "Random")
    %>% ungroup()
    ## gg3 %>% filter(resamp==FALSE, n==7, RR==1) %>% pull(rmse) %>% range()
    %>% select(-population)
    %>% pivot_longer(-c(RR, method, n, resamp), names_to="response")
    %>% group_by(RR, method, n, resamp, response)
    %>% summarise(mean = mean(value, na.rm = TRUE), stderr = se_fun(value),
                  .groups = "drop")
    %>% mutate(across(response, ~ ifelse(.=="rmse", "Prevalence", "RR")))
)

save("all_MSE","gg", "gg_sum", file=out_file)
