## three vanilla populations (three different prevalences) that had no variability in background risk across the towns.  As expected, the methods were pretty much equivalent.  We also calculated the ratio between the highest and lowest background prevalences for all the populations as a measure of spatial correlation.  I want to include something on that in the

## Would you produce the graphs as before with that ratio as the x-variable?  My hypothesis is that the lines will diverge more as the ratio increases.  If so the practical implication is that when there is a fair bit of spatial correlation you should use the Square (or Circle) method.  If there is only a little correlation, so the statistical advantage of the Square approach is smaller, then other logistical issues need to be considered.  We can note that in general, for prevalence estimation, the New EPI method is better than the OldEPI and most of its variants, but not the best approach available.

## I’m attaching the file that Patrick sent me.  You can see the ratio variable in column W in the Populations sheet.  And there are the results for the three extra population

## The graphs we need are:
## 1.      Graphs of Mean Ratios of RMSE, similar to Fig 2 in CIHR report, but with just the eight methods we’re including.
## We want them for Prevalences and RRs, for all four RRs.

##
## BMB: clean up and re-automate figure 2
##    run get_partabs with RR = 1,2, 3, then
##      select mse.rr


##  2.      Graphs as in Fig 3 in CIHR report, but separate graphs for each sample size:
## RMSE vs. Various parameters, the same ones as before and especially Prevalence ratio – want these for all 8 sheets in the Excel file, i.e., for all RR/Town sampling combinations.
## I expect the paper will just include the graphs for RR=1 using same towns for the simulations. Maybe RR=3 if any of them are at least somewhat different. The others will go into the Supplementary material.


## from lancet_final_setup.R
library(tidyverse)
theme_set(theme_bw())
library(directlabels)
source("funs.R")
L <- load("figs_dat.rda")
## prefix <- "lancet_final_fig_"
## system("rm lancet_final_fig_*.pdf")
## exclude_methods <- c("EPI3","Grid")
prefix <- "intjepi"
figpath <- "figs"

# Figure 1: RMSE vs sample size

data <- gg_sum %>% mutate(across(n, ~as.integer(as.character(.))))

plot_size <- list(width=7,height=5)
for (resp in c("Prevalence", "RR")) {
    for (focal_RR in unique(data$RR)) {
        for (do_resample in c(TRUE,FALSE)) {
            resamp_str <- if (do_resample) "resample" else "no_resample"
            fn <- sprintf("%s_SUPP_1_%s_RR%1.1f_%s.pdf",prefix,resp,focal_RR,resamp_str)
                cat(fn,"\n")
                plot_data <- filter(data, response==resp, RR==focal_RR,
                                    resamp==do_resample)
                if (exists("exclude_methods")) {
                    plot_data <- filter(plot_data, !method %in% exclude_methods)
                } else if (exists("include_methods")) {
                    plot_data <- filter(plot_data, method %in% include_methods)
                }
                g1 <- (ggplot(plot_data,aes(n, mean, colour=method))
                    + geom_point()
                    + geom_line()
                    + geom_ribbon(aes(ymin=mean-stderr,ymax=mean+stderr,fill=method),
                                  colour=NA,alpha=0.1)
                    + scale_x_continuous(breaks=unique(plot_data$n))
                    + scale_y_log10()
                    + colorspace::scale_colour_discrete_qualitative()
                    + labs(x="sample size per cluster",y="relative RMSE")
                    + geom_dl(aes(label=method),method=list(dl.trans(x = x + .3),cex=0.9,"last.bumpup"))
                    + theme(legend.position="none")
                    + expand_limits(x=32)
                    + ggtitle(sprintf("%s: RR=%1.1f",resp,focal_RR))
                )
                with(plot_size, ggsave(plot=g1, filename=fn, path=figpath, width=width, height=height))
            } ## do_resample
        } ## focal_RR
} ## resp


## exclude_methods <- c("EPI3","Grid")
plot_size <- list(width=10,height=8)
exp_lim <- 0
minval <- NA
point_alpha <- 0.05
legend_pos <- c(0.65,0.15)
use_ribbons <- FALSE
do_shape <- FALSE
## shape_vec <- c(3,  ## random
##                16:19, ## non-EPI
##                21:24) ## EPI
## mlevs <- c("Random","Square","Grid","Peri","Quad","OldEPI","NewEPI","EPI3","EPI5")
## iwh_vec <- c("#c77533","#8a66d3","#86a83c","#c85998","#60a16c","#897bbf","#c8615d","#49afcf")

## colours are (random = black + evenly spaced)
shape_vec <- c(3, ## random
               16:17, ## non-EPI
               21:22 ## EPI
               )
mlevs <- c("Random","Square","Quad","EPI","SA")
iwh_vec <- iwanthue(length(mlevs)-1)
col_vec <- c("#000000",iwh_vec)
names(col_vec) <- mlevs
names(shape_vec) <- mlevs

if (exists("exclude_methods")) {
    mlevs <- setdiff(mlevs,exclude_methods)
}
col_vec <- col_vec[mlevs]
shape_vec <- shape_vec[mlevs]
## solid for non-epi, dashed for EPI (includes SA)
lty_vec <- c("solid","11")[1+as.numeric(grepl("epi|^sa$",mlevs,ignore.case=TRUE))]

plot_list <- list()
for (focal_n in unique(gg$n)) {
    for (focal_RR in unique(gg$RR)) {
        for (focal_resamp in c(TRUE,FALSE)) {
            for (response in c("prev", "rr")) {
                focal_var <- NULL
                nval <- as.integer(as.character(focal_n))
                resamp_str <- if (focal_resamp) "resample" else "no_resample"
                fn <- sprintf("%s_SUPP_2_n%d_%s_%s_RR%1.1f.pdf",prefix,nval,resamp_str,response,focal_RR)
                ## COMMENT ME OUT!
                ## focal_n <- 7; focal_RR <- 1; focal_resamp <- TRUE; response <- "prev"
                ## focal_var <- "p_ratio"
                gg3 <- mkfig(focal_n, focal_RR, focal_resamp, response, focal_var=NULL)
                plot_list[[fn]] <- gg3
                with(plot_size, ggsave(plot=gg3, path=figpath, filename=fn, height=height, width=width))
            }
        }
    }
}

## new figure 1

### 
## exclude_methods <- c("EPI3","Grid")
data <- gg_sum
plot_size <- list(width=7,height=5)
focal_RR <- 1.0
do_resample <- FALSE
plot_data <- filter(data,
                    ## response==resp,
                    RR==focal_RR,
                    resamp==do_resample) %>%
    mutate(across(n, ~as.numeric(as.character(.))))
if (exists("exclude_methods")) {
    plot_data <- filter(plot_data, !method %in% exclude_methods)
} else if (exists("include_methods")) {
    plot_data <- filter(plot_data, method %in% include_methods)
}
plot_data <- plot_data %>%
    mutate(across("response",
              ~factor(.,
                      levels=c("Prevalence","RR"),
                      labels=c("(a) estimating prevalence",
                               "(b) estimating relative risk"))))

g1 <- (ggplot(plot_data,aes(n,mean,colour=method))
    + geom_point()
    + geom_line()
    + geom_ribbon(aes(ymin=mean-stderr,ymax=mean+stderr,fill=method),
                  colour=NA,alpha=0.1)
    + scale_x_continuous(breaks=unique(plot_data$n))
    + scale_y_log10()
    + colorspace::scale_colour_discrete_qualitative()
    + labs(x="Sample size per cluster",y="Mean RMSE ratio")
    + geom_dl(aes(label=method),method=list(dl.trans(x = x + .3),cex=0.9,"last.bumpup"))
    + theme(legend.position="none")
    + expand_limits(x=36)
    + facet_wrap(~response)
    + theme(panel.spacing=grid::unit(0,"lines"))
)


fn <- sprintf("%s_fig_1.pdf", prefix)
with(plot_size,ggsave(plot=g1, filename=fn, path=figpath, width=width,height=height))

## figure 2

## exclude_methods <- c("EPI3","Grid")
focal_RR <- 1.0
focal_resamp <- FALSE
mlevs <- all_methods

plot_data <- (gg
    %>% filter(RR==focal_RR,resamp==focal_resamp)
    %>% mutate(method=factor(method,levels=mlevs)
             , epi=grepl("EPI",method))
)

if (exists("exclude_methods")) {
    mlevs <- setdiff(mlevs, exclude_methods)
    plot_data <- plot_data %>% filter(!method %in%  exclude_methods)

}

plot_data_long <- (plot_data
    %>% select(all_of(c("mse","mse.rr","true_mean",
                        "method","epi","n")))
    %>% pivot_longer(cols=starts_with("mse"))
    %>% mutate_at("name",
                  ~ case_when(. == "mse" ~ "prevalence",
                              . == "mse.rr" ~ "relative risk"))
    %>% rename(response="name")
    %>% mutate(rmse = sqrt(value))
)

minval <- NA
point_alpha <- 0.2
use_ribbons <- FALSE

gg1 <- (ggplot(plot_data_long, aes(true_mean, rmse,
                                   colour=method,
                                   label=method))
    + scale_x_log10()
    + scale_y_log10(limits=c(minval,NA), oob=scales::squish)
    + scale_colour_manual(values=col_vec,
                          guide=guide_legend(reverse=TRUE)
                          )
    + facet_grid(response~n,scale="free_y",
                 labeller =labeller(n=function(s) 
                     sprintf("n=%s per cluster",s),
                     response=function(s)
                         sprintf("RMSE when\nestimating\n%s",s)),
                 switch="y")
    ## 
    + theme(strip.placement = "outside",
            strip.text.y.left=element_text(angle=0),
            panel.spacing=grid::unit(0,"lines"))
    + labs(x="True prevalence for each population",
           y="")
    + geom_point(alpha=point_alpha)
    + geom_smooth(alpha=0.1,span=0.9, se=use_ribbons,
                  ## data=subset(data,p_ratio>1),
                  aes(linetype=method),
                  method="loess",
                  formula=y~x)
    + scale_linetype_manual(values=lty_vec,
                            guide=guide_legend(reverse=TRUE)
                            )
)

fn <- sprintf("%s_fig_2.pdf", prefix)
with(plot_size,ggsave(plot=gg1, filename=fn, path=figpath, width=width,height=height))

zipname <- sprintf("%s_figs_%s.zip",prefix,format(Sys.time(),"%Y%m%d"))
system(sprintf("zip  %s %s/%s_*.pdf", zipname, figpath, prefix))
if (interactive()) {
    cat("copying to ms\n")
    system(sprintf("scp %s ms.mcmaster.ca:~/public_html/misc/",zipname))
}
