# load required packages
library(tidyverse) ; library(table1)



#################################################
## Custom color palettes & pre-set plot themes ##  
#################################################

# =================================
## Build pre-set color palettes 
# =================================
library("paletteer") ; library("RColorBrewer")

palettes <- list(NatComms= paletteer_d("ggsci::nrc_npg", n=10),
                 
                 NatMainAccent3=list(
                   Level1=c("#691814", "#0A2256", "#66551C"),
                   Level2=c("#8F2F24", "#00488D", "#957628"),
                   Level3=c("#B64342", "#2E6CA9", "#C39D3F"),
                   Level4=c("#DC6464", "#6494C9", "#E3C663"),
                   Level5=c("#D79EA0", "#A2C6E2", "#E8D48C")),
                 NatMainAccent_L2 = c("#8F2F24", "#00488D", "#957628"),
                 
                 NatMainBackground2=list(
                   Level1=c("#1B2B43", "#5E5948"),
                   Level2=c("#465267", "#888363"),
                   Level3=c("#6D788D", "#A5A083"),
                   Level4=c("#96A0B3", "#C5C1A5"),
                   Level5=c("#C6CAD6", "#DFDCCC")),
                 NatMainBackground2x2=c("#888363","#C5C1A5", "#96A0B3", "#435269"),
                 
                 NatExt=list(
                   Oranges=c("#793011", "#A6501E", "#E96900", "#F29741", "#FBBD7D", "#EBD3B8"),
                   Blues=c("#0A2256", "#1D4884", "#006EAE", "#5496CE", "#A5C9E6", "#C6DAE3"),
                   Purples=c("#3D104B", "#672668", "#A64791", "#B778B4", "#CCABCC", "#E0D0E3"),
                   Greens=c("#1A361A", "#356932","#429130", "#73B152", "#A9C981", "#C9D2B8")),
                 
                 #single color ramp palettes
                 greens5 = paletteer_dynamic("cartography::green.pal", 5),
                 greens = rev(paletteer_dynamic("cartography::green.pal", 10)),
                 oranges = rev(paletteer_dynamic("cartography::orange.pal", 10)),
                 blues = rev(paletteer_dynamic("cartography::blue.pal", 10)),
                 purples=rev(paletteer_dynamic("cartography::purple.pal", 10))[3:10],
                 
                 # Diverging palettes (heatmaps)
                 blured_bin = c("#5496CE", "#DC6464"),
                 hm_palette_rwb = colorRampPalette(c("#00488D", "white", "#8F2F24"))(n=100),
                 hm_palette_gwp = colorRampPalette(c("#A64791", "white", "#429130"), space="rgb")(n=100),
                 hm_palette_gwp_custom = circlize::colorRamp2(c(-5, 0, 5), c("#A64791", "white", "#429130")),
                 hm_corr_blue = colorRampPalette(c("white", "#CAE5EF", "#96CED4", "#48BDBC", "#0096A0"))(n=100),
                 
                 #Binary codings
                 plaus_24hr=c("forestgreen", "grey38"), has_24hr=c("forestgreen", "grey38")
)


# ==============
## basic ggplot 
# ==============

ggtheme_basic <- theme(
  axis.text = element_text(color="black", size=10), 
  axis.title.y = element_text(color="black", size=12, vjust=-0.5),
  axis.title.x = element_text(color="black", size=12, vjust=-0.5),
  legend.position = "bottom", 
  legend.box.background = element_rect(color = "black"),
  legend.key.size = unit(0.5, 'line'),
  legend.margin = margin(0.5,0.5,0.5,0.5, unit="pt"),
  legend.text = element_text(size=8), 
  legend.title = element_text(face="bold", size=8),
  plot.title=element_text(size=10),
  strip.text = element_text(face="bold", size=10))


# =============================
## basic ggplot (black/white)
# =============================

ggtheme_basic_bw <- theme_bw() + theme(
  axis.text = element_text(color="black", size=10), 
  axis.title.y = element_text(color="black", size=9, face="bold"),
  axis.text.y=element_text(size=7),
  axis.text.x = element_blank(), axis.ticks.x=element_blank(),
  axis.title.x = element_text(color="black", size=12, vjust=-0.5),
  panel.grid.minor = element_blank(), 
  panel.grid.major.x = element_blank(),
  legend.position = "top", 
  legend.box.background = element_rect(color = "black"),
  legend.key.size = unit(0.5, 'line'),
  legend.margin = margin(0.5,0.5,0.5,0.5, unit="pt"),
  legend.text = element_text(size=8), 
  legend.title = element_text(face="bold", size=8),
  plot.title=element_text(size=10),
  strip.background = element_blank(), 
  strip.text = element_text(size=8, face="plain"))


# =============================
## Fancy ggplot theme 
# =============================

ggtheme_fancy <- theme_bw() + 
  theme(panel.grid.minor.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(color="black", size=8), 
        axis.text.y = element_text(color="black", size=8),
        axis.title = element_text(color="black", size=10),
        legend.position = "right", 
        legend.box.background = element_rect(color = "black"),
        # legend.key.size = unit(0.35, 'line'),
        legend.key.size = unit(0.5, 'line'),
        legend.margin = margin(0.5,0.5,0.5,0.5, unit="pt"),
        legend.text = element_text(size=8), 
        legend.title = element_text(face="bold", size=8),
        plot.title=element_text(size=8),
        strip.text = element_text(face="bold", size=8)
  )


ggtheme_emm <- theme(axis.title.y = element_text(size=11),
                     axis.title.x = element_text(size=10, vjust=0.5),
                     axis.text.y = element_text(size=11), axis.text.x = element_text(size=10),
                     panel.background = element_rect(fill="#00000008"),
                     panel.border = element_rect(color="black", fill=NA),
                     legend.position = "right", legend.text = element_text(size=9),
                     legend.title = element_text(face="bold", size=10))

######################################
##  ~~~~~ Plotting functions ~~~~~  ##
######################################

# =====================================
## Waterfall plots (of PCA outputs)
# =====================================

plot_PC_waterfall.fun <- function(PC.df, nPCs=10) {
  PC.df %>%
    select(Diet, c(paste0("PC", 1:nPCs))) %>%
    mutate(Diet=factor(Diet, levels=Diet[order(PC1, decreasing=T)])) %>%
    pivot_longer(-Diet) %>% 
    mutate(name=factor(name, levels=c(paste0("PC", 1:nPCs)), labels=c(paste0("Diet PC", 1:nPCs)) )) %>%
    mutate(direction = factor(ifelse(value>0.2, "Positive/Major", ifelse(value<0.2 & value>0, "Positive/Minor", 
                                                                         ifelse(value<0 & value>-0.2, "Negative/Minor", "Negative/Major"))),
                              levels=c("Positive/Major", "Positive/Minor", "Negative/Minor", "Negative/Major"))) %>%
    ggplot(aes(x=value, y=Diet, fill=direction)) + 
    facet_wrap(~name, ncol=nPCs/2) + 
    geom_col() + ylab("") + xlab("Rotated Factor Loadings") +
    geom_vline(xintercept = c(-0.2, 0.2), color = "black", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black") +
    scale_fill_manual(values=palettes$nature_main4, name = "Factor Loadings") +
    
    ## waterfall plot theme
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(color="black"),
          legend.position = "top",
          strip.text = element_text(face="bold", size=6),
          axis.text.y=element_text(size=6),
          axis.text.x=element_text(size=6),
          axis.title.x = element_text(size=6),
          legend.key.size = unit(0.25,"cm"))
}




##############################
##  ~~~~  Basic plots  ~~~~ ##
##############################

# =================
## ggplot themes
# =================

# set ggplot theme standards
ggplot_theme_standard_continuous <- theme_bw() + theme(
  axis.text.x = element_text(size=10, vjust=0.65, color = "black"),
  axis.text.y = element_text(size=10, color="black"), 
  strip.text=element_text(size=8, face="bold"),
  axis.title = element_text(size=10, color = "black"))

ggplot_theme_standard_categorical <- theme_bw() + theme(
  axis.text.x = element_text(size=10, color = "black", angle=30, hjust=0.9),
  axis.text.y = element_text(size=10, color="black"), 
  strip.text=element_text(size=8, face="bold"),
  axis.title = element_text(size=10, color = "black"))



# ==========================
## basic descriptive plots 
# ==========================

plot_continuous <- function(cont_var, data=analysis) {
  data %>% select(var=all_of(cont_var)) %>% filter(!is.na(var)) %>%
    ggplot(aes(x=var)) + geom_histogram(bins=30) +
    labs(title=cont_var, x=cont_var, y="frequency") + 
    theme(plot.title = element_text(face="bold", size=8)) #+
  #ggplot_theme_standard_continuous
}

plot_categorical <- function(cat_var, data=analysis) {
  data %>% 
    select(var=all_of(cat_var)) %>% filter(!is.na(var)) %>%
    ggplot(aes(x=factor(var))) + geom_bar(stat="count") +
    labs(title=cat_var, x=cat_var) + xlab("") +
    theme(axis.text.x = element_text(angle=35, hjust=0.75, size=7)) +
    theme(plot.title = element_text(face="bold", size=8))
  #ggplot_theme_standard_categorical
}



# ==================================
## Function to extend plot limits 
# ==================================

ggplot_extend_ylim <- function(plot, y_max_extend=0.1, y_min_extend=0) {
  
  # Get current ylimits
  current_ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
  
  # Calculate extension amount based on current plot max & y_max_extend
  extended_ymax <- current_ylims[2] + (y_max_extend * current_ylims[2])
  extended_ymin <- current_ylims[1] + (y_min_extend * current_ylims[1])
  
  # Update the plot with the extended upper limit
  plot <- plot + expand_limits(y = c(extended_ymin, extended_ymax))
  plot
}



## END_OF_SCRIPT
