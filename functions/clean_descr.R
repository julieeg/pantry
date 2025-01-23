# load required packages
library(tidyverse) ; library(table1)



###########################################################
## ~~~~~~~~ Data Renaming & Cleaning Functions ~~~~~~~~ ##
###########################################################

# ======================================================
## Add descriptive labels (option to set factor order)
# ======================================================

#e.g., labs_vals = female_labs <- list("Female" = 0, "Male" = 1)
add_descr_labels <- function(data, base_var, labs_vals, ordered = T) {
  base <- data %>% select(all_of(base_var)) 
  x <- rep(NA, length(base))
  for(i in 1:length(labs_vals)) {
    x[base == labs_vals[i] ] <- names(labs_vals)[i]
  } ; if(ordered == T) {
    x <- factor(x, levels=names(labs_vals)) 
  } ; return(x)
}



########################################
##  ~~~~ Outliers & Missing data ~~~~ ##
########################################

# ========================
## Remove outliers by SD
# ========================

outliers <- function(x, SDs=5, rule, recode_find=c(1)) {
  bounds <- mean(x, na.rm=T) + SDs * c(-1, 1) * sd(x, na.rm=T)
  if(rule=="remove") {
    x <- ifelse(x>bounds[1] & x<bounds[2], x, NA) } else 
      if(rule == "find") {
        x <- as.factor(ifelse(x>bounds[1] & x<bounds[2], 0, recode_find[1], recode_find[2]))
        }
  x
}


# =============================
## Flag missing values as 1/0
# =============================

recode_na <- function(x, recode_as=0) {
  x <- ifelse(is.na(x) == T, recode_as, 1)
  x
}


# ================================================
## Median impute for negative or missing values
# ================================================
median_imp_ukb <- function(x) {
  x.new <- ifelse(x == -1 | x == -3 | x == -9 | is.na(x) == T, median(x, na.rm=T), x)
  return(x.new)
}


# ===================
## Calculate zscore
# ===================
zscore <- function(x) {
  z<-((x - mean(x, na.rm=T)) / sd(x, na.rm=T))
  return(z)
}


# ========================
## Winsorize data by SD
# ========================
winsorize <- function(x, SDs=5) {
  bounds <- mean(x, na.rm=T) + SDs * c(-1, 1) * sd(x, na.rm=T)
  x <- ifelse(x<bounds[1], bounds[1], ifelse(x>bounds[2], bounds[2], x))
  x
}



##############################
##  ~~~~ For "Table 1" ~~~~ ##
##############################

# ======================================
## Print continuous vars as mean +- SD 
# ======================================
mean_sd <-function(x, d=2) {
  sprintf("%s \u00B1 %s", round(mean(x, na.rm=T), digits = d), 
          round(sd(x, na.rm=T), digits = d))
}


# ==================================
## Print categorical vars as n (%)
# ==================================
n_pct <- function(x, level=F, d=1) {
  if(level==F) {
    sapply(as.list(names(table(x))), function(lvl) {
      paste0(lvl, ", ", sum(x == lvl, na.rm=T), " (", round(sum(x == lvl, na.rm=T)/n()*100, d), "%)") }) } 
  else{paste0(sum(x == level, na.rm=T), " (", round(sum(x == level, na.rm=T)/n()*100, d), "%)")}
}


# =====================================================
## Print continuous vars as median [25th, 75th %tile]
# =====================================================
median_25to75 <- function(x, d=2) {
  qs<-round(quantile(x, breaks=seq(0,1,0.25), na.rm=T), d)
  sprintf("%s [%s, %s]", round(median(x, na.rm=T), digits = d), 
          qs[[2]], qs[[4]])
}


# ======================================================
## Print P-values as rounded or scientific (if <0.01)
# ======================================================
format_p <- function(p, digits=3) {
  ifelse(p<0.01, format(p, scientific=T, digits=2), round(p, digits))
}


## END_OF_SCRIPT

