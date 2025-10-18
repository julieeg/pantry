# load required packages
library(tidyverse) ; library(table1)

#source("pantry_clean_descr.R", echo=F)


##########################################################
##  ~~~~~ Statistical analysis/summary functions ~~~~~  ##
##########################################################

# ==============================================
## Tabulate estimated marginal means
# ==============================================

get_emm.fun <- function(exposure, outcome, covars, reference, label=F, label.outcome=F, data=analysis, set.rg.limit=180000) {
  exp.dat <- data %>% dplyr::select(exp=exposure)
  dat <- data %>% mutate(exp=exp.dat$exp) ; dat$exp <- relevel(as.factor(dat$exp), ref=reference)
  mod <- lm(formula(paste0(outcome, "~", "exp", "+", covars)), dat)
  emm <- as.data.frame(emmeans(mod, ~ exp, rg.limit=set.rg.limit)) %>% mutate(outcome=outcome, .after=exp) %>% 
    mutate(n = as.vector(table(mod$model$exp)), .before=emmean)
  anv <- anova(mod) #$'Pr(>F)'[1]
  
  out<-matrix(NA, nrow(emm), 10, dimnames = list(NULL, c("outcome", "model", "exposure", "level", "n", "emmean", "SE", "df", "lowCI", "upCI")))
  out[,1]=rep(ifelse(label.outcome==F,outcome,label.outcome),nrow(out))
  out[,2]<-rep(ifelse(label==F,NA,label),nrow(out)) ; out[,3]<-rep(exposure,nrow(out))
  out[,4:10]<-as.matrix(emm[,c(1,3:8)]) ; out <- as.data.frame(out) %>% mutate(anv.p=anv[1,5])
  
  return(list(emm=as.data.frame(out), anv=anv))
}


# =======================================================================
## Run linear regression (main effects) with pairwise & F-test P-values
# =======================================================================

print_lm <- function(exposure, outcome, covariates=m, label, 
                     label.outcome=F, round=F, digits=c(3,3), lm_trend=T, data=analysis) {
  
  mod <- lm(formula(paste0(outcome, "~", exposure, "+", covariates)), data)
  if(label==F) {lab <- outcome} else {lab <- label}
  if(label.outcome==F) {lab.outcome <- outcome} else{lab.outcome <- label.outcome}
  
  # For categorical exposure variable 
  if(is.numeric(data[[exposure]]) == F) {
    nX <- length(mod$xlevels[[exposure]])
    out<-matrix(NA, nX, 9, dimnames = list(NULL, c("outcome", "model", "exposure", "n", "beta", "se", "p", "f", "f_p")))
    out[,1] <- rep(lab.outcome, nrow(out)) ; out[,2] <- rep(lab, nrow(out)) ; out[,3] <- mod$xlevels[[exposure]]
    out[2:nrow(out),c(5:7)] <- summary(mod)$coef[2:nX, c(1:2,4)] ; out[1,8:9] <-c(anova(mod)[1,4], anova(mod)[1,5])
    out[,4] <- as.vector(table(mod$model[[exposure]]))
    
    # If lm_trend should be printed
    if(lm_trend == T) {
      exposure_num <- as.numeric(data[[exposure]])
      data <- data %>% dplyr::mutate(exposure.num=exposure_num)
      lm.trend <- as.vector(summary(lm(formula(paste0(outcome, "~exposure.num+", covariates)), data))$coef[2,])
      out <- as.data.frame(out) %>% mutate(trend_p=c(lm.trend[4], rep(" ", nX-1) )) } 
  } 
  
  # For continuous exposure variable
  if(is.numeric(data[[exposure]]) == T) {
    out<-matrix(NA, 1, 7, dimnames = list(NULL, c("outcome", "model", "exposure", "n",  "beta", "se", "p")))
    out[5:7] <- summary(mod)$coef[2,c(1:2,4)]
    out[,4] <-length(mod$fitted.values) ; out[,c(1:3)] <- c(lab.outcome, lab, exposure) 
    out <- as.data.frame(out) %>% mutate(across(c("beta", "se", "p"), ~as.numeric(.)))
  }
  
  # Convert values to numeric
  out <- as.data.frame(out) %>% mutate(across(c("beta", "se"), ~as.numeric(.)))
  
  # If values should be rounded
  if(round == T) {
    out <- data.frame(out) %>%
      dplyr::mutate(across(c("beta", "se"), ~round(as.numeric(.), digits[1]))) %>%
      mutate_at("p", ~round(as.numeric(.), digits[2]) ) 
  }
  return(as.data.frame(out))
}


# ==========================================================================
## Run logistic regression (main effects) with pairwise & F-test P-values
# ==========================================================================

print_glm <- function(exposure, outcome, covariates=m, print_trend=F, label=F, data=analysis, exp=T) {
  
  mod <- glm(formula(paste0(outcome, "~", exposure, "+", covariates)), family=binomial("logit"), data) 
  mod.anv <- anova(mod)
  
  # For categorical exposure variable 
  if(is.numeric(data[, ..exposure][[1]]) == F) {
    nX <- length(mod$xlevels[[exposure]])
    out<-matrix(NA, nX, 6, dimnames = list(paste0(label, "_", mod$xlevels[[exposure]]), c("n", "beta", "se", "p", "f", "f_p")))
    out[2:nrow(out),c(2:4)] <- summary(mod)$coef[2:nX, c(1:2,4)] ; out[1,5] <- mod.anv[exposure,4] ; out[1,6] <- mod.anv[exposure,5]
    out[,1] <- as.vector(table(mod$model[[exposure]])) 
    if(print_trend == T) {
      exposure_num <- as.numeric(data[[exposure]])
      data <- data %>% dplyr::mutate(exposure.num=exposure_num)
      glm.trend <- as.vector(summary(glm(formula(paste0(outcome, "~exposure.num+", covariates)), family=binomial("logit"), data))$coef[2,])
      out <- as.data.frame(out) %>% mutate(trend_p=c(glm.trend[4], rep(" ", nX-1) )) } 
  } 
  
  # For continuous exposure variable
  if(is.numeric(data[, ..exposure][[1]]) == T) {
    out<-matrix(NA, 1, 6, dimnames = list(exposure, c("n", "beta", "se", "p", "f", "f_p")))
    out[2:4] <- summary(mod)$coef[2,c(1:2,4)]
    out[,1] <- length(mod$fitted.values) ; out[, 5] <- mod.anv[exposure, 4] ; out[, 6] <- mod.anv[exposure, 5]
    out <- as.data.frame(out) %>% mutate(across(c("beta", "se", "p"), ~as.numeric(.)))
  }
  
  #Exponentiate, if exp=T
  if(exp == T) {
    out <- out %>% as.data.frame() %>% mutate(or=ifelse(!is.na(beta), exp(beta), beta)) %>%
      mutate(or.lci = ifelse(!is.na(se), exp(beta-1.96*se), se),
             or.uci = ifelse(!is.na(se), exp(beta+1.96*se), se)) %>%
      select(n, or, or.lci, or.uci, p, f, ends_with("_p")) 
  }
  
  #Add model label 
  if(label != F) {out <- out %>% mutate(label=label, .before=n)}
  out <- out %>% mutate(exposure = gsub(".*[_]", "", rownames(.)), outcome=outcome, .after="label")
  return(as.data.frame(out))
  
}


# ==============================================================================
## Run linear regression with interaction with pairwise & F-test P-values
# ==============================================================================

print_lm_interaction <- function(exposure, interaction, outcome, 
                                 model=m, label_model=F, label_interaction=F, print_interaction_term=T, 
                                 lm_trend=T, round=T, digits=c(3,6), data) {
  
  mod.main <- lm(formula(paste0(outcome, "~", exposure, "+", interaction, "+", model)), data)
  mod.anova <- anova(mod.main)
  
  mod.int <- lm(formula(paste0(outcome, "~", exposure, "*", interaction, "+", model)), data)
  mod.int.anova <- anova(mod.int)
  
  if(label_model==F) {model_label <- outcome} ; if(label_interaction==F) {
    label_interaction <- paste0(exposure, "x", outcome) }
  
  # For categorical exposure variable 
  if(is.numeric(data[, ..exposure][[1]]) == F) {
    nExp <- length(mod$xlevels[[exposure]]) ; nInt <- length(mod$xlevels[[interaction]])
    out<-matrix(NA, nExp+nInt, 6, dimnames = list(c(mod$xlevels[[exposure]], mod$xlevels[[interaction]]), c("n", "beta", "se", "p", "f", "f_p")))
    out[2:nExp,c(2:4)] <- summary(mod)$coef[2:nExp, c(1:2,4)] ; out[1,5:6] <- c(anova(mod)[1,4], anova(mod)[1,5])
    out[(nExp+2):(nExp+nInt),c(2:4)] <- summary(mod)$coef[3:(3+nInt-2), c(1:2,4)] ; out[(nExp+1),5:6] <- c(mod.anova[2,4], mod.anova[2,5])
    out[,1] <- c(as.vector(table(mod$model[[exposure]])), as.vector(table(mod$model[[interaction]])))
    
    out <- rbind(c(rep(NA, 4), mod.anova[(nrow(mod.anova)-1),4], mod.anova[(nrow(mod.anova)-1),5]), out)
    rownames(out)[1] <- label_interaction
    
  } else {
    out<-matrix(NA, 1, 6, dimnames = list(c(exposure, interaction), c("n", "beta_main", "se_main", "p_main", "f_p_main", "beta_int", "se_int", "p_int", "f_p_main")))
    out[1,c(2:4)] <- summary(mod)$coef[2:nExp, c(1:2,4)] ; out[1,5:6] <- c(anova(mod)[1,4], anova(mod)[1,5])
    out[(nExp+2):(nExp+nInt),c(2:4)] <- summary(mod)$coef[3:(3+nInt-2), c(1:2,4)] ; out[(nExp+1),5:6] <- c(mod.anova[2,4], mod.anova[2,5])
    out[,1] <- c(as.vector(table(mod$model[[exposure]])), as.vector(table(mod$model[[interaction]])))
    
    out <- rbind(c(rep(NA, 4), mod.anova[(nrow(mod.anova)-1),4], mod.anova[(nrow(mod.anova)-1),5]), out)
    rownames(out)[1] <- label_interaction

  # If values should be rouded
  if(round == T) {
    out <- data.frame(out) %>%
      dplyr::mutate(across(c("beta", "se"), ~round(as.numeric(.), digits[1]))) %>%
      dplyr::mutate_at("p", ~round(as.numeric(.), digits[2]) ) 
  }
  
  return(as.data.frame(out))
  }
  
}


# =======================================================================
## Run linear mixed-effects model 
# =======================================================================

run_lme <- function(exposure, outcome, covariates, outcome_label=F,
                    coefficients_to_print=c("Genotype"="genotype", "Time"="time"),
                    round=F, digits=c(3,3), data_long) {
  
  # Run lme for genotype main effects
  lme_main=paste0(outcome, "~", exposure, "+", covariates, "+(1|id)")
  lme_main_summary=summary(lmerTest::lmer(formula(lme_main), data=data_long))$coef %>% as.data.frame()
  lme_main_anova=anova(lmerTest::lmer(formula(lme_main), data=data_long)) %>% as.data.frame()
  
  # Summarise selected coefficients
  coefs_main <- lme_main_summary %>% filter(grepl(paste0("^(", paste(coefficients_to_print, collapse = "|"), ")"), rownames(.)))
  coefs_anova <- lme_main_anova %>% filter(grepl(paste0("^(", paste(coefficients_to_print, collapse = "|"), ")"), rownames(.))) 
  rownames(coefs_anova) <- paste0(rownames(coefs_anova), "(joint)")    
  
  lme_summary <- as.data.frame(matrix(NA,nrow(coefs_main)+nrow(coefs_anova), 5, dimnames = list(
    c(rownames(coefs_main), rownames(coefs_anova)), c("beta", "se", "p", "anovaF", "anovaP"))))
  
  lme_summary[1:nrow(coefs_main),1:3] <- coefs_main[,c(1:2,5)] 
  lme_summary[(1+nrow(coefs_main)):nrow(lme_summary),4:5] <- coefs_anova[,5:6]
  
  # Format summary table
  lme_summary <- lme_summary %>% as.data.frame() %>%
    mutate(Effect=ifelse(grepl(":", rownames(.)), "Interaction", "Main")) %>%
    mutate(lowCI=beta-1.96*se, upCI=beta+1.96*se) %>%
    mutate(Outcome=ifelse(outcome_label==F, outcome, outcome_label)) %>%
    
    mutate(Beta.SE=sprintf("%s (%s, %s)", round(beta,digits[1]), round(lowCI,digits[1]), round(upCI,digits[1])), P=format_p(p, digits[2])) %>%
    mutate(P_signif = format_p_star(p, digits[2]), anovaP_signif=format_p_star(anovaP, digits[2]))
  
  # Add descriptive coefficient labels
  if(is.null(names(coefficients_to_print))) {coefficients_labels <- coefficients_to_print } else {
    coefficients_labels <- names(coefficients_to_print) }
  for(i in 1:length(coefficients_to_print)){
    rownames(lme_summary) <- gsub(":"," x ",gsub(coefficients_to_print[i], paste0(coefficients_labels[i], "_"), rownames(lme_summary)))
  } ; lme_summary <- lme_summary %>% mutate(Exposure=rownames(.), .before=beta)
  rownames(lme_summary) <- NULL
  
  lme_summary

}





##########################################################
##  ~~~~~ Pretty Table Summaries (for Rmarkdown) ~~~~~  ##
##########################################################

# ===================================
## Make "pretty" lm_table
# ===================================

make_pretty_lm <- function(lm_table, digits=c(3,3),show_SE=F, scientific_P=T) {
  
  # Set parameters
  d_est<-digits[1];d_pval<-digits[2]
  se<- as.numeric(lm_table$se)
  
  # Format P-value function
  format_p.fun <- function(p) {
    P<-ifelse(as.numeric(p) <0.001, format(as.numeric(p), scientific = T, digits=d_pval), 
              round(as.numeric(p), d_pval))
    return(P)
  }
  
  #Rename P-values if only p detected
  pretty_table <- lm_table %>%
    mutate(across(ends_with("_p"), ~format_p.fun(.))) %>%
    mutate(beta_se = sprintf("%s (%s, %s)", round(beta, d_est), round(beta-1.96*se, d_est), round(beta+1.96*se, d_est)))
  if("f" %in% names(pretty_table) & nrow(pretty_table) > 1) {
    pretty_table <- pretty_table %>% 
      rename(Outcome=outcome, Model=model, Exposure=exposure) %>% 
      mutate_at("p", ~format_p.fun(.)) %>%
      rename(P_t.test=p) %>%
      rename_with(~paste0("P_", gsub("[_].*", "", .), ".test"), ends_with("_p"))
  } else {
    pretty_table <- pretty_table %>% 
      mutate(Outcome=outcome, Model=model, Exposure=exposure) %>% 
      mutate_at("p", ~format_p.fun(.)) %>%
      rename(P_t.test=p) %>% mutate(N=n)
  } ; pretty_table <- pretty_table %>%
    dplyr::select(Outcome, Model, Exposure, N=n, Beta_95CI=beta_se, starts_with("P")) %>%
    mutate_all(~ifelse(is.na(.), "-", .)) %>%
    mutate_at("Beta_95CI", ~ifelse(. == "NA (NA, NA)", "-", .))
  
  if(show_SE==T) { pretty_table <- pretty_table %>% mutate(SE=ifelse(is.na(se), "-", round(se, d_pval)), .after=Beta_95CI) } else{
    pretty_table <- pretty_table
  } ; rownames(pretty_table) <- NULL
  
  return(pretty_table)
}


# ===================================
## Make "pretty" glm_table
# ===================================

make_pretty_glm <- function(glm_table, digits=c(3,3), show_SE=F, scientific_P=T) {
  
  # Set parameters
  d_est<-digits[1];d_pval<-digits[2]
  se<-as.numeric(glm_table$se)
  
  # Format P-value function
  format_p.fun <- function(p) {
    P<-ifelse(as.numeric(p) <0.001, format(as.numeric(p), scientific = T, digits=d_pval), 
              round(as.numeric(p), d_pval))
    return(P)
  }
  
  #Rename P-values if only p detected
  pretty_table <- glm_table %>%
    mutate(across(ends_with("_p"), ~format_p.fun(.))) %>%
    mutate(or_se = sprintf("%s (%s, %s)", round(or, d_est), round(or.lci, d_est), round(or.uci, d_est)))
  if("f" %in% names(pretty_table) & nrow(pretty_table) > 1) {
    pretty_table <- pretty_table %>% 
      #rename(Outcome=outcome, Model=model, Exposure=exposure) %>% 
      mutate_at("p", ~format_p.fun(.)) %>%
      rename(P_pairwise.test=p) %>%
      rename_with(~paste0("P_", gsub("[_].*", "", .), ".test"), ends_with("_p"))
  } else {
    pretty_table <- pretty_table %>% 
      mutate(Outcome=outcome, Model=model, Exposure=exposure) %>% 
      mutate_at("p", ~format_p.fun(.)) %>%
      rename(P_pairwise=p) %>% mutate(N=n)
  } ; pretty_table <- pretty_table %>%
    select(#Outcome, Model, Exposure
      Model=label, Exposure=exposure, N=n, OR_95CI=or_se, starts_with("P")) %>%
    mutate_all(~ifelse(is.na(.), "-", .)) %>%
    mutate_at("OR_95CI", ~ifelse(. == "NA (NA, NA)", "-", .))
  
  if(show_SE==T) { pretty_table <- pretty_table %>% mutate(SE=ifelse(is.na(se), "-", round(se, d_pval)), .after=Beta_95CI) } else{
    pretty_table <- pretty_table
  } ; rownames(pretty_table) <- NULL
  
  return(pretty_table)
}


##END_OF_SCRIPT