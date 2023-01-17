##########################################################################################################################################
#LCA TIPO DE VOTANTE.
#ELSOC 2022.
##########################################################################################################################################

#Clean environment, call packages, and set working directory.

rm(list=ls())
options(max.print = 200000, scipen = 100000)
pacman::p_load(readxl, data.table, lubridate, stringr, tidyverse, labelled, ggplot2, lavaan, semPlot,
               psych, poLCA, nnet, hrbrthemes, stargazer, elsoc)

Sys.setlocale("LC_ALL","ES_ES.UTF-8")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("ELSOC_Wide_2016_2022_v1.00_R.RData")

#DATABASE FOR LCA.

set.seed(1)

perfiles <- elsoc_wide_2016_2022 %>% 
  filter(tipo_atricion == 1 | tipo_atricion == 33) %>% 
  filter(c11_w03 != 3) %>% 
  dplyr::select(idencuesta, c11_w03, c43_w05, c54_w06, c50_w06, c52_w06) %>%
  filter(!is_nsnr(c11_w03, c43_w05, c54_w06, c50_w06, c52_w06)) %>%
  drop_na(c11_w03, c43_w05, c54_w06, c50_w06, c52_w06) %>%
  mutate(c11_w03 = factor(car::recode(c11_w03, "1 = 2; 2 = 1"),
                          levels = 1:2,
                          labels = c('No votó', 'Votó'))) %>%
  mutate(c43_w05 = factor(car::recode(c43_w05, "1 = 2; 2:3 = 1"),
                          levels = 1:2,
                          labels = c('No votó', 'Votó'))) %>%
  mutate(c54_w06 = factor(car::recode(c54_w06, "1 = 2; 2 = 1"),
                          levels = 1:2,
                          labels = c('No votó', 'Votó'))) %>%
  mutate(c50_w06 = factor(car::recode(c50_w06, "1 = 2; 2 = 1"),
                          levels = 1:2,
                          labels = c('No votó', 'Votó'))) %>%
  mutate(c52_w06 = factor(car::recode(c52_w06, "1 = 2; 2 = 1"),
                          levels = 1:2,
                          labels = c('No votó', 'Votó')))

#LATENT CLASS ANALYSIS.

var <- cbind(c11_w03, c43_w05, c54_w06, c50_w06, c52_w06)~1

#3 Classes.

prof_3 <- poLCA(var, perfiles, nclass=3, na.rm = T, maxiter = 5000, nrep=10)
fit_3 <- data.frame(Model=c("prof_3"),
                    BIC = prof_3$bic,
                    AIC = prof_3$aic)
perfiles$pp_3 <- as.factor(prof_3$predclass)
tab_pp_3 <- table(perfiles$pp_3)
tab_pp_3
prop.table(tab_pp_3)

#4 Classes.

prof_4 <- poLCA(var, perfiles, nclass=4, na.rm = T, maxiter = 5000, nrep=10)
fit_4 <- data.frame(Model=c("prof_4"),
                    BIC = prof_4$bic,
                    AIC = prof_4$aic)
perfiles$pp_4 <- as.factor(prof_4$predclass)
tab_pp_4 <- table(perfiles$pp_4)
tab_pp_4
prop.table(tab_pp_4)


#5 Classes.

prof_5 <- poLCA(var, perfiles, nclass=5, na.rm = T, maxiter = 5000, nrep=10)
fit_5 <- data.frame(Model=c("prof_5"),
                    BIC = prof_5$bic,
                    AIC = prof_5$aic)
perfiles$pp_5 <- as.factor(prof_5$predclass)
tab_pp_5 <- table(perfiles$pp_5)
tab_pp_5
prop.table(tab_pp_5)

#Fit.

fit <- rbind(fit_3, fit_4, fit_5)

#SAVE DATABASE.

perfiles <- perfiles %>% 
  dplyr::select(idencuesta, pp_3, pp_4, pp_5) %>%
  mutate(pp_3 = car::recode(pp_3, "3 = 1; 1 = 3"))

save(perfiles, file = "perfiles.RData")

