library(tidyverse)
library(brms)
library(ggplot2)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

playback = read.table("Data_psychoacoustic_experiment.csv",sep=";",dec=",",header=TRUE)
playback <- playback %>% mutate_if(is.character,as.factor)

obs = aggregate(rated_baby_status_binom ~ real_baby_status + sex + parentality + subjectID, playback, mean) 
obs$rated_baby_status_binom = obs$rated_baby_status_binom * 100

baby_recognition <- brm(rated_baby_status_binom ~ real_baby_status * parentality * sex + (real_baby_status + parentality|babyID_ref) + (real_baby_status|subjectID), 
                        data = playback, family = 'bernoulli',  chains = 2, cores = 2, warmup = 1000, iter = 3000,init_r = 0.1, control = list(adapt_delta = 0.99),
                        prior = c(set_prior('normal(0, 1)'), set_prior('normal(-1.386, 1)', class = 'Intercept')  # log(.2/.8) = -1.386 (chance at 1/5)
                        ))
summary(baby_recognition)
pp_check(baby_recognition)
conditional_effects(baby_recognition)

# Model predictions
newdata_baby_recognition = expand.grid(real_baby_status = levels(playback$real_baby_status),parentality = levels(playback$parentality),
                                       sex = levels(playback$sex))
fit_baby_recognition = fitted(baby_recognition, newdata = newdata_baby_recognition, robust = TRUE, re_formula = NA) * 100  # *100 = convert to %
colnames(fit_baby_recognition) = c('fit', 'se', 'lwr', 'upr')
pred_baby_recognition = cbind(newdata_baby_recognition, fit_baby_recognition)

sex.labs <- c("Women", "Men")
names(sex.labs) <- c("female", "male")

parentality.labs <- c("Non parent", "Parent")
names(parentality.labs) <- c("nonparent", "parent")

plot_test <- ggplot(pred_baby_recognition, aes(x = real_baby_status, y = fit/100, color=real_baby_status)) +
  geom_violin(data=obs, aes(x=real_baby_status, y=rated_baby_status_binom/100), alpha=0.3, color="gray70", fill='gray95') +
  geom_jitter(data=obs, aes(x=real_baby_status, y=rated_baby_status_binom/100), alpha=0.3, position = position_jitter(width = 0.07)) +
  geom_pointrange(aes(ymin=lwr/100, ymax=upr/100), position = position_dodge(.3), size=1) +
  geom_hline(yintercept=0.20, linetype=2, linewidth=0.8) +
  xlab("") +
  ylab("") +
  facet_grid(rows=vars(sex), cols=vars(parentality), labeller = labeller(sex=sex.labs, parentality=parentality.labs))+
  theme_classic()+
  scale_color_manual(values=c("blue4","chocolate2"))+
  scale_y_continuous(labels=label_percent(accuracy=1), limits=c(-0.05, 1.05))+
  theme(axis.text.y = element_text(size=14, color="black"), 
        axis.text.x = element_text(size=12, color="black"),
        axis.title.y = element_text(angle=0))+
  scale_x_discrete(labels=c("familiar" = "familiar \n baby", "unknown" = "unknown \n babies"))+
  theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14, angle=0),
        strip.background = element_rect(color="white", linewidth=1.5, linetype="solid"))
plot_test


# Full MCMC results
contrast = as.data.frame(fitted(baby_recognition, newdata = newdata_baby_recognition, re_formula = NA, summary = FALSE)) * 100
colnames(contrast) = c("familiar_NP_fem", "unknown_NP_fem", "familiar_P_fem", "unknown_P_fem",
                       "familiar_NP_mal", "unknown_NP_mal", "familiar_P_mal", "unknown_P_mal")


# Contrasts : credible interval (median of posterior distribution in %)
## Are familiar babies better recognize than chance ?
reco_familiar = rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar')])
quantile(reco_familiar, probs = c(.5, .025, .975))
mean(reco_familiar > 20)

## Are unknown babies better recognize than chance ?
reco_unknown = rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'unknown')])
quantile(reco_unknown, probs = c(.5, .025, .975))
mean(reco_unknown > 20)

## Are familiar babies successfully recognized compared to unknown ones ?
familiar_vs_unknown = rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar')]) - 
                       rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'unknown')])
quantile(familiar_vs_unknown, probs = c(.5, .025, .975))
mean(familiar_vs_unknown > 0)

## Are parents better than non parents at recognizing their familiar baby ?
parents_vs_np = rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar' & newdata_baby_recognition$parentality == 'parent')]) - 
                rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar' & newdata_baby_recognition$parentality == 'nonparent')])
quantile(parents_vs_np, probs = c(.5, .025, .975))
mean(parents_vs_np < 0)

## Are females better than males at recognizing their familiar baby ?
females_vs_males = rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar' & newdata_baby_recognition$sex == 'female')]) - 
                   rowMeans(contrast[, which(newdata_baby_recognition$real_baby_status == 'familiar' & newdata_baby_recognition$sex == 'male')])
quantile(females_vs_males, probs = c(.5, .025, .975))
mean(females_vs_males < 0)

## Are dads better than moms regarding the false-positive rate ?
str_moms_vs_dads = contrast$unknown_P_fem - contrast$unknown_P_mal
quantile(str_moms_vs_dads, probs = c(.5, .025, .975))
mean(str_moms_vs_dads > 0)

## Are dads better than non-parent men regarding the false-positive rate ?
str_NPmen_vs_dads = contrast$unknown_NP_mal - contrast$unknown_P_mal
quantile(str_NPmen_vs_dads, probs = c(.5, .025, .975))
mean(str_NPmen_vs_dads > 0)