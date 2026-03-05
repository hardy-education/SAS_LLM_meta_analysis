library(tidyverse)
library(janitor)
library(psych)
library(modelsummary)
library(lme4)
library(lmerTest)
library(here)
library(tictoc)
library(broom)
library(broom.helpers)
library(broom.mixed)
library(magrittr)

sas = read_csv(here("ASAP_SAS_metastudy_models.csv"))


# paper box plot --------
sas |> 
  mutate(item = factor(item),
         subj = if_else(read==1,"Reading","Science")) |> 
  ggplot(aes(x=item,y=qwk)) + 
  geom_boxplot(aes(color=subj,group=interaction(item,subj))) + 
  geom_boxplot(aes(x=item,y=humqwk,group=item,linetype = "Human"), data = sas) + 
  theme_minimal() +
  labs(x="ASAP SAS Item Number",y="LLM-Human QWK",color="Subject Area",
       linetype = "Human-Human QWK"
  ) +
  theme(text = element_text(family = "times",size = 14)) +
  scale_color_manual(values = c("darkblue","coral"))

# Main body 6 fits ---------

m1 = lmer(qwk ~0+ token
          + humqwk #
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|implementation)          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)

m2 = lmer(qwk ~0+ token
          + humqwk 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|study)
          + (1|model)
          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              item = as_factor(item),
              humqwk = psych::fisherz(humqwk)),
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)

m3 = lmer(qwk ~ 0 + token
          + humqwk # 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)

m4 = lmer(qwk ~0+ token
          + humqwk # 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training)
          + (1|item:study)
          + (1|item:model)
          + (1|item:training)
          + (1|model:training)
          + (1|model:study)
          + (1|training:study)
          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)


m5 = lmer(qwk ~0+ token
          + humqwk # 0 + factor(item) + 
          + read 
          + vocab 
          + gpt
          # + read:gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training)
          + (1|item:study)
          + (1|item:model)
          + (1|item:training)
          + (1|model:training)
          + (1|model:study)
          + (1|training:study)
          + (1|model:training:item)
          + (1|model:training:study)
          + (1|training:item:study)
          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
)

m6 = lmer(qwk ~0+ token
          + humqwk # 0 + factor(item) + 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (0+item|model) 
          + (0+item|study) 
          + (0+item|training)           
          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 9e5))
          
          
)




## Main body model 6 Bayesian -----
brf = brm(qwk ~0+ token
          + humqwk
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (0+item|model) 
          + (0+item|study) 
          + (0+item|training)
          ,
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              item = as_factor(item),
              humqwk = psych::fisherz(humqwk))
          ,
          control = list(adapt_delta = 0.95, max_treedepth = 15),
          iter = 4000,
          warmup = 1000,
          chains = 5,
          cores = 5,
          backend = "cmdstanr",
          stan_model_args=list(stanc_options = list("O1")) ,
          save_pars = save_pars(all=T),
          thin = 5,
          threads = threading(4))


## Summary functions -----


myfmtfun = function(x) sprintf("%.2f",psych::fisherz2r(x))

modelsummary::modelsummary(named_list(n4,n2,m5,m9aa,m9aaa,m9i,brf),
                           stars = T, statistic = NULL, 
                           fmt=myfmtfun, 
                           estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
                           coef_rename = function(x) if_else(x == "b_IvocabE2", 
                                                             "I(vocab^2)",
                                                             str_remove(x,"b_")), # coef_omit = c("Cor|cor_|SD|sd_"),
                           digits = 2)  



# Appendix additional 9 fits ---------




m1 = lmer(qwk ~ 1 
          + humqwk 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m1 |> summary()

m2 = lmer(qwk ~ 1 
          + humqwk
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m2 |> summary()



m3 = lmer(qwk ~ 1 
          + humqwk 
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m3 |> summary()


m4 = lmer(qwk ~ 1
          + humqwk 
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m4 |> summary()

m5 = lmer(qwk ~ 0 + token
          + humqwk 
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (1|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m5 |> summary()



m6 = lmer(qwk ~ token
          + humqwk 
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + logsize
          + (1|item)
          + (1|model) 
          + (1|study) 
          + (gpt+logsize|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m6 |> summary()


m7 = lmer(qwk ~0+ token
          + humqwk 
          + read 
          + vocab 
          + gpt
          + I(vocab^2)
          + logsize
          + (0+token|item)
          + (1|model) 
          + (1|study) 
          + (gpt+logsize|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m7 |> summary()





m8 = lmer(qwk ~0+ token
          + humqwk
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + logsize
          + (0+token|item)
          + (1|model) 
          + (1|study) 
          + (gpt+logsize|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m8 |> summary()



m9 = lmer(qwk ~0+ token
          + humqwk 
          + read 
          + vocab 
          + gpt
          + read:gpt
          + I(vocab^2)
          + logsize
          + (0+token|item)
          + (1|model) 
          + (gpt|study) 
          + (gpt+logsize|training),
          data =sas |> 
            mutate(
              qwk = psych::fisherz(qwk),
              humqwk = psych::fisherz(humqwk))
          ,
          REML = FALSE,
          control = lmerControl(optimizer = "bobyqa", 
                                calc.derivs = FALSE,
                                optCtrl = list(maxfun = 3e5))
          
          
)
m9 |> summary()

# Anova Table
anova(m1,m2,m3,m4,m5,m6,m7,m8,m9) |> tidy() |> kableExtra::kable(format = "latex", booktabs=T,digits = 2) |> kableExtra::kable_styling(latex_options = "scale_down")




myfmtfun = function(x) sprintf("%.2f",psych::fisherz2r(x))

## Table
modelsummary::modelsummary(named_list(m1,m2,m3,m4,m5,m6,m7,m8,m9),stars = T,
                           fmt=myfmtfun,output = "kableExtra",
                           statistic = "conf.int",
                           format = "latex",booktabs=T,digits = 2)  |> 
  kableExtra::kable_styling(latex_options = "scale_down")

## R2 (manual entry because the summary above didn't work)
MuMIn::r.squaredGLMM(m6)
MuMIn::r.squaredGLMM(m7)
MuMIn::r.squaredGLMM(m8)
MuMIn::r.squaredGLMM(m9)
