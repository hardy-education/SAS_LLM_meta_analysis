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




# simple fits ---------




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
