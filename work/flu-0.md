# Working analysis: survey weights




```r
load("clean/cleaning2.RData")
data <- data2
names(data) <- old_name
rm(data2)
```

> Load some recoded variables.


```r
#source(recoding.R)
load("clean/recoding1.RData")
df <- datar
#tail(df)
```

> Create a survey object with weights.


```r
# survey object
options(digits = 4)
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~1, weights = ~weight, data = data[is.na(data$weight) == F, ])
#summary(des)
```

## Q1. Before receiving this survey, did you know influenza is different from the stomach flu?

> Comparing sex ratios in the unweighted and weighted data frames for Q1.


```r
with(data, addmargins(table(Q1, PPGENDER)))  # unweighted Q1
```

```
##      PPGENDER
## Q1    Female Male  Sum
##   Yes    888  776 1664
##   No     205  283  488
##   Sum   1093 1059 2152
```

```r
svytable(~Q1 + PPGENDER, design = des, round = T)  # weighted Q1
```

```
##      PPGENDER
## Q1    Female Male
##   Yes    887  725
##   No     231  304
```

```r
with(data, prop.table(table(Q1, PPGENDER), margin = 1))  # unweighted prop
```

```
##      PPGENDER
## Q1    Female   Male
##   Yes 0.5337 0.4663
##   No  0.4201 0.5799
```

```r
prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 1)  # weighted prop
```

```
##      PPGENDER
## Q1    Female   Male
##   Yes 0.5502 0.4498
##   No  0.4312 0.5688
```

In the unweighted data frame, more females (53.37%) answered Yes to Q1 than males (46.63%); more males (57.99%) answered No compared to females (42.01%).  
In the weighted data frame, 55.02% of females answered Yes; males = 44.98%; more males (56.88%) answered No compared to females (43.12%).  


```r
# table above with standard errors
svyby(formula = ~Q1, by = ~PPGENDER, design = des, FUN = svymean, na.rm = T)
```

```
##        PPGENDER  Q1Yes   Q1No se.Q1Yes se.Q1No
## Female   Female 0.7937 0.2063  0.01373 0.01373
## Male       Male 0.7045 0.2955  0.01562 0.01562
```

```r
# prop table with SE, by gender + race
svyby(formula = ~Q1, by = ~PPGENDER + PPETHM, design = des, FUN = svymean, na.rm = T)
```

```
##                               PPGENDER                 PPETHM  Q1Yes
## Female.White, Non-Hispanic      Female    White, Non-Hispanic 0.8249
## Male.White, Non-Hispanic          Male    White, Non-Hispanic 0.7287
## Female.Black, Non-Hispanic      Female    Black, Non-Hispanic 0.7116
## Male.Black, Non-Hispanic          Male    Black, Non-Hispanic 0.7143
## Female.Hispanic                 Female               Hispanic 0.7555
## Male.Hispanic                     Male               Hispanic 0.6242
## Female.Other, Non-Hispanic      Female    Other, Non-Hispanic 0.7192
## Male.Other, Non-Hispanic          Male    Other, Non-Hispanic 0.6496
## Female.2+ Races, Non-Hispanic   Female 2+ Races, Non-Hispanic 0.7719
## Male.2+ Races, Non-Hispanic       Male 2+ Races, Non-Hispanic 0.7314
##                                 Q1No se.Q1Yes se.Q1No
## Female.White, Non-Hispanic    0.1751  0.01438 0.01438
## Male.White, Non-Hispanic      0.2713  0.01750 0.01750
## Female.Black, Non-Hispanic    0.2884  0.04794 0.04794
## Male.Black, Non-Hispanic      0.2857  0.05113 0.05113
## Female.Hispanic               0.2445  0.04366 0.04366
## Male.Hispanic                 0.3758  0.04690 0.04690
## Female.Other, Non-Hispanic    0.2808  0.07333 0.07333
## Male.Other, Non-Hispanic      0.3504  0.06920 0.06920
## Female.2+ Races, Non-Hispanic 0.2281  0.06514 0.06514
## Male.2+ Races, Non-Hispanic   0.2686  0.08201 0.08201
```

```r
# out of all females and males
prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 2)
```

```
##      PPGENDER
## Q1    Female   Male
##   Yes 0.7937 0.7045
##   No  0.2063 0.2955
```
Out of all females and males:  
79.37% of females answered Yes to Q1; 0.2063 said No  
70.45% of males answered Yes to Q1; 0.2955 said No  

***

> Apply survey design + weights for Q2.

Note: df contains recoded variables


```r
# glm + design weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

# Q2: being sick, by female gender
m1 <- svyglm(sick ~ female, des2, family = quasibinomial())
summary(m1)
```

```
## 
## Call:
## svyglm(formula = sick ~ female, des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.5559     0.0888  -17.53   <2e-16 ***
## female1       0.2981     0.1188    2.51    0.012 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# odds ratios
(or1 <- exp(coefficients(m1)))
```

```
## (Intercept)     female1 
##       0.211       1.347
```

```r
# Q2: being sick, by male gender
m <- svyglm(sick ~ PPGENDER, des2, family = quasibinomial())
summary(m)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPGENDER, des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.2578     0.0789  -15.94   <2e-16 ***
## PPGENDERMale  -0.2981     0.1188   -2.51    0.012 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# odds ratios
(or2 <- exp(coefficients(m)))
```

```
##  (Intercept) PPGENDERMale 
##       0.2843       0.7422
```

```r
# Q2: being sick, by ethnicity
m2 <- svyglm(sick ~ PPETHM, des2, family = quasibinomial())
summary(m2)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPETHM, des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -1.5566     0.0699  -22.28  < 2e-16 ***
## PPETHMBlack, Non-Hispanic      0.1689     0.1998    0.85  0.39789    
## PPETHMHispanic                 0.5917     0.1677    3.53  0.00043 ***
## PPETHMOther, Non-Hispanic      0.4717     0.2612    1.81  0.07106 .  
## PPETHM2+ Races, Non-Hispanic   0.4078     0.2799    1.46  0.14526    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
(or3 <- exp(coefficients(m2)))  # odds ratios
```

```
##                  (Intercept)    PPETHMBlack, Non-Hispanic 
##                       0.2109                       1.1840 
##               PPETHMHispanic    PPETHMOther, Non-Hispanic 
##                       1.8071                       1.6028 
## PPETHM2+ Races, Non-Hispanic 
##                       1.5035
```

```r
# same as above, but binomial -- remove reference variable (white)
m <- svyglm(sick ~ black+hispanic+otherrace+mixedrace, des2, family = quasibinomial())
summary(m)
```

```
## 
## Call:
## svyglm(formula = sick ~ black + hispanic + otherrace + mixedrace, 
##     des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.5566     0.0699  -22.28  < 2e-16 ***
## black1        0.1689     0.1998    0.85  0.39789    
## hispanic1     0.5917     0.1677    3.53  0.00043 ***
## otherrace1    0.4717     0.2612    1.81  0.07106 .  
## mixedrace1    0.4078     0.2799    1.46  0.14526    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
or4 <- exp(coefficients(m))  # odds ratios
```

Odds ratios for being sick compared to:  
  - males: 1.3474  
  - females: 0.7422  
  - white people: 0.2109, 1.184, 1.8071, 1.6028, 1.5035  


## Q2. Have you had an illness with influenza-like symptoms since August 2015?

> Without weights:


```r
# Q2 by gender
with(data, table(Q2, PPGENDER))
```

```
##      PPGENDER
## Q2    Female Male
##   Yes    234  180
##   No     858  877
```

```r
# without weights
fit1 <- glm(sick ~ PPGENDER, data = df, family = binomial())
summary(fit1)
```

```
## 
## Call:
## glm(formula = sick ~ PPGENDER, family = binomial(), data = df)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.695  -0.695  -0.611  -0.611   1.882  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.2993     0.0737  -17.62   <2e-16 ***
## PPGENDERMale  -0.2843     0.1102   -2.58   0.0099 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2106.2  on 2148  degrees of freedom
## Residual deviance: 2099.5  on 2147  degrees of freedom
##   (19 observations deleted due to missingness)
## AIC: 2103
## 
## Number of Fisher Scoring iterations: 4
```

```r
(q2.or <- exp(coefficients(fit1)))
```

```
##  (Intercept) PPGENDERMale 
##       0.2727       0.7526
```

```r
#contrasts(df$sick)
#contrasts(data$PPGENDER)
```

Unweighted OR for being sick last year = 0.7526 compared to females.  


## Q13. getting the flu vaccine


```r
# use data = df, with recodes
df$vax <- recode(data$Q13, "'Yes, every year' = 1; 'Yes, some years' = 1; NA = NA; else = 0")

# check that the newly recoded column makes sense
str(df$vax)
```

```
##  Factor w/ 2 levels "0","1": 2 NA 2 2 2 2 1 2 2 1 ...
```

```r
str(data$Q13)
```

```
##  Factor w/ 3 levels "Yes, every year",..: 1 NA 1 2 1 2 3 1 2 3 ...
```

```r
summary(data$Q13)
```

```
## Yes, every year Yes, some years       No, never            NA's 
##             908             423             819              18
```

```r
summary(df$vax)
```

```
##    0    1 NA's 
##  819 1331   18
```

```r
# glm + design weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

# getting sick ~ getting vaccine
m3 <- svyglm(sick ~ vax, des2, family = quasibinomial)
summary(m3)
```

```
## 
## Call:
## svyglm(formula = sick ~ vax, des2, family = quasibinomial)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.5648     0.0988  -15.84   <2e-16 ***
## vax1          0.2800     0.1231    2.27    0.023 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9979)
## 
## Number of Fisher Scoring iterations: 4
```

```r
exp(coefficients(m3)[2])
```

```
##  vax1 
## 1.323
```

OR for being sick last year for those receiving vaccine = 1.3231


```r
# tables with weights
svytable(~vax + sick, design = des2, round = T)
```

```
##    sick
## vax   0   1
##   0 711 149
##   1 999 276
```

```r
prop.table(svytable(~vax + sick, design = des2), margin = 2)
```

```
##    sick
## vax      0      1
##   0 0.4157 0.3497
##   1 0.5843 0.6503
```

```r
prop.table(svytable(~vax + sick, design = des2), margin = 1)
```

```
##    sick
## vax      0      1
##   0 0.8270 0.1730
##   1 0.7833 0.2167
```

Out of those who reported being sick, 65.03% reported receiving vaccine.  
Out of those who were healthy, 58.43% received vaccine (41.57% did not).  
Vaccinated group: 78.32% report no illness.  
Unvaccinated group: 82.7% report no illness.  
  


Other questions with design + weights.




