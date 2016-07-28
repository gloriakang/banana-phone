# analysis: subset




```r
# load data2, new_name, old_name; ggplot variables
load("clean/cleaning2.RData")
#load("clean/plotting1.RData")

data <- data2
names(data) <- old_name  # data = old names
names(data2) <- new_name  # data2 = new names
```

Apply column weights and compare to original data.


```r
library(survey)

# survey design object
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~1, weights = ~weight, data = data2[is.na(data2$weight) == F, ])
```

Q1.


```r
# Q1 tables
with(data2, summary(Q1))  # unweighted
```

```
##  Yes   No NA's 
## 1664  488   16
```

```r
svytable(~Q1, design = des, round = T)  # weighted
```

```
## Q1
##  Yes   No 
## 1612  535
```

```r
# Q1 by gender
with(data2, table(Q1, PPGENDER))
```

```
##      PPGENDER
## Q1    Female Male
##   Yes    888  776
##   No     205  283
```

```r
with(data2, prop.table(table(Q1, PPGENDER), margin = 2))
```

```
##      PPGENDER
## Q1       Female      Male
##   Yes 0.8124428 0.7327668
##   No  0.1875572 0.2672332
```

```r
# Q1 by gender, with design + weights
svytable(~Q1 + PPGENDER, design = des, round = T)
```

```
##      PPGENDER
## Q1    Female Male
##   Yes    887  725
##   No     231  304
```

```r
prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 2)
```

```
##      PPGENDER
## Q1       Female      Male
##   Yes 0.7937090 0.7045344
##   No  0.2062910 0.2954656
```

```r
# prop table above but with standard errors
svyby(formula = ~Q1, by = ~PPGENDER, design = des, FUN = svymean, na.rm = T)
```

```
##        PPGENDER     Q1Yes      Q1No   se.Q1Yes    se.Q1No
## Female   Female 0.7937090 0.2062910 0.01372576 0.01372576
## Male       Male 0.7045344 0.2954656 0.01561686 0.01561686
```

```r
# prop table with gender + race
svyby(formula = ~Q1, by = ~PPGENDER + PPETHM, design = des, FUN = svymean, na.rm = T)
```

```
##                               PPGENDER                 PPETHM     Q1Yes
## Female.White, Non-Hispanic      Female    White, Non-Hispanic 0.8248914
## Male.White, Non-Hispanic          Male    White, Non-Hispanic 0.7287453
## Female.Black, Non-Hispanic      Female    Black, Non-Hispanic 0.7115935
## Male.Black, Non-Hispanic          Male    Black, Non-Hispanic 0.7142656
## Female.Hispanic                 Female               Hispanic 0.7555245
## Male.Hispanic                     Male               Hispanic 0.6242082
## Female.Other, Non-Hispanic      Female    Other, Non-Hispanic 0.7192030
## Male.Other, Non-Hispanic          Male    Other, Non-Hispanic 0.6495851
## Female.2+ Races, Non-Hispanic   Female 2+ Races, Non-Hispanic 0.7718791
## Male.2+ Races, Non-Hispanic       Male 2+ Races, Non-Hispanic 0.7313605
##                                    Q1No   se.Q1Yes    se.Q1No
## Female.White, Non-Hispanic    0.1751086 0.01437618 0.01437618
## Male.White, Non-Hispanic      0.2712547 0.01750219 0.01750219
## Female.Black, Non-Hispanic    0.2884065 0.04794206 0.04794206
## Male.Black, Non-Hispanic      0.2857344 0.05113200 0.05113200
## Female.Hispanic               0.2444755 0.04365956 0.04365956
## Male.Hispanic                 0.3757918 0.04689536 0.04689536
## Female.Other, Non-Hispanic    0.2807970 0.07332513 0.07332513
## Male.Other, Non-Hispanic      0.3504149 0.06920426 0.06920426
## Female.2+ Races, Non-Hispanic 0.2281209 0.06513902 0.06513902
## Male.2+ Races, Non-Hispanic   0.2686395 0.08201280 0.08201280
```

Recode some variables.


```r
library(car)
df <- data2

# female
df$female <- recode(data2$PPGENDER, recodes = "'Female' = 1; 'Male' = 0")

# ethnicity
df$white <- recode(data2$PPETHM, recodes = "'White, Non-Hispanic' = 1; NA = NA; else = 0")
df$black <- recode(data2$PPETHM, recodes = "'Black, Non-Hispanic' = 1; NA = NA; else = 0")
df$hispanic <- recode(data2$PPETHM, recodes = "'Hispanic' = 1; NA = NA; else = 0")
df$otherrace <- recode(data2$PPETHM, recodes = "'Other, Non-Hispanic' = 1; NA = NA; else = 0")
df$mixedrace <- recode(data2$PPETHM, recodes = "'2+ Races, Non-Hispanic' = 1; NA = NA; else = 0")

# Q1
df$Q1 <- recode(data2$Q1, recodes = "'Yes' = 1; 'No' = 0; NA = NA")

# Q2
df$sick <- recode(data2$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
```

Apply survey design + weights for Q2.


```r
# glm + design weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

# Q2: being sick, by female gender
m1 = svyglm(sick ~ female, des2, family = binomial())
summary(m1)
```

```
## 
## Call:
## svyglm(formula = sick ~ female, des2, family = binomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.55592    0.08877  -17.53   <2e-16 ***
## female1      0.29815    0.11876    2.51   0.0121 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9992246)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# odds ratios
exp(coefficients(m1))
```

```
## (Intercept)     female1 
##   0.2109947   1.3473578
```

```r
# Q2: being sick, by male gender
m = svyglm(sick ~ PPGENDER, des2, family = binomial())
summary(m)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPGENDER, des2, family = binomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.25778    0.07889  -15.94   <2e-16 ***
## PPGENDERMale -0.29815    0.11876   -2.51   0.0121 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9992246)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# odds ratios
exp(coefficients(m))
```

```
##  (Intercept) PPGENDERMale 
##    0.2842854    0.7421934
```

```r
# Q2: being sick, by ethnicity
m2 = svyglm(sick ~ PPETHM, des2, family = binomial())
summary(m2)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPETHM, des2, family = binomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  -1.55661    0.06985 -22.285  < 2e-16 ***
## PPETHMBlack, Non-Hispanic     0.16891    0.19976   0.846 0.397894    
## PPETHMHispanic                0.59172    0.16773   3.528 0.000428 ***
## PPETHMOther, Non-Hispanic     0.47173    0.26120   1.806 0.071059 .  
## PPETHM2+ Races, Non-Hispanic  0.40781    0.27990   1.457 0.145265    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9992246)
## 
## Number of Fisher Scoring iterations: 4
```

```r
exp(coefficients(m2))  # odds ratios
```

```
##                  (Intercept)    PPETHMBlack, Non-Hispanic 
##                    0.2108502                    1.1840113 
##               PPETHMHispanic    PPETHMOther, Non-Hispanic 
##                    1.8070876                    1.6027619 
## PPETHM2+ Races, Non-Hispanic 
##                    1.5035254
```

```r
# same as above, but binomial
m = svyglm(sick ~ white+black+hispanic+otherrace+mixedrace, des2, family = binomial())
summary(m)
```

```
## 
## Call:
## svyglm(formula = sick ~ white + black + hispanic + otherrace + 
##     mixedrace, des2, family = binomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.14879    0.27104  -4.238 2.35e-05 ***
## white1      -0.40781    0.27990  -1.457    0.145    
## black1      -0.23890    0.32938  -0.725    0.468    
## hispanic1    0.18390    0.31100   0.591    0.554    
## otherrace1   0.06392    0.36988   0.173    0.863    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9992246)
## 
## Number of Fisher Scoring iterations: 4
```

```r
exp(coefficients(m))  # oods ratios
```

```
## (Intercept)      white1      black1   hispanic1  otherrace1 
##   0.3170186   0.6651035   0.7874901   1.2019003   1.0660025
```

Q2. Have you had an illness with influenza-like symptoms since August 2015?


```r
# Q2 by gender
with(data2, table(Q2, PPGENDER))
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
##     Min       1Q   Median       3Q      Max  
## -0.6945  -0.6945  -0.6110  -0.6110   1.8816  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.29928    0.07375 -17.617  < 2e-16 ***
## PPGENDERMale -0.28427    0.11016  -2.581  0.00986 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2106.2  on 2148  degrees of freedom
## Residual deviance: 2099.5  on 2147  degrees of freedom
##   (19 observations deleted due to missingness)
## AIC: 2103.5
## 
## Number of Fisher Scoring iterations: 4
```

```r
exp(coefficients(fit1))
```

```
##  (Intercept) PPGENDERMale 
##    0.2727273    0.7525656
```

```r
contrasts(df$sick)
```

```
##   1
## 0 0
## 1 1
```

```r
contrasts(data2$PPGENDER)
```

```
##        Male
## Female    0
## Male      1
```

Q13. getting the flu vaccine


```r
# use data = df, with recodes
df$vax <- recode(data2$Q13, "'Yes, every year' = 1; 'Yes, some years' = 1; NA = NA; else = 0")
summary(df$vax)
```

```
##    0    1 NA's 
##  819 1331   18
```

```r
## glm + design weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

# getting sick ~ getting vaccine
m3 = svyglm(sick ~ vax, des2, family = binomial)
summary(m3)
```

```
## 
## Call:
## svyglm(formula = sick ~ vax, des2, family = binomial)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.56483    0.09881 -15.837   <2e-16 ***
## vax1         0.27998    0.12314   2.274   0.0231 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9978977)
## 
## Number of Fisher Scoring iterations: 4
```

```r
exp(coefficients(m3))
```

```
## (Intercept)        vax1 
##    0.209124    1.323100
```

```r
exp(m3$coef[2])
```

```
##   vax1 
## 1.3231
```

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
## vax         0         1
##   0 0.4157303 0.3497125
##   1 0.5842697 0.6502875
```

Other questions with design + weights.





```r
# count total number of unweighted records
#nrow(data2)
#unwtd.count(~one, des)

# by gender
#svyby(~one, ~PPGENDER, des, unwtd.count)

# count the weighted number of individuals
#svytotal(~one, des)

#
#svymean(~Q1, na.rm = T, design = des)

# by sex
#svyby(
#	~Q1 ,
#	~PPGENDER ,
#	design = des ,
#	svymean, na.rm = T
#)
```



