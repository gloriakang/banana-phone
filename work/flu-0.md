# flu-0

## Bivariate and multivariate analyses of flu survey data with weights.



### Load data and recoded variables.


```r
load("clean/cleaning2.RData")
data <- data2
names(data) <- old_name  # give data old names
rm(data2)

#source(recoding.R)
load("clean/recoding1.RData")
df <- datar  # df contains recoded variables
#tail(df)
```

### Create a survey object with weights.


```r
options(digits = 4)
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~1, weights = ~weight, data = data[is.na(data$weight) == F, ])
#summary(des)
```

# Survey questions
## Q1. Before receiving this survey, did you know influenza is different from the stomach flu?
  
### Compare sex ratios in unweighted and weighted data frames for Q1.
  

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
(u <- with(data, prop.table(table(Q1, PPGENDER), margin = 1))*100)  # unweighted prop
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  53.37 46.63
##   No   42.01 57.99
```

```r
(w <- prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 1)*100)  # weighted prop
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  55.02 44.98
##   No   43.12 56.88
```
In the unweighted data frame, 53.3654% of females answered Yes to Q1; males = 46.6346%; 57.9918% of males answered No; females = 57.9918%  
In the weighted data frame, 55.0212% of females answered Yes; males = 44.9788%; 56.8791% of males answered No; females = 43.1209%.  
  
  

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
(w <- prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 2)*100)
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  79.37 70.45
##   No   20.63 29.55
```
Out of all females and males:  
79.3709% of females answered Yes to Q1; 20.6291% said No.  
70.4534% of males answered Yes to Q1; 29.5466% said No.  
  
  
## Q2. Have you had an illness with influenza-like symptoms since August 2015?

### Without weights.


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
fit1 <- glm(sick ~ PPGENDER, data = df, family = binomial())  # unweighted
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
(q2.u <- exp(coefficients(fit1)))
```

```
##  (Intercept) PPGENDERMale 
##       0.2727       0.7526
```

```r
#contrasts(data$PPGENDER)
```

In the unweighted data frame, males had 0.7526 odds of being sick compared to females.  

### Apply survey design + weights for Q2. Analyze sick vs. not sick by gender.


```r
# create survey object + weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)  # data = df

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

Females had 1.3474 odds of being sick compared to males.


```r
# Q2: being sick, by male gender
m2 <- svyglm(sick ~ PPGENDER, des2, family = quasibinomial())
summary(m2)
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
(or2 <- exp(coefficients(m2)))
```

```
##  (Intercept) PPGENDERMale 
##       0.2843       0.7422
```

Males had 0.7422 odds of being sick compared to females.  

### What about by ethnicity?


```r
# Q2: being sick, by ethnicity
m3 <- svyglm(sick ~ PPETHM, des2, family = quasibinomial())
summary(m3)
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
(or3 <- exp(coefficients(m3)))  # odds ratios
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
m4 <- svyglm(sick ~ black+hispanic+otherrace+mixedrace, des2, family = quasibinomial())
summary(m4)
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
(or4 <- exp(coefficients(m4)))  # odds ratios
```

```
## (Intercept)      black1   hispanic1  otherrace1  mixedrace1 
##      0.2109      1.1840      1.8071      1.6028      1.5035
```

Compared to white ethnicity, odds ratio for being sick are 1.184, 1.8071, 1.6028, 1.5035 for black, hispanic, other, and 2+ mixed race groups, respectively.  
  
  
### Create and compare models for sick vs. not sick.


```r
# adding variables to model with weights
a1 <- svyglm(sick ~ PPGENDER, des2, family = quasibinomial())
a2 <- update(a1, ~ . + PPETHM)  # add race
a3 <- update(a2, ~ . + income)  # add income

# memisc
mtable(a1, a2, a3)
```

```
## 
## Calls:
## a1: svyglm(formula = sick ~ PPGENDER, des2, family = quasibinomial())
## a2: svyglm(formula = sick ~ PPGENDER + PPETHM, des2, family = quasibinomial())
## a3: svyglm(formula = sick ~ PPGENDER + PPETHM + income, des2, family = quasibinomial())
## 
## =======================================================================================
##                                                          a1         a2         a3      
## ---------------------------------------------------------------------------------------
##   (Intercept)                                         -1.258***  -1.416***  -1.248***  
##                                                       (0.079)    (0.088)    (0.175)    
##   PPGENDER: Male/Female                               -0.298*    -0.314**   -0.310**   
##                                                       (0.119)    (0.119)    (0.119)    
##   PPETHM: Black, Non-Hispanic/White, Non-Hispanic                 0.162      0.153     
##                                                                  (0.201)    (0.201)    
##   PPETHM: Hispanic/White, Non-Hispanic                            0.603***   0.598***  
##                                                                  (0.168)    (0.170)    
##   PPETHM: Other, Non-Hispanic/White, Non-Hispanic                 0.487      0.491     
##                                                                  (0.264)    (0.264)    
##   PPETHM: 2+ Races, Non-Hispanic/White, Non-Hispanic              0.401      0.400     
##                                                                  (0.284)    (0.286)    
##   income: $20k to $40k/under $20k                                           -0.227     
##                                                                             (0.215)    
##   income: $40k to $75k/under $20k                                           -0.216     
##                                                                             (0.198)    
##   income: over $75k/under $20k                                              -0.174     
##                                                                             (0.184)    
## ---------------------------------------------------------------------------------------
##   Aldrich-Nelson R-sq.                                    0.0        0.0        0.0    
##   McFadden R-sq.                                          0.0        0.0        0.0    
##   Cox-Snell R-sq.                                         0.0        0.0        0.0    
##   Nagelkerke R-sq.                                        0.0        0.0        0.0    
##   phi                                                     1.0        1.0        1.0    
##   Likelihood-ratio                                        7.5       27.7       29.6    
##   p                                                       0.0        0.0        0.0    
##   Log-likelihood                                       2133.2     2113.0     2111.2    
##   Deviance                                             2133.2     2113.0     2111.2    
##   N                                                    2146       2146       2146      
## =======================================================================================
```

## Q13. Do you get an influenza vaccine?


```r
# use data = df, with recodes
# recode Q13 as 1 = yes always; 1 = yes sometimes; 0 = no never
str(df$vax)
```

```
##  Factor w/ 2 levels "Yes","No": 1 NA 1 1 1 1 2 1 1 2 ...
```

```r
str(data$Q13)
```

```
##  Factor w/ 3 levels "Yes, every year",..: 1 NA 1 2 1 2 3 1 2 3 ...
```

```r
# glm + design weights
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

# getting sick ~ getting vaccine
m5 <- svyglm(sick ~ vax, des2, family = quasibinomial)
summary(m5)
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
## (Intercept)  -1.2849     0.0735  -17.49   <2e-16 ***
## vaxNo        -0.2800     0.1231   -2.27    0.023 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9979)
## 
## Number of Fisher Scoring iterations: 4
```

```r
(or5 <- exp(coefficients(m5)))
```

```
## (Intercept)       vaxNo 
##      0.2767      0.7558
```

Those who did not receive vaccine had 0.7558 odds of getting sick compared to those who received the vaccine.


```r
# Q2 + Q13 tables with weights
svytable(~vax + sick, design = des2, round = T)  # sick = 1, not sick = 0
```

```
##      sick
## vax     0   1
##   Yes 999 276
##   No  711 149
```

```r
(t2 <- prop.table(svytable(~vax + sick, design = des2), margin = 2)* 100)
```

```
##      sick
## vax       0     1
##   Yes 58.43 65.03
##   No  41.57 34.97
```

```r
(t1 <- prop.table(svytable(~vax + sick, design = des2), margin = 1)* 100)
```

```
##      sick
## vax       0     1
##   Yes 78.33 21.67
##   No  82.70 17.30
```

Out of those who reported being sick, 65.0288% reported receiving vaccine.  
Out of those who were healthy, 58.427% received vaccine (41.573% did not).  
Vaccinated group: 78.3274% report no illness.  
Unvaccinated group: 82.7045% report no illness.  
  




