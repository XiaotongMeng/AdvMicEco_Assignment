
clear all 
capture log close

cd "C:\Users\Marimo\Desktop"

* start log file
log using homework_sub.log, replace


***homework
* name : Ling Wang    365543
* name: Xiaotong Meng 351191

use prison.dta

***Exercise1
** part a 
* i 
gen lmetro= ln(metro)
gen lpolpc= ln(polpc)
gen lblack= ln(black)
gen lunem = ln(unem)

* criv has already the normal shape, since the long tail at the right side has a extremmly small density less than 0.005, whicn can be ignored. Taking log make it have much heavier tail, which even not decay as normal. So we do not take log.
histogram criv,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lcriv,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))

* pris has the normal shape, and  taking log make its shape more normal-like with respect to symmetric, however there is outlier, which with the highest density  of all. Moreover, take lmetro insead of metro decrease the r^2, so we don't take log.
histogram pris,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lpris,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))

*unem has already the normal shape, taking log makes it much more normal-like and increase the r^2, so we take lunem.
histogram unem,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lunem,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))


*The variable black and metro have really bad distribution, taking log also not really make it more normal-like. In addition take log of them insead of original ones decrease the r^2, so we keep original ones.
histogram black,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lblack,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram metro,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lmetro,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))

*polpc has the mormal shape, taking log make it less normal-like and decrease the r^2, so we do not take log.
histogram polpc,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))
histogram lpolpc,kden norm normopts(lcolor(blue)) legend(on position(2) ring(0))


reg criv pris lunem black metro polpc y81-y93


*ii
margins, dydx(pris) 
*null hypothesis and the marignal effect is significant at > 99.9% significance level. 

* Interpretation:  pris(person pop. per 100000 ) increases in one unit leads to the increasing of criv (violent crime rates) by 0.08 unit， ceteris paribus.
*This gives the answer to our research question that the more crowed the prison is,  the higher is the violent crime rates.

*iii one sieded  significance test
test _b[pris]=0
*As long as the F test has 1 numerator degree of freedom, the square root of the F statistic is the absolute value of the t statistic for the one-sided test.

display "Ho: coef <= 0  p-value = " ttail(r(df_r), sqrt(r(F)))
*null hypothesis and the marignal effect is significant at > 99.9% level. 

*iv

estat imtest, white
* nul hypothesis  of homoscedasticity is rejected, taking robusted version to get better estimation of standard errors.
reg criv pris lunem black metro polpc y81-y93, vce(robust)


predict re1, resid
graph twoway (lfit pris re1) (scatter pris re1)
graph twoway (lfit metro re1) (scatter metro re1)

* from the scatter plots above we can say some of the explanatory variables may not strictly exogenous, which might cause the biased estimation.



*part b
*i
**panel model:
* criv= beta_0+ beta_1*pris+ beta_2*lunem+ beta_3*black+ beta_4*metro+ beta_5*polpc+ C_i+U_it
* Ci=αZi， 
* example: the age distribution in populations and the education level of the populations might also be the unobserved individual effets. 

xtset state year

*ii

/* Assumptions of RE:
Exogeneity: 
RE.1a: The error term U_it is not correlated with criv pris unem black lmetro polpc or C_i in t and all other years.
RE.1 C_i not correlated with criv pris unem black lmetro polpc or in all years


Rank:
RE.2: The matrix ofcriv pris i.year unem black lmetro polpc including a constant and weighted with the RE error variance should be of full rank


Homoskedasticity:
RE.3: The variance of U_it is constant in the cross-sectional dimension of U_it and C_i. The error terms U_it are not autocorrelated. The variance of C_i is is constant in the cross-sectional dimension of pris unem black lmetro polpc.
*/


xtreg criv pris i.year lunem black metro polpc, re 
est sto random
* the coefficient of pris in panel model(using radam effects) is less than  that of pooled OLS model by about 0.003. 
*The answer to the research question is the same as in a. The more crowed the prison is,  the higher is the violent crime rates, however, C_i in POLS is assumeed to be 0 to avoid violation of assumption, but here it is allowed to be non-zero. Thus the unobserved effects are now considered in the estimation. And the decrease of coefficient of pris, tells this case and indicate that there  might be other sources of unobserved individual effects, which also playing an important role in affecting violate crime rate.


*iii
/*Assumptions of FE:
Exogeneity: 
FE.1: The error term U_it is not correlated with criv pris  unem black lmetro polpc or C_i in t and all other years. 

Rank:
FE.2: The matrix of demeaned criv pris i.year unem black lmetro polpc including a constant should be of full rank

Homoskedasticity:
FE.3: The variance of U_it is constant in the cross-sectional dimension of pris unem black lmetro polpc and C_i. The error terms U_it are not autocorrelated.
*/

xtreg criv pris i.year unem black lmetro polpc, fe
est sto fixed
* the coefficient of pris in panel model(using radam effects) is less than  that of pooled OLS model by about 0.001. 
*The answer to the research question is the same as in a. The more crowed the prison is,  the higher is the violent crime rates, however, now C_i are allowed to be corelated with explanatory variables , which means there are sources of unobserved effects correlated with pris. 

*C_i  is allowed to be correlated with criv pris unem black lmetro polpc or in all years.
hausman random fixed, sigmamore
*null hypothes rejected

*iv
** all explanatory variables are time- and cross-sectional varying variables.
egen ave_pris = mean(pris), by(state)
egen ave_unem = mean(unem), by(state)
egen ave_black = mean(black), by(state)
egen ave_lmetro = mean(lmetro), by(state)
egen ave_polpc = mean(polpc), by(state)

xtreg criv pris i.year unem black lmetro polpc ave_pris ave_unem ave_black ave_lmetro ave_polpc, re vce(robust)


xtreg criv pris i.year unem black lmetro polpc ave_pris ave_unem ave_black ave_lmetro ave_polpc, fe vce(robust)
*null hypothesiscan not be rejected, the model with fixed effects should be decided.


reg criv polpc
predict residualpp, resid
scatter criv residualpp


reg criv unem
predict residualuu, resid
scatter criv residualuu

graph twoway (lfit criv unem) (scatter criv unem)


* according to scatter plot above, the possible sources of heteroskedasticity are pris，lmetro，black, though unmem and polpc amight not be strictly uncorrelated to the error.

*OLS estimators are not Best Linear Unbiased Estimators (BLUE) any more. The OLS estimaror remains consistentm but its asymptotic variance is different. The standard errors will be biased, along with their corresponding test statistics and confidence intervals. 
* We need to adjust the estimation of its variance in oder to make correct inference about the estimated paramaters by taking the robust estimators.
 
 
 
*the increasing of violent crime rates may lead to the increase of the prison overcrowding.
xtreg  pris criv i.year unem black lmetro polpc, re 
*p<0.001 , null Hypothesis is rejected, 
* In expectation, if criv (violent crime rates) increases by one unit, then  the pris(person pop. per 100000 ) increases by 13.439 unit， ceteris paribus. Therefore, criv causes pris.

* part c
*i
reg criv black pris unem lmetro polpc y81-y93 
reg black final1 pris unem lmetro polpc y81-y93

ivreg criv pris unem lmetro polpc y81-y93 (black = final1)

reg black final2 pris unem lmetro polpc y81-y93

ivreg criv pris unem lmetro polpc y81-y93 (black = final2)

* both intrumental variables are not satisfied.
reg criv pris black unem lmetro polpc y81-y93

reg pris final2 black unem lmetro polpc y81-y93

ivreg criv black unem lmetro polpc y81-y93 (pris = final2)
* P=0.036>>0.01, null hypothesis is rejected. Therefore, the intrumental variable final2 is weak.



reg pris final1 black unem lmetro polpc y81-y93

ivreg criv black unem lmetro polpc y81-y93 (pris = final1)

* P=0.094>>0.01, null hypothesis is rejected. Therefore, the intrumental variable final1 is weak.

* relevance: correlation between pris and final2 is not zero.
* exogeneity: correlation between final2 and error term should be zero



use loanapp.dta
***Excercise 2
**a
*i
reg approve black hispan married sch hrat loanprc
* interpretation:  In expectation, 
* - if the person is black, mortgage approval will be less possible by about 0.21 unit,ceteris paribus.

* - if the person is hispan, mortgage approval will be less possible by about 0.12 unit,ceteris paribus.

*-  if the monthly housing expense as the percentage pf total income increses by one unit, mortgage approval will be less possible by about 0.001 unit,,ceteris paribus.

* - if the ratio of loan to purchase price increases by 1 unit, mortgage approval will be more possible by about 0.2 unit,ceteris paribus.

*ii
test black hispan married sch hrat loanprc
* p < 0.001, null hypothesis rejected. The coefficients are jointly significant at > 99.99% level

*iii
* This model is a linear probability model, since the respnse approve is binary. In this case, heteroskedasticity is always present unless all sloe coefficient are zero. The reason is that the LPM is based on Bernouli distribution, where the variance is calculated with x*beta, which is estimated by x*beta^hat+ error. The covariance is then dependent on the error terms. W

*The OLS estimaror remains consistent but its asymptotic variance is different. The standard errors will be biased, along with their corresponding test statistics and confidence intervals. 

* With LPM, We should always adjust the estimation of its variance by taking the White heteroskedasticith-robust variance-covatiance matrix.

* Marginal effects are not affected under heteroskedasticity, however, the significance tests will run either too high or too low. 

// sum loanprc, detail
// margins, dydx(loanprc) at(loanprc=(0.1(0.1)1))
// marginsplot



estat imtest, white
* p = 0.0001
* null hypothesis of homoscedasticity is rejected.

* adjusted estimation
reg approve black hispan married sch hrat loanprc, vce(robust)

*iv
reg approve male black hispan married sch hrat loanprc, vce(robust)
margins, dydx(sch) at(male = 1)
margins, dydx(sch) at(male = 0)
* the marginal effect of education does not change with gender. The marginal effect of education is not significant, since p = 0.503 >> 0.1.


**b
* i

logit approve male black hispan married sch hrat loanprc,vec(robust)

*The odds-ratio here is the ratio of the probability of the mortgage approval over the probability of the mortgage rejected. 
*Interpretation: In expectation,
* - if the person is black, the odds-ratio will be decreased by about 1.44%, ceteris paribus.

* - if the person is hispanic, the odds-ratio will be decreased by about 0.97%, ceteris paribus.

*-  if the monthly housing expense as the percentage pf total income increses by one unit, the odds-ratio will be decreased by about 0.02%, ceteris paribus.

* - if the ratio of loan to purchase price increases by 1 unit, the odds-ratio will be decreased by about 2.15%, ceteris paribus.

*ii
test male black hispan married sch hrat loanprc
* null hypothesis rejected, the coefficient are jointly significant.

*iii
margins, dydx(black)
margins, dydx(hispan)

* margins of OLS in a
qui reg approve male black hispan married sch hrat loanprc, vce(robust)
margins, dydx(black)
margins, dydx(hispan)


* maginal effects of both variable, black and hispan, have been increasd in the negative(!!!) direction. The reseaon might be that the hererosckedaticity in non-linear models might affect consistency, thus changes the estimations.


*iv
tab race 
qui logit approve male black hispan married sch hrat loanprc
margins, dydx(*) at(black=0 hispan=0 married=1) 

tab sch
margins, dydx(*) at(black=0 hispan=0 married=1 sch =(0,1)) 
marginsplot


*v
* The heteroskedasticity affect both standard errors and marginal effects in non linear model.
*The standard errors can be either too big or too small and the marginal effects can be biased, because of inconsistgency in non linear model. 

*vi
 qui logit approve male black hispan white married sch hrat loanprc
 margins, dydx(sch) at(black=1)
 margins, dydx(sch) at(hispan=1)
 margins, dydx(sch) at(white=1)

qui logit approve male i.race married sch hrat loanprc
margins, dydx(sch) at(race = (3,4,5))
marginsplot

* The marginal effect chanfe with the race. For example, the marginal effect of education for a hispanic person is less than that for a black person by about 0.0038 and the marginal effect of education for a white person is less than that for a black person by about 0.0113. The difference is not significance.
log close
 
