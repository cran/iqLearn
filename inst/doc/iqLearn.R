### R code from vignette source 'iqLearn.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: iqLearn.Rnw:456-457
###################################################
options(width=70)


###################################################
### code chunk number 2: iqLearn.Rnw:460-461
###################################################
library (iqLearn)


###################################################
### code chunk number 3: iqLearn.Rnw:464-465
###################################################
data (bmiData)


###################################################
### code chunk number 4: iqLearn.Rnw:470-472
###################################################
dim (bmiData)
head (bmiData)


###################################################
### code chunk number 5: iqLearn.Rnw:476-482
###################################################
bmiData$A1[which (bmiData$A1=="MR")] = 1
bmiData$A1[which (bmiData$A1=="CD")] = -1
bmiData$A2[which (bmiData$A2=="MR")] = 1
bmiData$A2[which (bmiData$A2=="CD")] = -1
bmiData$A1 = as.numeric (bmiData$A1)
bmiData$A2 = as.numeric (bmiData$A2)


###################################################
### code chunk number 6: iqLearn.Rnw:486-488
###################################################
y = -100*(bmiData$month12_BMI -
          bmiData$baseline_BMI)/bmiData$baseline_BMI


###################################################
### code chunk number 7: iqLearn.Rnw:531-534
###################################################
fitIQ2 = learnIQ2 (y ~ gender + parent_BMI + month4_BMI + 
  A2*(parent_BMI + month4_BMI), data=bmiData, treatName="A2", 
  intNames=c ("parent_BMI", "month4_BMI"))


###################################################
### code chunk number 8: iqLearn.Rnw:551-553
###################################################
s2vars = bmiData[, c(1,3,5)]
head (s2vars)


###################################################
### code chunk number 9: iqLearn.Rnw:558-559
###################################################
s2ints = c (2,3)


###################################################
### code chunk number 10: iqLearn.Rnw:562-563
###################################################
fitIQ2 = learnIQ2 (H2=s2vars, Y=y, A2=bmiData$A2, s2ints=s2ints)


###################################################
### code chunk number 11: iqLearn.Rnw:567-568
###################################################
summary (fitIQ2)


###################################################
### code chunk number 12: iqLearn.Rnw:574-575
###################################################
plot (fitIQ2)


###################################################
### code chunk number 13: iqLearn.Rnw:584-585
###################################################
fitIQ2$betaHat20


###################################################
### code chunk number 14: iqLearn.Rnw:588-589
###################################################
fitIQ2$betaHat21


###################################################
### code chunk number 15: iqLearn.Rnw:615-619
###################################################
fitIQ1main = learnIQ1main (~ gender + race + parent_BMI + 
  baseline_BMI + A1*(gender + parent_BMI), data=bmiData, 
  treatName="A1", intNames=c ("gender", "parent_BMI"), s2object=fitIQ2)
summary (fitIQ1main);


###################################################
### code chunk number 16: iqLearn.Rnw:628-630
###################################################
s1vars = bmiData[, 1:4]
head (s1vars)


###################################################
### code chunk number 17: iqLearn.Rnw:636-637
###################################################
s1mainInts = c (1,3)


###################################################
### code chunk number 18: iqLearn.Rnw:640-642
###################################################
fitIQ1main = learnIQ1main (object=fitIQ2, H1Main=s1vars,
    A1=bmiData$A1, s1mainInts=s1mainInts)


###################################################
### code chunk number 19: iqLearn.Rnw:650-651
###################################################
plot (fitIQ1main)


###################################################
### code chunk number 20: iqLearn.Rnw:660-661
###################################################
fitIQ1main$alphaHat0


###################################################
### code chunk number 21: iqLearn.Rnw:664-665
###################################################
fitIQ1main$alphaHat1


###################################################
### code chunk number 22: iqLearn.Rnw:681-687
###################################################
fitIQ1cm = learnIQ1cm (~ gender + race + parent_BMI + 
  baseline_BMI + A1*(gender + parent_BMI + baseline_BMI),
  data=bmiData, treatName="A1", intNames=c ("gender", "parent_BMI",
                                    "baseline_BMI"), 
  s2object=fitIQ2);
summary (fitIQ1cm)


###################################################
### code chunk number 23: iqLearn.Rnw:698-699
###################################################
s1cmInts = c (1,3,4)


###################################################
### code chunk number 24: iqLearn.Rnw:702-704
###################################################
fitIQ1cm = learnIQ1cm (object=fitIQ2, H1CMean=s1vars, A1=bmiData$A1,
    s1cmInts=s1cmInts); 


###################################################
### code chunk number 25: iqLearn.Rnw:709-710
###################################################
plot (fitIQ1cm)


###################################################
### code chunk number 26: iqLearn.Rnw:720-721
###################################################
fitIQ1cm$betaHat10


###################################################
### code chunk number 27: iqLearn.Rnw:724-725
###################################################
fitIQ1cm$betaHat11


###################################################
### code chunk number 28: iqLearn.Rnw:733-734
###################################################
fitIQ1var = learnIQ1var (fitIQ1cm)


###################################################
### code chunk number 29: iqLearn.Rnw:739-740
###################################################
fitIQ1var = learnIQ1var (object=fitIQ1cm, method="homo")


###################################################
### code chunk number 30: iqLearn.Rnw:758-765
###################################################
fitIQ1var = learnIQ1var (~ gender + race + parent_BMI + 
  baseline_BMI + A1*(parent_BMI), data=bmiData, treatName="A1",
    intNames=c ("parent_BMI"), method="hetero", cmObject=fitIQ1cm)  
s1varInts = c (3, 4)
fitIQ1var = learnIQ1var (object=fitIQ1cm, H1CVar=s1vars,
    s1sInts=s1varInts, method="hetero")
summary (fitIQ1var)


###################################################
### code chunk number 31: iqLearn.Rnw:770-771
###################################################
plot (fitIQ1var)


###################################################
### code chunk number 32: iqLearn.Rnw:779-780
###################################################
fitIQ1var$gammaHat0


###################################################
### code chunk number 33: iqLearn.Rnw:783-784
###################################################
fitIQ1var$gammaHat1


###################################################
### code chunk number 34: iqLearn.Rnw:801-802
###################################################
fitResids = iqResids (fitIQ1var)


###################################################
### code chunk number 35: iqLearn.Rnw:806-807
###################################################
plot (fitResids)


###################################################
### code chunk number 36: iqLearn.Rnw:825-827
###################################################
fitIQ1 = learnIQ1 (mainObj=fitIQ1main, cmObj=fitIQ1cm, sigObj=fitIQ1var, 
    dens="nonpar")  


###################################################
### code chunk number 37: iqLearn.Rnw:852-859
###################################################
h1 = c (1, 1, 30, 35)
h1main = h1
h1cm = h1
h1var = h1
optIQ1 = IQ1 (mainObj=fitIQ1main, cmObj=fitIQ1cm, sigObj=fitIQ1var,
    dens="nonpar", h1main=h1main, h1cm=h1cm, h1sig=h1var)
optIQ1


###################################################
### code chunk number 38: iqLearn.Rnw:870-873
###################################################
h2 = c (1, 30, 45);
optIQ2 = IQ2 (fitIQ2, h2);
optIQ2


###################################################
### code chunk number 39: iqLearn.Rnw:890-894
###################################################
fitQ2 = qLearnS2 (H2=s2vars, Y=y, A2=bmiData$A2, s2ints=s2ints);
fitQ2 = qLearnS2 (y ~ gender + parent_BMI + month4_BMI +
  A2*(parent_BMI + month4_BMI), data=bmiData, treatName="A2", 
  intNames=c("parent_BMI", "month4_BMI")); 


###################################################
### code chunk number 40: iqLearn.Rnw:903-908
###################################################
fitQ1 = qLearnS1 (object=fitQ2, H1q=s1vars, A1=bmiData$A1,
    s1ints=c(3,4)); 
fitQ1 = qLearnS1 (~ gender + race + parent_BMI + baseline_BMI +
  A1*(gender + parent_BMI), data=bmiData, treatName="A1", 
    intNames=c ("gender", "parent_BMI"), qS2object=fitQ2);  


###################################################
### code chunk number 41: iqLearn.Rnw:916-920
###################################################
fitQ2$betaHat20
fitQ2$betaHat21
fitQ1$betaHat10
fitQ1$betaHat11


###################################################
### code chunk number 42: iqLearn.Rnw:940-948
###################################################
summary (fitQ1)
h1q = c (1, 1, 30, 35);
optQ1 = qLearnQ1 (fitQ1, h1q);
optQ1
summary (fitQ2)
h2q = c (1, 30, 45);
optQ2 = qLearnQ2 (fitQ2, h2q);
optQ2


###################################################
### code chunk number 43: iqLearn.Rnw:986-989
###################################################
estVal = value (d1=fitIQ1$optA1, d2=fitIQ2$optA2, Y=y, A1=bmiData$A1, 
  A2=bmiData$A2)
estVal


