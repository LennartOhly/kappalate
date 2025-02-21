

//Replication of the results SUW Version 24 March, 2023
//This version: 19092023

clear

do "kappalate.ado"
do "cbps.ado"

cls

//Angrist 1991

use "Angrist1991_data.dta", clear

ge age2 = age_5^2
ge age3 = age_5^3
cls

log using replication_suw
//Table 4
//Col (1)
kappalate lwage nvstat rsncode nrace educ, which(all) zmodel(logit)
kappalate lwage nvstat rsncode nrace educ, zmodel(cbps)

//Col (2)
kappalate lwage nvstat rsncode age_5, which(all) zmodel(logit)
kappalate lwage nvstat rsncode age_5, zmodel(cbps)

//col (3)
kappalate lwage nvstat rsncode age_5 age2 age3, which(all) zmodel(logit)
kappalate lwage nvstat rsncode age_5 age2 age3, zmodel(cbps)

//col (4)
kappalate lwage nvstat rsncode nrace educ age_5, which(all) zmodel(logit)
kappalate lwage nvstat rsncode nrace educ age_5, zmodel(cbps)


//col (5)
kappalate lwage nvstat rsncode nrace educ age_5 age2 age3, which(all) zmodel(logit)
kappalate lwage nvstat rsncode nrace educ age_5 age2 age3, zmodel(cbps) 

log off
cls
//Card

use "Card1995_data.dta", clear

log on
//Table 5:

//Some college

//Col (1) X_c
kappalate lwage somecol nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, which(all) zmodel(logit)
kappalate lwage somecol nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, zmodel(cbps)

//Col (2) X_K
kappalate lwage somecol nearc4 smsa smsa66 black south south66, which(all) zmodel(logit)
kappalate lwage somecol nearc4 smsa smsa66 black south south66, zmodel(cbps)



//two year degree

//Col (3) X_c
kappalate lwage educ14 nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, which(all) zmodel(logit)
kappalate lwage educ14 nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, zmodel(cbps)

//Col (4) X_K
kappalate lwage educ14 nearc4 smsa smsa66 black south south66, which(all) zmodel(logit)
kappalate lwage educ14 nearc4 smsa smsa66 black south south66, zmodel(cbps)

//four year degree 

//Col (5) X_c
kappalate lwage educ16 nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, which(all) zmodel(logit)
kappalate lwage educ16 nearc4 black south smsa smsa66 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 exper expersq, zmodel(cbps)

//Col (6) X_K
kappalate lwage educ16 nearc4 smsa smsa66 black south south66, which(all) zmodel(logit)
kappalate lwage educ16 nearc4 smsa smsa66 black south south66, zmodel(cbps)

log off
//Farbmacher 2018
use "Farbmacher2018_data.dta", clear
cls
log on
//Table 6

//Col (1) 
kappalate workedm morekids Zddot agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate workedm morekids Zddot agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)


//Col (2) 
kappalate workedm morekids Zdot agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate workedm morekids Zdot agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)



//Col (3) 
kappalate workedm morekids samesex agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate workedm morekids samesex agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)

//Col (4) 
kappalate lincomem morekids Zddot agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate lincomem morekids Zddot agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)


//Col (5) 
kappalate lincomem morekids Zdot agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate lincomem morekids Zdot agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)



//Col (6) 
kappalate lincomem morekids samesex agem agefstm sexk sex2nd blackm hispm othracem, which(all) zmodel(logit)
kappalate lincomem morekids samesex agem agefstm sexk sex2nd blackm hispm othracem, zmodel(cbps)

log close
