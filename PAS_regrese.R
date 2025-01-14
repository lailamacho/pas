# Otevrete datovy soubor Deti.RData

library(DescTools)
library(TeachingDemos)
library(lmtest)
  # aktivace knihoven

#############################
##### Vztah dvou ciselnych promennych
### Korelacni koeficient

## Souvisi spolu nalezita vitalni kapacita a vyska jedenactiletych deti?
prom1<-Deti$X.nvk11
prom2<-Deti$vyska11
# Graficky znazornime pomoci bodoveho grafu
plot(prom1~prom2,pch=19, main="Bodovy graf zavislosti")
  # na grafu je videt mirna rostouci linearni zavislost

# souvislost hodnotime korelacnim koeficientem
#   Pearsonuv pro normalne rozlozena data
#   Spearmanuv pro nenormalne rozlozena data
#   Kendalluv pro usporadane kategoricke promenne

# Test normality pro kazdou promennou zvlast
par(mfrow=c(1,2))
PlotQQ(prom1);PlotQQ(prom2)
par(mfrow=c(1,1))
  # body lezi priblizne na primce, data maji priblizne normalni rozdeleni
# Shapiro-Wilkuv test normality
#   H0: normalita vs. H1: neni normalita
shapiro.test(prom1)
shapiro.test(prom2)
  # obe p-hodnoty > alfa 0.05 -> nezamitame H0 -> 
  #   data maji priblizne normalni rozdeleni
  #   pouziji Pearsonuv korelacni koeficient

# Pearsonuv korelacni koeficient
cor(prom1,prom2)
  # hodnota 0.32 znamena, ze jde o velmi mirnou zavislost
# test nezavislosti
#   H0: promenne spolu linearne nesouvisi vs. H1: promenne spolu linearne souvisi
#   H0: korelacni koeficient = 0 vs. H1: korelacni koeficient <> 0 
cor.test(prom1,prom2)
  # p-hodnota 7.7e-07 < alfa -> zamitame H0
  #   I pres malou hodnotu korelacniho koeficientu je zavislost statisticky vyznamna.

## Souvisi spolu diastolicky tlak a tep jedenactiletych deti?
prom1<-Deti$tep11
prom2<-Deti$dias11

# Nejprve graf
plot(prom1~prom2,pch=19, main="Bodovy graf zavislosti")
  # Z grafu je zrejme, ze data nemaji normalni rozdeleni

# Q-Q plot pro normalitu obou promennych
par(mfrow=c(1,2))
PlotQQ(prom1);PlotQQ(prom2)
par(mfrow=c(1,1))
  # z obou grafu je patrne, ze data nemaji normalni rozdeleni
# Shapiro-Wilkuv test normality
  # H0: normalita vs. H1: neni normalita
shapiro.test(prom1)
shapiro.test(prom2)
  # obe p-hodnoty < alfa 0.05 -> zamitame H0
  #   normalita neni splnena, pouziji Spearmanuv korelacni koeficient

# Spearmanuv korelacni koeficient
cor(prom1,prom2,method="spearman")
# nebo pres poradi
R.prom1<-rank(prom1)
R.prom2<-rank(prom2)
cor(R.prom1,R.prom2)
  # je videt, ze vychazi stejne
  # hodnota 0.1 znaci temer nezavislost

# a co test - je mezi promennymi vyznamna (monotonni) zavislost
#   H0: promenne spolu monotonne nesouvisi vs. H1: promenne spolu souvisi
cor.test(prom1,prom2,method="spearman")
# p-hodnota 0.1281 > alfa (0.05) -> nezamitam H0
#   -> mezi tepem a diastolickym tlakem jedenactiletych deti neni
#   monotonni souvislost.

## Souvisi spolu tep v jedenactem a ve dvanactem roce veku?
## Souvisi spolu hmotnost a procento tuku v jedenactem roce?

# Souvisi spolu diastolicky tlak v jedenactema ve dvanactem roce?
prom1<-Deti$dias11
prom2<-Deti$dias12
# graficky
plot(prom1~prom2,pch=19, main="Bodovy graf zavislosti")
# z grafu toho moc nevidim, jen ze mam nekolik malo usporadanych hodnot

# Vztah dvou ordinalnich velicin resi Kendalluv korelacni koeficient
cor(prom1,prom2,method="kendall")
# hodnota 0.22 znaci velmi slabou az temer zadnou zavislost 
# Test o jeho vyznamnosti
#   H0: promenne spolu linearne nesouvisi vs. H1: promenne spolu linearne souvisi
cor.test(prom1,prom2,method="kendall")
  # p-hodnota 0.0001611 < alfa(0.05) -> zamitam H0
  #   diastolicky tlak v jedenactem roce souvisi s diastolickym 
  #   tlakem ve dvanactem roce

##############################
## Jednoducha linearni regrese

## Jakym zpusobem zavisi hmotnost na vysce jedenactiletych deti?
prom1<-Deti$hmot11
prom2<-Deti$vyska11
#	graficky opet bodovy graf
plot(prom1~prom2,pch=19, main="Bodovy graf zavislosti",xlab="vyska",ylab="hmotnost")
abline(lm(prom1~prom2),col=2,lwd=2)
  # prikresli primku linearni zavislosti

# Odhad modelu linearni regrese
Model1<-lm(prom1~prom2)
  # odhad regresni primky
coef(Model1)
  # regresni primka ma tvar hmot11 = -73.81 + 0.75 * vyska11
  #   Zvysi-li se vyska o 1 cm, vzroste hmotnost v prumeru o 0.75 kg
# vyznamnost zavislosti
#   H0: hmotnost na vysce nezavisi vs. H1: hmotnost na vysce zavisi
summary(Model1)
  # p-hodnota v radku vyska11 2*e-16 < alfa 0.05 -> zamitame H0 -> hmotnost na vysce zavisi
  # Koeficient determinace (Multiple R-squared)
  #   Zavislosti na vysce se vysvetlilo 61.5% variability hmotnosti.

# Overeni predpokladu
#	graficke testy
par(mfrow=c(2,2))
plot(Model1)
par(mfrow=c(1,1))
  # linearni vztah: na prvnim grafu nema byt videt trend 
  #   zde je mirny oblouk, ale ne moc vyrazny - OK
  # normalita residui: na druhem grafu maji body lezet na primce 
  #   v horni casti utikaji - s normalitou muze byt problem
  # stabilita rozptylu: krivka na tretim grafu nema mit trend
  #   zde mirne stoupa - se stabilitou rozptylu muze byt problem
  # vlivna pozorovani: na ctvrtem grafu nemaji body lezet vne mezi 
  #   splneno - OK 

# ciselne testy
# test stability rozptylu
#	  H0: rozptyl se nemeni vs. H1: rozptyl se meni v zavislosti na nezavisle promenne
bptest(Model1)
  # p-hodnota 0.01046 < alfa 0.05 -> zamitame H0 -> predpoklad stability rozptylu neni splnen
# test normality residui
# 	H0: residua maji normalni rozdeleni vs. H1: residua normalni rozdeleni nemaji
shapiro.test(residuals(Model1))
  # p-hodnota 4.627e-08 < alfa 0.05 -> zamitame H0 -> predpoklad normality residui neni splnen

# Celkovy zaver: Prokazala se zavislost hmotnosti na vysce u jedenactiletych deti ve tvaru 
#  hmot11 = -73.81 + 0.75 * vyska11.	Modelem se vysvetlilo 61.5% variability zavisle promenne.
#  Predpoklady nejsou splneny - vysledky nemusi byt dostatecne presne.

# Predpoved
# Jakou ocekavame hmotnost u jedenactileteho ditete s vyskou 160 cm?
(b<-coef(Model1))
b[1]+b[2]*160
-73.81 + 0.75 *160
  # U jedenactileteho ditete s vyskou 160 cm ocekavame hmotnost 46.86 kg.

## Popiste zavislost procenta tuku na hmotnosti dvanactiletych deti (promenne X.tuk12, hmot12)
#  Kolik procent variability procenta tuku se zavislosti vysvetli?
#  Jake je ocekavane procento tuku pro dite s vahou 45 kg? 
#  Muze byt linearni clen roven 0.3?
prom1<-Deti$X.tuk12
prom2<-Deti$hmot12

#	graficke vykresleni zavislosti
plot(prom1~prom2,pch=19);abline(lm(prom1~prom2),col="red",lwd=2)  
  # zavislost je na prvni pohled zrejma

# Odhad modelu linearni regrese
Model2<-lm(prom1~prom2)
  # odhad regresni primky
coef(Model2)
  # regresni primka ma tvar X.tuk12 = 3.83 + 0.2688 * hmot12
  #   Na jeden kilogram hmotnosti pripada v prumeru 0.2688 procenta tuku.
confint(Model2)
  # Interval spolehlivosti pro linearni clen obsahuje hodnotu 0.3. 
  #   Na hladine vyznamnosti 5% tedy nemohu hodnotu 0.3 pro linearni clen vyloucit.
# vyznamnost zavislosti
#   H0: procento tuku na hmotnosti nezavisi vs. H1: procento tuku na hmotnosti zavisi
summary(Model2)
  # p-hodnota v radku hmot12 vyska 2e-16 < alfa 0.05 -> zamitame H0 -> procento tuku na hmotnosti zavisi
  # Koeficient determinace (Multiple R-squared)
  # Zavislosti na hmotnosti se vysvetlilo 26.55% variability procenta tuku

# Overeni predpokladu
#	graficke testy
par(mfrow=c(2,2))
plot(Model2)
par(mfrow=c(1,1))
  # linearni vztah: na prvnim grafu neni evidentni trend - OK
  # normalita residui: na druhem grafu body lezi na primce - OK
  # stabilita rozptylu: krivka na tretim grafu v prvni casti roste 
  #   - se stabilitou rozptylu muze byt problem
  # vlivna pozorovani: na ctvrtem grafu body nelezi vne mezi - OK 

# ciselne testy
# test stability rozptylu
#	  H0: rozptyl se nemeni vs. H1: rozptyl se meni v zavislosti na nezavisle promenne
bptest(Model2)
  # p-hodnota 0.002501 < alfa 0.05 -> zamitame H0 -> predpoklad stability rozptylu neni splnen
# test normality residui
# 	H0: residua maji normalni rozdeleni vs. H1: residua normalni rozdeleni nemaji
shapiro.test(residuals(Model2))
  # p-hodnota 0.8245 > alfa 0.05 -> nezamitame H0 -> predpoklad normality residui je splnen

# Celkovy zaver: Prokazala se zavislost procenta tuku na hmotnosti u dvanactiletych deti
#   ve tvaru X.tuk12 = 3.83 + 0.2688 * hmot12.
#   Modelem se vysvetlilo 26.6% variability zavisle promenne. Linearni clen muze byt roven 0.3.
#   Predpoklad stability rozptylu neni splnen - vysledky nemusi byt dostatecne presne.

# Predpoved
# Jaka je ocekavane procento tuku pro dite s hmotnosti 50 kg?
(b<-coef(Model2))
b[1]+b[2]*50
3.83 + 0.2688 *50
  # U dvanactileteho ditete s hmotnosti 50 kg ocekavame procento tuku 17.27%.

## Jak zavisi hmotnost dvanactiletych deti na jejich hmotnosti v jedenacti letech?


