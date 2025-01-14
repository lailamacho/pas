# Otevrete datovy soubor Deti.RData

library(DescTools)
library(TeachingDemos)
library(rstatix)
library(datarium)
# aktivace knihoven

#################################
## Jednovyberovy t-test

# Na zaklade intervalu spolehlivosti rozhodnete, zda stredni hodnota
#   vysky dvanactiletych deti muze byt 152 cm?
#   A co 153 cm, 154 cm, 155, cm, 156 cm, ...
vyska <- Deti$vyska12
MeanCI(vyska)
  # rozhodnuti je stejne, jako u jednovyberoveho t-testu
  t.test(vyska, mu=153)

## Vykresleni p-hodnoty
# Jednovyberovy test, nejprve pro jednostrannou alternativu 
# Budeme testovat, zda jsou dvanactilete deti v prumeru mensi nez 156 cm
# testovane hypotezy: H0: vyska dvanactiletych deti = 156 cm
#					  H1: vyska dvanactiletych deti < 156 cm
t.test(vyska, mu=156, alternative="less")
  # p-hodnota = 0.01889 < alfa (= 0.05) -> zamitam H0, plati H1
  # Stredni hodnota vysky dvanactiletych deti je mensi nez 156 cm.

# t-test pracuje s testovou statistikou T: T = sqrt(n)*(mean(X) - mu)/sd(X)
#	pro tuto statistiku je pak definovana p-hodnota

# p-hodnota je pravdepodobnost, ze za platnosti nulove hypotezy
# 	nastane vysledek, ktery nastal,
# 	nebo jakykoliv jiny, ktery jeste vic vyhovuje alternative

# graf - definice p-hodnoty
# 1. pravdepodobnost, ze za platnosti nulove hypotezy ...
#   H0 rika, ze testova statistika T ma t-rozdeleni o n-1 stupnich volnosti
plot(x <- seq(-4,4,by=0.1), y <- dt(x,length(vyska)-1), type="l", 
     col="blue", main="Hustota t-rozdeleni za H0")
  # hustota t-rozdeleni

# 2. nastane vysledek, ktery nastal ...
(T <- sqrt(length(vyska))*(mean(vyska)-156)/sd(vyska))
  # testova statistika 
lines(c(T,T), c(0,dt(T,length(vyska)-1)), col="red", lwd=2)
  # zakreslim do grafu

# 3. nebo jakakoliv jina hodnota, ktera jeste vic odpovida alternative
#   alternativa je mensi nez
xx <- c(seq(-4,T,by=0.1), T, T, -4)
yy <- c(dt(c(seq(-4,T,by=0.1),T), length(vyska)-1), 0, 0)
polygon(xx, yy, density=40, col="red")

# rucni vypocet p-hodnoty
# pravdepodobnost hodnot mensich nez testova statistika T
pt(T, length(vyska)-1)
  # p-hodnota je distribucni funkce v hodnote testove statistiky

# v pripade oboustranne alternativy pridam jeste druhou skupinu hodnot 
#   - symetricky podle testove statistiky T
#	U otazky typu: Muze byt populacni prumer dvanactiletych deti 156 cm?
#   H0: vyska dvanactiletych deti = 156 cm vs. H1: vyska dvanactiletych deti <> 156 cm
xx2 <- c(-T, -T, seq(-T,4,by=0.1), 4, -T)
yy2 <- c(0, dt(c(-T,seq(-T,4,by=0.1)),length(vyska)-1), 0, 0)
polygon(xx2, yy2, density=40, col="green")

# rucni vypocet p-hodnoty
pt(T,length(vyska)-1) + (1 - pt(-T,length(vyska)-1))
  2*pt(T, length(vyska)-1)
  # t-rozdeleni je symetricke kolem nuly
t.test(vyska, mu=156)
  # kontrolni test
  # p-hodnota 0.03778 < alfa 0.05 -> zamitame H0
  #   prokazalo se, ze vyska dvanactiletych deti neni 156 cm

# Jake rozdeleni ma p-hodnota za platnosti nulove hypotezy? 
#	Jaka je pst, ze Vam vyjde p < 0.05? A jaka je pst, ze Vam vyjde p < 0.5?
# Vyzkousime empiricky
#	Predpokladejme, ze IQ ma normalni rozdeleni se stredni hodnotou 100 a rozptylem 225
#   provedeme nahodny vyber z tohoto rozdeleni o rozsahu 200
#	  a otestujeme nulovou hypotezu, ze stredni hodnota = 100
#   ziskanou p-hodnotu zakreslim do grafu a cely postup opakuji 1000 krat
#	vysledkem bude graf rozdeleni p-hodnot

N <- 1000			# pocet vyberu
n <- 200			# pocet pozorovani v jednom vyberu
p.hodnoty <- rep(0,N)	# prazdny vektor pro prumery
for (i in 1:N){
  vyber <- round(rnorm(n,100,sqrt(225)),0)
  p.hodnoty[i] <- t.test(vyber,mu=100)$p.value
}
hist(p.hodnoty)
  # v idealnim pripade by vysly vsechny sloupce stejne vysoke
(Y = sum(p.hodnoty <= 0.05))
  # v kolika pripadech vysla p-hodnota < 0.05
(Y = sum(p.hodnoty <= 0.5))
  # v kolika pripadech vysla p-hodnota < 0.5

# => p-hodnota ma rovnomerne rozdeleni na intervalu [0,1]

## Co je sila testu
# Pravdepodobnost, ze zamitnu nulovou hypotezu, kdyz plati vybrana alternativa
power.examp()
power.examp(n = 25)
power.examp(alpha = 0.1)

###################################
### Jednovyberovy test - obecny postup

## Zjistete, zda stredni hodnota systolickeho tlaku jedenactiletych deti muze byt 113.
syst <- Deti$syst11

# Nejprve otestujeme normalitu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
PlotQQ(syst, pch=19)
shapiro.test(syst)
  # i Q-Q plot, i test normality ukazuji, ze promenna nema normalni rozdeleni
  #   p-hodnota 4.8e-07 < 0.05 -> zamitame H0, plati H1

# pouzijeme neparametricky test
# Testujeme
#   H0: median syst11 = 113 vs. H1: median syst11 <> 113

# Znamenkovy test
syst.n <- syst[syst != 113]
  # znamenkovy test nepracuje s hodnotami, ktere se primo rovnaji testovanemu cislu
  #   vynecham hodnoty rovne 113
binom.test(sum(syst.n > 113), length(syst.n))
  # p-hodnota 0.1586 > alfa 0.05 -> nezamitam H0
  # Stredni hodnota systolickeho tlaku jedenactiletych deti muze byt 113
  #   (neprokazala jsem, ze to neni 113)

# Wilcoxonuv test (silnejsi nez znamenkovy)
wilcox.test(syst, mu=113)
  # p-hodnota 0.4968 > alfa 0.05 -> nezamitam H0
  # I na zaklade tohoto testu muze byt stredni hodnota systolickeho tlaku
  #   jedenactiletych deti 113

##############################

## Samostatne

## Na petiprocentni hladine vyznamnosti zjistete, zda muze byt populacni 
#   prumer vahy dvanactiletych deti 44 kg (hmot12)?
# A co na desetiprocentni hladine vyznamnosti?
## Zjistete, zda stredni hodnota tepu jedenactiletych deti je vetsi nez 70.

##############################
## Parovy t-test

## Zmenilo se procento tuku u deti mezi jedenactym a dvanactym rokem?
tuk12 <- Deti$X.tuk12
tuk11 <- Deti$X.tuk11

# Predpokladem paroveho testu je normalita rozdilu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
PlotQQ(tuk12 - tuk11, pch=19)
hist(tuk12 - tuk11)
shapiro.test(tuk12 - tuk11)
  # p-hodnota 0.007159 < alfa 0.05 -> zamitame H0, plati H1
  #   data nemaji normalni rozdeleni

# v pripade primerene symetrickeo rozdeleni bez odlehlych hodnot
#   mohu pouzit t-test, i kdyz je normalita ciselnym testem zamitnuta

# testovane hypotezy
#   H0: stredni hodnota rozdilu tuk12-tuk11 = 0 
#   H1: stredni hodnota rozdilu tuk12-tuk11 <> 0
t.test(tuk12 - tuk11)
  # p-hodnota 0.2091 > alfa 0.05 -> nezamitame H0
  # Neprokazali jsme, ze by se procento tuku zmenilo. 
  wilcox.test(tuk12 - tuk11)
    # Wilcoxonuv test dava stejny zaver jako t-test

## Zvysila se vaha deti mezi jedenactym a dvanactym rokem o 2 kg?

##############################
## Dvouvyberovy t-test

## Lisi se u jedenactiletych deti nalezita vitalni kapacita mezi pohlavimi (X.nvk11, pohlavi)?
cislo <- Deti$X.nvk11
kategorie <- Deti$pohlavi

# Normalita se testuje pro kazdou skupinu zvlast
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
oldpar <- par(mfrow = c(1,2))
tapply(cislo, kategorie, PlotQQ)
par(oldpar)
tapply(cislo, kategorie, shapiro.test)
  # obe p-hodnoty vetsi nez alfa -> nezamitame H0, 
  #   data maji priblizne normalni rozdeleni -> pouziji t-test

# nejprve graficke zobrazeni
boxplot(cislo ~ kategorie, col="mistyrose", border="darkred")
  plot(cislo ~ kategorie, col= "oldlace", border = "orange4")
  # vidime poradi kategorii, muze se hodit

# testujeme hypotezy
#   H0: X.nvk11 hosi - X.nvk11 divky = 0
#   H1: X.nvk11 hosi - X.nvk11 divky <> 0

# Mame na vyber dva dvouvyberove t-testy:
#   pro shodne rozptyly
#   pro ruzne rozptyly

# Jsou rozptyly shodne?
#   H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo ~ kategorie)
  # p-hodnota 0.2931 > alfa 0.05 -> nezamitame H0
  #   rozptyly jsou priblizne shodne, pouzijeme t-test pro shodne rozptyly
t.test(cislo ~ kategorie, var.eq = T)
  t.test(cislo ~ kategorie, mu=0, alternative = "two.sided", var.eq = T)
  # p-hodnota 0.004854 < alfa 0.05 -> zamitame H0, plati H1
  #   nalezita vitalni kapacita plic se lisi podle pohlavi
  # A kdo ji ma vyssi?

## Lisi se u jedenactiletych deti diastolicky tlak mezi pohlavimi (dias11, pohlavi)?
cislo <- Deti$dias11
kategorie <- Deti$pohlavi

# Normalita se testuje pro kazdou skupinu zvlast
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
oldpar <- par(mfrow=c(1,2))
tapply(cislo, kategorie, PlotQQ)
par(oldpar)
tapply(cislo, kategorie, shapiro.test)
  # obe p-hodnoty mensi nez alfa -> zamitame H0, data nemaji normalni rozdeleni
  
# Wilcoxonuv test
#   H0: dias11 hosi - dias11 divky = 0;  H1: dias11 hosi - dias11 divky <> 0
# Graficky
plot(cislo ~ kategorie, col = "lightskyblue", border = "darkblue")
  # vidime poradi kategorii, muze se hodit
  
# I u dvouvyberoveho Wilcoxonova testu je pozadavek na shodu rozptylu
#   H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo ~ kategorie)
  # p-hodnota 0.1415 > alfa 0.05 -> nezamitame H0
  # predpoklad shody rozptylu je splnen
wilcox.test(cislo ~ kategorie)
  # p-hodnota 0.5857 > alfa 0.05 -> nezamitame H0
  #   diastolicky tlak jedenactiletych deti se podle pohlavi nelisi
  
## Lisi se mezi pohlavimi BMI u dvanactiletych?
## Maji dvanactilete divky vyssi procento tuku nez hosi?

#################################
# ANOVA

# Lisi se nalezita vitalni kapacita dvanactiletych deti mezi jednotlivymi druhy sportu (hmot12, sport)?
cislo <- Deti$X.nvk12
kategorie <- Deti$sport

# Test normality se zde provadi pres residua modelu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
res <- residuals(lm(cislo ~ kategorie))
PlotQQ(res, pch=19)
  shapiro.test(res)
    # p-hodnota 0.5624 > alfa 0.05 -> nezamitam H0
  #   residua maji priblizne normalni rozdeleni -> pouziji klasickou ANOVU

# Testuji se hypotezy
#   H0: vaha se nelisi; H1: vaha se lisi
# Graficky
plot(cislo ~ kategorie, col = "mistyrose", border = "darkred")

# I zde musim rozlisovat pripad, kdy se rozptyl lisi ve skupinach a kdy ne
#   H0: rozptyly se nelisi; H1: rozptyly se lisi
bartlett.test(cislo ~ kategorie)
  # p-hodnota = 0.1311 > alfa (0.05) -> nezamitame H01
  #   rozptyly se nelisi -> pouziji ANOVu pro shodne rozptyly

# Ciselne
anova(aov(cislo ~ kategorie))
  # p-hodnota 1.097e-05 < alfa 0.05 -> zamitam H0
  #   NVK u dvanactiletych se vyznamne se lisi mezi sporty.

# Ktere dvojice sportu se mezi sebou vyznamne lisi?
# Parove srovnani
TukeyHSD(aov(cislo ~ kategorie))
  # vsechny dvojice se od sebe vyznamne lisi
plot(TukeyHSD(aov(cislo ~ kategorie)))
  # vyznamne rozdily mezi dvojicemi graficky
  #   tam, kde je rozdil vyznamny, interval neobsahuje nulu (svisla carkovana cara)

# Analyza rozptylu pro pripad, ze se lisi variabilita ve skupinach
oneway.test(cislo ~ kategorie, var.equal = FALSE)

## Lisi se BMI jedenactiletych deti mezi jednotlivymi druhy sportu (bmi11, sport)?
cislo <- Deti$bmi11
kategorie <- Deti$sport

# Graficke znazorneni
plot(cislo ~ kategorie, col="palegreen", border="darkgreen")

# Test normality 
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
res <- residuals(lm(cislo ~ kategorie))
PlotQQ(res, pch=19)
shapiro.test(res)
  # p-hodnota 5.473e-08 < alfa 0.05
  #   normalitu zamitam -> pouziji Kruskal-Wallisovu ANOVU
# predpoklad shody rozptylu
#   H0: rozptyly se nelisi; H1: rozptyly se lisi
bartlett.test(cislo ~ kategorie)
  # p-hodnota 0.0001701 < alfa 0.05
  # predpoklad neni splnen, vysledky je treba kontrolovat graficky
  #   a ohodnotit, zda nejsou zpusobeny jen ruznymi rozptyly

# testujeme
#   H0: bmi11 se nelisi; H1: bmi11 se lisi
# Kruskal-Wallisova anova
kruskal.test(cislo ~ kategorie)
  # p-hodnota 0.003101 < alfa (= 0.05) -> zamitam H0
  #   bmi jedenactiletych se mezi sporty lisi 

# Parove srovnani
DunnTest(cislo ~ kategorie)
  # lisi se atletika a ledni hokej
  
## A lisi se v zavislosti na sportu systolicky tlak jedenactiletych (syst11, sport)?
## Lisi se v zavislosti na sportu procento tuku jedenactiletych deti?
## Lisi se v zavislosti na sportu vyska dvanactiletych deti?

##################################

## ANOVA pro opakovana mereni - porovnani zavislych vyberu
data("selfesteem", package = "datarium")
  # nacteni dat, u deseti jedincu bylo trikrat v case mereno skore
  #   lisi se hodnoty v jednotlivych casech?

# Pro vyhodnoceni nejprve prevedeme data do jineho formatu
id <- rep(selfesteem$id, 3)
time <- c(rep("t1",10), rep("t2",10), rep("t3",10))
score <- c(selfesteem$t1, selfesteem$t2, selfesteem$t3)
vstup <- data.frame("id"=as.factor(id), time, score)

# V tomto formatu uz mohu testovat normalitu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
res <- residuals(lm(score ~ time, data=vstup))
PlotQQ(res)
shapiro.test(res)
  # p-hodnota 0.1892 > alfa 0.05 -> nezamitam H0
  #   normalitu nezamitam -> pouziji parametrickou ANOVU s opakovanymi merenimi

# testujeme
#   H0: skory v ruznych casech se nelisi; H1: skory se v case meni
# Graficky
boxplot(score ~ time, data=vstup, col="lightblue", border="royalblue")

# Ciselny test
res.aov <- anova_test(score~time + Error(id/time), data=vstup)
get_anova_table(res.aov)
  # p-hodnota 2.01e-08 < alfa 0.05 -> zamitam H0
  #   hodnoty se v case lisi

# v pripade, ze bych mela nenormalni (napr. usporadana) data
# se pouzije Friedmanuv test
pom <- as.matrix(selfesteem[,-1])
friedman.test(pom)
friedman.test(score ~ time | id, data = vstup)
  # p-hodnota 0.0001117 < alfa 0.05 -> zamitam H0
  #   hodnoty se v case lisi