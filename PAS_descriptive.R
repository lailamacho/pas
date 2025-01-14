######################################
## Otevrete datovy soubor Deti (kliknout vpravo dole a otevrit)
#   Databaze pouzita na zpracovani DP kdysi davno
# prohlednete si soubor kliknutim na databazi v casti Environment

library(DescTools)
  # aktivace knihovny s popisnymi statistikami
library(TeachingDemos)
  # aktivace knihovny s vyukovymi pomuckami

######################################
## spocteme popisne statistiky pro vysku jedenactiletych deti
vyska <- Deti$vyska11

#####################
## Popisne statistiky polohy
mean(vyska) 
  # prumer 
  sum(vyska)/length(vyska)
    # prumer podle vzorce

# Jak ziskat median a dalsi percentily
sort(vyska)
  # usporadana rada
min(vyska)
  # minimum
max(vyska)
  # maximum
median(vyska)
  # median
quantile(vyska,0.25)
  # dolni kvartil
quantile(vyska,0.75)
  # horni kvartil

# Obecny percentil podle vzorce
#   Chceme oddelit jednu osminu nejmensich hodnot
n <- length(vyska)
  # pocet pozorovani
p <- 1/8
  # dil dat, ktere chci oddelit
(k <- floor(1+(n-1)*p)) 	
  # prikaz floor vraci celou cast
  # budu prumerovat 28 a 29 usporadanou hodnotu
sort(vyska)[c(k,k+1)]
(q <- (1+(n-1)*p) - k)	
  # vaha, se kterou se pozorovani prumeruji (desetinna cast) q = 0.625 
(O1 <- (1-q)*sort(vyska)[k]+q*sort(vyska)[k+1])
  # percentil v jedne osmine
  quantile(vyska,1/8)

fivenum(vyska)
  # zakladnich 5 percentilu najednou
summary(vyska)
  # to same vcetne prumeru

#######################
## Grafy
boxplot(vyska, col="orange",border="darkred", main="Boxplot pro vysku")
  # krabicovy graf
  # co je v nem vse videt?
hist(vyska,col="lightblue1",border="darkblue",labels=T,main="Histogram vysky",
     ylab="Absolutni cetnosti",xlab="vyska v cm",ylim=c(0,70))
  # histogram

# Sturgesovo pravidlo
# napovi, jaky je optimalni pocet sloupcu v histogramu
1+3.3*log10(n)
  # vyslo 9

################################
## Popisne statistiky variability 

var(vyska)
  # rozptyl
  sum((vyska-mean(vyska))^2)/(length(vyska)-1)
    # rozptyl podle vzorce 
sd(vyska)
  # smerodatna odchylka
  sqrt(var(vyska))
  # odmocnina z rozptylu

max(vyska)-min(vyska)
  # rozpeti
  diff(range(vyska))
    #	minimum a maximum
IQR(vyska)
  # mezikvartilove rozpeti
  quantile(vyska,3/4)-quantile(vyska,1/4)
    # mezikvartilove rozpeti podle vzorce
MAE(vyska,median(vyska))
  # stredni absolutni odchylka od medianu
  mean(abs(vyska-median(vyska)))
    # stredni absolutni odchylka podle vzorce

CoefVar(vyska)
  # variacni koeficient
  sd(vyska)/mean(vyska)
    # variacni koeficient podle vzorce

# Jake jsou jednotky jednotlivych charakteristik variability?
# Jak tyto charakteristiky interpretovat, k cemu se pouzivaji?

#############
## Popisne statistiky tvaru rozdeleni

# pocitaji se ze z-skoru
(z.vyska <- scale(vyska))
  # vypocte z-skory 
  (vyska-mean(vyska))/sd(vyska)
  # pomoci vzorce

# vlastnosti z-skoru
mean(z.vyska); sd(z.vyska)
  # prumer a smerodatna ochylka z-skoru
  #	z-skory jsou tvoreny tak, aby jejich prumer byl nula a rozptyl 1

Skew(vyska)
  # Sikmost
  mean(z.vyska^3)
    # sikmost podle vzorce
# zakladni charakteristiky sikmosti
# 	hodnota 0 odpovida symetrickemu rozdeleni
# 	kladne hodnoty znaci sesikmeni doleva, zaporne doprava

Kurt(vyska)  
  # Spicatost
  mean(z.vyska^4)-3
    # spicatost podle vzorce
# slouzi predevsim pro porovnani s normalnim rozdelenim
#   nastavena tak, ze hodnota 0 odpovida normalnimu rozdeleni
#   kladne hodnoty ukazuji na spicatejsi rozdeleni nez je normalni
#   zaporne hodoty ma rozdeleni, ktere je plossi nez normalni
#     u asymetrickeho rozdeleni se spicatost prilis hodnotit neda

# Graficke znazorneni ruznych hodnot sikmosti
oldpar <- par(mfrow=c(1,2))
  # rozdeleni grafickeho okna na dve casti
hist(d.pizza$temperature,main=paste("Sikmost = ",round(Skew(d.pizza$temperature,na.rm=T),3)))
hist(d.pizza$delivery_min,main=paste("Sikmost = ",round(Skew(d.pizza$delivery_min),3)))
  # ukazka kladne a zaporne sikmosti pro ukazkova data
par(oldpar)	
  # spojeni grafickeho okna

# Jake jsou jednotky sikmosti a spicatosti?

####################
# jak se meni hodnota prumeru, smerodatne odchylky, sikmosti a spicatosti
#	  se zmenou polohy (posunuti) a meritka (vynasobeni)?

# vytvorime dve nove promenne vychazejici z promenne vyska11
vyska.p <- vyska + 100
  # posunuti o 100
vyska.m <- vyska/ 100
  # zmena meritka - vydeleni 100

# jak se meni popisne statistiky: 
vyst <- matrix(NA,3,4)
vyst[1,1] <- mean(vyska); vyst[1,2] <- sd(vyska)
vyst[1,3] <- Skew(vyska); vyst[1,4] <- Kurt(vyska)
vyst[2,1] <- mean(vyska.p); vyst[2,2] <- sd(vyska.p)
vyst[2,3] <- Skew(vyska.p); vyst[2,4] <- Kurt(vyska.p)
vyst[3,1] <- mean(vyska.m); vyst[3,2] <- sd(vyska.m)
vyst[3,3] <- Skew(vyska.m); vyst[3,4] <- Kurt(vyska.m)
rownames(vyst) <- c("vyska11","vyska11+100","vyska11/100")
colnames(vyst) <- c("Prumer","Sm.odchylka","Sikmost","Spicatost")
vyst

# Graficka ukazka
oldpar <- par(mfrow=c(1,3))
hist(vyska); hist(vyska.p); hist(vyska.m);
par(oldpar)	
  # grafy jsou na prvni pohled stejne, jen hodnoty na x-ove ose se meni

####################################
## Popiste vahu dvanactiletych deti
vaha <- Deti$hmot12
# popisne statistiky polohy
summary(vaha)
# popisne statistiky variability
variabilita <- c(var(vaha),sd(vaha),diff(range(vaha)),IQR(vaha),MAE(vaha,median(vaha)),CoefVar(vaha))
names(variabilita) <- c("var","sd","range","IQR","MAE","CV")
variabilita
# popisne statistiky tvaru rozdeleni
tvar <- c(Skew(vaha),Kurt(vaha))
names(tvar) <- c("Sikmost","Spicatost")
tvar
# grafy
oldpar <- par(mfrow=c(1,2))
boxplot(vaha, col="orange",border="darkred", main="Boxplot pro hmotnost")
hist(vaha,col="lightblue1",border="darkblue",labels=T,main="Histogram hmotnosti",
     ylab="Absolutni cetnosti",xlab="hmotnost v kg",ylim=c(0,70))
par(oldpar)

## Samostatne pro procento tuku davnactiletych

####################################
## Popisne statistiky kategoricke promenne sport
sport <- Deti$sport
(ac <- table(sport))
  # tabulka absolutnich cetnosti
(rc <- round(prop.table(ac)*100,2))
  # relativni cetnosti (procenta zaokrouhlena na 2 desetinna mista)
cbind("absolutni"=ac,"relativni"=rc)

# graficke znazorneni, tedy sloupcovy a kolacovy graf
barplot(ac, ylim=c(0,125), col=2:4,main="Sloupcovy graf sportu")
x.val <- barplot(ac, plot=F)
text(x.val,ac,ac,pos=3)

  # sloupcovy graf s vyuzitim ruznych barev
popis <- paste(names(ac),"(",rc,"%)")
pie(ac,labels = popis, col=4:6,main="Kolacovy graf sportu") 
  # kolacovy graf

# Samostatne pro kategorizovane BMI jedenactiletych

###############################
## Testy normality

# Ma vyska jedenactiletych deti normalni rozdeleni?
vyska <- Deti$vyska11
# Prvne se podivam na histogram
hist(vyska, col="skyblue",border="darkblue",main="Histogram vysky 11-letych deti")
  # z tvaru histogramu popisuji, jak moc se lisi od Gaussovy krivky
  # je mozne si do histogramu tuto krivku prikreslit
  hist(vyska, col="skyblue",border="darkblue",main="Histogram vysky 11-letych deti",freq=F)
    # histogram s hodnotami hustoty na y-ove ose
  x.osa <- hist(vyska,plot=F)$breaks
  lines(x<-seq(min(x.osa),max(x.osa),by=0.1),dnorm(x,mean(vyska),sd(vyska)),col=2)
    # prikresli Gaussovu krivku - hustotu normalniho rozdeleni

# Dale komentuji, jak se sikmost a spicatost lisi od 0
tvar <- c(Skew(vyska),Kurt(vyska))
names(tvar) <- c("Sikmost","Spicatost")
tvar
  
# pravdepodobnostni graf - graficky test normality
PlotQQ(vyska,pch=19,cex=0.5)
  qqnorm(vyska,pch=19,cex=0.5); qqline(vyska,col=2,lwd=1.5)
  # bez intervalu
# pokud body lezi priblizne na primce, pak je rozdeleni priblizne normalni
#   pokud se od primky hodne odchyluji (hodne jich vybocuje z mezi intervalu)
#     rozdeleni normalni neni
#   pokud body lezi na oblouku, jsou data sesikmena, tj. nemaji normalni rozdeleni 

# ciselne testy - pro data royumneho rozsahu (kolem 100)
#   pokud p-hodnota < 0.05, pak data nemaji normalni rozdeleni
shapiro.test(vyska)
  # Shapiro-Wilkuv test
  LillieTest(vyska)
    # Lillieforsuvova adaptace Kolmogorov-Smirnovova testu 
  AndersonDarlingTest(vyska,"pnorm",mean=mean(vyska),sd=sd(vyska))
    # Anderson-Darlinguv test
# vsechny p-hodnoty > 0.05 => vyska11 ma priblizne normalni rozdeleni

## Zkuste si pro procento tuku jedenactiletych deti (X.tuk11)

###############################
## Bodovy a intervalovy odhad

## Odhad stredni hodnoty vysky jedenactiletych deti 
vyska <- Deti$vyska11
mean(vyska)
  # nejlepsi bodovy odhad stredni hodnoty
  # Jaka je chyba tohoto odhadu? Na cem zavisi?

# Mejme vektor vysek jedenactiletych deti a spocteme z nej vyberovy prumer
# 30 hodnot
mean(x <- rnorm(30,150,7))	# vyberovy prumer
MeanSE(x)			            # stredni chyba prumeru
  sd(x)/sqrt(30)          # podle vzorce

mean(x <- rnorm(60,150,7))
MeanSE(x)

mean(x <- rnorm(90,150,7))
MeanSE(x)
  # je videt, ze s rostoucim poctem pozorovani chyba prumeru klesa

# Jak se chova vyberovy prumer zjistime, kdyz jich spocitame vic
N <- 1000			# pocet prumeru
n <- 150			# pocet pozorovani do jednoho prumeru
prumery <- rep(0,N)	# prazdny vektor pro prumery
for (i in 1:N) prumery[i] <- mean(rnorm(n, 150, 7))
  # vypocet tisice prumeru
hist(prumery)		# histogram ma tvar Gaussovy krivky

# jake jsou parametry techto prumeru (stredni hodnota a rozptyl)
c("odhad"=mean(prumery),"skutecnost"=150)
c("odhad"=var(prumery),"skutecnost"=7^2/n)

###############################
## Centralni limitni veta
clt.examp(1)
clt.examp(2)
clt.examp(10)
clt.examp(50)
  # Jak rychle konverguji k normalite

###############################

#### Interval spolehlivosti pro stredni hodnotu

# Jaka je pravdepodobnost, ze se stredni hodnota 
#   bude skutecne rovnat vyberovemu prumeru?
# A jak tuto pravdepodobnost zvetsit?

# 95% interval spolehlivosti pro prumer vysky jedenactiletych deti
## Kdyz mi nekdo dopredu rekne, ze mam pracovat se smerodatnou odchylkou 7.
#   Smerodatna odchylka je dana, nepocita se z dat, pouziva se 
#     kvantil normalniho rozdeleni

# Podle vzorce
(q.n <- qnorm(0.975,0,1)) # kvantil normalniho rozdeleni
(n <- length(vyska))		# pocet pozorovani
(prumer <- mean(vyska))	# vyberovy prumer
(sm.od <- 7)	          # smerodatna odchylka
prumer - q.n*sm.od/sqrt(n)
  # dolni mez intervalu spolehlivosti
prumer + q.n*sm.od/sqrt(n)
  # horni mez intervalu spolehlivosti

# Jednoduchym prikazem
MeanCI(prom4,sd=7)
  # S  pravdepodobnosti 95% skutecna stredni hodnota vysky jedenactiletych
  #   deti lezi v intervalu od 147.86 cm do 149.7 cm. 

## Kdyz rozptyl neznam a musim ho odhadnout z dat, pouziva se 
#   kvantil t-rozdeleni o n-1 stupnich volnosti
# Spoctete 99%-ni interval spolehlivosti
# Podle vzorce
(n <- length(vyska))		# pocet pozorovani
(q.t <- qt(0.995,n-1))  # kvantil t-rozdeleni
(prumer <- mean(vyska))	# vyberovy prumer
(sm.od <- sd(vyska))	# smerodatna odchylka
prumer - q.t*sm.od/sqrt(n)
  # dolni mez intervalu spolehlivosti
prumer + q.t*sm.od/sqrt(n)
  # horni mez intervalu spolehlivosti

# Jednoduchym prikazem
MeanCI(vyska,conf.level=0.99)
# S  pravdepodobnosti 99% skutecna stredni hodnota vysky jedenactiletych
#   deti lezi v intervalu od 147.54 cm do 150.02 cm. 

# Bootstrapovy 95%-ni interval spolehlivosti
MeanCI(vyska, method="boot")
  # Vychazi pokazde malicko jinak, ale vzdy velmi podobne tomu klasickemu
  MeanCI(vyska)
    # kontrola
  
## Spoctete 90%-ni interval spolehlivosti pro stredni hodnotu vahy dvanactiletych deti.
  
##### Interval spolehlivosti pro pravdepodobnost
## Uvazujme nahodny vyber 500 dospelych obyvatel CR. 
#   Z techto ma 178 vysokoskolske vzdelani. 
#   Spoctete 95%-ni interval spolehlivosti pro procento VS vzdelanych.
X <- 178    # pocet s VS vzdelnim
n <- 500    # pocet vsech
p <- X/n    # relativni cetnost

# interval spolehlivosti vyuzivajici aproximaci normalnim rozdelenim
p - qnorm(0.975)*sqrt(p*(1-p)/n)
  # dolni mez
p + qnorm(0.975)*sqrt(p*(1-p)/n)
  # horni mez

# vyse uvedeny interval spolehlivosti se jmenuje Walduv
BinomCI(x=X, n=n, method = "wald")
  # S pravdepodobnosti 95% je skutecny podil VS vzdelanych v intervalu od 31.5% do 39.9%.
  BinomCI(x=X, n=n, method = "clopper-pearson")
    # presny interval spolehlivosti vyuzivajici kvantily binomickeho rozdeleni
  
## Spoctete 90%-ni interval spolehlivosti pro procento nemocnych ve skole,
#   kdyz ve dvou tridach chybi 11 zaku z 33.

#### Interval spolehlivosti pro rozptyl 
## Spoctete a interpretujte 95%-ni interval spolehlivosti pro vysku jedenactiletych deti.
VarCI(vyska)
  # se spolehlivosti 95% lezi skutecny rozptyl vysek v tomto intervalu
  sqrt(VarCI(vyska))
    # interval spolehlivosti pro smerodatnou odchylku

##################################

### Jednovyberovy t-test

## Na zaklade intervalu spolehlivosti rozhodnete, zda stredni hodnota
#   vysky dvanactiletych deti muze byt 152 cm?
#   A co 153 cm, 154 cm, 155, cm, 156 cm, ...
vyska12 <- Deti$vyska12
MeanCI(vyska12)

## Co je p-hodnota
# Jednovyberovy test, nejprve pro jednostrannou alternativu 
# Budeme testovat, zda jsou dvanactilete deti v prumeru mensi nez 156 cm
# testovane hypotezy: H0: vyska dvanactiletych deti = 156 cm
#					  H1: vyska dvanactiletych deti < 156 cm
t.test(vyska12, mu=156, alternative="less")
  # p-hodnota = 0.01889 < alfa (= 0.05) -> zamitam H0, plati H1
  # Stredni hodnota vysky dvanactiletych deti je mensi nez 156 cm.

# predpokladem t-testu je normalni rozdeleni, to je u vysky splneno

# t-test pracuje s testovou statistikou T: T = sqrt(n)*(mean(X) - mu)/sd(X)
#	pro tuto statistiku je pak definovana p-hodnota

# p-hodnota je pravdepodobnost, ze za platnosti nulove hypotezy
# 	nastane vysledek, ktery nastal,
# 	nebo jakykoliv jiny, ktery jeste vic vyhovuje alternative

# graf - definice p-hodnoty
# 1. pravdepodobnost, ze za platnosti nulove hypotezy ...
#   H0 rika, ze testova statistika se rovna nule a ma t-rozdeleni s n-1 stupni volnosti
plot(x<-seq(-4,4,by=0.1),y=dt(x,length(vyska12)-1),type="l",col="blue",main="Hustota t-rozdeleni za H0")
  # hustota t-rozdeleni

# 2. nastane vysledek, ktery nastal
(T <- sqrt(length(vyska12))*(mean(vyska12)-156)/sd(vyska12))
  # testova statistika 
lines(c(T,T),c(0,dt(T,length(vyska12)-1)),col="red",lwd=2)
  # zakreslim do grafu

# 3. nebo jakakoliv jina hodnota, ktera jeste vic odpovida alternative
#   alternativa je mensi nez
xx<-c(seq(-4,T,by=0.1),T,T,-4)
yy<-c(dt(c(seq(-4,T,by=0.1),T),length(vyska12)-1),0,0)
polygon(xx,yy,density=40,col="red")

# rucni vypocet p-hodnoty
# pravdepodobnost hodnot mensich nez testova statistika T
pt(T,length(vyska12)-1)
  # p-hodnota je distribucni funkce v hodnote testove statistiky

# v pripade oboustranne alternativy pridam jeste druhou skupinu hodnot 
#   - symetricky podle nuly
#	U otazky typu: Muze byt populacni prumer dvanactiletych deti 156 cm?
#   H0: vyska dvanactiletych deti = 156 cm vs. H1: vyska dvanactiletych deti <> 156 cm
xx2<-c(-T,-T,seq(-T,4,by=0.1),4,-T)
yy2<-c(0,dt(c(-T,seq(-T,4,by=0.1)),length(vyska12)-1),0,0)
polygon(xx2,yy2,density=40,col="green")

# rucni vypocet p-hodnoty
pt(T,length(vyska12)-1)+1-pt(-T,length(vyska12)-1)
2*pt(T,length(vyska12)-1)
  # pst je symetricka kolem nuly
t.test(vyska12,mu=156)
  # kontrolni test

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
  vyber<-round(rnorm(n,100,sqrt(225)),0)
  p.hodnoty[i] <- t.test(vyber,mu=100)$p.value
}
hist(p.hodnoty)
  # v idealnim pripade by vysly vsechny sloupce stejne vysoke
(Y <- sum(p.hodnoty<=0.05))
  # v kolika pripadech vysla p-hodnota < 0.05
(Y <- sum(p.hodnoty<=0.5))
  # v kolika pripadech vysla p-hodnota < 0.5

# => za platnosti H0 ma p-hodnota rovnomerne rozdeleni na intervalu [0,1]

## Co je sila testu
# Pravdepodobnost, ze zamitnu nulovou hypotezu, kdyz plati vybrana alternativa
power.examp()
power.examp(n=25)
power.examp(alpha=0.1)

##############################

## Samostatne

# Na petiprocentni hladine vyznamnosti zjistete, zda muze byt populacni 
#   prumer vahy dvanactiletych deti 44 kg (hmot12)?
# A co na desetiprocentni hladine vyznamnosti?
# Maji dvanactilete deti v prumeru mene nez 15% tutku (X.tuk12)


