###############################
## Vypocet pravdepodobnosti

# Hod dvema kostkami: cervenou a modrou
# A. Jaka je pst, ze na cervene kostce padne sude cislo
# B. Jaka je pst, ze na modre kostce padne cislo delitelne trema
# C. Jaka je pst, ze soucet na obou kostkach bude alespon 10

# Pravdepodobnost nahodneho jevu = (pocet priznivych moznost)/(pocet vsech moznosti)

(M <- 6*6)
# pocet vsech moznych vysledku (elementarnich jevu), ktere mohou nastat

(MA <- 3*6)
# pocet priznivych vysledku k jevu A
(PA <- MA/M)
# pst jevu A

(MB <- 2*6)
# pocet priznivych vysledku k jevu B
(PB <- MB/M)
# pst jevu B (= 1/3)

outer(1:6,1:6,"+")	# soucet bodu na dvou kostkach
(MC<-sum(outer(1:6,1:6,"+")>=10))
# pocet priznivych vysledku k jevu C
(PC <- MC/M)
# pst jevu C (= 1/6)

###########################
# Najdete nejaky jev, ktery je disjuktni s jevem C
# Jaky je opacny jev k jevu B? Jaka je jeho pravdepodobnost?

# Jsou jevy A a B nezavisle?
(MAB <- 6)
# pocet vysledku, ve kterych nastal jev A a zaroven jev B
(PAB <- MAB/M)
# pravdepodobnost pruniku jevu A a B (= 1/6)
PA*PB
# kontrola pomoci nasobku pravdepodobnosti
# jevy A a B jsou nezavisle

# Jsou jevy A a C nezavisle?
(MAC <- 4)
# pocet vysledku, ve kterych nastal jev A a zaroven jev C
(PAC <- MAC/M)
# pravdepodobnost pruniku jevu A a C (= 1/9)
PA*PC
# kontrola pomoci nasobku pravdepodobnosti (= 1/12)
# jevy A a C nejsou nezavisle

# Jsou jevy B a C nezavisle?
(MBC <- 3)
# pocet vysledku, ve kterych nastal jev B a zaroven jev C
(PBC <- MBC/M)
# pravdepodobnost pruniku jevu B a C (= 1/12)
PB*PC
# kontrola pomoci nasobku pravdepodobnosti (= 1/18)
# jevy B a C nejsou nezavisle

###########################
# Jaka je pravdepodobnost jevu A za podminky jevu B?
MB		# pocet vsech moznosti, kdy nastal jev B
MAB		# pocet moznosti, kdy nastal jev A a zaroven jev B
(PAB.B<-MAB/MB)		# pravdepodobnost jevu A za podminky jevu B
# Podle Bayesovy vety: P(A|B) = P(A prunik B)/P(B)
PAB/PB

# Jaka je pravdepodobnost jevu A za podminky jevu C?
MC		# pocet vsech moznosti, kdy nastal jev C
MAC		# pocet moznosti, kdy nastal jev A a zaroven jev C
(PAC.C<-MAC/MC)		# pravdepodobnost jevu A za podminky jevu C
# Podle Bayesovy vety
PAC/PC

###########################
## Mame 5 hrnicku: cerveny, zeleny, modry, zluty a oranzovy,
#	  dva jsou s potiskem, tri bez potisku.

# Jaka je pravdepodobnost, ze kdyz nahodne srovnam hrnky na policku, 
#	tak na prvnim miste bude cerveny?
(M <- factorial(5))
# pocet vsech moznosti ~ kolika zpusoby mohu srovnat hrnky na policku
(MA <- 1*factorial(4))
# pocet priznivych moznosti ~ na prvnim miste cerveny, ostatni rovnam nahodne
(PA <- MA/M)
# pravdepodobnost, ze na prvnim miste bude cerveny 

# Prijdou ke mne 2 pratele na navstevu, kazdemu nahodne podam jeden hrnek,
#	jaka je pravdepodobnost, ze budou mit oba hrnek bez potisku?
(M <- choose(5,2))
# pocet vsech moznosti, jak mohu vybrat 2 hrnky z peti.
(MB <- choose(2,0)*choose(3,2))
# pocet priznivych moznosti
(PB <- MB/M)
# pravdepodobnost, ze dva vybrane hrnky budou bez potisku

# Jaka je pst, kdyz z Vas nahodne vyberu 2 k tabuli, tak ze to bude 
#	  jeden kluk a jedna holka

##########################
# Dva stejne dobri hraci hraji 10 tenisovych utkani.
# Jaka je pravdepodobnost, ze kazdy z nich vyhraje prave 5 zapasu?
(M <- 2^10)
# vsechny mozne vysledky, jak muze onech 10 utkani ve finale dopadnout
(MA <- choose(10,5))
# priznive moznosti
(PA <- MA/M)
# pravdepodobnost, ze kazdy z nich vyhraje prave 5 zapasu

# Jaka je pravdepodobnost, ze ten, co prisel prvni, vyhraje alespon 7 zapasu
(MB <- choose(10,7) + choose(10,8) + choose(10,9) + choose(10,10))
# priznive moznosti
(PB <- MB/M)
# pravdepodobnost, ze jeden konkretni vyhraje alespon 7 zapasu

##########################
## Senzitivita a specificita testu

# Senzitivita = P(test je pozitivni | osoba je nemocna)
# Specificita = P(test je negativni | osoba je zdrava)

# Urcete senzitivitu a specificitu testu, kdyz vime, ze testovano bylo 
#	1000 pacientu z nichz 20 melo danou nemoc. Test vysel pozitivni pro
#	18 nemocnych pacientu a pro 50 zdravych pacientu.

(senz <- 18/20)
# senzitivita
(spec <- (980-50)/980)
# specificita

# Urcitou nemoc ma 1.2% populace (prevalence nemoci).
#	Senzitivita testu vychazi 92%, specificita testu pak 95%.
#	Jaka je pravdepodobnost, ze mam danou nemoc, kdyz mi vysel test pozitivni?
senz <- 0.92
spec <- 0.95
prev <- 0.012
(p.nemoc <- senz*prev/(senz*prev+(1-spec)*(1-prev)))

## Výzkumu se zúčastnilo 2000 pacientů, z nichž 50 melo danou nemoc. 
#   Všichni podstoupili test na tuto nemoc. Test vyšel pozitivní pro 45 nemocnych pacientů 
#   a pro 200 zdravych. Spočtěte senzitivitu a specificitu testu.

## Test na urcitou nemoc ma senzitivitu 0.97 a specificitu 0.985. 
#   Prevalence nemoci je 3%. Jaka je pravdepodobnost, ze mam
#   danou nemoc, kdyz mi vysel test pozitivni?

########################################
### Rozdeleni nahodnych velicin

## Modifikace zaveru souteze A-Z kviz:
#	je dano 10 poli, kde na kazdem se skryva financni odmena v dane vysi
#	soutezici nahodne vybira 1 pole, odhadujeme jeho prumernou vyhru
# Priklad mozneho financniho oceneni jednotlivych poli
v<- c(100, 200, 200, 500, 1000, 1000, 1000, 1000, 2000, 5000)

# rozdeleni kazde nahodne veliciny je dano moznymi hodnotami a jejich pstmi
table(v)				              # tabulka moznych vyher
(hodnoty <- sort(unique(v)))	# jednotlive vyhry
pocet<- length(v)			        # pocet policek
(psti<-table(v)/pocet)		    # pravdepodobnosti jednotlivych vyher
  # jaky je soucet pravdepodobnosti jednotlivych vyher?

## Vypocet stredni hodnoty
(prumvyhra<-sum(psti*hodnoty))
  # vypocte hodnotu prumerne vyhry (stredni hodnota vyhry)
  weighted.mean(hodnoty,psti)
  mean(v)
     # alternativni vypocty
# Stredni hodnota je vlastne prumer, kdyz mate k dispozici vsechny existujici hodnoty (celou populaci)
#	   pokud vsechny hodnoty k dispozici nejsou, pocitame jen odhad stredni hodnoty - vyberovy prumer

## vypocet rozptylu
#	v promenne prumvyhra je ulozena stredni hodnota vyhry
#	spoctu druhe mocniny odchylek od stredni hodnoty, vynasobim pstmi a sectu je dohoromady
(rozptyl<-(sum(psti*(hodnoty-prumvyhra)^2)))
  # vypocita rozptyl ... vazeny prumer druhych mocnin odchylek od stredni hodnoty

(sd<-sqrt(rozptyl))
  # smerodatna odchylka je ve stejnych jednotkach jako vyhra samotna

############################	
## Zkouseni ze dvou predmetu (statistika, matematika)

X <- matrix(c(15,10,5,0, 5,20,10,5, 5,5,10,5, 0,0,0,5),byrow=T, nr=4, nc=4)
rownames(X)<- c("S1","S2","S3","S4")
colnames(X)<- c("M1","M2","M3","M4")
X
  # absolutni cetnosti (pocty studentu s jednotlivymi kombinacemi znamek)
(p <-X/sum(X))
  # relativni cetnosti (odhady pravdepodobnosti)
addmargins(p)
  # prida marginalni soucty
pS<-apply(p,1,sum)
  # psti znamek ze statistiky
  # prikaz apply ma tri parametry: matice, po radcich nebo sloupcich, matematicka operace

# distribucni funkce znamek z demografie
cumsum(pS)
  # kumulativni soucty
# Vykresleni distribucni funkce pomoci pripravene funkce
distrFce <- function(x,pX,emp=FALSE){
  q <- (max(x)-min(x))*0.2
  xx <- c(min(x)-q,x,max(x)+q) # pro
  yy <- c(0,cumsum(pX),1)
  k <- length(x)
  col <- ifelse(emp,"red","black")
  namex <- deparse(substitute(x))
  if(!emp) plot(c(xx[1],xx[k+2]),0:1,type="n",
                xlab=namex, ylab=paste0("F(",namex,")"))
  for(i in 1:(k+1)){
    lines(xx[i+0:1],rep(yy[i],2),lwd=2,col=col,
          lty=ifelse(emp,2,1))
  }
  for(i in 1:k){
    points(xx[i+1],yy[i],pch=1,col=col)
    points(xx[i+1],yy[i+1],pch=16,col=col)
  }
  abline(h=0:1,col="grey")
}
#  Distribucni funkce znamek ze statistiky:
distrFce(1:4,pS)

# Vypocet teoretickych charakteristik znamek ze statistiky
(muS<-sum((1:4)*pS))
weighted.mean(1:4,pS)
  # stredni hodnota znamek ze statistiky
(varS<-sum(((1:4)-muS)^2*pS))
weighted.mean((1:4-muS)^2,pS)
  # rozptyl znamek ze statistiky

###############################
### Samostatne

# Jaka je stredni hodnota a rozptyl znamek z matematiky?
pM <- apply(p,2,sum)
(muM <-  )
(VarM <-  )

###############################

# Jsou znamky z matematiky a ze statistiky nezavisle?
outer(pS,pM,"*")
  # sdruzene psti v pripade nezavislosti
p
  # skutecne psti
outer(pS,pM,"*") - p
  # rozdil mezi nezavislosti a skutecnosti

#######################################
##  Teoreticke rozdeleni: Binomicke rozdeleni

# popisuje pokus, ktery sestava z 
#	n dilcich pokusu; v kazdem muze nastat jeden ze dvou vysledku: uspech x neuspech
# parametry rozdeleni: pocet pokusu n, pravdepodobnost uspechu p

## Mezi 10 000 obyvateli mesta ma 1 500 vysokoskolske vzdelani.
#   Jaka je pravdepodobnost, ze kdyz ve meste nahodne potkam n = 10 osob  
#   (jednotlive osoby mohu potkat i opakovane),
#   budou mit prave k = 4 vysokolske vzdelani? 

# pracuji s binomickym rozdelenim, ktere je dano parametry n a p
n <- 10
p <- 1500/10000
  # pravdepodobnost ze z n (=10) pokusu nastane prave k (=4) krat uspech
k<-4
dbinom(k,n,p)
  # pravdepodobnostni funkce

# vykresleni pravdepodobnosti vsech vysledku pokusu
plot(0:n,dbinom(0:n,n,p),type="h",xlab="k",ylab="P(Y=k)")
points(0:n,dbinom(0:n,n,p),pch=19)

# S jakou psti budou mezi deseti nahodnymi jedinci nejvyse 4 s VS vzdelanim?
sum(dbinom(0:4,n,p))  # soucet psti priznivych vysledku
pbinom(4,n,p)         # pouziti prikazu pro vypocet distribucni funkce

distrFce(0:n,dbinom(0:n,n,p))
  # vykresleni distribucni funkce

# S jakou psti budou mezi deseti nahodnymi jedinci alespon 4 s VS vzdelanim?
sum(dbinom(4:n,n,p))  # soucet psti priznivych vysledku
1-pbinom(3,n,p)       # pres distribucni funkci, pres jev opacny

# Jaky je ocekavany pocet vysokoskolaku mezi n nahodne vybranymi lidmi? 
# Tedy jaka je teoreticka stredni hodnota pri n pokusech?
(muX <- n*p)
  # Musi patrit stredni hodnota mezi mozne hodnoty?
# Da se vypocitat i teoreticka smerodatna odchylka
(sigmaX <- sqrt(n*p*(1-p)))

# Nasimulujme vyse popsanou situaci 20x (jde 20 lidi a kazdy nahodne potka 10 obyvatel mesta), 
#   vysledky ulozime do x:
(x <- rbinom(20,n,p))
# realizace 20ti pokusu (pocty osob s VS vzdelanim v techto pokusech)
mean(x)    
  # prumerny pocet VS osob (odhad stredni hodnoty muX)

## Jak se odhaduji popoulacni charakteristiky
# Udela se velky pocet pokusu a z nich se pak pocita
#   Udelejme/Nasimulujme B=1000 pokusu:
B <- 1000
x <- rbinom(B,n,p)
table(x)    # cetnosti poctu osob s VS vzdelanim
table(x)/B  # relativni cetnosti = odhady psti
round(psti,3) 
mean(x)     # odhad stredni hodnoty
muX         # teoreticka stredni hodnota

sd(x)           # odhad smerodatne odchylky
sqrt(n*p*(1-p)) # teoreticka smerodatna odchylka

############################################################
##  Samostatne:
#
# Hazime 20x osmistennou kostkou.
#   S jakou psti padne NEJVYSE petkrat trojka?
#   S jakou psti padne ALESPON petkrat trojka?

############################################################

# Jaka je pravdepodobnost, ze z 20ti hodu osmistennou kostkou
#   padne trojka alespon 2x a maximalne 6x?
pbinom(6,20,1/8)-pbinom(1,20,1/8)
# P(a <= X <= b) = P(X <= b) - P(X < a) = P(X <= b) - P(X <= a-1)

# Jaka je pst, ze z dvaceti studentu, kteri jdou na zkousku
#   dela zkousku mezi 10 a 15, kdyz kazdy umi 2/3 latky?

############################################################

## Normalni rozdeleni
#  Spojite  rozdeleni  je  dano  distribucni  funkci  nebo  hustotou
# 	parametry  urcujici  konkretni  normalni  rozdeleni  jsou
#   stredni hodnota a rozptyl (respektive smerodatna odchylka)

# IQ ma v bezne populaci normalni rozdeleni  N(100, 15^2)

## Vypocty a zobrazeni vybranych pravdepodobnosti
#	Jaka je pravdepodobnost, ze nahodne vybrany clovek bude mit IQ 100?

#	Jaka je pravdepodobnost, ze nahodne vybrany clovek bude mit IQ nizsi nez 100?

#	Jaka je pravdepodobnost, ze nahodne vybrany clovek bude mit IQ nejvyse 90?
# zakresleni
curve(dnorm(x,100,15),from=50,to=150, main="Hustota N(100, 225)")
lines(c(0,90),c(0,0),lwd=3,col="green") 
xx <- seq(50,90,length.out=101)
polygon(c(50,xx,90),c(0,dnorm(xx,100,15),0),col="green")
  # umime-li zmerit velikost zelene plochy, mame pst :)
# vypocet presne hodnoty
pnorm(90,100,15)

#	Jaka je pravdepodobnost, ze nahodne vybrany clovek bude mit IQ vyssi nez 90?
1-pnorm(90,100,15)

#	Jaka je pravdepodobnost, ze nahodne vybrany clovek bude mit IQ v rozmezi od 110 do 120?
# zakresleni
curve(dnorm(x,100,15),from=50,to=150, main="Hustota N(100, 225)")
lines(c(110,120),c(0,0),lwd=3,col="blue") 
xx <- seq(110,120,length.out=101)
polygon(c(110,xx,120),c(0,dnorm(xx,100,15),0),col="lightblue")
# vypocet presne hodnoty
pnorm(120,100,15)-pnorm(110,100,15)
# P(a <= X <= b) = P(X <= b) - P(X < a) = P(X <= b) - (P(X < a) + P(X = a)) = P(X <= b) - P(X <= a)

# Obracene
# Jaka je hodnota IQ, kterou presahne jen 5% nejchytrejsich v populaci?
(q95 <- qnorm(0.95,100,15))
# kvantilova funkce
curve(dnorm(x,100,15),from=50,to=150, main="Hustota N(100, 225)")
lines(c(q95,150),c(0,0),lwd=3,col="orange3")
xx <- seq(q95,150,length.out=101)
polygon(c(q95,xx,150),c(0,dnorm(xx,100,15),0),col="orange3")

###########################################
## Samostatne

# 1. Jaka je pst, ze nahodne vybrana osoba bude mit IQ maximalne 112?
# 2. Jaka je pst, ze nahodne vybrana osoba bude mit IQ v intervalu od 75 do 95?
# 3. Jak siroky je nejuzsi interval, v nemz bude 50% populace?

###################################

# Aproximace binomickeho rozdeleni pomoci normalniho rozdeleni 
#	(pro velky pocet pokusu a pst uspechu vetsi nez 0.1 a mensi nez 0.9)
# Jaka je pravdepodobnost, ze ze 75 studentu udela zkousku 60, kdyz pst uspechu u zkousky je 0.85?
dbinom(60,75,0.85)
  # presna pst
dnorm(60,75*0.85,sqrt(75*0.85*0.15))
  # aproximace normalnim rozdelenim - na zaklade toho binomickeho zadavam stredni hodnotu a smerodatnou odchylku
  #   N(n*p, n*p*(1-p))
cbind(55:75,round(dbinom(55:75,75,0.85),6),dnorm(55:75,75*0.85,sqrt(75*0.85*0.15)))
  # ciselne porovnani psti
plot(55:75,dbinom(55:75,75,0.85),pch=19,col="red")
  # cervene binomicke psti
points(55:75,dnorm(55:75,75*0.85,sqrt(75*0.85*0.15)),col="green")
  # zelene normalni psti
  # graficke porovnani psti

