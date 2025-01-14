##############################
##### Kategoricke promenne
### Test dobre shody - multinomicke rozdeleni

## Nactete databazi Deti.RData
library(DescTools)

## Rozhodnete, zda cetnosti 95, 169, 89 odpovidaji idealnimu stepnemu pomeru 1 : 2 : 1
tab = c(95,168,89)	#	pozorovane cetnosti vlozime do vektoru s nazvem nn

# chceme otestovat hypotezy 
#   H0: cetnosti odpovidaji pomeru 1:2:1 vs. H1: cetnosti neodpovidaji pomeru 1:2:1
# otestujeme pomoci chi-kvadrat testu dobre shody
chisq.test(tab, p = c(1/4,2/4,1/4))
	# p je vektor testovanych pravdepodobnosti (da se zadat jako uvedeny pomer deleny souctem vsech hodnot)
	# p-hodnota 0.6276 vyssi nez hladina vyznamnosti -> nulovou hypotezu nezamitame
	#	Neprokazalo se, ze by dane cetnosti neodpovidaly idealnimu stepnemu pomeru.

# vlastni test je zalozen na rozdilu skutecne namerenych a ocekavanych cetnosti
(ob <- chisq.test(c(95,168,89), p=c(1/4,2/4,1/4))$observed)
	# pozorovane cetnosti (ty, co jsme zadali)
(ex <- chisq.test(c(95,168,89), p=c(1/4,2/4,1/4))$expected)
	# ocekavane cetnosti (ty, co presne odpovidaji nulove hypoteze - testovanemu pomeru)
rbind(ob,ex)	# ve sloupcich pod sebou jsou vzajemne porovnavane hodnoty
# pozorovane i ocekavane cetnosti musi mit stejny soucet
sum(ob); sum(ex)

# predpokladem pouziti testu jsou dostatecne velke ocekavane cetnosti, vetsi nez 5
#	zde jsou vsechny vetsi nez 5

## Je kategorizovane bmi jedenactiletych deti rozlozeno v pomeru 2:3:1 
#   v poradi low:normal:high?
# testujeme hypotezy H0: kategorie jsou v pomeru 2:3:1 (v poradi low:normal:high)
#	    vs. H1: kategorie nejsou v tomto pomeru
# nejprve se podivame na tabulku absolutnich a relativnich cetnosti
nn <- Deti$bmi11_kat
cbind("Absolutni" = table(nn), "Relativni" = prop.table(table(nn)))
	# od pohledu to vypada, ze pomer je jiny
# musim si dat pozor na poradi kategorii
tab <- table(nn)
	# do prikazu se vklada tabulka absolutnich cetnosti
chisq.test(tab, p = c(1/6,2/6,3/6))
	# p-hodnota testu 0.0007 < alfa 0.05 -> zamitame H0
	# Kategorie bmi jedenactiletych deti nejsou zastoupeny v pomeru 2:3:1 v poradi low:normal:high.
# Test predpokladu
sum(tab)/6
	# nejmensi ocekavana cetnost > 5 -> predpoklad splnen

# V pripade, ze neni splnen predpoklad, tj. kdyz vsechny ocekavane cetnosti
#	nejsou vetsi nez 5, objevi se varovani.

## Ve vyberu je 10 lidi s modryma ocima, 15 s hnedyma ocima a 2 se zelenyma.
#   Jsou pravdepodobnosti barev oci 40% modra barva, 50% hneda barva a 10% zelena barva?

## Mam ve vyberu polovinu atletu a 40% lednich hokejistu?

########################### 
## Chi-kvadrat test nezavislosti
## Souvisi spolu sport a kategorie bmi?
# testujeme 
#   H0: sport a kategorie bmi spolu nesouvisi vs. 
#		H1: sport a kategorie bmi spolu souvisi
sport <- Deti$sport
bmi <- Deti$bmi11_kat

# Ciselne popisujeme pomoci kontingencni tabulky.
(tab <- table(sport, bmi))
	# vidite z ni zavislost?
# relativni cetnosti
addmargins(prop.table(tab))
	# tabulkova procenta
addmargins(prop.table(tab,1))
	# radkova procenta
addmargins(prop.table(tab,2))
	# sloupcova procenta
# ze kterych je zavislost videt nejlepe?

# Graficke znazorneni pomoci sloupcoveho grafu
plot(sport ~ bmi)
	# graf pro sloupcova procenta

# Chi-kvadrat test nezavislosti
#   H0: sport a bmi spolu nesouvisi vs. H1: sport a bmi spolu souvisi
chisq.test(tab)
	# nejsou splneny predpoklady

# Jak ziskam ocekavane cetnosti
addmargins(tab)
	# pozorovane cetnosti
# ocekavana cetnost ke kombinaci atletika + BMI low
(22/222)*(109/222)*222
# kontrola pocitacem
chisq.test(tab)$expected
	# jedna ocekavana cetnost je mensi nez 5, ptedpoklad neni splnen
# Pouziji Fisheruv exaktni test
fisher.test(tab)
	# p-hodnota 0.017 < alfa 0.05 -> zamitame H0
	# Sport a kategorie BMI spolu souvisi.

## Souvisi spolu sport a pohlavi?

## Porovnavam deti v Usti, v Decine a v Litomericich, jake maji doma zvire. 
# 21 deti z Usti ma psa, 12 kocku, 12 hlodavce a 19 nic. Mezi Decinskymi
# ma 13 psa, 2 kocku, 7 hlodavce a 15 nic. Z Litomeric ma 20 deti psa, 
# 8 kocku, 5 hlodavce a 10 nic. Jsou na tom vsechna mesta stejne?
tab <- t(matrix(c(21,12,12,19,13,2,7,15,20,8,5,10),4,3))
rownames(tab) <- c("Usti","Decin","Litomerice")
colnames(tab) <- c("pes","kocka","hlodvec","nic")
tab

########################### 
### Pomer sanci
## Porovnavame mesto a venkov vzhledem k sanci dostat chripku.
# Ve meste melo za sledovane obdobi 51 lidi chripku a 53 lidi ne. 
# Na vesnici melo chripku 12 a bez chripky bylo 20. 
# Kolikrat je vetsi sance mit chripku ve meste nez na venkove?
tab <- t(matrix(c(51,53,12,20),2,2))
rownames(tab) <- c("mesto","venkov")
colnames(tab) <- c("chripka","zdrav")
tab
	# absolutni cetnosti
prop.table(tab,1)
	# radkove relativni cetnosti
	# ve meste je sance na chripku vetsi

# sance na chripku ve meste
tab[1,1]/tab[1,2]
# sance na chripku na venkove
tab[2,1]/tab[2,2]
# pomer sanci ve meste ku na venkove
(tab[1,1]/tab[1,2])/(tab[2,1]/tab[2,2])
	# ve meste je sance 1.5 krat vetsi
OddsRatio(tab)
OddsRatio(tab, conf.level=0.95)
	# doplni i interval spolehlivosti

# test o vyznamnosti 
#   H0: pomer sanci = 1 (sance jsou stejne) vs. H1: pomer sanci <> 1 (sance se lisi)
fisher.test(tab)
OddsRatio(tab, method="mle")
# jina metoda odhadu pomeru sanci
#	p-hodnota testu 0.3123 > alfa 0.05 -> nezamitame H0 -> 
#		Sance na chripku ve meste a na venkove se vyznamne nelisi.

# Je vetsi sance ziskat dobrou praci v Usti nebo v Decine, kdyz vime ze,
#   v Usti ji ze 70 ti uchazecu ziskalo 55 a v Decine ji ze 45 uchazecu
#   ziskalo 30.

tab <- t(matrix(c(55,15,30,15),2,2))
rownames(tab) <- c("Usti","Decin")
colnames(tab) <- c("dobra_prace","spatna_prace")
tab