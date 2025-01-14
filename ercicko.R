load("prij.RData")
library(DescTools)

pohlavi <- prij$Pohlavi
obor <- prij$Obor

#H0: pohlavi a obor nesouvisi
#H1 : pohlavi a obor spolu souvisi

#kontingencni tabulka
tab <- table(obor, pohlavi)

fisher.test(tab)
# p-hodnota = 0.1803 > alfa=0.05 -> nezamítáme H0
# zaver: pohlavi a obor nesouvisi

plot(pohlavi ~ obor)


######################################
#H0: ss3 a příjímačky nesouvisi
#H1 : ss3 a příjimačky spolu souvisi

# korelacni koeficien - Pearsonx
cor.test(prij$ss3, prij$celprij)
plot(znamky ~ pr_body)
# p-hodnota = 0.032 < aplfa = 0.05 -> zamitame H0
# korelacni koef (r) = -0.3033
# |r| je v intervalu <0.1, 0.3>, tzn. jedná se o slabou zavislost
# urceni znamínka: r < 0 -> nepřímá (negativní) linearni závislost


######################################
matzem_q <- quantile(prij$matzem, probs=c(0.25, 0.5, 0.75))
geol_q <- quantile(prij$geol, probs=c(0.25, 0.5, 0.75))
matzem_q
geol_q


#######################################
data <- prij$ss3
qqnorm(data)
qqline(data, col="red")

hist(data)

shapiro.test(data)
# p-hodnota=0.013 < alfa -> promenna neni normalne rozdelena
# u qqplotu se data odchylují od diagonály