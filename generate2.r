################wczytanie danych#################
for (i in 1987:2008) 
{
  sciezka <- paste("C:/Users/wojow/Contacts/",i,".csv.bz2",sep="")
  df <- read.csv(sciezka)
  dane[[i-1986]] <- df
}
#####################################################################

###NAJWIEKSI PRZEWOZNICY W STANACH I ICH ROZWOJ###

library("dplyr")
install.packages("ggplot2")

wektor_KM_AA <- c(1987:2007)
wektor_liczba_lotow_AA <- c(1987:2007)

wektor_KM_UA <- c(1987:2007)
wektor_liczba_lotow_UA <- c(1987:2007)

wektor_KM_DL <- c(1987:2007)
wektor_liczba_lotow_DL <- c(1987:2007)

wektor_KM_WN <- c(1987:2007)
wektor_liczba_lotow_WN <- c(1987:2007)

for (i in 1987:2007) 
{
  dane[[i-1986]][,c("UniqueCarrier","Cancelled","Distance")] -> dane_z_danego_roku
  na.omit(dane_z_danego_roku) -> dane_z_danego_roku #profilaktyczne czyszczenie
  subset(dane_z_danego_roku,Cancelled==0)  ->  dane_z_danego_roku
  dane_z_danego_roku[,c("UniqueCarrier","Distance")]->dane_z_danego_roku
  
  aggregate(Distance~UniqueCarrier,dane_z_danego_roku,sum) -> ile_km_zrobili_przewoznicy
  aggregate(Distance~UniqueCarrier,dane_z_danego_roku,length) -> ile_lotow_zrobili_przewoznicy
  
  ile_km_zrobili_przewoznicy <- ile_km_zrobili_przewoznicy[order(-ile_km_zrobili_przewoznicy$Distance), ] 
  ile_lotow_zrobili_przewoznicy <- ile_lotow_zrobili_przewoznicy[order(-ile_lotow_zrobili_przewoznicy$Distance), ] 
  
  wynikowa_ramka_danych_km <- cbind(wynikowa_ramka_danych_km, "xd"=ile_km_zrobili_przewoznicy$UniqueCarrier[1:3])
  wynikowa_ramka_danych_liczba_lotow <- cbind(wynikowa_ramka_danych_liczba_lotow,"xd"=  ile_lotow_zrobili_przewoznicy$UniqueCarrier[1:3])
  
  wektor_KM_AA[i-1986]<-ile_km_zrobili_przewoznicy[ile_km_zrobili_przewoznicy$UniqueCarrier == "AA", "Distance"]
  wektor_liczba_lotow_AA[i-1986]<-ile_lotow_zrobili_przewoznicy[ile_lotow_zrobili_przewoznicy$UniqueCarrier == "AA", "Distance"]
  
  wektor_KM_UA[i-1986] <- ile_km_zrobili_przewoznicy[ile_km_zrobili_przewoznicy$UniqueCarrier == "UA", "Distance"]
  wektor_liczba_lotow_UA[i-1986]<-ile_lotow_zrobili_przewoznicy[ile_lotow_zrobili_przewoznicy$UniqueCarrier == "UA", "Distance"]
  
  wektor_KM_DL[i-1986] <- ile_km_zrobili_przewoznicy[ile_km_zrobili_przewoznicy$UniqueCarrier == "DL", "Distance"]
  wektor_liczba_lotow_DL[i-1986]<-ile_lotow_zrobili_przewoznicy[ile_lotow_zrobili_przewoznicy$UniqueCarrier == "DL", "Distance"]
  
  wektor_KM_WN[i-1986] <- ile_km_zrobili_przewoznicy[ile_km_zrobili_przewoznicy$UniqueCarrier == "WN", "Distance"]
  wektor_liczba_lotow_WN[i-1986]<-ile_lotow_zrobili_przewoznicy[ile_lotow_zrobili_przewoznicy$UniqueCarrier == "WN", "Distance"]
  
  
}

wynikowa_ramka_danych_km  <- subset(wynikowa_ramka_danych_km , select = -c1)
wynikowa_ramka_danych_liczba_lotow <- subset(wynikowa_ramka_danych_liczba_lotow , select = -c1)
colnames(wynikowa_ramka_danych_km) <- 1987:2007
colnames(wynikowa_ramka_danych_liczba_lotow) <-1987:2007

t(wynikowa_ramka_danych_km) -> wynikowa_ramka_danych_km
t(wynikowa_ramka_danych_liczba_lotow) -> wynikowa_ramka_danych_liczba_lotow

table(wynikowa_ramka_danych_km)
table(wynikowa_ramka_danych_liczba_lotow)

v1 <- c("AA","DL","UA","WN")
v2 <-c(22,17,21,6)


v3 <- c("AA","DL","OO","UA","US","WN")
v4 <- c(16,19,3,8,7,13)


v5 <- c("AA","DL","OO","UA","US","WN")
v6 <- c(38,36,3,29,7,19)

barplot(v2, names.arg = v1, col = "blue", xlab = "", ylab = "Liczba wystapień na podium w latach 1987-2008", main = "Kilometraż")

barplot(v4, names.arg = v3, col = "red", xlab = "", ylab = "Liczba wystapień na podium w latach 1987-2008", main = "Liczba Lotow")

barplot(v6, names.arg = v5, col = "blue", xlab = "", ylab = "liczba wystapień na podium w latach 1987-2008", main = "Liczba Lotow + Kilometraż")

wektor_KM_AA<-wektor_KM_AA[2:length(wektor_KM_AA)]
wektor_KM_DL<-wektor_KM_DL[2:length(wektor_KM_DL)]
wektor_KM_UA<-wektor_KM_UA[2:length(wektor_KM_UA)]
wektor_KM_WN<-wektor_KM_WN[2:length(wektor_KM_WN)]

wektor_KM_AA<-wektor_KM_AA/10^6
wektor_KM_DL<-wektor_KM_DL/10^6
wektor_KM_UA<-wektor_KM_UA/10^6
wektor_KM_WN<-wektor_KM_WN/10^6

z <- 1988:2007
max_value <- max(wektor_KM_AA, wektor_KM_UA, wektor_KM_DL, wektor_KM_WN)

par(mar = c(5, 4, 4, 6) + 0.1)

plot(z, wektor_KM_AA, type = "l", xlab = "", ylab = "kilometraż przewodnika (w mln km)", main = "Kilometraż największych przewoźników.", ylim = c(0, max_value), yaxt = "n")
lines(z, wektor_KM_UA, col = "red")
lines(z, wektor_KM_DL, col = "blue")
lines(z, wektor_KM_WN, col = "green")

legend("topleft", legend = c("AA", "UA", "DL", "WN"), col = c("black", "red", "blue", "green"), lty = 1)

axis(2,las=2)

#TWORZYMY DRUGI WYKRES

wektor_liczba_lotow_AA<-wektor_liczba_lotow_AA[2:length(wektor_liczba_lotow_AA)]
wektor_liczba_lotow_DL<-wektor_liczba_lotow_DL[2:length(wektor_liczba_lotow_DL)]
wektor_liczba_lotow_UA<-wektor_liczba_lotow_UA[2:length(wektor_liczba_lotow_UA)]
wektor_liczba_lotow_WN<-wektor_liczba_lotow_WN[2:length(wektor_liczba_lotow_WN)]

wektor_liczba_lotow_AA<-wektor_liczba_lotow_AA/10^3
wektor_liczba_lotow_DL<-wektor_liczba_lotow_DL/10^3
wektor_liczba_lotow_UA<-wektor_liczba_lotow_UA/10^3
wektor_liczba_lotow_WN<-wektor_liczba_lotow_WN/10^3

z <- 1988:2007  # Lata

# Obliczanie maksymalnej wartości z wektorów danych
maxx_value <- max(wektor_liczba_lotow_AA, wektor_liczba_lotow_UA, wektor_liczba_lotow_DL, wektor_liczba_lotow_WN)

par(mar = c(5, 4, 4, 6) + 0.1)

plot(z, wektor_liczba_lotow_AA, type = "l", xlab = "", ylab = "liczba lotów (w tys.) ", main = "Liczba lotów wykonanych przez największych przewoźników.", ylim = c(0, maxx_value), yaxt = "n")
lines(z, wektor_liczba_lotow_UA, col = "red")
lines(z, wektor_liczba_lotow_DL, col = "blue")
lines(z, wektor_liczba_lotow_WN, col = "green")

legend("topleft", legend = c("AA", "UA", "DL", "WN"), col = c("black", "red", "blue", "green"), lty = 1)

axis(2,las=2)

################################################################################
#OPOZNIENIA KASKADOWE#

dane[[19]] -> rok2005
wektorek_korelacji <- c(0,0,0,0,0,0,0,0,0,0,0,0)
wektorek_korelacji2 <- c(0,0,0,0,0,0,0,0,0,0,0,0)

for(i in range: 1:12){
rok2005[,c("Month","DayofMonth","ArrDelay","Dest","Cancelled")]->potrzebne_dane
potrzebne_dane<-subset(potrzebne_dane,Cancelled==0)
potrzebne_dane <- potrzebne_dane[,c("Month","DayofMonth","ArrDelay","Dest")]
potrzebne_dane <- na.omit(potrzebne_dane)
potrzebne_dane <- subset(potrzebne_dane,ArrDelay>180)
  
rok2005[,c("Month","DayofMonth","DepDelay","Origin","Cancelled")]->potrzebne_danev2
potrzebne_danev2<-subset(potrzebne_danev2,Cancelled==0)
potrzebne_danev2 <- potrzebne_danev2[,c("Month","DayofMonth","DepDelay","Origin")]
potrzebne_danev2 <- na.omit(potrzebne_danev2)
potrzebne_danev2 <- subset(potrzebne_danev2,DepDelay>45)

do_usuniecia <- which(rownames(potrzebne_danev2) %in% rownames(potrzebne_dane))
wynikowa_ramka <- potrzebne_danev2[-do_usuniecia, ]

potrzebne_dane<-subset(potrzebne_dane,Month==i & DayofMonth==i)
wynikowa_ramka<-subset(wynikowa_ramka,Month==i & DayofMonth==i)

aggregate(Month~Origin,wynikowa_ramka,length) -> ramka1
aggregate(Month~Dest,potrzebne_dane,length) -> ramka2

names(ramka2)[1] <- "Origin"

merge(ramka1,ramka2,by="Origin",all=TRUE) -> rameczka
rameczka[is.na(rameczka)] <- 0

cor(rameczka$Month.x, rameczka$Month.y)->wektorek_korelacji[i]
cor(rameczka$Month.x, rameczka$Month.y, method="spearman")->wektorek_korelacji2[i]

}

dates <- c('01.01', '02.02', '03.03', '04.04', '05.05', '06.06', '07.07', '08.08', '09.09', '10.10', '11.11', '12.12')


barplot(wektorek_korelacji, names.arg = dates, xlab = "", ylab = "Wartość korelacji Pearsona", main = "Wykres korelacji w wybranych dniach 2005 roku. (Pearson) ", col = "blue", las = 1)

barplot(wektorek_korelacji2, names.arg = dates, xlab = "", ylab = "Wartość korelacji Spearmana ", main = "Wykres korelacji w wybranych dniach 2005 roku. (Spearman) ", col = "red", las = 1)

############################
#WPLYW WTC NA PRZEWOZNIKOW

dane2000<-subset(dane[[14]],Cancelled==0)
dane2001<-subset(dane[[15]],Cancelled==0)
dane2002<-subset(dane[[16]],Cancelled==0)
dane2003<-subset(dane[[17]],Cancelled==0)

dane2000p<-subset(dane2000,((Month==9 & DayofMonth>11 )|Month==10|Month==11|Month==12))
dane2000p<-dane2000p$UniqueCarrier
dane2001p <- subset(dane2001,((Month==9 & DayofMonth>11 )|Month==10|Month==11|Month==12))
dane2001p <- dane2001p$UniqueCarrier
dane2002p <- subset(dane2002,((Month==9 & DayofMonth>11 )|Month==10|Month==11|Month==12))
dane2002p <- dane2002p$UniqueCarrier
dane2003p <- subset(dane2003,((Month==9 & DayofMonth>11 )|Month==10|Month==11|Month==12))
dane2003p <- dane2003p$UniqueCarrier

nazwy <- c("AA","AS","CO","DL","HP","NW","UA","US","WN")

licznik <- table(dane2000p)
rok2000<- data.frame(Element = names(licznik), LiczbaWystapien = as.vector(licznik))
licznik <- table(dane2001p)
rok2001<- data.frame(Element = names(licznik), LiczbaWystapien = as.vector(licznik))
licznik <- table(dane2002p)
rok2002<- data.frame(Element = names(licznik), LiczbaWystapien = as.vector(licznik))
licznik <- table(dane2003p)
rok2003<- data.frame(Element = names(licznik), LiczbaWystapien = as.vector(licznik))

left_join(rok2000,rok2001,by="Element") %>% left_join(rok2002,by="Element") %>% left_join(rok2003,by="Element") -> zmergowanelata
na.omit(zmergowanelata)->zmergowanelata
names(zmergowanelata) <- c("przewoznicy","a2000","a2001","a2002","a2003")
zmergowanelata

roczek2000 <- zmergowanelata$a2000
roczek2001 <- zmergowanelata$a2001
roczek2002 <- zmergowanelata$a2002
roczek2003 <- zmergowanelata$a2003

wektor_wartosci_oczekiwanej<-4*roczek2000
wektor_rzeczywisty <- roczek2000+roczek2001+roczek2002+roczek2003
wektorek_obrazujacy_poradzenie_sobie_z_wtc<-wektor_rzeczywisty/wektor_wartosci_oczekiwanej


barplot(wektor_wartosci_oczekiwanej, names.arg = nazwy, col = "blue", ylim = c(0, max(wektor_wartosci_oczekiwanej, wektor_rzeczywisty)),ylab="Liczba lotów",main="Wpływ zamachu z 11 września na przewoźników.")

barplot(wektor_rzeczywisty, add = TRUE, col = "red")

legend("topleft", legend = c("Wartość oczekiwana", "Wartość rzeczywista"), fill = c("blue", "red"))