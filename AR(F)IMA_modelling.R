#Instalacja niedostepnych pakietow 
packages<-c('ggplot2','dplyr','urca','aTSA','xts','forecast','pracma','fracdiff','lmtest','tidyverse','knitr','kableExtra','rugarch')
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

set.seed(48)

#Ustawienie lokalizacji na sciezke tego pliku
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


aud<-read.csv('data\\aud_usd.csv')
btc<-read.csv('data\\btc_usd.csv')
cad<-read.csv('data\\cad_usd.csv')
doge<-read.csv('data\\doge_usd.csv')
eth<-read.csv('data\\eth_usd.csv')
eur<-read.csv('data\\eur_usd.csv')
gbp<-read.csv('data\\gbp_usd.csv')
jpy100<-read.csv('data\\jpy100_usd.csv')
jpy<-jpy100
jpy$Zamkniecie<-jpy$Zamkniecie/100
ltc<-read.csv('data\\ltc_usd.csv')
xrp<-read.csv('data\\xrp_usd.csv')


aud$Data<-as.Date(aud$Data)
aud<-aud[,c('Data','Zamkniecie')]
btc$Data<-as.Date(btc$Data)
btc<-btc[,c('Data','Zamkniecie')]
cad$Data<-as.Date(cad$Data)
cad<-cad[,c('Data','Zamkniecie')]
doge$Data<-as.Date(doge$Data)
doge<-doge[,c('Data','Zamkniecie')]
eth$Data<-as.Date(eth$Data)
eth<-eth[,c('Data','Zamkniecie')]
eur$Data<-as.Date(eur$Data)
eur<-eur[,c('Data','Zamkniecie')]
gbp$Data<-as.Date(gbp$Data)
gbp<-gbp[,c('Data','Zamkniecie')]
jpy$Data<-as.Date(jpy$Data)
jpy<-jpy[,c('Data','Zamkniecie')]
ltc$Data<-as.Date(ltc$Data)
ltc<-ltc[,c('Data','Zamkniecie')]
xrp$Data<-as.Date(xrp$Data)
xrp<-xrp[,c('Data','Zamkniecie')]

#Statystyki opisowe
#waluty
summary(aud$Zamkniecie)
std(aud$Zamkniecie)
summary(cad$Zamkniecie)
std(cad$Zamkniecie)
summary(eur$Zamkniecie)
std(eur$Zamkniecie)
summary(gbp$Zamkniecie)
std(gbp$Zamkniecie)
summary(jpy$Zamkniecie)
std(jpy$Zamkniecie)

#kryptowaluty
summary(btc$Zamkniecie)
std(btc$Zamkniecie)
summary(doge$Zamkniecie)
std(doge$Zamkniecie)
summary(eth$Zamkniecie)
std(eth$Zamkniecie)
summary(ltc$Zamkniecie)
std(ltc$Zamkniecie)
summary(xrp$Zamkniecie)
std(xrp$Zamkniecie)

par(mfrow=c(2,1))
#Przykladowe wykresy
plot(btc$Data, btc$Zamkniecie, type = "l", )
plot(eur$Data, eur$Zamkniecie, type = "l")
plot(doge$Data, doge$Zamkniecie, type = "l")
plot(xrp$Data, xrp$Zamkniecie, type = "l")
plot(btc$Data, log(btc$Zamkniecie), type = "l", )


plot(eur$Data, log(eur$Zamkniecie), type = "l")


plot(doge$Data, log(doge$Zamkniecie), type = "l")


plot(xrp$Data, log(xrp$Zamkniecie), type = "l")


#Warto zlogarytmować notowania krypto oraz walut i na ich podstawie tworzyć kolejne analizy.
aud['Log_Zamkniecie']<-log(aud$Zamkniecie)
btc['Log_Zamkniecie']<-log(btc$Zamkniecie)
cad['Log_Zamkniecie']<-log(cad$Zamkniecie)
doge['Log_Zamkniecie']<-log(doge$Zamkniecie)
eth['Log_Zamkniecie']<-log(eth$Zamkniecie)
eur['Log_Zamkniecie']<-log(eur$Zamkniecie)
gbp['Log_Zamkniecie']<-log(gbp$Zamkniecie)
jpy['Log_Zamkniecie']<-log(jpy$Zamkniecie)
ltc['Log_Zamkniecie']<-log(ltc$Zamkniecie)
xrp['Log_Zamkniecie']<-log(xrp$Zamkniecie)


#Formalny test potwierdzajacy istnienie trendu w danych Cox-Stuart
trend.test(aud$Log_Zamkniecie)
trend.test(cad$Log_Zamkniecie)
trend.test(eur$Log_Zamkniecie)
trend.test(gbp$Log_Zamkniecie)
trend.test(jpy$Log_Zamkniecie)

trend.test(btc$Log_Zamkniecie)
trend.test(doge$Log_Zamkniecie)
trend.test(eth$Log_Zamkniecie)
trend.test(ltc$Log_Zamkniecie)
trend.test(xrp$Log_Zamkniecie)
#hipoteza zerowa o braku trendu odrzucona => Każdy szereg czasowy posiada trend


#testy stacjonarnosci, adf: h0: niestacjonarnosc

#fiat
summary(ur.df(aud$Log_Zamkniecie, type = c("trend"),lags=18,selectlags='AIC' ))
summary(ur.df(na.omit(diff.xts(aud$Log_Zamkniecie)), type="trend",lags=18,selectlags='AIC'))

summary(ur.df(cad$Log_Zamkniecie, type = c("trend"), lags=18,selectlags='AIC'))
summary(ur.df(na.omit(diff.xts(cad$Log_Zamkniecie)), type="trend",lags=18,selectlags='AIC'))

summary(ur.df(eur$Log_Zamkniecie, type = c("trend"),lags=18,selectlags='AIC' ))
summary(ur.df(na.omit(diff.xts(eur$Log_Zamkniecie)), type="trend",lags=18,selectlags='AIC'))

summary(ur.df(gbp$Log_Zamkniecie, type = c("trend"), lags=18,selectlags='AIC'))
summary(ur.df(na.omit(diff.xts(gbp$Log_Zamkniecie)), type="trend",lags=18,selectlags='AIC'))

summary(ur.df(jpy$Log_Zamkniecie, type = c("trend"),lags=18,selectlags='AIC' ))
summary(ur.df(na.omit(diff.xts(jpy$Log_Zamkniecie)), type="trend",lags=18,selectlags='AIC'))


#krypto
summary(ur.df(btc$Log_Zamkniecie, type = c("trend"),lags=24,selectlags='AIC' ))
summary(ur.df(na.omit(diff.xts(btc$Log_Zamkniecie)), type="trend",lags=24,selectlags='AIC'))

summary(ur.df(doge$Log_Zamkniecie, type = c("trend"),lags=24, selectlags='AIC'))
summary(ur.df(na.omit(diff.xts(doge$Log_Zamkniecie)), type="trend",lags=24,selectlags='AIC'))

summary(ur.df(eth$Log_Zamkniecie, type = c("trend"), lags=24,selectlags='AIC'))
summary(ur.df(na.omit(diff.xts(eth$Log_Zamkniecie)), type="trend",lags=24,selectlags='AIC'))

summary(ur.df(ltc$Log_Zamkniecie, type = c("trend"), lags=24,selectlags='AIC'))
summary(ur.df(na.omit(diff.xts(ltc$Log_Zamkniecie)), type="trend",lags=24,selectlags='AIC'))

summary(ur.df(xrp$Log_Zamkniecie, type = c("trend"),lags=24,selectlags='AIC' ))
summary(ur.df(na.omit(diff.xts(xrp$Log_Zamkniecie)), type="trend",lags=24,selectlags='AIC'))

#Wszystkie szeregi posiadaja pierwiastek jednostkowy, czyli sa niestacjonarne na
# poziomie istotnosci 5% w tescie adf z typem trend

#Sprawdzmy  innym testem kpss, gdzie h0 to stacjonarnosc.

#fiat
summary(ur.kpss(aud$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(aud$Log_Zamkniecie)), type="tau",lags="short"))

summary(ur.kpss(cad$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(cad$Log_Zamkniecie)),type="tau", lags="short"))

summary(ur.kpss(eur$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(eur$Log_Zamkniecie)), type="tau",lags="short"))

summary(ur.kpss(gbp$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(gbp$Log_Zamkniecie)), type="tau",lags="short"))

summary(ur.kpss(jpy$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(jpy$Log_Zamkniecie)), type="tau",lags="short"))

#krypto
summary(ur.kpss(btc$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(btc$Log_Zamkniecie)),type="tau", lags = "short"))

summary(ur.kpss(doge$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(doge$Log_Zamkniecie)),type="tau", lags="short"))

summary(ur.kpss(eth$Log_Zamkniecie, type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(eth$Log_Zamkniecie)),type="tau",lags="short"))

summary(ur.kpss(ltc$Log_Zamkniecie,type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(ltc$Log_Zamkniecie)),type="tau", lags="short"))

summary(ur.kpss(xrp$Log_Zamkniecie,type="tau", lags = "short"))
summary(ur.kpss(na.omit(diff.xts(xrp$Log_Zamkniecie)),type="tau",lags="short"))

#KPSS pokazuje ze wszystkie szeregi sa niestacjonarne

# Przekonwertujmy dane do obiektu typu `xts`.
eur.xts <- xts(eur$Log_Zamkniecie, # kolumny z danymi
               eur$Data)  # kolumny z datą/czasem

btc.xts <- xts(btc$Log_Zamkniecie, # kolumny z danymi
               btc$Data)  # kolumny z datą/czasem
aud.xts <- xts(aud$Log_Zamkniecie, # kolumny z danymi
               aud$Data)  # kolumny z datą/czasem
cad.xts <- xts(cad$Log_Zamkniecie, # kolumny z danymi
               cad$Data)  # kolumny z datą/czasem
doge.xts <- xts(doge$Log_Zamkniecie, # kolumny z danymi
               doge$Data)  # kolumny z datą/czasem
eth.xts <- xts(eth$Log_Zamkniecie, # kolumny z danymi
               eth$Data)  # kolumny z datą/czasem
gbp.xts <- xts(gbp$Log_Zamkniecie, # kolumny z danymi
               gbp$Data)  # kolumny z datą/czasem
jpy.xts <- xts(jpy$Log_Zamkniecie, # kolumny z danymi
               jpy$Data)  # kolumny z datą/czasem
ltc.xts <- xts(ltc$Log_Zamkniecie, # kolumny z danymi
               ltc$Data)  # kolumny z datą/czasem
xrp.xts <- xts(xrp$Log_Zamkniecie, # kolumny z danymi
               xrp$Data)  # kolumny z datą/czasem


#Sprawdzenie czy proces jest dluga pamiecia funkcjami w R

hurstexp(aud.xts)
hurstexp(cad.xts)
hurstexp(eur.xts)
hurstexp(gbp.xts)
hurstexp(jpy.xts)


hurstexp(btc.xts)
hurstexp(doge.xts)
hurstexp(eth.xts)
hurstexp(ltc.xts)
hurstexp(xrp.xts)

#Wartosci roznia sie od tych otrzymanych w Pythonie, problem interpretacji.
#Tutaj wykadnik Hursta blisko znaczaco powyzej 0.5 co oznacza pozytywna autokorelacje
# i implikuje dluga pamiec procesu

#Estymacja parametru d dla modelu ARFIMA

fdGPH(aud$Log_Zamkniecie)
fdGPH(cad$Log_Zamkniecie)
fdGPH(eur$Log_Zamkniecie)
fdGPH(gbp$Log_Zamkniecie)
fdGPH(jpy$Log_Zamkniecie)


fdGPH(btc$Log_Zamkniecie)
fdGPH(doge$Log_Zamkniecie)
fdGPH(eth$Log_Zamkniecie)
fdGPH(ltc$Log_Zamkniecie)
fdGPH(xrp$Log_Zamkniecie)

#Kolejny estymator parametru roznicujacego

fdSperio(btc$Log_Zamkniecie)
fdSperio(eur$Log_Zamkniecie)
fdSperio(aud$Log_Zamkniecie)
fdSperio(cad$Log_Zamkniecie)
fdSperio(doge$Log_Zamkniecie)
fdSperio(eth$Log_Zamkniecie)
fdSperio(gbp$Log_Zamkniecie)
fdSperio(jpy$Log_Zamkniecie)
fdSperio(ltc$Log_Zamkniecie)
fdSperio(xrp$Log_Zamkniecie)


#W obu przypadkach d znaczaco rozne od zera co by oznaczalo proces z dluga pamiecia

#Podzial danych na treningowe i testowe

btc_train<-head(btc,ceil(0.8 * nrow(btc)))
btc_test<-tail(btc,floor(0.2* nrow(btc)))

doge_train<-head(doge,ceil(0.8 * nrow(doge)))
doge_test<-tail(doge,floor(0.2* nrow(doge)))

eth_train<-head(eth,ceil(0.8 * nrow(eth)))
eth_test<-tail(eth,floor(0.2* nrow(eth)))

ltc_train<-head(ltc,ceil(0.8 * nrow(ltc)))
ltc_test<-tail(ltc,floor(0.2* nrow(ltc)))

xrp_train<-head(xrp,ceil(0.8 * nrow(xrp)))
xrp_test<-tail(xrp,floor(0.2* nrow(xrp)))


aud_train<-head(aud,0.8 * nrow(aud))
aud_test<-tail(aud,0.2* nrow(aud))

cad_train<-head(cad,0.8 * nrow(cad))
cad_test<-tail(cad,0.2* nrow(cad))

eur_train<-head(eur,0.8 * nrow(eur))
eur_test<-tail(eur,0.2* nrow(eur))

gbp_train<-head(gbp,0.8 * nrow(gbp))
gbp_test<-tail(gbp,0.2* nrow(gbp))

jpy_train<-head(jpy,0.8 * nrow(jpy))
jpy_test<-tail(jpy,0.2* nrow(jpy))



# Trenowanie modeli za pomoca auto.arima 
par(mfrow=c(2,1))

#Dolar Australijski
Acf(na.omit(diff.xts(aud_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(aud_train$Log_Zamkniecie)))

aud_arima<-auto.arima(aud_train$Log_Zamkniecie,trace = T,max.p = 2, max.q=2,d=1,stepwise=F)
aud_arfima<-arfima(aud_train$Log_Zamkniecie,trace=T,max.p = 2, max.q=2,stepwise=F,drange = c(0, 1))
summary(aud_arima)
summary(aud_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(aud_arima)
checkresiduals(aud_arfima)
Box.test(aud_arfima$residuals,type="Ljung-Box",lag=10)

#W tym przypadku również reszty nie sa bialym szumem

#Dolar kanadyjski
Acf(na.omit(diff.xts(cad_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(cad_train$Log_Zamkniecie)))

cad_arima<-auto.arima(cad_train$Log_Zamkniecie,trace = T,max.p = 1, max.q=1,d=1,stepwise=F)
cad_arfima<-arfima(cad_train$Log_Zamkniecie,trace=T,max.p = 1, max.q=1,stepwise=F,drange = c(0, 1))
summary(cad_arima)
summary(cad_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(cad_arima)
checkresiduals(cad_arfima)
Box.test(cad_arfima$residuals,type="Ljung-Box",lag=10)

#Euro
Acf(na.omit(diff.xts(eur_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(eur_train$Log_Zamkniecie)))

eur_arima<-auto.arima(eur_train$Log_Zamkniecie,trace = T,max.p = 1,max.q=1,d=1,stepwise = F)
eur_arfima<-arfima(eur_train$Log_Zamkniecie,trace=T,max.p = 1,max.q=1,stepwise=F,drange = c(0, 1))

summary(eur_arima)
summary(eur_arfima)
#Sprawdzenie reszt z modelu
checkresiduals(eur_arima)
checkresiduals(eur_arfima)
Box.test(eur_arfima$residuals,type="Ljung-Box",lag=10)
#model arfima dla euro nie przeszedl pozytywnie testow

#Funt brytyjski
Acf(na.omit(diff.xts(gbp_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(gbp_train$Log_Zamkniecie)))

gbp_arima<-auto.arima(gbp_train$Log_Zamkniecie,trace = T,max.p = 2, max.q=2,d=1,stepwise=F)
gbp_arfima<-arfima(gbp_train$Log_Zamkniecie,trace=T,max.p = 2, max.q=2,stepwise=F,drange = c(0, 1))
summary(gbp_arima)
summary(gbp_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(gbp_arima)
checkresiduals(gbp_arfima)
Box.test(gbp_arfima$residuals,type="Ljung-Box",lag=10)
#Reszty z modelu arima nie sa bialym szumem od 7 opoznienia

#Jen japoński
Acf(na.omit(diff.xts(jpy_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(jpy_train$Log_Zamkniecie)))

jpy_arima<-auto.arima(jpy_train$Log_Zamkniecie,trace = T,max.p = 2, max.q=2,d=1,stepwise=F)
jpy_arfima<-arfima(jpy_train$Log_Zamkniecie,trace=T,max.p = 2, max.q=2,stepwise=F,drange = c(0, 1))
summary(jpy_arima)
summary(jpy_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(jpy_arima)
checkresiduals(jpy_arfima)
Box.test(jpy_arfima$residuals,type="Ljung-Box",lag=10)
#Reszty z modelu arima oraz arfima nie sa bialym szumem

#Krypto
#Bitcoin
Acf(na.omit(diff.xts(btc_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(btc_train$Log_Zamkniecie)))

btc_arima<-auto.arima(btc_train$Log_Zamkniecie,trace=T,max.p = 1, max.q=1,d=1,stepwise=F)
btc_arfima<-arfima(btc_train$Log_Zamkniecie,trace=T,max.p = 1, max.q=1,stepwise=F,drange = c(0, 1))
summary(btc_arima)
summary(btc_arfima)


#Sprawdzenie reszt z modelu
checkresiduals(btc_arima)
checkresiduals(btc_arfima)
Box.test(btc_arfima$residuals,type="Ljung-Box",lag=10)
#Reszty z modelu arfima nie sa bialym szumem

#dogecoin
Acf(na.omit(diff.xts(doge_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(doge_train$Log_Zamkniecie)))

doge_arima<-auto.arima(doge_train$Log_Zamkniecie,trace=T,max.p = 3, max.q=3,d=1,stepwise=F)
doge_arfima<-arfima(doge_train$Log_Zamkniecie,trace=T,max.p = 3, max.q=3,stepwise=F,drange = c(0, 1))
summary(doge_arima)
summary(doge_arfima)


#Sprawdzenie reszt z modelu
checkresiduals(doge_arima)
checkresiduals(doge_arfima)
Box.test(doge_arfima$residuals,type="Ljung-Box",lag=10)

#Reszty z modelu arfima nie sa bialym szumem

#Ethereum
Acf(na.omit(diff.xts(eth_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(eth_train$Log_Zamkniecie)))

eth_arima<-auto.arima(eth_train$Log_Zamkniecie,trace=T,max.p = 4, max.q=4,d=1,stepwise=F)
eth_arfima<-arfima(eth_train$Log_Zamkniecie,trace=T,max.p = 4, max.q=4,stepwise=F,drange = c(0, 1))
summary(eth_arima)
summary(eth_arfima)


#Sprawdzenie reszt z modelu
checkresiduals(eth_arima)
checkresiduals(eth_arfima)
Box.test(eth_arfima$residuals,type="Ljung-Box",lag=10)
#reszty z modelu arfima nie sa bialym szumem

#Litecoin
Acf(na.omit(diff.xts(ltc_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(ltc_train$Log_Zamkniecie)))

ltc_arima<-auto.arima(ltc_train$Log_Zamkniecie,trace=T,max.p = 6, max.q=6,d=1,stepwise=F)
ltc_arfima<-arfima(ltc_train$Log_Zamkniecie,trace=T,max.p = 6, max.q=6,stepwise=F,drange = c(0, 1))
summary(ltc_arima)
summary(ltc_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(ltc_arima)
checkresiduals(ltc_arfima)
Box.test(ltc_arfima$residuals,type="Ljung-Box",lag=10)

#reszty nie sia bialym szumem

#XRipple
Acf(na.omit(diff.xts(aud_train$Log_Zamkniecie)))
Pacf(na.omit(diff.xts(aud_train$Log_Zamkniecie)))

xrp_arima<-auto.arima(xrp_train$Log_Zamkniecie,trace=T,max.p = 2, max.q=2,d=1,stepwise=F)
xrp_arfima<-arfima(xrp_train$Log_Zamkniecie,trace=T,max.p = 2, max.q=2,stepwise=F,drange = c(0, 1))
summary(xrp_arima)
summary(xrp_arfima)

#Sprawdzenie reszt z modelu
checkresiduals(xrp_arima)
checkresiduals(xrp_arfima)
Box.test(xrp_arfima$residuals,type="Ljung-Box",lag = 10)
#reszty nie sa bialym szumem




# ###################################################################
# #Dodatkowo
# #Szukanie najlepszego modelu w trainie za pomoca tsCV (Kroswalidacja)
# ###################################################################
# #Euro
# 
# arima_best_rmse <-Inf
# arfima_best_rmse <-Inf
# best_params_arima<-c(null,null)
# best_params_arfima<-c(null,null)
# start_time <- Sys.time()
# for (p in 0:1) {
#   for (q in 0:1) {
#     arima_pred_eur <- tsCV(ts(eur_train$Log_Zamkniecie), function(x, h){forecast(Arima(x,order=c(p,1,q)), h=h)}, h=14)
#     arfima_pred_eur <- tsCV(ts(eur_train$Log_Zamkniecie), function(x, h){forecast(arfima(x,max.p=p,max.q=q), h=h)}, h=14)
#     if(sqrt(mean(arima_pred_eur ^ 2, na.rm = TRUE))<arima_best_rmse){arima_best_rmse<-sqrt(mean(arima_pred_eur ^ 2, na.rm = TRUE)); best_params_arima<-c(p,q)}
#     if(sqrt(mean(arfima_pred_eur ^ 2, na.rm = TRUE))<arfima_best_rmse){arfima_best_rmse<-sqrt(mean(arfima_pred_eur ^ 2, na.rm = TRUE)); best_params_arfima<-c(p,q)}
#     
#   }
# }
# 
# end_time <- Sys.time()
# end_time - start_time
# #3 min
# 
# #Ostateczne stworzenie modelu na calej probie treningowej
# #euro
# eur_arima<-Arima(eur_train$Log_Zamkniecie,order = c(1,1,0))
# eur_arfima<-arfima(eur_train$Log_Zamkniecie,trace=T,max.p = 1,max.q=1)
# 
# checkresiduals(eur_arima)
# checkresiduals(eur_arfima)
# #Nie rozwiazalo to problemow z autokorelacja reszt
# 
# #GBP
# 
# arima_best_rmse <-Inf
# arfima_best_rmse <-Inf
# best_params_arima<-c(null,null)
# best_params_arfima<-c(null,null)
# start_time <- Sys.time()
# for (p in 0:2) {
#   for (q in 0:2) {
#     arima_pred_gbp <- tsCV(ts(gbp_train$Log_Zamkniecie), function(x, h){forecast(Arima(x,order=c(p,1,q)), h=h)}, h=30)
#     arfima_pred_gbp <- tsCV(ts(gbp_train$Log_Zamkniecie), function(x, h){forecast(arfima(x,max.p=p,max.q=q), h=h)}, h=30)
#     if(sqrt(mean(arima_pred_gbp ^ 2, na.rm = TRUE))<arima_best_rmse){arima_best_rmse<-sqrt(mean(arima_pred_gbp ^ 2, na.rm = TRUE)); best_params_arima<-c(p,q)}
#     if(sqrt(mean(arfima_pred_gbp ^ 2, na.rm = TRUE))<arfima_best_rmse){arfima_best_rmse<-sqrt(mean(arfima_pred_gbp ^ 2, na.rm = TRUE)); best_params_arfima<-c(p,q)}
#     
#   }
# }
# 
# end_time <- Sys.time()
# end_time - start_time
# #12 min
# #Ostateczne stworzenie modelu na calej probie treningowej
# 
# gbp_arima<-Arima(gbp_train$Log_Zamkniecie,order = c(0,1,0))
# gbp_arfima<-arfima(gbp_train$Log_Zamkniecie,trace=T,max.p = 2,max.q=2)
# checkresiduals(gbp_arima)
# checkresiduals(gbp_arfima)
# #Nie rozwiazalo to problemow z autokorelacja reszt
# #Nie kontynuuje tego ze wzgledow czasowych




#################################################
###Prognozy na pojedynczym oknie (ostatni rok)###
#################################################

#Euro
pred_eur_arima<-forecast(eur_arima,h=nrow(eur_test))
pred_eur_arfima<-forecast(eur_arfima,h=nrow(eur_test))

accuracy(pred_eur_arima,eur_test$Log_Zamkniecie)
accuracy(pred_eur_arfima,eur_test$Log_Zamkniecie)

autoplot(pred_eur_arima,xlim = c(750, 1294))+autolayer(ts(eur_test$Log_Zamkniecie,start=1294-nrow(eur_test)+1,end = 1294),series="actual values")
autoplot(pred_eur_arfima,xlim = c(750, 1294),)+autolayer(ts(eur_test$Log_Zamkniecie,start=1294-nrow(eur_test)+1,end = 1294),series='actual values')


#AUD
pred_aud_arima<-forecast(aud_arima,h=nrow(aud_test))
pred_aud_arfima<-forecast(aud_arfima,h=nrow(aud_test))

accuracy(pred_aud_arima,aud_test$Log_Zamkniecie)
accuracy(pred_aud_arfima,aud_test$Log_Zamkniecie)

autoplot(pred_aud_arima,xlim = c(750, 1294))+autolayer(ts(aud_test$Log_Zamkniecie,start=1294-nrow(aud_test)+1,end = 1294),series="actual values")
autoplot(pred_aud_arfima,xlim = c(750, 1294),)+autolayer(ts(aud_test$Log_Zamkniecie,start=1294-nrow(aud_test)+1,end = 1294),series='actual values')

#dolar kanadyjski

pred_cad_arima<-forecast(cad_arima,h=nrow(cad_test))
pred_cad_arfima<-forecast(cad_arfima,h=nrow(cad_test))

accuracy(pred_cad_arima,cad_test$Log_Zamkniecie)
accuracy(pred_cad_arfima,cad_test$Log_Zamkniecie)

autoplot(pred_cad_arima,xlim = c(750, 1294))+autolayer(ts(cad_test$Log_Zamkniecie,start=1294-nrow(cad_test)+1,end = 1294),series="actual values")
autoplot(pred_cad_arfima,xlim = c(750, 1294),)+autolayer(ts(cad_test$Log_Zamkniecie,start=1294-nrow(cad_test)+1,end = 1294),series='actual values')


#funt brytyjski
pred_gbp_arima<-forecast(gbp_arima,h=nrow(gbp_test))
pred_gbp_arfima<-forecast(gbp_arfima,h=nrow(gbp_test))

accuracy(pred_gbp_arima,gbp_test$Log_Zamkniecie)
accuracy(pred_gbp_arfima,gbp_test$Log_Zamkniecie)

autoplot(pred_gbp_arima,xlim = c(750, 1294))+autolayer(ts(gbp_test$Log_Zamkniecie,start=1294-nrow(gbp_test)+1,end = 1294),series="actual values")
autoplot(pred_gbp_arfima,xlim = c(750, 1294),)+autolayer(ts(gbp_test$Log_Zamkniecie,start=1294-nrow(gbp_test)+1,end = 1294),series='actual values')

#jen japonski
pred_jpy_arima<-forecast(jpy_arima,h=nrow(jpy_test))
pred_jpy_arfima<-forecast(jpy_arfima,h=nrow(jpy_test))

accuracy(pred_jpy_arima,jpy_test$Log_Zamkniecie)
accuracy(pred_jpy_arfima,jpy_test$Log_Zamkniecie)

autoplot(pred_jpy_arima,xlim = c(750, 1294))+autolayer(ts(jpy_test$Log_Zamkniecie,start=1294-nrow(jpy_test)+1,end = 1294),series="actual values")
autoplot(pred_jpy_arfima,xlim = c(750, 1294),)+autolayer(ts(jpy_test$Log_Zamkniecie,start=1294-nrow(jpy_test)+1,end = 1294),series='actual values')

#krypto
#bitcoin
pred_btc_arima<-forecast(btc_arima,h=nrow(btc_test))
pred_btc_arfima<-forecast(btc_arfima,h=nrow(btc_test))

accuracy(pred_btc_arima,btc_test$Log_Zamkniecie)
accuracy(pred_btc_arfima,btc_test$Log_Zamkniecie)

autoplot(pred_btc_arima,xlim = c(1000, 1826))+autolayer(ts(btc_test$Log_Zamkniecie,start=1826-nrow(btc_test)+1,end = 1826),series="actual values")
autoplot(pred_btc_arfima,xlim = c(1000, 1826),)+autolayer(ts(btc_test$Log_Zamkniecie,start=1826-nrow(btc_test)+1,end = 1826),series='actual values')

#dogecoin
pred_doge_arima<-forecast(doge_arima,h=nrow(doge_test))
pred_doge_arfima<-forecast(doge_arfima,h=nrow(doge_test))

accuracy(pred_doge_arima,doge_test$Log_Zamkniecie)
accuracy(pred_doge_arfima,doge_test$Log_Zamkniecie)

autoplot(pred_doge_arima,xlim = c(1000, 1826))+autolayer(ts(doge_test$Log_Zamkniecie,start=1826-nrow(doge_test)+1,end = 1826),series="actual values")
autoplot(pred_doge_arfima,xlim = c(1000, 1826),)+autolayer(ts(doge_test$Log_Zamkniecie,start=1826-nrow(doge_test)+1,end = 1826),series='actual values')

#Ethereum
pred_eth_arima<-forecast(eth_arima,h=nrow(eth_test))
pred_eth_arfima<-forecast(eth_arfima,h=nrow(eth_test))

accuracy(pred_eth_arima,eth_test$Log_Zamkniecie)
accuracy(pred_eth_arfima,eth_test$Log_Zamkniecie)

autoplot(pred_eth_arima,xlim = c(1000, 1826))+autolayer(ts(eth_test$Log_Zamkniecie,start=1826-nrow(eth_test)+1,end = 1826),series="actual values")
autoplot(pred_eth_arfima,xlim = c(1000, 1826),)+autolayer(ts(eth_test$Log_Zamkniecie,start=1826-nrow(eth_test)+1,end = 1826),series='actual values')

#Litecoin

pred_ltc_arima<-forecast(ltc_arima,h=nrow(ltc_test))
pred_ltc_arfima<-forecast(ltc_arfima,h=nrow(ltc_test))

accuracy(pred_ltc_arima,ltc_test$Log_Zamkniecie)
accuracy(pred_ltc_arfima,ltc_test$Log_Zamkniecie)

autoplot(pred_ltc_arima,xlim = c(1000, 1826))+autolayer(ts(ltc_test$Log_Zamkniecie,start=1826-nrow(ltc_test)+1,end = 1826),series="actual values")
autoplot(pred_ltc_arfima,xlim = c(1000, 1826),)+autolayer(ts(ltc_test$Log_Zamkniecie,start=1826-nrow(ltc_test)+1,end = 1826),series='actual values')

#Xripple
pred_xrp_arima<-forecast(xrp_arima,h=nrow(xrp_test))
pred_xrp_arfima<-forecast(xrp_arfima,h=nrow(xrp_test))

accuracy(pred_xrp_arima,xrp_test$Log_Zamkniecie)
accuracy(pred_xrp_arfima,xrp_test$Log_Zamkniecie)

autoplot(pred_xrp_arima,xlim = c(1000, 1826))+autolayer(ts(xrp_test$Log_Zamkniecie,start=1826-nrow(xrp_test)+1,end = 1826),series="actual values")
autoplot(pred_xrp_arfima,xlim = c(1000, 1826),)+autolayer(ts(xrp_test$Log_Zamkniecie,start=1826-nrow(xrp_test)+1,end = 1826),series='actual values')




#####################################################################
###PROGNOZY Z MODELI AR(F)IMA - NA RUCHOMYM OKNIE W PETLI###
#####################################################################


#Zastosowano expanding window

#AUD
aud_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
aud_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
  for (i in 1:nrow(aud_test)) {
    
    temp<-aud[index(aud)>-1 & index(aud)<nrow(aud_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
    #Czemu przesuwamy poczatek datasetu?
    aud_forecast_arima <- Arima(temp$Log_Zamkniecie,model=aud_arima) %>% forecast(h=k)

    aud_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=aud_arfima) %>% forecast(h=k)
    for(j in 1:14){
      aud_forecast[i,paste0("actual_t+",j)]<-aud[index(aud)==nrow(aud_train)+i-k+j,2]
      aud_forecast[i,paste0("arima_predicted_t+",j)]<- exp(aud_forecast_arima$mean[j])
      aud_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(aud_forecast_arfima$mean[j])
      
    }
  }
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(aud_forecast[,paste0("arima_predicted_t+",i)],aud_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(aud_forecast[,paste0("arfima_predicted_t+",i)],aud_forecast[,paste0("actual_t+",i)]))

  p<-ggplot() + geom_line(aes(x=aud[index(aud)>nrow(aud_train)-k+i & index(aud)<=nrow(aud)-k+i,1], y =aud_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=aud[index(aud)>nrow(aud_train)-k+i & index(aud)<=nrow(aud)-k+i,1],y = aud_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=aud[index(aud)>nrow(aud_train)-k+i & index(aud)<=nrow(aud)-k+i,1], y = aud_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('AUD/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_AUD_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\AUD_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(aud_forecast[,paste0("arima_predicted_t+",i)],aud_forecast[,paste0("actual_t+",i)]))))
  aud_forecast_accuracy<-rbind(aud_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(aud_forecast[,paste0("arfima_predicted_t+",i)],aud_forecast[,paste0("actual_t+",i)]))))
  aud_forecast_accuracy<-rbind(aud_forecast_accuracy,temp)  
  
}
colnames(aud_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
aud_forecast_accuracy[,2:6]<-sapply(aud_forecast_accuracy[,2:6],as.numeric)
kbl(aud_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\AUD_USD\\AR(F)IMA_AUD_USD_Forecast_accuracy.html")

write.csv(aud_forecast,"Forecasting_result\\AUD_USD\\AR(F)IMA_AUD_USD_Forecast.csv")


#cad
cad_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
cad_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(cad_test)) {
  
  temp<-cad[index(cad)>-1 & index(cad)<nrow(cad_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  cad_forecast_arima <- Arima(temp$Log_Zamkniecie,model=cad_arima) %>% forecast(h=k)
  
  cad_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=cad_arfima) %>% forecast(h=k)
  for(j in 1:14){
    cad_forecast[i,paste0("actual_t+",j)]<-cad[index(cad)==nrow(cad_train)+i-k+j,2]
    cad_forecast[i,paste0("arima_predicted_t+",j)]<- exp(cad_forecast_arima$mean[j])
    cad_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(cad_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(cad_forecast[,paste0("arima_predicted_t+",i)],cad_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(cad_forecast[,paste0("arfima_predicted_t+",i)],cad_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=cad[index(cad)>nrow(cad_train)-k+i & index(cad)<=nrow(cad)-k+i,1], y =cad_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=cad[index(cad)>nrow(cad_train)-k+i & index(cad)<=nrow(cad)-k+i,1],y = cad_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=cad[index(cad)>nrow(cad_train)-k+i & index(cad)<=nrow(cad)-k+i,1], y = cad_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('CAD/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_CAD_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\cad_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(cad_forecast[,paste0("arima_predicted_t+",i)],cad_forecast[,paste0("actual_t+",i)]))))
  cad_forecast_accuracy<-rbind(cad_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(cad_forecast[,paste0("arfima_predicted_t+",i)],cad_forecast[,paste0("actual_t+",i)]))))
  cad_forecast_accuracy<-rbind(cad_forecast_accuracy,temp)  
  
}
colnames(cad_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
cad_forecast_accuracy[,2:6]<-sapply(cad_forecast_accuracy[,2:6],as.numeric)
kbl(cad_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\cad_USD\\AR(F)IMA_CAD_USD_Forecast_accuracy.html")

write.csv(cad_forecast,"Forecasting_result\\cad_USD\\AR(F)IMA_CAD_USD_Forecast.csv")


#eur
eur_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
eur_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(eur_test)) {
  
  temp<-eur[index(eur)>-1 & index(eur)<nrow(eur_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  eur_forecast_arima <- Arima(temp$Log_Zamkniecie,model=eur_arima) %>% forecast(h=k)
  
  eur_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=eur_arfima) %>% forecast(h=k)
  for(j in 1:14){
    eur_forecast[i,paste0("actual_t+",j)]<-eur[index(eur)==nrow(eur_train)+i-k+j,2]
    eur_forecast[i,paste0("arima_predicted_t+",j)]<- exp(eur_forecast_arima$mean[j])
    eur_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(eur_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(eur_forecast[,paste0("arima_predicted_t+",i)],eur_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(eur_forecast[,paste0("arfima_predicted_t+",i)],eur_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=eur[index(eur)>nrow(eur_train)-k+i & index(eur)<=nrow(eur)-k+i,1], y =eur_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=eur[index(eur)>nrow(eur_train)-k+i & index(eur)<=nrow(eur)-k+i,1],y = eur_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=eur[index(eur)>nrow(eur_train)-k+i & index(eur)<=nrow(eur)-k+i,1], y = eur_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('EUR/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_EUR_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\eur_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(eur_forecast[,paste0("arima_predicted_t+",i)],eur_forecast[,paste0("actual_t+",i)]))))
  eur_forecast_accuracy<-rbind(eur_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(eur_forecast[,paste0("arfima_predicted_t+",i)],eur_forecast[,paste0("actual_t+",i)]))))
  eur_forecast_accuracy<-rbind(eur_forecast_accuracy,temp)  
  
}
colnames(eur_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
eur_forecast_accuracy[,2:6]<-sapply(eur_forecast_accuracy[,2:6],as.numeric)
kbl(eur_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\eur_USD\\AR(F)IMA_EUR_USD_Forecast_accuracy.html")

write.csv(eur_forecast,"Forecasting_result\\eur_USD\\AR(F)IMA_EUR_USD_Forecast.csv")


#gbp
gbp_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
gbp_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(gbp_test)) {
  
  temp<-gbp[index(gbp)>-1 & index(gbp)<nrow(gbp_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  gbp_forecast_arima <- Arima(temp$Log_Zamkniecie,model=gbp_arima) %>% forecast(h=k)
  
  gbp_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=gbp_arfima) %>% forecast(h=k)
  for(j in 1:14){
    gbp_forecast[i,paste0("actual_t+",j)]<-gbp[index(gbp)==nrow(gbp_train)+i-k+j,2]
    gbp_forecast[i,paste0("arima_predicted_t+",j)]<- exp(gbp_forecast_arima$mean[j])
    gbp_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(gbp_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(gbp_forecast[,paste0("arima_predicted_t+",i)],gbp_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(gbp_forecast[,paste0("arfima_predicted_t+",i)],gbp_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=gbp[index(gbp)>nrow(gbp_train)-k+i & index(gbp)<=nrow(gbp)-k+i,1], y =gbp_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=gbp[index(gbp)>nrow(gbp_train)-k+i & index(gbp)<=nrow(gbp)-k+i,1],y = gbp_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=gbp[index(gbp)>nrow(gbp_train)-k+i & index(gbp)<=nrow(gbp)-k+i,1], y = gbp_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('GBP/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_GBP_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\gbp_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(gbp_forecast[,paste0("arima_predicted_t+",i)],gbp_forecast[,paste0("actual_t+",i)]))))
  gbp_forecast_accuracy<-rbind(gbp_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(gbp_forecast[,paste0("arfima_predicted_t+",i)],gbp_forecast[,paste0("actual_t+",i)]))))
  gbp_forecast_accuracy<-rbind(gbp_forecast_accuracy,temp)  
  
}
colnames(gbp_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
gbp_forecast_accuracy[,2:6]<-sapply(gbp_forecast_accuracy[,2:6],as.numeric)
kbl(gbp_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\gbp_USD\\AR(F)IMA_GBP_USD_Forecast_accuracy.html")

write.csv(gbp_forecast,"Forecasting_result\\gbp_USD\\AR(F)IMA_GBP_USD_Forecast.csv")



#jpy
jpy_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
jpy_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(jpy_test)) {
  
  temp<-jpy[index(jpy)>-1 & index(jpy)<nrow(jpy_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  jpy_forecast_arima <- Arima(temp$Log_Zamkniecie,model=jpy_arima) %>% forecast(h=k)
  
  jpy_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=jpy_arfima) %>% forecast(h=k)
  for(j in 1:14){
    jpy_forecast[i,paste0("actual_t+",j)]<-jpy[index(jpy)==nrow(jpy_train)+i-k+j,2]
    jpy_forecast[i,paste0("arima_predicted_t+",j)]<- exp(jpy_forecast_arima$mean[j])
    jpy_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(jpy_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(jpy_forecast[,paste0("arima_predicted_t+",i)],jpy_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(jpy_forecast[,paste0("arfima_predicted_t+",i)],jpy_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=jpy[index(jpy)>nrow(jpy_train)-k+i & index(jpy)<=nrow(jpy)-k+i,1], y =jpy_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=jpy[index(jpy)>nrow(jpy_train)-k+i & index(jpy)<=nrow(jpy)-k+i,1],y = jpy_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=jpy[index(jpy)>nrow(jpy_train)-k+i & index(jpy)<=nrow(jpy)-k+i,1], y = jpy_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('JPY/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_JPY_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\jpy_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(jpy_forecast[,paste0("arima_predicted_t+",i)],jpy_forecast[,paste0("actual_t+",i)]))))
  jpy_forecast_accuracy<-rbind(jpy_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(jpy_forecast[,paste0("arfima_predicted_t+",i)],jpy_forecast[,paste0("actual_t+",i)]))))
  jpy_forecast_accuracy<-rbind(jpy_forecast_accuracy,temp)  
  
}
colnames(jpy_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
jpy_forecast_accuracy[,2:6]<-sapply(jpy_forecast_accuracy[,2:6],as.numeric)
kbl(jpy_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\jpy_USD\\AR(F)IMA_JPY_USD_Forecast_accuracy.html")

write.csv(jpy_forecast,"Forecasting_result\\jpy_USD\\AR(F)IMA_JPY_USD_Forecast.csv")


#btc
btc_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
btc_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(btc_test)) {
  
  temp<-btc[index(btc)>-1 & index(btc)<nrow(btc_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  btc_forecast_arima <- Arima(temp$Log_Zamkniecie,model=btc_arima) %>% forecast(h=k)
  
  btc_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=btc_arfima) %>% forecast(h=k)
  for(j in 1:14){
    btc_forecast[i,paste0("actual_t+",j)]<-btc[index(btc)==nrow(btc_train)+i-k+j,2]
    btc_forecast[i,paste0("arima_predicted_t+",j)]<- exp(btc_forecast_arima$mean[j])
    btc_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(btc_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(btc_forecast[,paste0("arima_predicted_t+",i)],btc_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(btc_forecast[,paste0("arfima_predicted_t+",i)],btc_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=btc[index(btc)>nrow(btc_train)-k+i & index(btc)<=nrow(btc)-k+i,1], y =btc_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=btc[index(btc)>nrow(btc_train)-k+i & index(btc)<=nrow(btc)-k+i,1],y = btc_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=btc[index(btc)>nrow(btc_train)-k+i & index(btc)<=nrow(btc)-k+i,1], y = btc_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('BTC/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_BTC_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\btc_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(btc_forecast[,paste0("arima_predicted_t+",i)],btc_forecast[,paste0("actual_t+",i)]))))
  btc_forecast_accuracy<-rbind(btc_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(btc_forecast[,paste0("arfima_predicted_t+",i)],btc_forecast[,paste0("actual_t+",i)]))))
  btc_forecast_accuracy<-rbind(btc_forecast_accuracy,temp)  
  
}
colnames(btc_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
btc_forecast_accuracy[,2:6]<-sapply(btc_forecast_accuracy[,2:6],as.numeric)
kbl(btc_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\btc_USD\\AR(F)IMA_BTC_USD_Forecast_accuracy.html")

write.csv(btc_forecast,"Forecasting_result\\btc_USD\\AR(F)IMA_BTC_USD_Forecast.csv")


#doge
doge_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
doge_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(doge_test)) {
  
  temp<-doge[index(doge)>-1 & index(doge)<nrow(doge_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  doge_forecast_arima <- Arima(temp$Log_Zamkniecie,model=doge_arima) %>% forecast(h=k)
  
  doge_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=doge_arfima) %>% forecast(h=k)
  for(j in 1:14){
    doge_forecast[i,paste0("actual_t+",j)]<-doge[index(doge)==nrow(doge_train)+i-k+j,2]
    doge_forecast[i,paste0("arima_predicted_t+",j)]<- exp(doge_forecast_arima$mean[j])
    doge_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(doge_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(doge_forecast[,paste0("arima_predicted_t+",i)],doge_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(doge_forecast[,paste0("arfima_predicted_t+",i)],doge_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=doge[index(doge)>nrow(doge_train)-k+i & index(doge)<=nrow(doge)-k+i,1], y =doge_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=doge[index(doge)>nrow(doge_train)-k+i & index(doge)<=nrow(doge)-k+i,1],y = doge_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=doge[index(doge)>nrow(doge_train)-k+i & index(doge)<=nrow(doge)-k+i,1], y = doge_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('DOGE/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_DOGE_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\doge_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(doge_forecast[,paste0("arima_predicted_t+",i)],doge_forecast[,paste0("actual_t+",i)]))))
  doge_forecast_accuracy<-rbind(doge_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(doge_forecast[,paste0("arfima_predicted_t+",i)],doge_forecast[,paste0("actual_t+",i)]))))
  doge_forecast_accuracy<-rbind(doge_forecast_accuracy,temp)  
  
}
colnames(doge_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
doge_forecast_accuracy[,2:6]<-sapply(doge_forecast_accuracy[,2:6],as.numeric)
kbl(doge_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\doge_USD\\AR(F)IMA_DOGE_USD_Forecast_accuracy.html")

write.csv(doge_forecast,"Forecasting_result\\doge_USD\\AR(F)IMA_DOGE_USD_Forecast.csv")


#eth
eth_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
eth_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(eth_test)) {
  
  temp<-eth[index(eth)>-1 & index(eth)<nrow(eth_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  eth_forecast_arima <- Arima(temp$Log_Zamkniecie,model=eth_arima) %>% forecast(h=k)
  
  eth_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=eth_arfima) %>% forecast(h=k)
  for(j in 1:14){
    eth_forecast[i,paste0("actual_t+",j)]<-eth[index(eth)==nrow(eth_train)+i-k+j,2]
    eth_forecast[i,paste0("arima_predicted_t+",j)]<- exp(eth_forecast_arima$mean[j])
    eth_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(eth_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(eth_forecast[,paste0("arima_predicted_t+",i)],eth_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(eth_forecast[,paste0("arfima_predicted_t+",i)],eth_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=eth[index(eth)>nrow(eth_train)-k+i & index(eth)<=nrow(eth)-k+i,1], y =eth_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=eth[index(eth)>nrow(eth_train)-k+i & index(eth)<=nrow(eth)-k+i,1],y = eth_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=eth[index(eth)>nrow(eth_train)-k+i & index(eth)<=nrow(eth)-k+i,1], y = eth_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('ETH/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_ETH_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\eth_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(eth_forecast[,paste0("arima_predicted_t+",i)],eth_forecast[,paste0("actual_t+",i)]))))
  eth_forecast_accuracy<-rbind(eth_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(eth_forecast[,paste0("arfima_predicted_t+",i)],eth_forecast[,paste0("actual_t+",i)]))))
  eth_forecast_accuracy<-rbind(eth_forecast_accuracy,temp)  
  
}
colnames(eth_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
eth_forecast_accuracy[,2:6]<-sapply(eth_forecast_accuracy[,2:6],as.numeric)
kbl(eth_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\eth_USD\\AR(F)IMA_ETH_USD_Forecast_accuracy.html")

write.csv(eth_forecast,"Forecasting_result\\eth_USD\\AR(F)IMA_ETH_USD_Forecast.csv")


#ltc
ltc_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
ltc_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(ltc_test)) {
  
  temp<-ltc[index(ltc)>-1 & index(ltc)<nrow(ltc_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  ltc_forecast_arima <- Arima(temp$Log_Zamkniecie,model=ltc_arima) %>% forecast(h=k)
  
  ltc_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=ltc_arfima) %>% forecast(h=k)
  for(j in 1:14){
    ltc_forecast[i,paste0("actual_t+",j)]<-ltc[index(ltc)==nrow(ltc_train)+i-k+j,2]
    ltc_forecast[i,paste0("arima_predicted_t+",j)]<- exp(ltc_forecast_arima$mean[j])
    ltc_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(ltc_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(ltc_forecast[,paste0("arima_predicted_t+",i)],ltc_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(ltc_forecast[,paste0("arfima_predicted_t+",i)],ltc_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=ltc[index(ltc)>nrow(ltc_train)-k+i & index(ltc)<=nrow(ltc)-k+i,1], y =ltc_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=ltc[index(ltc)>nrow(ltc_train)-k+i & index(ltc)<=nrow(ltc)-k+i,1],y = ltc_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=ltc[index(ltc)>nrow(ltc_train)-k+i & index(ltc)<=nrow(ltc)-k+i,1], y = ltc_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('LTC/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_LTC_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\ltc_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(ltc_forecast[,paste0("arima_predicted_t+",i)],ltc_forecast[,paste0("actual_t+",i)]))))
  ltc_forecast_accuracy<-rbind(ltc_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(ltc_forecast[,paste0("arfima_predicted_t+",i)],ltc_forecast[,paste0("actual_t+",i)]))))
  ltc_forecast_accuracy<-rbind(ltc_forecast_accuracy,temp)  
  
}
colnames(ltc_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
ltc_forecast_accuracy[,2:6]<-sapply(ltc_forecast_accuracy[,2:6],as.numeric)
kbl(ltc_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\ltc_USD\\AR(F)IMA_LTC_USD_Forecast_accuracy.html")

write.csv(ltc_forecast,"Forecasting_result\\ltc_USD\\AR(F)IMA_LTC_USD_Forecast.csv")


#xrp
xrp_forecast_accuracy<-data.frame(matrix(ncol=6,nrow=0))
xrp_forecast<-data.frame()
#Horyzonty prognozy - k
k=14
#okno weryfikacji trafnosci prognoz = ostatni rok
for (i in 1:nrow(xrp_test)) {
  
  temp<-xrp[index(xrp)>-1 & index(xrp)<nrow(xrp_train)+i-k+1,]  # czy tu nie powinno byc i-k+1?
  #Czemu przesuwamy poczatek datasetu?
  xrp_forecast_arima <- Arima(temp$Log_Zamkniecie,model=xrp_arima) %>% forecast(h=k)
  
  xrp_forecast_arfima <- arfima(temp$Log_Zamkniecie,model=xrp_arfima) %>% forecast(h=k)
  for(j in 1:14){
    xrp_forecast[i,paste0("actual_t+",j)]<-xrp[index(xrp)==nrow(xrp_train)+i-k+j,2]
    xrp_forecast[i,paste0("arima_predicted_t+",j)]<- exp(xrp_forecast_arima$mean[j])
    xrp_forecast[i,paste0("arfima_predicted_t+",j)]<- exp(xrp_forecast_arfima$mean[j])
    
  }
}
for(i in 1:14){
  print(paste("Horyzont prognozy: ",i))
  print("Wyniki ARIMA")
  print(accuracy(xrp_forecast[,paste0("arima_predicted_t+",i)],xrp_forecast[,paste0("actual_t+",i)]))
  
  print("Wyniki ARFIMA")
  print(accuracy(xrp_forecast[,paste0("arfima_predicted_t+",i)],xrp_forecast[,paste0("actual_t+",i)]))
  
  p<-ggplot() + geom_line(aes(x=xrp[index(xrp)>nrow(xrp_train)-k+i & index(xrp)<=nrow(xrp)-k+i,1], y =xrp_forecast[,paste0("actual_t+",i)] ,colour='REAL'))+geom_line(aes( x=xrp[index(xrp)>nrow(xrp_train)-k+i & index(xrp)<=nrow(xrp)-k+i,1],y = xrp_forecast[,paste0("arima_predicted_t+",i)],colour="ARIMA"))+geom_line(aes(x=xrp[index(xrp)>nrow(xrp_train)-k+i & index(xrp)<=nrow(xrp)-k+i,1], y = xrp_forecast[,paste0("arfima_predicted_t+",i)],colour="ARFIMA"))+scale_color_manual(name = "Legenda", values = c("REAL" = "black", "ARIMA" = "red", "ARFIMA" = "green"))+
    labs(title=paste0('XRP/USD Forecast - Horizon: ',i),y="Price",x="Date")
  ggsave(filename=paste0("AR(F)IMA_XRP_USD Forecast - Horizon ",i," plot.png"),plot=p,path="Forecasting_result\\xrp_USD\\")
  print(p)
  temp<-rbind(c(paste0("ARIMA Forecast - horizon: ",i),as.numeric(accuracy(xrp_forecast[,paste0("arima_predicted_t+",i)],xrp_forecast[,paste0("actual_t+",i)]))))
  xrp_forecast_accuracy<-rbind(xrp_forecast_accuracy,temp)
  temp<-rbind(c(paste0("ARFIMA Forecast - horizon: ",i),as.numeric(accuracy(xrp_forecast[,paste0("arfima_predicted_t+",i)],xrp_forecast[,paste0("actual_t+",i)]))))
  xrp_forecast_accuracy<-rbind(xrp_forecast_accuracy,temp)  
  
}
colnames(xrp_forecast_accuracy)<-c('Model','ME','RMSE','MAE','MPE','MAPE')
xrp_forecast_accuracy[,2:6]<-sapply(xrp_forecast_accuracy[,2:6],as.numeric)
kbl(xrp_forecast_accuracy) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),font_size = 11) %>% 
  save_kable("Forecasting_result\\xrp_USD\\AR(F)IMA_XRP_USD_Forecast_accuracy.html")

write.csv(xrp_forecast,"Forecasting_result\\xrp_USD\\AR(F)IMA_XRP_USD_Forecast.csv")
