# Pakiety -----------------------------------------------------------------
library(lawstat)
library(dplyr)
library(corrplot)
library(lmtest)
library(plotly)
library(tseries)
library(plotly)


# Sprawdzenie zmienności --------------------------------------------------
Y <- dane_projektR$`PKB_per_capita`
X <- dane_projektR[, 3:7]

options(scipen = 999)
srednie <- colMeans(X)
odchylenia_std <- apply(X, 2, sd)

Xcor <- cor(X)
corrplot(Xcor, method="circle", order="hclust", tl.col = "black", tl.srt = 45,
         col=rainbow(10))
colnames(Xcor) <- c('X1', 'X2', 'X3', 'X4', 'X5')
rownames(Xcor) <- c('X1', 'X2', 'X3', 'X4', 'X5')
zmienność <- odchylenia_std/srednie * 100
opis <- summary(dane_projektR)
opis
round(zmienność, 0)

X1 <- dane_projektR$Drogi_ekspresowe_i_autostrady
X2 <- dane_projektR$Kredyty_i_pożyczki
X3 <- dane_projektR$Załadunek_z_tranzytem
X4 <- dane_projektR$Inwestycje_zagraniczne
X5 <- dane_projektR$Bilans_handlowy

# Histogramy --------------------------------------------------------------
# Histogram dla 'Drogi ekspresowe i autostrady'
hist(X1, 
     breaks = 10, 
     col = "blue", 
     main = "Histogram dla dróg ekspresowych i autostrad", 
     xlab = "Drogi ekspresowe i autostrady [tys. km]",
     ylab = "Częstotliwość",
     border = "black")

# Histogram dla 'Kredyty i pożyczki'
hist(X2, 
     breaks = 10, 
     col = "red", 
     main = "Histogram dla kredytów i pożyczek", 
     xlab = "Kredyty i pożyczki [mln zł]",
     ylab = "Częstotliwość",
     border = "black")

# Histogram dla 'Załadunek z tranzytem'
hist(X3, 
     breaks = 10, 
     col = "green", 
     main = "Histogram dla załadunku z tranzytem", 
     xlab = "Załadunek z tranzytem [tys. ton]",
     ylab = "Częstotliwość",
     border = "black")

# Histogram dla 'Inwestycje zagraniczne'
hist(X4, 
     breaks = 10, 
     col = "purple", 
     main = "Histogram dla inwestycji zagranicznych", 
     xlab = "Inwestycje zagraniczne [mln USD]",
     ylab = "Częstotliwość",
     border = "black")

# Histogram dla 'Bilans handlowy'
hist(X5, 
     breaks = 10, 
     col = "orange", 
     main = "Histogram dla bilansu handlowego", 
     xlab = "Bilans handlowy [mln USD]", 
     ylab = "Częstotliwość",
     border = "black")

# Model regresji 1 --------------------------------------------------------
dane_do_modelu <- data.frame(X1, X4, X3, Y)
model_reg <- lm(Y ~ X1 + X2 + X3, data = dane_do_modelu)

opis_model1 <- summary(model_reg)
# Z modelu wypada X3 ze względu na test studenta 

# Model regresji 2 --------------------------------------------------------
model_reg2 <- lm(Y~ X1 + X4, data = dane_do_modelu)


opis_model2 <- summary(model_reg2)


# Testy -------------------------------------------------------------------

reszty <- residuals(model_reg2)
reszty

hist(reszty, breaks=10, col="green", main="Histogram reszt modelu", 
     xlab="Reszty", ylab="Częstotliwość", border="black", col.lab="blue")

#Test Durbina-Watsona
wyniki_testu_dw <- dwtest(model_reg2)

#Test Shapiro-Wilka
wyniki_testu_sw <- shapiro.test(reszty)

#Test Goldfelda-Quanta
wyniki_testu_gq <- gqtest(model_reg2, order.by = ~X1 + X2, fraction = 5)


# Wykres HTML -------------------------------------------------------------

plot_ly(data = dane_projektR, 
        x = ~Drogi_ekspresowe_i_autostrady, 
        y = ~Bilans_handlowy, 
        type = 'scatter', 
        mode = 'markers',
        marker = list(size = 10, color = 'green')) %>%
  layout(title = 'Wykres zależności bilansu handlowego od dróg ekspresowych i autostrad',
         xaxis = list(title = 'Długość dróg ekspresowych i autostrad [tys. km]'), 
         yaxis = list(title = 'Bilans handlowy [mld USD]'),
         hovermode = 'closest')



