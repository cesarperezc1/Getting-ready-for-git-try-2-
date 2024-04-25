### Limo 2024 Woche 2
### Prognose covid 

## Datengrundlage : Stand 9.KW 2021  
## Es gab 2 Varianten des Coronavirus.
## Für beide wird eine getrennte prognose durchgeführt 
## Zusammen ergibt sich eine Trendumkehr
## Prognosen mit einfacher exponentieller Extrapolation 

#### siehe hier 
## https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Maerz_2021/2021-03-12-de.pdf?__blob=publicationFile


### Datengrundlage für Prädiktion (KW2 2-9)
Voc  = data.frame(
  # Kalenderwoche
  kw=2:9,
  # Probenanzahl
  n_test_sample=c(49,3344,30449,26849,33943,29770,45581,35755),
  # Davon mit B117-Mutation
  n_b117_sample = c(1,122,1441,1931,5978,7698,18224,19472),
  # Inzidenz
  # Survstat incidences - queried from https://survstat.rki.de/Content/Query/Create.aspx
  inc=c(144.70,116.23, 95.19,78.62,61.85, 63.84,68.67,71.03))

# Daten zur Überprüfung (KW)10-15)
Inc <- data.frame(kw = 10:15,
                  inc = c(85.90, 111.52, 140.16, 132.70, 142.44, 171.33))


### Variantenspezifische Inzidenz

  # Inzidenz B117 = Anteil B117 * Inzidenz
  Voc$ivoc <- Voc$n_b117_sample / Voc$n_test_sample * Voc$inc
  # Inzidenz andere
  Voc$iother <- (1 - (Voc$n_b117_sample / Voc$n_test_sample)) * Voc$inc

  
  
### Regressionsmodelle
  
# log(Inzdienz B117) über Zeit
  
reg_voc = lm(log(ivoc) ~ kw, data = Voc)
summary(reg_voc)

ggplot(Voc,aes(x = kw, y = log(ivoc))) + geom_point() +
geom_smooth(method = 'lm') 


Voc$pred<-exp(predict(reg_voc))
ggplot(Voc, aes(x = kw, y = ivoc))+geom_point()+
  geom_line(aes(x=kw,y=pred),data=Voc)


# log(Inzidenz andere) über Zeit
reg_other = lm(log(iother) ~ kw, data = Voc)
summary(reg_other)




### Prädiktion
# Datensatz für den Werte präditiert werden sollen
new <- data.frame(kw=2:15)

# Prädiktion für B117 mit KI
pred_voc_KI <- as.data.frame(predict(reg_voc, new, interval="confidence"))

# Prädiktion für B117 mit PI
pred_voc_PI <- as.data.frame(predict(reg_voc, new, interval="prediction"))


# Prädiktion für other mit KI
pred_other_KI <- as.data.frame(predict(reg_other, new, interval="confidence"))

# Prädiktion für other mit PI
pred_other_PI <- as.data.frame(predict(reg_other, new, interval="prediction"))



# zusammengefasst:

pred <- data.frame(kw = new$kw, 
                   # VOC-Prädiktion (exp(), da im Modell log(target))
                   pred_voc = exp(pred_voc_KI$fit),
                   KI_voc_lwr = exp(pred_voc_KI$lwr),
                   KI_voc_upr = exp(pred_voc_KI$upr),
                   PI_voc_lwr = exp(pred_voc_PI$lwr),
                   PI_voc_upr = exp(pred_voc_PI$upr),
                   beob_voc = c(Voc$ivoc, rep(as.numeric("NA"), 6)),
                   # Other-Prädiktion (exp(), da im Modell log(target))
                   pred_other = exp(pred_other_KI$fit),
                   KI_other_lwr = exp(pred_other_KI$lwr),
                   KI_other_upr = exp(pred_other_KI$upr),
                   PI_other_lwr = exp(pred_other_PI$lwr),
                   PI_other_upr = exp(pred_other_PI$upr),
                   beob_other = c(Voc$iother, rep(as.numeric("NA"), 6)),
                   # Gesamt-Prädiktion
                   pred_ges = exp(pred_voc_PI$fit) + exp(pred_other_PI$fit) ,
                   PI_ges_lwr = exp(pred_voc_PI$lwr) + exp(pred_other_PI$lwr),
                   PI_ges_upr = exp(pred_voc_PI$upr) + exp(pred_other_PI$upr),
                   beob_ges = c(Voc$inc, Inc$inc))



### Plots
library(ggplot2)


### Plot für Inzidenz von B117-Variante mit 
  # beobachtenten Werten (Punkte)
  # Prädiktion (Linie)
  # Konfidenz-Intervall (dunkles shading)
  # Prädiktions-Intervall (helles shading)
ggplot(pred, aes(x = kw, y = pred_voc)) +
  # KI
  geom_ribbon(aes(ymin = KI_voc_lwr, ymax = KI_voc_upr), 
              fill = "firebrick", 
              alpha = 0.4) +
  # PI
  geom_ribbon(aes(ymin = PI_voc_lwr, ymax = PI_voc_upr), 
              fill = "firebrick", 
              alpha = 0.2) +
 # Beobachtungen
  geom_point(aes(y = beob_voc), col="firebrick", size = 2.5) +
  # Prädiktion
  geom_line(col = "firebrick", lwd = 1.3) +
  # Beschriftung
  labs(title = "Inzidenz B117 mit Beob, KI/PI und Präd.",
       x = "Kalenderwoche",
       y = "Inzidenz B117") +
  ylim(0, 800)
  



### Plot für Inzidenz von Anderen mit 
# beobachtenten Werten (Punkte)
# Prädiktion (Linie)
# Konfidenz-Intervall (dunkles shading)
# Prädiktions-Intervall (helles shading)
ggplot(pred, aes(x = kw, y = pred_other)) +
  # KI
  geom_ribbon(aes(ymin = KI_other_lwr, ymax = KI_other_upr), 
              fill = "royalblue", 
              alpha = 0.4) +
  # PI
  geom_ribbon(aes(ymin = PI_other_lwr, ymax = PI_other_upr), 
              fill = "royalblue", 
              alpha = 0.2) +
  # Beobachtungen
  geom_point(aes(y = beob_other), col="royalblue", size = 2.5) +
  # Prädiktion
  geom_line(col = "royalblue", lwd = 1.3) +
  labs(title = "Inzidenz Andere mit Beob, KI/PI und Präd.",
       x = "Kalenderwoche",
       y = "Inzidenz Andere") +
  ylim(0, 800)


### Plot für Inzidenz Gesamt mit 
# beobachtenten Werten (Punkte)
# Prädiktion (Linie)
# Konfidenz-Intervall (dunkles shading)
# Prädiktions-Intervall (helles shading)
ggplot(pred, aes(x = kw, y = pred_ges)) +
  # PI
  geom_ribbon(aes(ymin = PI_ges_lwr, ymax = PI_ges_upr), 
              fill = "slateblue", 
              alpha = 0.4) +
  # Beobachtungen
  geom_point(aes(y = beob_voc), col="firebrick", size = 2.5) +
  geom_point(aes(y = beob_other), col="royalblue", size = 2.5) +
  geom_point(aes(y = inc), col = "slateblue", data = Voc, size = 2.5) +
  geom_point(aes(y = inc), col = "slateblue", data = Inc, pch = 4, size = 3) +
  # Prädiktion
  geom_line(aes(y = pred_voc), col = "firebrick", lwd = 0.5) +
  geom_line(aes(y = pred_other), col = "royalblue", lwd = 0.5) +
  geom_line(col = "slateblue", lwd = 1.3) +
  labs(title = "Inzidenz Gesamt mit Beob, PI und Präd. (violett)",
       subtitle = "B117 (rot), andere (blau), neu beob. Werte (x)",
       x = "Kalenderwoche",
       y = "Inzidenz") +
  ylim(0, 800)