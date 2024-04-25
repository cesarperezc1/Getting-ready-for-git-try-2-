# ---
# title: "Lineare Modelle 2024 Vorlesungswoche 2"
# subtitle: "Das multiple lineare Regressionsmodell"
# author: "Helmut Küchenhoff"
# ---
  

## Einlesen der Daten
# Beispiel: Faktoren für Fehler bei einem Lesetest  
# Daten von 177 Kindern aus den 8 Klassen (3. und 4. Klassen Grundschule) von Christa Kieferle (Pädagogik, LMU)  
library(ggplot2)
load("daten/Lesen.RData") 

head(lesen)
# Variablen:  
# Fehlerzahl - Anzahl von Fehlern von Schülern bei einem Test zu starken Verben  
# sex - Geschlecht (= 1 für männlich, =0  für weiblich)  
# Lesezeitmin - Leseförderzeit (Lesezeit in der Schule)  
# WieoftLesen - Häufigkeit des sonstigen Lesens (Likertskala: Nie=0 bis sehr haeufig=4)  
# WieoftFern - Häufigkeit des Fernsehens (Likertskala: Nie=0 bis sehr haeufig=4) 
# WieoftMusik - Häufigkeit des Musik hörens (Likertskala: Nie=0 bis sehr haeufig=4)  
# WieoftWalk - Häufigkeit der benutzung des Walkmans (Likertskala: Nie=0 bis sehr haeufig=4)  
# WieoftComp - Häufigkeit der Computernutzung (Likertskala: Nie=0 bis sehr haeufig=4)  
# WieoftGame - Häufigkeit des Gameboy spielens (Likertskala: Nie=0 bis sehr haeufig=4)  
# Jahrgang - Idikator für Jahrgang (=0 für 3. Klasse, =1 für 4. Klasse)  
# Klasse -   name 


#Scatterplot1
ggplot(lesen,aes(x = WOL, y = Fehlerzahl)) + 
  geom_jitter(width=0,height = 0.5) + # jitter statt geom_point um 
  ## Overplotting zu vermeiden Jitter nur in höhe 
  geom_smooth(method = 'lm') + # geschätzte Regressionsgerade
  ylim(0, 50)+ xlim(0,5)


## Modell
# Scatterplot mit einfacher Regressionsgerade

ggplot(lesen,aes(x = Lesezeitmin, y = Fehlerzahl)) + 
  geom_jitter(width=0,height = 0.5) + # jitter statt geom_point um 
  ## Overplotting zu vermeiden Jitter nur in höhe 
  geom_smooth(method = 'lm') + # geschätzte Regressionsgerade
  ylim(0, 50)+ xlim(0,70)


# Zielgröße: Anzahl der Fehler bei einem Lesetest
# potentielle Einflussgrößen: Geschlecht, Jahrgang, Leseförderzeit, sonstiges Lesen,
# Häufigkeit des Gameboy spielens und Häufigkeit des Fernsehens

# multiples lineares Regressionsmodell

model1 <- lm(Fehlerzahl ~ sex + Jahrgang + Lesezeitmin + WOL + WOG + WOTV, data = lesen)
summary (model1)




library(effects)

### Darstellung der Zusammenhänge (marginale Effekte): Eine Einflussgröße wir variiert und die anderen
bei einem plausiblen Wert (Mittelwert)  festgehalten 

plot(allEffects(model1))
### besser mit einheitlicher y-Skala
plot(allEffects(model1), ylim = c(10, 24))





### mit ggplot 

all_effects<- allEffects(model1)

# Loop through all effects


for (effect_name in names(all_effects)) {
  effect_data <- as.data.frame(all_effects[[effect_name]])
  p <- ggplot(effect_data, aes_string(x = names(effect_data)[1], y = "fit", ymin = "lower", ymax = "upper")) +
    geom_ribbon(alpha = 0.2, fill = "lightblue") +
    geom_line(color = "blue") +
    labs(title = paste("Effect of", effect_name, "on Fehlerzahl"),
         x = effect_name,
         y = "Predicted Fehlerzahl") +
         ylim(10,24)+
        theme_minimal()
  print(p) 
}


