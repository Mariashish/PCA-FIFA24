require(tidyverse)
require(magrittr)
require(dplyr)
require(plotly)
require(ggplot2)
require(GGally)
require(FactoMineR)
require(FactoInvestigate)
require(Factoshiny)
require(factoextra)
require(highcharter)
require(utf8)

#``

fifa24 <- read_csv("C:/Users/emanu/Desktop/DataMining/PCA e MCA 04/fifa24.csv")

fifa24 <- fifa24 %>%
  rename(
    Giocatore = player,
    Paese = country,
    Altezza = height,
    Peso = weight,
    Età = age,
    Club = club,
    Controllo_della_palla = ball_control,
    Dribbling = dribbling,
    Marcatura = marking,
    Placcaggio_in_scivolata = slide_tackle,
    Placcaggio_in_piedi = stand_tackle,
    Aggressività = aggression,
    Reazioni = reactions,
    Posizione_d_attacco = att_position,
    Intercettazioni = interceptions,
    Visione = vision,
    Compostezza = composure,
    Cross = crossing,
    Passaggio_corto = short_pass,
    Passaggio_lungo = long_pass,
    Accelerazione = acceleration,
    Resistenza = stamina,
    Forza = strength,
    Equilibrio = balance,
    Velocità_di_sprint = sprint_speed,
    Agilità = agility,
    Salto = jumping,
    Direzione = heading,
    Potenza_di_tiro = shot_power,
    Finitura = finishing,
    Tiri_lunghi = long_shots,
    Curva = curve,
    Precisione_nei_calci_di_punizione = fk_acc,
    Rigori = penalties,
    Volée = volleys,
    Posizionamento_del_portiere = gk_positioning,
    Tuffo_del_portiere = gk_diving,
    Gestione_del_portiere = gk_handling,
    Calci_del_portiere = gk_kicking,
    Riflessi_del_portiere = gk_reflexes,
    Valore = value
  )

fifa24 <- fifa24[, -9]
fifa24 <- na.omit(fifa24)
colMeans(fifa24[,-c(1,2,6,40)])
colnames(fifa24)

fifa24 <- fifa24[-c(5679,5676),] #Removing non-utf8 club name row
fifa24$Club <- as_utf8(fifa24$Club)

fifa24$Valore <- gsub("\\$", "", fifa24$Valore)
fifa24$Valore <- as.numeric(gsub("\\.", "", fifa24$Valore))





#top 10 calciatori per potenza tiro
{Top10Throwers <- fifa24 %>%
  arrange(desc(Potenza_di_tiro)) %>% #Desc = Ordine discendente
  head(10)
Top10Throwers}

#10 squadre con la media d'età più giovane (Clarmont ligue 1)
{AVGAGE <- fifa24 %>%
  filter(Club != "B. Dortmund II") %>%
  group_by(Club) %>%
  summarise(AgeMean = mean(Età)) %>%
  arrange(AgeMean) %>%
  head(10)
AVGAGE}


#10 squadre con la media d'età più anziana 
{AVGAGE <- fifa24 %>%
    group_by(Club) %>%
    summarise(AgeMean = mean(Età)) %>%
    arrange(AgeMean) %>%
    tail(10)
  AVGAGE}

#giocatore con più valore
{which.max(fifa24$Valore)
fifa24[5230,]
fifa24[5230,]$Valore}


EtàValore <- fifa24 %>% 
  hchart('scatter', hcaes(x = Età, y = Valore, group = Giocatore)) %>%
  hc_colorAxis(
    minColor = "##ffdb00",
    maxColor = "#400b0b"
  ) %>%
  hc_xAxis(title = list(text="Età")) %>%
  hc_yAxis(title = list(text="Valore"))%>%
  hc_title(text = "Valore del Giocatore in Base all'Età") %>%
  hc_add_theme(hc_theme_smpl()) %>% hc_legend(enabled=FALSE)

EtàValore
cor(fifa24$Età, fifa24$Valore)

cor(fifa24$Posizione_d_attacco, fifa24$Agilità)


{PlayerStrengthPlot <- plot_ly(fifa24, x = ~Posizione_d_attacco, y = ~Potenza_di_tiro, z = ~Agilità, text = ~Giocatore, marker = list(color = ~Posizione_d_attacco, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))

PlayerStrengthPlot <- PlayerStrengthPlot %>% add_markers()
PlayerStrengthPlot <- PlayerStrengthPlot %>% layout(scene = list(xaxis = list(title = "X: Posizione d'attacco"), yaxis = list(title = 'Y: Potenza di tiro'), zaxis = list(title = 'Z: Agilità')), annotations = list(
  x = 1.02,
  y = 1.0,
  text = "Posizione d'attacco",
  xref = 'paper',
  yref = 'paper',
  font = list(family="Arial", size=20),
  showarrow = FALSE),
  title = list(
    text = "Correlazione tra Posizione d'Attacco, Potenza di Tiro e Agilità",
    font = list(family = "Arial", size = 20), y=0.98))

PlayerStrengthPlot}


fifa24$BMI = fifa24$Peso/(fifa24$Altezza/100)^2

MedianBMI <- fifa24 %>%
  group_by(Paese) %>%
  summarise(BMIMedian = median(BMI)) %>%
  arrange(BMIMedian)

#view(MedianBMI)


{MBMIPlot <- MedianBMI %>% 
  hchart('scatter', hcaes(x = Paese, y = BMIMedian, group = Paese)) %>%
  hc_colors(c("#00bfff", "#ed9121", "#d70a53", "#00cc99", "#d9ad7c", "#36D636", "#588c7e", "#D526BB", "#34AACB")) %>%
  hc_xAxis(title = list(text="Paese")) %>%
  hc_yAxis(title = list(text="BMIMedian"))%>%
  hc_title(text = "BMIMedian e Paese") %>%
  hc_add_theme(hc_theme_smpl())

MBMIPlot}

fifa24 <- fifa24[, -41] #Rimozione della colonna del BMI


#PCA
fifaPCA <- PCA(fifa24[,-c(1,2,6)]) #Rappresentazione della correlazione (positiva o negativa) su un piano cartesiano
fviz_screeplot(fifaPCA, addlabels = TRUE, ylim = c(0, 60))
#summary(fifaPCA)
#dimdesc(fifaPCA)

fviz_pca_var(fifaPCA, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "red"), repel = TRUE, title = "Grafico PCA - Contrib") #Repel = Miglioramento Leggibilità


fifaPCAClustering <- HCPC(fifaPCA, graph = FALSE, metric="manhattan", method="ward")


fviz_cluster(fifaPCAClustering,
             show.clust.cent = TRUE,
             palette = c("darkorange", "darkgreen", "purple"),
             ggtheme = theme_minimal(),
             main = "Grafico Clustering")

fviz_pca_biplot(fifaPCA, col.var = "black", col.ind = fifaPCAClustering$data.clust$clust, )

fviz_pca_ind(fifaPCA, axes = c(1,2), title="Contributo delle osservazioni alla varianza",col.ind = "cos2",
             ggtheme=theme_minimal(base_size = 11), gradient.cols = c("orange", "green", "blue"))


#contrib è il contributo che le variabili hanno nel costruire per determinare le dimensioni
#cos2 è la percentuale di varianza spiegata delle 2 dimensioni


Investigate(fifaPCA)
#Inertia = Varianza

PCAshiny(fifaPCA)