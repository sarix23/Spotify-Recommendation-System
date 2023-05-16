library(tidyverse)
library(dplyr)
library(tidyr)
# Importazione del dataset
original.csv <- as_tibble(
  read_csv('/Users/sara/Desktop/Nava_Perani_Saresini/data.csv'))

data <- original.csv
print(data, width = Inf)
# Fonte: https://www.kaggle.com/datasets/bricevergnou/spotify-recommendation 
# Descrizione del Dataset: https://github.com/Brice-Vergnou/spotify_recommendation 
# From Spotify's API documentation :
# acousticness : A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 
#               1.0 represents high confidence the track is acoustic.
# danceability : Danceability describes how suitable a track is for dancing based on 
#               a combination of musical elements including tempo, rhythm stability, 
#               beat strength, and overall regularity. A value of 0.0 is least danceable 
#               and 1.0 is most danceable.
# duration_ms : The duration of the track in milliseconds.
# energy : Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of 
#         intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. 
#         For example, death metal has high energy, while a Bach prelude scores low on the 
#         scale. Perceptual features contributing to this attribute include dynamic range, 
#         perceived loudness, timbre, onset rate, and general entropy.
# instrumentalness : Predicts whether a track contains no vocals. “Ooh” and “aah” sounds 
#                   are treated as instrumental in this context. Rap or spoken word tracks 
#                   are clearly “vocal”. The closer the instrumentalness value is to 1.0, 
#                   the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.
# key : The key the track is in. Integers map to pitches using standard Pitch Class notation . 
#       E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on.
# liveness : Detects the presence of an audience in the recording. Higher liveness values 
#           represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.
# loudness : The overall loudness of a track in decibels (dB). Loudness values are averaged 
#           across the entire track and are useful for comparing relative loudness of tracks. 
#           Loudness is the quality of a sound that is the primary psychological correlate of 
#           physical strength (amplitude). Values typical range between -60 and 0 db.
# mode : Mode indicates the modality (major or minor) of a track, the type of scale from which 
#       its melodic content is derived. Major is represented by 1 and minor is 0.
# speechiness : Speechiness detects the presence of spoken words in a track. The more 
#               exclusively speech-like the recording (e.g. talk show, audio book, poetry), 
#               the closer to 1.0 the attribute value. Values above 0.66 describe tracks that 
#               are probably made entirely of spoken words. Values between 0.33 and 0.66 
#               describe tracks that may contain both music and speech, either in sections 
#               or layered, including such cases as rap music. Values below 0.33 most likely 
#               represent music and other non-speech-like tracks.
# tempo : The overall estimated tempo of a track in beats per minute (BPM). In musical 
#         terminology, tempo is the speed or pace of a given piece and derives directly 
#         from the average beat duration.
# time_signature : An estimated overall time signature of a track. The time signature (meter) 
#           is a notational convention to specify how many beats are in each bar (or measure).
# valence : A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. 
#         Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
#         while tracks with low valence sound more negative (e.g. sad, depressed, angry).
# liked : 1 for liked songs , 0 for disliked songs


str(data$liked) # type numeric
# Trasformo in factor la variabile, e la memorizzo in un tibble
data$liked <- as.factor(data$liked)
levels(data$liked) <- c('Dislike', 'Like')
table(data$liked) # le classi sono quasi perfettamente bilanciate (Dislike:95, Like:100)
labels <- select(data, liked)

# Rimuovo mode e time_signature poichè non quantitative, dunque non utilizzabili
# all'interno dei modelli di clustering e cassification che stimeremo, i quali 
# accettano in input dati quantitativi
data <- data %>% select(-time_signature, -mode)


# ANALISI DELLA CORRELAZIONE
library(ggcorrplot)
R <- cor(select(data, -liked))
ggcorrplot(R, hc.order=T, type="upper", lab=T)
# si può notare che il dataset è caratterizzato da variabili non molto correlate
# tra loro.


library(ggplot2)
library(patchwork)

measure <- colnames(select(data, -liked))
# Creiamo una lista all'interno della quale salveremo tutti i boxplot
graph <- list()
# Generiamo i boxplot
for (i in 1:length(measure)){
  p <- ggplot(data, aes(x = liked, y = .data[[measure[i]]], fill = liked)) +
    geom_boxplot() +
    labs(y = measure[i], fill = "liked") +
    ggtitle(paste(measure[i])) +
    theme(plot.title = element_text(hjust = 0.5))
  graph[[i]] <- p
}
# Visualizzazione in una griglia 3x4 dei grafici
wrap_plots(graph, nrow = 3, ncol = 4)


# Variabili con distribuzione più problematica:
# loudness speechiness instrumentalness liveness duration_ms

# Proviamo a costruire dei grafici con la distribuzione di densità condizionata
to.verify <- c("loudness", "speechiness" ,"instrumentalness", "liveness", "duration_ms")
library(ggplot2)
graph <- list()
for (i in 1:length(to.verify)) {
  p <- data %>% 
    ggplot(aes(x = as.numeric(.data[[to.verify[i]]]))) +
    geom_histogram(aes(y = ..density.., fill = liked), bins = 30, alpha = 0.5, color = "black") +
    geom_density(aes(color = liked), alpha = 0.5) +
    scale_fill_manual(values = c("pink", "lightgreen")) +
    scale_color_manual(values = c("#FF00FF", "darkgreen")) +
    facet_wrap(~liked, ncol = 2) + labs(x = to.verify[i])
  graph[[i]] <- p
}
wrap_plots(graph, nrow = 2, ncol = 3)

# Variabili fortemente asimmetriche: loudness, speechiness(solo per dislike), liveness
# e duration_ms. instrumentalness ha un andamento molto strano.

# instrumentalness.
summary(data$instrumentalness)

# Calcoliamo le statistiche per classe
summary(data %>% 
          filter(liked=='Like') %>%
          select(instrumentalness))

summary(data %>% 
          filter(liked=='Dislike') %>%
          select(instrumentalness))
# Effettivamente assumono valori in intervalli completamente diversi, di cui
# quello della classe Like compreso in quello di Dislike

# Estraiamo correlazioni di instrumentalness con le altre variabili
R[which(colnames(select(data, -liked))=='instrumentalness'),]
mean(abs(R)) #0.3130214 (correlazione media)

# instrumentalness risulta abbastanza correlata (almeno più della media) con la metà
# delle variabili del dataset.

# Decidiamo di rimuovere la variabile instrumentalness principalmente per due motivi:
# 1. Benchè i due cluster siano distribuiti diversamente, la classe 'Dislike' risulta comunque
#    avere una varianza molto elevata, di conseguenza discriminerebbe poco le classi;
# 2. Variabili altamente correlate con altre rischiano di influire negativamente sul
#    modello di clustering, aumentando la possibilità di multicollinearità.
data <- data %>% select(-instrumentalness)


# Analizziamo le altre variabili.

# duration_ms.
# Molti outlier
data.transform <- data #Dataset con variabili trasformate
data.transform$duration_ms <- log(data$duration_ms)
#  Verifica
data.transform %>% ggplot(aes(x = as.numeric(data.transform[["duration_ms"]]))) +
  geom_histogram(aes(y = ..density.., fill = liked), bins = 30, alpha = 0.5, color = "black") +
  geom_density(aes(color = liked), alpha = 0.5) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  scale_color_manual(values = c("#FF00FF", "darkgreen")) +
  facet_wrap(~liked, ncol = 2)+
  labs(title="Distribuzione della variabile duration_ms nelle due classi",
       x = 'duration_ms')+
  theme(plot.title = element_text(hjust = 0.5))

# La variabile sembrerebbe distribuita decisamente meglio! Inoltre, era espressa secondo un
# ordine di grandezza maggiore rispetto a quello delle altre variabili.

# Verifichiamo però come differiscono i due boxplot condizionati
# tra dati trasformati e non
b.nt <- ggplot(data, aes(x = liked, y = .data[['duration_ms']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'duration_ms', fill = "liked") +
  ggtitle(paste("Boxplot dati originali di", 'duration_ms'))+
  theme(plot.title = element_text(hjust = 0.5))
b.t <- ggplot(data, aes(x = liked, y = data.transform[['duration_ms']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'duration_ms', fill = "liked") +
  ggtitle(paste("Boxplot dati trasformati di", 'duration_ms'))+
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(b.nt, b.t, ncol=2)

# La distribuzione si è spostata ma:
# 1. Gli outlier si sono redistribuiti (e in 'Like' sono aumentati).
# 2. La varianza di 'Like' è aumentata.


# Loudness.
# Problema: outliers e distribuzione molto asimmetrica.
data.transform$loudness <- log((data$loudness)**2)
data.transform %>% ggplot(aes(x = as.numeric(data.transform[["loudness"]]))) +
  geom_histogram(aes(y = ..density.., fill = liked), bins = 30, alpha = 0.5, color = "black") +
  geom_density(aes(color = liked), alpha = 0.5) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  scale_color_manual(values = c("#FF00FF", "darkgreen")) +
  facet_wrap(~liked, ncol = 2)+
  labs(title="Distribuzione della variabile loudness nelle due classi",
       x = 'loudness')+
  theme(plot.title = element_text(hjust = 0.5))
# Essendo negativa, prima di applicare il logaritmo è stato necessario elevarla al quadrato

# Verifichiamo però come differiscono i due boxplot condizionati
# tra dati trasformati e non
b.nt <- ggplot(data, aes(x = liked, y = .data[['loudness']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'loudness', fill = "liked") +
  ggtitle(paste("Boxplot dati originali di", 'loudness'))+
  theme(plot.title = element_text(hjust = 0.5))
b.t <- ggplot(data, aes(x = liked, y = data.transform[['loudness']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'loudness', fill = "liked") +
  ggtitle(paste("Boxplot dati trasformati di loudness")) +
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(b.nt, b.t, ncol=2)

# Gli outlier di loudness si sono ridotti ma le due classi risultano essere più
# uniformi tra loro, col rischio che incida negativamente sul clustering.


# Speechiness.
# La presenza di outliers nella classe 'Dislike' potrebbe generare qualche problema,
# tuttavia, le due classi risultano ben distinte. Non applichiamo trasformazioni.


# Liveness.
# Assumendo valori tra 0 e 1 tentiamo di normalizzarla mediante
# arcsine square root transformation. 
data.transform$liveness <- asin(sqrt(data$liveness))

# Verifica
data.transform %>% ggplot(aes(x = as.numeric(data.transform[["liveness"]]))) +
  geom_histogram(aes(y = ..density.., fill = liked), bins = 30, alpha = 0.5, color = "black") +
  geom_density(aes(color = liked), alpha = 0.5) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  scale_color_manual(values = c("#FF00FF", "darkgreen")) +
  facet_wrap(~liked, ncol = 2)+
  labs(title="Distribuzione della variabile liveness nelle due classi",
       x = 'liveness') +
  theme(plot.title = element_text(hjust = 0.5))
# La distribuzione sembrerebbe migliorata rispetto a prima della trasfromazione.


# Verifichiamo però come differiscono i due boxplot condizionati
# tra dati trasformati e non
b.nt <- ggplot(data, aes(x = liked, y = .data[['liveness']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'liveness', fill = "liked") +
  ggtitle(paste("Boxplot dati originali di", 'liveness'))+
  theme(plot.title = element_text(hjust = 0.5))
b.t <- ggplot(data, aes(x = liked, y = data.transform[['liveness']], fill = liked)) +
  geom_boxplot() +
  labs(y = 'liveness', fill = "liked") +
  ggtitle(paste("Boxplot dati trasformati di liveness"))+
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(b.nt, b.t, ncol=2)

# Le distribuzioni sembrano aver ridotto il numero di outliers ma aumentato la varianza 
# intra - gruppo.


# La variabile key
data$key
# E' categorica nonostante assuma un valore numerico.
# Ai fini dell'applicazione dei modelli va rimossa per le ragioni già citate.
data <- data %>% select(-key)
data.transform <- data.transform %>% select(-key)



R.new <- cor(select(data, -liked))
R.transform <- cor(select(data.transform, -liked))
gg1 <- ggcorrplot(R.new, type="upper", lab=T,
                  title = "Grafico correlazioni dati non trasformati")
gg2 <- ggcorrplot(R.transform, type="upper", lab=T,
                  title = "Grafico correlazioni dati trasformati")
grid.arrange(gg1, gg2, ncol = 2)
# La trasformazione di loudness (che ricordiamo assumere valori negativi nei dati
# originali), ha cambiato segno alle correlazioni della variabile con le restanti.
# Al momento siamo indecisi sulla scelta di trasformare le variabili.
# Procediamo quindi con la feature selection.


# -----------------
# FEATURE SELECTION
# -----------------

# DATI NON TRASFORMATI
# --------------------
par(mfrow=c(1,1))
pca <- princomp(select(data, -liked), cor=T)
summary(pca)
plot(pca, type="l") # Dubbio se tenerne 3 o 4
sum((pca$sdev[1:4])^2)/ncol(select(data, -liked)) # 0.7697528
# Le prime 4 componenti spiegano il 76% della varianza
# Verifichiamo quali sono le variabili che incidono maggiormente
# sulle 4 componenti
names(data)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))]
# "loudness"    "speechiness" "liveness"    "liveness" 

# Poichè livness incide maggiormente sulla terza e quarta c.p., prendiamo in
# considerazione solo le prime 3 componenti, che spiegano il 67% della varianza.


# Scatterplot delle 3 variabili selezionate
colors <- c("orange", "purple")
var <- c("loudness","speechiness", "liveness")
# Generiamo tutte le possibili combinazioni di coppie degli elementi del vettore
combinazioni <- combn(var, 2)
# Creiamo la matrice di output con le combinazioni come colonne
matrice <- t(matrix(combinazioni, ncol = ncol(combinazioni)))


# Scatterplot di ciascuna coppia di variabili
library(patchwork)
graph <- list()
for (i in 1:nrow(matrice)){
  p <- data %>% ggplot(mapping = aes(x=.data[[matrice[i,1]]], 
                                     y=.data[[matrice[i,2]]], col=liked)) +
    scale_color_manual(values = c("orange", "purple")) +
    geom_point(alpha= .5, size=3)
  graph[[i]] <- p
}
wrap_plots(graph, nrow = 1, ncol = nrow(matrice))
# Speechiness discrimina meglio le due classi.


# Scatterplot di tutte le variabili
library(GGally)
pairs(select(data, -liked), col=colors[as.numeric(data$liked)])
# Vediamo quanto speechiness lavori discretamente nella separazione dei cluster
# con tutte le variabili.
# Complessivamente, fatta eccezione per qualche caso in cui i gruppi non sono ben distinti,
# la distribuzione sembrerebbe non essere casuale. Tuttavia, non c'è una separazione
# perfetta dei cluster con qualche zona di sovrapposizione e, a volte, i dati sembrerebbero 
# suddividersi in più di 2 gruppi.



# DATI TRASFORMATI
# --------------------
pca <- princomp(select(data.transform, -liked), cor=T)
summary(pca)
plot(pca, type="l") # Dubbio se tenerne 3 o 4
sum((pca$sdev[1:4])^2)/ncol(select(data.transform, -liked)) # 0.7663884
# Le prime 4 componenti spiegano il 76% della varianza
# Verifichiamo quali sono le variabili che incidono maggiormente
# sulle 4 componenti
names(data)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))]
# "loudness"    "speechiness" "liveness"    "liveness" 
# La situazione è analoga al caso dei dati non trasformati!!
# Notiamo che la variabile speechiness non è mai stata trasformata mentre le altre due si.
# Può essere utile capire come cambiano le distribuzioni tra dati trasformati e non.


# Scatterplot delle 3 variabili selezionate
colors <- c("orange", "purple")
g1 <- list()
g2 <- list()
library(patchwork)
for (i in 1:nrow(matrice)){
  p1 <- data.transform %>% ggplot(mapping = aes(x=data.transform[[matrice[i,1]]], 
                                                y=data.transform[[matrice[i,2]]], col=liked)) +
    scale_color_manual(values = c("orange", "purple")) +
    geom_point(alpha= .5, size=3) +
    labs(title='Scatterplot sulle Variabili Trasformate', 
         x = matrice[i,1], y = matrice[i,2])
  p2 <- data %>% ggplot(mapping = aes(x=.data[[matrice[i,1]]], 
                                      y=.data[[matrice[i,2]]], col=liked)) +
    scale_color_manual(values = c("orange", "purple")) +
    geom_point(alpha= .5, size=3) +
    labs(title='Scatterplot sulle Variabili Non Trasformate')
  g1[[i]] <- p1
  g2[[i]] <- p2
}
library(cowplot)
a <- plot_grid(plotlist = c(g1), ncol = 1, nrow = nrow(matrice), align = "o")
b <- plot_grid(plotlist = c(g2), ncol = 1, nrow = nrow(matrice), align = "o")
grid.arrange(a, b, ncol = 2)


# Realizziamo lo scatterplot di tutte le variabili
library(GGally)
pairs(select(data.transform, -liked), col=colors[as.numeric(data.transform$liked)])
pairs(select(data, -liked), col=colors[as.numeric(data$liked)])

# Concentriamoci su: loudness, liveness, duration_ms
ggpairs(data, mapping = aes(color=liked))
ggpairs(data.transform, mapping = aes(color=liked))
# Gli scatterplot in cui una variabile è speechiness sembrerebbero mostrare una maggiore
# sovrapposizione delle classi rispetto ai dati non trasformati (ricordando che
# è la variabile che spiega maggiormente la varianza dei dati).
# La distribuzione sembrerebbe essere in linea generale comunque abbastanza simile ad occhio
# (forse qualche caso di peggioramento con un aumento della casualità nei dati trasformati), dovuto
# comunque al fatto che la trasformazione è stata effettuata solo su 3 variabili.



# Poichè non c'è stato alcun margine di miglioramento trasformando i dati, 
# procediamo alla stima dei modelli sui dati originali (considerando anche che,
# in fase di trasformazione, le distribuzioni per classe si sono uniformate per le
# variabili trasformate).



# --------------------------
# MODEL BASED CLUSTERING
# --------------------------

# Per non creare troppa confusione, creiamo una copia di data rimuovendo 'liked'
spotify.data <- select(data, -liked)

# Proviamo ora a stimare un modello di clustering (criterio di scelta del modello: BIC)
library(mclust)
spotify.Mclust <- Mclust(spotify.data, G = 2) #VVE
summary(spotify.Mclust)
# Clustering table:
#  1   2 
# 88 107 
# La numerosità delle classi originarie si è sbilanciata di sole 7 unità: aumentata in Like
# rispetto a Dislike. Esito che, in realtà, non stupisce dal momento che Dislike 
# era la classe con maggior numero di outliers.

# Stimiamo il modello anche usando come criterio ICL.
spotify.mclustICL <- mclustICL(spotify.data, G = 2)
summary(spotify.mclustICL) #VVE

# Sia con ICL che con BIC viene stimato un modello VVE, ovvero un modello che stima
# due cluster di volume e forma diversi ma uguale orientamento.

# Verifichiamo graficamente:
plot(spotify.Mclust, what = 'density')
# Dal plot si osservano, per quasi tutte le coppie di variabili, ellissi concentriche,
# senza distinzione delle curve di livello in due gruppi. Questo accade probabilmente
# a causa delle zone di sovrapposizione individuate in fase di analisi esplorativa,
# e suggerirebbero inoltre una possibile distribuzione in ogni cluster non proprio Gaussiana,
# spiegata dalla forma delle curve di livello non sempre ellittica.

plot(spotify.Mclust, what = "classification", col = colors) 
# Il grafico conferma effettivamente quanto appena detto: fatta eccezione per i plot
# con le variabili danceability, speechiness e loudness, i cluster si sovrappongono,
# in particolare a causa dell'elevata variabilità nel gruppo Dislike.
# Prime possibili conclusioni: l'insieme delle canzoni che piacciono all'utente
# è ben definito rispetto a quello delle canzoni che non gradisce, che invece 
# probabilmente spazia in generi musicali differenti, spiegando il perchè della varianza
# tanto elevata.




#-----------------------------
#EVALUATION OF CLASSIFICATION
#-----------------------------
(cluster.fitted <- spotify.Mclust$classification)   
(cluster.real <- as.numeric(as_vector(labels)))          


# Classification error measures
#------------------------------
# Error Rate
classError(cluster.fitted, class=cluster.real) #0.1794872
length(classError(cluster.fitted, class=cluster.real)$misclassified) 
# 35 osservazioni su 195 mal classificate


# ARI
adjustedRandIndex (cluster.fitted, cluster.real) 
# ARI: 0.407889
# il clustering ottenuto ha una certa corrispondenza con le etichette di classe 
# reali, ma c'è ancora margine di miglioramento. Si potrebbe valutare l'effetto 
# dell'utilizzo di tecniche di clustering più complesse (che per esempio tengano conto
# anche di variabili qualitative, come il genere musicale), che purtroppo non conosciamo
# e non siamo in grado di applicare al momento.


# Confusion Matrix
library("caret")
(true<-as.factor(cluster.real))
(clust<-as.factor(cluster.fitted))
confusionMatrix(clust, true)
# Accuracy : 0.8205
# Sensitivity : 0.7789          
# Specificity : 0.8600 

# Conclusione: il modello ha mostrato sia un discreto adattamento ai dati
# che una discreta capacità di clustering.



# --------------------------------------
# TO PLOT CLASSIFICATION AND UNCERTAINTY
# --------------------------------------
# Effettuiamo un plot sulle 3 variabili selezionate inizialmente
# per visualizzare l'incertezza nella classificazione delle osservazioni.
# Inoltre, controllare anche sulla variabile duration_ms, poichè, guardando
# il grafico pairs di tutto il dataset, sembra discriminare bene i cluster

# Rendere data.frame il dataset spotify.data
spotify.df <-as.data.frame(spotify.data)

# Realizzazione dei plot
var2 <- c("loudness","speechiness", "liveness", "duration_ms")
# Generiamo tutte le possibili combinazioni di lunghezza 2 degli elementi del vettore
combinazioni2 <- combn(var2, 2)
# Creiamo la matrice di output con le combinazioni come colonne
matrice2 <- t(matrix(combinazioni2, ncol = ncol(combinazioni2)))

par(mfrow=c(3,2))
for (i in 1:nrow(matrice2)){
  a <- which(colnames(spotify.df)==matrice2[i,1])
  b <- which(colnames(spotify.df)==matrice2[i,2])
  
  # Plot true classification
  coordProj (spotify.df, dimens=c(a,b), what="classification",
             classification=cluster.real,
             col=c("orange", "purple"), symbols=c(17,16),
             sub="(a) true classification")
  
  # Plot mclust classification
  coordProj (spotify.df, dimens=c(a,b), what="classification",
             classification=cluster.fitted,
             col=c("orange", "purple"), symbols=c(17,16),
             sub="(b) Model-Based Clustering")
  
  # Aggiungere missclassified colorando i punti di nero
  (miss_class<-classError(cluster.fitted, class=cluster.real)$misclassified)
  points(spotify.data[miss_class,c(a,b)],pch=19)
}

# Commenti:
# loudness - speechiness: errori di classificazione solo nell'area di sovrapposizione
#                         dei due cluster, per il resto praticamente perfetto.
# loudness - liveness: il gruppo meglio distinto è quello dei dislike, infatti
#                     è sul gruppo dei likes una maggiore imprecisione
# speechiness - liveness: anche qui più errori nella zona di sovrapposizione dei cluster
# loudness - duration_ms: missclassified solo nella zona di sovrapposizione
# speechiness - duration_ms: missclassified solo nella zona di sovrapposizione
# liveness - duration_ms: missclassified solo nella zona di sovrapposizione
par(mfrow = c(1, 1))

# In linea generale, si può giustificare il perchè della "non-ottimalità" del modello
# di clustering stimato: si può notare che spesso, i gruppi di osservazioni sono
# abbastanza ben - distinti, fatta eccezione per una zona in cui alcune osservazioni
# di cluster differenti sono ravvicinate, creando quindi una zona di sovrapposizione.
# E' proprio in quella zona che, come mostrato nei grafici, si concentra maggiormente
# l'incertezza del clustering, generando così delle osservazioni mal classificate.
# A livello interpretativo possono esserci due possibili ipotesi relative al fenomeno:
# 1. Come già detto, l'insieme dei contenuti non apprezzati dall'utente potrebbe
# spaziare generi musicali differenti, spiegando la varianza maggiore del cluster.
# 2. Delle canzoni, pur appartendendo a generi differenti, possono avere KPI
# molto simili, generando quindi le zone di sovrapposizione individuate.
# Sarebbe utile in futuro poter confermare queste ipotesi integrando i dati sul 
# genere mediante interrogazione dell'API.



# Uncertainty plot
#------------------
uncerPlot (z=spotify.Mclust$z,truth=cluster.real)
# Più della metà delle osservazioni ha un'incertezza quasi nulla: è un buon
# risultato.

# Plot mclust uncertainty
par(mfrow=c(2,3))
for (i in 1:nrow(matrice2)){
  a <- which(colnames(spotify.df)==matrice2[i,1])
  b <- which(colnames(spotify.df)==matrice2[i,2])
  coordProj (data=spotify.df, dimens=c(a,b), what="uncertainty",
             parameters=spotify.Mclust$parameters, z=spotify.Mclust$z, 
             colors = colors)
}
par(mfrow=c(1,1))
# Plottando anche la distribuzione dell'incertezza, si mostra maggiore
# per la classe Dislike: questo conferma quando già affermato in precedenza,
# ovvero che l'alta varianza della classe causa una sovrapposizione con il
# cluster Like, generando incertezza sul gruppo di assegnazione.


# ---------------------------
# MIXTURE COMPONENTS ANALYSIS
# ---------------------------

# KULLBACK - LEIBLER
# Calcoliamo la distanza (simmetrizzata) tra le distribuzioni dei due cluster
(mu1<-spotify.Mclust$parameters$mean[,1])
(mu2<-spotify.Mclust$parameters$mean[,2])
(sigma1<-spotify.Mclust$parameters$variance$sigma[, ,1])
(sigma2<-spotify.Mclust$parameters$variance$sigma[, ,2])

library(bayesmeta)
kldiv(mu1, mu2, sigma1, sigma2, symmetrized=TRUE) # 71.18951
# la SKL di 71.18951 indica una notevole differenza tra le due distribuzioni e 
# suggerisce che ci sono alcune caratteristiche distintive nelle canzoni che 
# piacciono all'utente rispetto a quelle che non piacciono (fatta evidentemente
# eccezione per qualcuna, il che spiega la sovrapposizione).



# ENTROPIA
bic <- abs(spotify.Mclust$bic)
icl <- abs(spotify.Mclust$icl)
# ICL = ENTROPIA + BIC
(entropia <- icl - bic) # 15.05612
# Standardizzazione.
# n = unità statistiche, k = componenti della mistura
n <- spotify.Mclust$n
k <- spotify.Mclust$G
(entropia.sd <- entropia/(n*log(k)) )# 0.1113918
# il valore ottenuto suggerisce che la qualità del clustering è abbastanza 
# buona rispetto alla dimensione dei dati e al numero di componenti della mistura.


# Abbiamo quindi individuato i seguenti pattern:
# 1. Due gruppi di volume e forma variabile e medesimo orientamento
# 2. Alta varianza nella classe Dislike che può generare incertezza in fase di
#    classification, poichè alcune osservazioni ad essa associate si confondono
#    tra quelle della classe like
# 3. Bassa correlazione tra le variabili


# --------------------------------------
# CLASSIFICATION
# --------------------------------------

# Preparazione del Test set
# --------------------------------------

set.seed(234)
# Estraggo casualmente 20% di osservazioni in un intervallo da 1 a n
test.set.labels<-sample(1:n,39, replace = FALSE)
labels.v <- unlist(data[10])

# --------------------------------------
# EDDA
# --------------------------------------

library(Rmixmod)
# Per la scelta dei modelli da testare sia con EDDA che con MDA ci siamo basati sulla
# struttura dei cluster ottenuta da Mclust
ListMod <- c('Gaussian_p_Lk_D_Ak_D')

Model_Best<-mixmodLearn(spotify.df[-test.set.labels,], labels.v[-test.set.labels],
                        models=mixmodGaussianModel(listModels = ListMod), criterion=c('CV','BIC'))

pred <- mixmodPredict(spotify.df[test.set.labels,], classificationRule=Model_Best["bestResult"])
errorRate <- 1 - mean(as.integer(labels.v[test.set.labels]) == pred["partition"])
# ErrorRate: 0.1538462

# abbiamo poi deciso di utilizzare la tecnica LOO per testare le performance del 
# modello selezionato in modo 'imparziale'. Infatti, date le dimensioni relativamente
# piccole del dataset la tecnica LOO (Leave-One-Out) rappresenta un'opzione 
# valida per valutare le performance del modello. Questo perché, in LOO, viene
# iterativamente creato un modello su un sottoinsieme di dati, lasciando fuori un
# singolo punto dati alla volta, che viene usato come set di test. 
# In questo modo, viene ottenuto un insieme di errori di test, uno per ogni 
# punto dati, che vengono poi utilizzati per calcolare una stima dell'errore di 
# generalizzazione del modello.

library(Rmixmod)
set.seed(234)
idxs <- sample(1:195, 195, replace=FALSE)
vec_list <- split(idxs, rep(1:5, each = 39, length.out = length(idxs)))
# Definisco il vettore per salvare gli error rate
error_rates <- c()

par(mfrow=c(2,3))
# Itero sui gruppi di test

for (i in 1:5) {
  test.set.labels <- vec_list[[i]]
  # Addestro mixmodLearn e seleziono il modello migliore
  Model_Best <- mixmodLearn(spotify.df[-test.set.labels,], labels.v[-test.set.labels],
                            models=mixmodGaussianModel(listModels = ListMod),
                            criterion = c('CV', 'BIC'))
  
  # Calcolo l'error rate sul gruppo di test
  pred <- mixmodPredict(spotify.df[test.set.labels,], classificationRule = Model_Best["bestResult"])
  error_rates[i] <- 1 - mean(as.integer(labels.v[test.set.labels]) == pred['partition'])
  
  
  #GRAFICO MISCLASSIFIED DI EDDA:
  #------------------------------
  
  # PLOT DEI CINQUE TRAINING CON I TEST E GLI ERROR RATE DI OGNUNO COME TITOLO
  a <- which(colnames(spotify.df)==matrice2[1,1])
  b <- which(colnames(spotify.df)==matrice2[1,2])
  coordProj (spotify.df, dimens=c(a,b), what="classification",
             classification=cluster.real,
             col=c("orange", "purple"), symbols=c(17,16),
             sub=c(i))
  # Aggiungere missclassified colorando i punti di nero
  (miss_class<-classError(pred['partition'], class=cluster.real[test.set.labels])$misclassified)
  points(spotify.df[miss_class,c(a,b)],pch=19)
  
}

# Stampo gli error rate e il nome del modello migliore per ogni gruppo di test
for (i in 1:5) {
  cat("Gruppo di test:", i, "\n")
  cat("Error rate:", error_rates[i], "\n")
}

#Range ErrorRate: [0.7,0.21]


# --------------------------------------
# MDA
# --------------------------------------

set.seed(234)
mod = MclustDA(spotify.df[-test.set.labels ,],labels.v[-test.set.labels],G=2,
               modelNames=c('VVE'))

cluster.mda <- predict(mod, spotify.df[test.set.labels ,])$class

ErrorRateMDA <- 1- mean(as.numeric(predict(mod, spotify.df[test.set.labels ,])$class) == cluster.real[test.set.labels])
#error rate: 0.05128205

#GRAFICO MISCLASSIFIED DI MDA:
#-----------------------------

par(mfrow=c(1,1))
for (i in 1:nrow(matrice2)){
  a <- which(colnames(spotify.df)==matrice2[i,1])
  b <- which(colnames(spotify.df)==matrice2[i,2])
  
  # Plot MDA classification
  coordProj (spotify.df, dimens=c(a,b), what="classification",
             classification=cluster.real,
             col=c("orange", "purple"), symbols=c(17,16),
             sub="(b) Model-Based Clustering")
  
  # Aggiungere missclassified colorando i punti di nero
  (miss_class<-classError(as.numeric(cluster.mda), class=cluster.real[test.set.labels])$misclassified)
  points(spotify.df[miss_class,c(a,b)],pch=19)
  # Aggiungi legenda
  legend("top", legend=c("Like", "Dislike", "Missclassified"), 
         col=c("orange", "purple", "black"), pch=c(19,19,19), 
         ncol=2, cex=0.8, pt.cex=1.2, box.lty=0, title="Legend")
  
  
  print(p)
  readline("Premi invio per continuare...")
}

library(Rmixmod)
set.seed(234)
ErrorRateMDA <- c()

par(mfrow=c(2,3))
# Itero sui gruppi di test

for (i in 1:5) {
  test.set.labels <- vec_list[[i]]
  # Addestro mixmodLearn e seleziono il modello migliore
  mod = MclustDA(spotify.df[-test.set.labels ,],labels.v[-test.set.labels],G=2,
                 modelNames=c('VVE'))
  
  cluster.mda <- predict(mod, spotify.df[test.set.labels ,])$class
  
  ErrorRateMDA[i] <- 1- mean(as.numeric(predict(mod, spotify.df[test.set.labels ,])$class) == cluster.real[test.set.labels])
  
  #GRAFICO MISCLASSIFIED DI MDA:
  #------------------------------
  
  # PLOT DEI CINQUE TRAINING CON I TEST E GLI ERROR RATE DI OGNUNO COME TITOLO
  a <- which(colnames(spotify.df)==matrice2[1,1])
  b <- which(colnames(spotify.df)==matrice2[1,2])
  coordProj (spotify.df, dimens=c(a,b), what="classification",
             classification=cluster.real,
             col=c("orange", "purple"), symbols=c(17,16),
             sub=c(i))
  # Aggiungere missclassified colorando i punti di nero
  (miss_class<-classError(as.numeric(cluster.mda), class=cluster.real[test.set.labels])$misclassified)
  points(spotify.df[miss_class,c(a,b)],pch=19)
  
}

# Stampo gli error rate e il nome del modello migliore per ogni gruppo di test
for (i in 1:5) {
  cat("Gruppo di test:", i, "\n")
  cat("Error rate:", ErrorRateMDA[i], "\n")
}

#ErrorRate range [0.05, 0.2]
