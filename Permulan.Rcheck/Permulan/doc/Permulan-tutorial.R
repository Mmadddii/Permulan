## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Permulan)

## -----------------------------------------------------------------------------
a <- Atributua("adina", c(21, 35, 50))
a
datuak <- data.frame(adina = c(21, 35, 50), sexua = c("M", "F", "M"))
dm <- DatuMultzoa(datuak, klase_izena = "sexua")
dm


## -----------------------------------------------------------------------------
#data.frame motako objektuentzat:
df <- data.frame(
  adina = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  soldata = c(1200, 1500, 1600, 2000, 2300, 2600, 2800, 3100, 3500, 3900),  
  altuera = c(1.55, 1.62, 1.68, 1.70, 1.75, 1.78, 1.79, 1.80, 1.82, 1.83),  
  pisua = c(70, 68, 72, 74, 78, 79, 85, 88, 90, 92),                        
  zoriontasuna = c(8, 7, 6, 5, 5, 4, 3, 3, 2, 2))

normalize(df)
standardize(df)


## -----------------------------------------------------------------------------
x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
discretizeEW(x, 5)

## -----------------------------------------------------------------------------
a <- Atributua("adina", x)
discretizeEW(a, 5)


## -----------------------------------------------------------------------------
discretizeEF(x, 3)
discretizeEF(a, 3)


## -----------------------------------------------------------------------------
cut.points <- c(5.5, 10.5, 15.5)
discretize(x, cut.points)

#DatuMultzoa objektuarekin:
dm_disk <- discretize(dm, cut.points)
dm_disk
lapply(dm_disk@atributuak, function(a) list(izena = a@izena, mota = a@mota, balioak = head(a@balioak)))


## -----------------------------------------------------------------------------
df <- data.frame(
  adina = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  soldata = c(1200, 1500, 1600, 2000, 2300, 2600, 2800, 3100, 3500, 3900),  
  altuera = c(1.55, 1.62, 1.68, 1.70, 1.75, 1.78, 1.79, 1.80, 1.82, 1.83),  
  pisua = c(70, 68, 72, 74, 78, 79, 85, 88, 90, 92),                        
  zoriontasuna = c(8, 7, 6, 5, 5, 4, 3, 3, 2, 2))

kor <- korrelazio_matrizea(df)
print(kor)


## -----------------------------------------------------------------------------
Korrelazioak_irudikatu(df)


## -----------------------------------------------------------------------------
f <- factor(c("A", "A", "B", "B", "C"))
entropy(f)



## -----------------------------------------------------------------------------
b <- Atributua("sexua", factor(c("M", "F", "M", "F", "F")))
entropy(b)


## -----------------------------------------------------------------------------
df <- data.frame(
  adina = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  soldata = c(1200, 1500, 1600, 2000, 2300, 2600, 2800, 3100, 3500, 3900),  
  altuera = c(1.55, 1.62, 1.68, 1.70, 1.75, 1.78, 1.79, 1.80, 1.82, 1.83),  
  pisua = c(70, 68, 72, 74, 78, 79, 85, 88, 90, 92),                        
  zoriontasuna = c(8, 7, 6, 5, 5, 4, 3, 3, 2, 2))
bariantza<-var_col(df)
print(bariantza)


## -----------------------------------------------------------------------------

df_test <- data.frame(
  balioa = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.2, 0.6, 0.5, 0.7, 0.3),
  etiketa = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

emaitza <- roc_analisi(df_test)
emaitza

## -----------------------------------------------------------------------------

Visualizar_ROC(emaitza)

## -----------------------------------------------------------------------------
df1 <- data.frame(
  konstantea = rep(5, 10),         
  aldakorra = 1:10,                
  txikia = c(1,1,2,2,1,2,1,2,1,2)  
)
var_col(df1)
# Bariantza >= 1 bakarrik duten aldagaiak gorde
filter_metrics(df1, var_min = 1)


## -----------------------------------------------------------------------------
df_ent <- data.frame(
  kol1 = factor(c("A", "A", "A", "A", "A")),   # Entropia = 0 (balio bakarra)
  kol2 = factor(c("A", "B", "A", "B", "A")),   # Entropia ertaina
  kol3 = factor(c("X", "Y", "Z", "X", "Y")))

entropy_by_column(df_ent)

# Aplikatu filtraketa entropiaren arabera (adib. entropy_min = 0.8)
df_ent_filtratua <- filter_metrics(df_ent, entropy_min = 0.8)
df_ent_filtratua


df_auc <- data.frame(
aldagaia = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.2, 0.6, 0.5, 0.7, 0.3),
etiketa  = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

# ROC kurba eta AUC kalkulatu

roc_ema <- roc_analisi(df_auc)
roc_ema$AUC

# Filtratu AUC minimoarekin

df_auc_filtratua <- filter_metrics(df_auc, auc_min = 0.6, klase = df_auc$etiketa)
df_auc_filtratua


