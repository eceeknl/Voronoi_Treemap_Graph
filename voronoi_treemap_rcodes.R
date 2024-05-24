
# Kütüphanelerin indirilmesi ve çagirilmasi:
#install.packages("WeightedTreemaps")
#install.packages("plotly")
#install.packages("grid")
library(WeightedTreemaps)
library(plotly)
library(grid)
library(lubridate)
library(dplyr)
library(forcats)
library(colorspace)

# Veri setinin yüklenmesi:
suicide_data <- read.csv("master.csv")
head(suicide_data)

# Veri ön islemenin yapilmasi:

#Veri setinde kayip gözlem kontrolünün yaplmasi:
colSums(is.na(suicide_data))

#Yil degiskeninin veri tipinin Date veri tipi yapilmasi:
attach(suicide_data)
suicide_data$year <- ymd(suicide_data$year, truncated = 2L)

#Kullanilacak degiskenler:country,age,sex:

#Yas degiskeninin tekil degerlerinin görüntülenmesi:
unique(suicide_data$age)

#Yas degiskenine karsilik gelen intihar sayilari:
age_intihar <- suicide_data %>%
  group_by(age) %>%
  summarize(toplam_intihar = sum(suicides_no)) %>%
  arrange(desc(toplam_intihar))

#Ülke degiskenine karsilik gelen intihar sayilari:
ulke_intihar <- suicide_data %>%
  group_by(country) %>%
  summarize(toplam_intihar = sum(suicides_no)) %>%
  arrange(desc(toplam_intihar))

#Yil degiskenine karsilik gelen intihar sayilari:
yil_intihar <- suicide_data %>%
  group_by(year) %>%
  summarize(toplam_intihar = sum(suicides_no)) %>%
  arrange(desc(toplam_intihar))

# Grafigin çizdirilmesi:

# Degiskenlerin ayrilmasi ve toplam intihar oranlarinin hesaplanmasi:
df4 <- suicide_data %>%
  filter(year == "1999-01-01") %>%
  filter(country %in% c("Russian Federation", "United States", 
                        "Japan", "France", "Ukraine",
                        "Germany", "Republic of Korea", "Brazil", 
                        "Poland", "United Kingdom")) %>%
  filter(age %in% c("35-54 years", "55-74 years", "25-34 years",
                    "15-24 years","75+ years")) %>%
  filter(sex %in% c("female", "male")) %>%  
  # Sadece kadinlar ve erkekler için filtreleme
  group_by(country, sex, age) %>%
  summarise(total_suicides = sum(suicides.100k.pop), .groups = "drop")

# Ülke kategorilerinde cinsiyetlerin ayrilmasi:
df4 <- df4 %>%
  mutate(sex_within_country = paste(sex, sep = "-"))  
# Cinsiyeti ülkelerin içinde ayirarak yeni bir sütun olusturuyoruz.

# Her bir cinsiyete birer renk atama:
renkler <- c("female" = "#f9e0fc", "male" = "#d8ebff")

# Renkleri veri çerçevesine ekleme:
df4 <- df4 %>%
  mutate(color = renkler[as.character(sex)])

# Voronoi Treemap olusturma
tm <- voronoiTreemap(
  data = df4,
  levels = c("age", "country", "sex_within_country"), 
  # "country" ve "sex_within_country" sütunlarini kullanarak 
  # hücreleri bölelim
  cell_size = "total_suicides",
  shape = "circle",
  error_tol = 0.005,
  maxIteration = 200,
  positioning = "clustered_by_area",
  seed = 1
)

# Treemap çizimi
drawTreemap(
  tm,
  color_palette = renkler,
  color_level = 3,  # Degisken sayisina göre renklendirme seviyesi
  label_level = c(2), # Sadece ülke isimlerini göstermek için
  label_size = 3.3,
  label_color = "black",
  border_color = "white",
  layout = c(1, 1),
  position = c(1, 1)
)

grid.text("1999 Yilinda Yas Gruplarina Göre Ülkelerde Kadin-Erkek Intihar Oranlari",
          x = 0.5, y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))


# Yas gruplarina göre toplam intihar oranlarini hesaplama
age_intihar <- age_intihar %>%
  filter(age != "5-14 years")

# Her bir yas grubuna bir renk atama
renkler <- c("35-54 years"="#ffc6c6", "55-74 years"="#fcebc4", "25-34 years"="#ecfcc4","15-24 years"="#cbfcc4","75+ years"="#d6d6ff")

# Renkleri veri çerçevesine ekleme
df_age <- df_age %>%
  mutate(color = renkler[as.character(age)])

# Voronoi Treemap olusturma
tm_age <- voronoiTreemap(
  data = age_intihar,
  levels = c("age"),
  cell_size = "toplam_intihar",
  shape = "circle",
  error_tol = 0.005,
  maxIteration = 200,
  positioning = "clustered_by_area",
  seed = 1
)

# Treemap çizimi
drawTreemap(
  tm_age,
  color_palette = renkler,
  label_level = c(1),  # Yas grubu etiketlerini göstermek için
  label_size = 2,
  label_color = "black",
  border_color = "white",
  layout = c(1, 1),
  position = c(1, 1)
)

grid.text("1999 Yilinda Yas Gruplarina Göre Intihar Oranlari",
          x = 0.5, y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))



# Ülkeleri, cinsiyetleri ve yas gruplarini ayirma ve toplam intihar oranlarini hesaplama
ulke_intihar <- ulke_intihar %>%
  filter(country %in% c("Russian Federation", "United States", "Japan", "France", "Ukraine",
                        "Germany", "Republic of Korea", "Brazil", "Poland", "United Kingdom"))

# Voronoi Treemap olusturma
tm_country <- voronoiTreemap(
  data = ulke_intihar,
  levels = c("country"), # "country" ve "sex_within_country" sütunlarini kullanarak hücreleri bölelim
  cell_size = "total_suicides",
  shape = "circle",
  error_tol = 0.005,
  maxIteration = 200,
  positioning = "clustered_by_area",
  seed = 1
)

# Her bir ülke grubuna bir renk atama
renkler <- c("Russian Federation"="#ffe4c4", "United States"="#eecbad", "Japan"="#eee0e5", "France"="#e6e6fa", "Ukraine"="#eee8cd",
             "Germany"="#e0eee0", "Republic of Korea"="#ffdead", "Brazil"="#eee5de", "Poland"="#cdb7b5", "United Kingdom"="#cdaf95")

# Renkleri veri çerçevesine ekleme
ulke_intihar <- ulke_intihar %>%
  mutate(color = renkler[as.character(country)])

# Treemap çizimi
drawTreemap(
  tm_country,
  color_palette = renkler,
  #color_type = "cell_size",
  label_level = c(1), # Sadece ülke isimlerini göstermek için
  label_size = 2.5,
  label_color = "black",
  border_color = "white",
  layout = c(1, 1),
  position = c(1, 1)
)

grid.text("1999 Yilinda Ülkelerde Göre Intihar Oranlari",
          x = 0.5, y = 0.98, gp = gpar(fontsize = 14, fontface = "bold"))
