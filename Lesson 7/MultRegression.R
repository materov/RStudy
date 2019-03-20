# загрузка библиотек
library(readxl)
library(magrittr)
library(tidyverse)
# тема
library(ggridges)
# цвета темы
library(ggsci)

#library(ggpubr)
#library(ggthemes)
#library(car)

# Время ликвидации открытого горения
# определение
#      |
#      V
# http://wiki-fire.org/%D0%92%D1%80%D0%B5%D0%BC%D1%8F%20%D0%BB%D0%B8%D0%BA%D0%B2%D0%B8%D0%B4%D0%B0%D1%86%D0%B8%D0%B8%20%D0%BE%D1%82%D0%BA%D1%80%D1%8B%D1%82%D0%BE%D0%B3%D0%BE%20%D0%B3%D0%BE%D1%80%D0%B5%D0%BD%D0%B8%D1%8F.ashx

#data_regr <- read_xlsx("/Users/materov/Dropbox/RGit/RStudy/Lesson 7/MultRegression.xlsx")
data_regr <- read_xlsx("C:/Users/ТТТ/Dropbox/RGit/RStudy/Lesson 7/MultRegression.xlsx")

data_regr                %<>% as_tibble()
data_regr$T_тушения      %<>% as.numeric()
data_regr$T_локализации  %<>% as.numeric()
data_regr$T_ликв_горения %<>% as.numeric()
data_regr$ФО             %<>% as_factor()
data_regr$субъект        %<>% as_factor()

str(data_regr)
glimpse(data_regr)
skimr::skim(data_regr)

# гистограммы
data_regr %>% 
  ggplot(., aes(x = T_тушения)) + geom_histogram()

data_regr %>% 
  ggplot(., aes(x = T_локализации)) + geom_histogram()

data_regr %>% 
  ggplot(., aes(x = T_ликв_горения)) + geom_histogram()

# 3D-модель
library(scatterplot3d)
scatterplot3d(data_regr[,1:3],
                type = "h", color = "blue",
                angle=55, pch = 16)

# корреляции
cor(data_regr[,1:3])

# парные графики
data_regr[,1:3] %>%
  GGally::ggpairs()

# парные графики
data_regr[,1:4] %>%
  ggpairs(aes(color = ФО,
              alpha = 0.6)) 

model = lm(data = data_regr, T_тушения ~ T_локализации + T_ликв_горения)


library(broom)
tidy(model) 

GGally::ggnostic(model)

sqrt(car::vif(model))

gvmodel <- gvlma::gvlma(model)

summary(gvmodel)
