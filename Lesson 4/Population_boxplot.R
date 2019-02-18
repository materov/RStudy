# загрузка необходимых библиотек
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# здесь указывается путь к Excel-файлу
setwd("C:/Users/ТТТ/Dropbox/RGit/RStudy/Lesson 4")

population <- read_xlsx("Population.xlsx")

# обзор таблицы данных
population %>%
  select(Население, Городское, Сельское, ФО) %>%
  glimpse()

# преобразование таблицы данных
population %>%
  select(Население, Городское, Сельское, ФО) %>%
  gather(Городское, Сельское, 
                  key = "Тип",
                  value = "Значение") %>%
  glimpse()

# диаграммы размаха с разделением на типы населения
population %>%
  select(Население, Городское, Сельское, ФО) %>%
  gather(Городское, Сельское,
                    key = "Тип",
                    value = "Значение") %>%
  ggplot(aes(x = reorder(ФО, Население, FUN = sum),
             y = Значение,
             fill = Тип)) +
  geom_boxplot() + coord_flip() +
  ggsci::scale_fill_d3() + 
  ggridges::theme_ridges() +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = seq(100000, 100000000,
                                  by = 3000000)) +
  xlab("Федеральный округ") +
  ylab("Численность населения")
