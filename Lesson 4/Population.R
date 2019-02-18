# загрузка необходимых библиотек
library(readxl)
library(ggplot2)
library(dplyr)

# здесь указывается путь к Excel-файлу
setwd("C:/Users/ТТТ/Dropbox/RGit/RStudy/Lesson 4")

population <- read_xlsx("Population.xlsx")

# структура данных
str(population)

# первоначальная установка - пустой график
population %>%
  ggplot(aes(x = Площадь, y = Население))

# точками изображены области
population %>%
  ggplot(aes(x = Площадь, y = Население)) + geom_point()

# добавление плотности населения на график
# размер точек соответствует плотности населения
population %>%
  mutate(Плотность = Население/Площадь) %>%
  ggplot(aes(x = Площадь,
             y = Население,
             size = Плотность)) +
  geom_point()

# добавление цвета = ФО
# добавление шага масштаба
population %>%
  mutate(Плотность = Сельское/Площадь) %>%
  ggplot(aes(x = Площадь, y = Сельское)) +
  geom_point(aes(color = ФО, size = Плотность)) +
  scale_x_continuous(breaks = seq(50000, 100000000,
                                  by = 500000)) +
  scale_y_continuous(breaks = seq(10000, 100000000,
                                  by = 500000)) 

# сохранение графика
graph <- 
population %>%
  mutate(Плотность = Сельское/Площадь) %>%
  ggplot(aes(x = Площадь, y = Сельское)) +
  geom_point(aes(color = ФО, size = Плотность)) +
  scale_x_continuous(breaks = seq(50000, 100000000,
                                  by = 500000)) +
  scale_y_continuous(breaks = seq(10000, 100000000,
                                  by = 500000))

# добавление цветов, темы и заголовка
graph + 
  ggsci::scale_color_nejm() +
  ggridges::theme_ridges() +
  ggtitle("График численности сельского населения")
