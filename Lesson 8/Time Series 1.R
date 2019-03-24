# оператор %s+%
library(stringi)
# весь tidyverse
library(tidyverse)
# оператор pipe
library(magrittr)

my_path <- "/Users/materov/Git/MLProject/"
# путь в Windows
# my_path <- "C:/Users/ТТТ/Dropbox/СВ/MachineLearning/"

# имя файла
read_file <- "WorkingData/data_2016.csv"
# data_2016 WinEncoding.csv ???

# чтение данных из CSV
df <- read.csv(my_path %s+% read_file)

df     %<>% as_tibble()
df$F6  %<>% as.Date()
df$F11 %<>% as.factor()

# количество пожаров и загораний за каждые сутки
df %>%
  group_by(F6) %>%
  summarise(n_pozh = n())

# график количества пожаров
df %>%
  filter(F201 == 1) %>%
  group_by(F6) %>%
  summarise(n_pozh = n()) %>%
  ggplot(., aes(x = F6, y = n_pozh)) + geom_line() +
  labs(
    x = "\nДата",
    y = "Количество пожаров\n",
    title = "Количество пожаров в Красноярском крае в 2016 году"
  ) + 
  ggridges::theme_ridges()

# график количества пожаров с сглаживанием
data <- 
  df %>%
  filter(F201 == 1) %>%
  group_by(F6) %>%
  summarise(n_pozh = n()) 

data$ma_month <- forecast::ma(data$n_pozh, order = 30)
data$ma_week  <- forecast::ma(data$n_pozh, order = 7)

data %>%
  ggplot(.) + 
  geom_line(aes(x = F6, y = n_pozh, 
                colour = "Временной ряд"), size = 0.8)   +
  geom_line(aes(x = F6, y = ma_month,
                colour = "Месячное сглаживание"), size = 0.8) +
  geom_line(aes(x = F6, y = ma_week,
                colour = "Недельное сглаживание"), size = 0.8) +
  labs(
    x = "\nДата",
    y = "Количество пожаров\n",
    title = "Количество пожаров в Красноярском крае в 2016 году",
    color = ""
  ) + 
  scale_color_manual(values = c("grey80", "#E69F00", "#56B4E9")) +
  ggridges::theme_ridges() +
  theme(legend.position = "top")

# панелирование
plot <-
df %>%
  filter(F201 == 0) %>%
  group_by(F6, F11) %>%
  summarise(n_pozh = n()) %>%
  filter(n_pozh > 5) %>%
  rename("Дата" = F6, "Количество" = n_pozh, Hac = F11) %>%
  ggplot(., aes(x = Дата, y = Количество, color = Hac)) + 
  geom_point(size = 2, alpha = 0.8) +
  labs(
    x = "\nДата",
    y = "Количество загораний\n",
    title = "Количество загораний в Красноярском крае в 2016 году",
    color = ""
  ) + 
  #ggridges::theme_ridges() +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  theme(legend.position = "top") +
  facet_wrap(Hac~.)

plotly::ggplotly(plot)

###############
# временной ряд
###############
library(xts)

fire_ts <-
  xts::xts(x = data$n_pozh, order.by = data$F6)

# начало ряда
fire_ts %>%
  head()
# конец ряда
fire_ts %>%
  tail()

# график
plot(fire_ts, main = "Временной ряд")
# интерактивный график
highcharter::hchart(fire_ts)

# преобразование Бокса-Кокса
library(forecast)
(lambda <- BoxCox.lambda(fire_ts))
# вывод преобразованного ряда
plot(BoxCox(fire_ts, lambda))

library(astsa)
lag1.plot(fire_ts, 4)

library(tseries)
adf.test(fire_ts, alternative = "stationary")
