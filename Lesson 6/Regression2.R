# загрузка библиотек
library(readxl)
library(ggpubr)
library(magrittr)
library(tidyverse)
library(ggridges)
library(ggthemes)
library(car)
library(ggsci)

#data_regr <- read_xlsx("/Users/materov/Git/book/Data/Regression.xlsx")
data_regr <- read_xlsx("C:/Users/ТТТ/Dropbox/СВ/Пособие Мат методы (адъюнктура)/Data/Regression.xlsx")

data_regr            %<>% as.tibble()
data_regr$ликвидация %<>% as.numeric()
data_regr$тушение    %<>% as.numeric()

# гистограммы
data_regr %>% 
  ggplot(., aes(x = ликвидация)) + geom_histogram()

data_regr %>% 
  ggplot(., aes(x = тушение)) + geom_histogram()

# тесты на нормальность распределения
shapiro.test(data_regr$ликвидация)
shapiro.test(data_regr$тушение)

# cor.test(data_regr$ликвидация, data_regr$тушение, method = "spearman")
cor.test(data_regr$ликвидация, data_regr$тушение, method = "kendall")

# график
data_regr %>%
  ggplot(aes(ликвидация, тушение)) + 
  geom_density2d(aes(color = ..level.., alpha = ..level..)) + 
  geom_rug(color = "gray70", sides = "tr") + # geom_smooth(method = "lm", se = F) +
  scale_color_gradient(low = "green", high = "red") +
  geom_point() +  
  labs(title    = "Данные по временам оперативного реагирования на пожары, \nпроизошедшие в городской местности",
       subtitle = "в субъектах РФ в целом за 12 мес. 2016 г.",
       caption  = "\nпо данным ФГБУ ВНИИПО МЧС России"
  ) + xlab("\nСреднее время ликвидации открытого горения, мин.") + 
  ylab("Среднее время тушения, мин.\n") +
  guides(color="none", alpha="none") +
  theme_ridges()


# боксплоты
data_regr %>%
  ggplot(aes(ликвидация, тушение)) + geom_boxplot(aes(fill = I("#00a8e8"), 
                                                      group = cut_width(ликвидация, 2.5))) + 
  geom_point(alpha = 0.3) +
  labs(title    = "Данные по временам оперативного реагирования на пожары, \nпроизошедшие в городской местности",
       subtitle = "в субъектах РФ в целом за 12 мес. 2016 г.",
       caption  = "\nисточник: ФГБУ ВНИИПО МЧС России"
  ) + xlab("\nСреднее время ликвидации открытого горения, мин.") + 
  ylab("Среднее время тушения, мин.\n") + 
  theme_ridges() 

# регрессионная прямая
data_regr %>%
  ggplot(aes(ликвидация, тушение)) + geom_point() + geom_smooth(method = "lm", se = T) + 
  #geom_density2d() +
  labs(title    = "Данные по временам оперативного реагирования на пожары, \nпроизошедшие в городской местности",
       subtitle = "в субъектах РФ в целом за 12 мес. 2016 г.",
       caption  = "\nпо данным ФГБУ ВНИИПО МЧС России"
  ) + xlab("\nСреднее время ликвидации открытого горения, мин.") + 
  ylab("Среднее время тушения, мин.\n") +
  theme_ridges()

# модель
FireModel <- data_regr %>% lm(formula = тушение~ликвидация)

# информация по модели
summary(FireModel)

library(broom)

tidy(FireModel)

coefficients(FireModel)
confint(FireModel)
residuals(FireModel)
anova(FireModel)
vcov(FireModel)
AIC(FireModel)
BIC(FireModel)

# прогноз
predict(FireModel)
newdata = data.frame( ликвидация=20 )

# отображение всех стандартных графиков
plot(FireModel)

abline(FireModel)

# выбросы
outlierTest(FireModel)

# QQ-график для остатков
qqPlot(resid(FireModel), main="Сравнение квантилей ЭР и НР", 
       xlab = "Квантили нормального распределения", 
       ylab = "Наблюдаемые квантили")

# тест на нормальность для остатков
shapiro.test(resid(FireModel))

# точки напряженности
leveragePlots(FireModel)

# точки максимального влияния
influencePlot(FireModel)
spreadLevelPlot(FireModel)

library(GGally)

ggpairs(data_regr)
ggpairs(FireModel)
