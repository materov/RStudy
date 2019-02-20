# установим корреляции между двумя векторами
x <- c(28, 25, 33, 46, 32, 24, 32, 24, 36, 32)
y <- c(34, 28, 38, 48, 36, 27, 28, 29, 31, 37)

# коэффициент корреляции
cor(x, y)

# статистическая проверка наличия корреляционной связи
cor.test(x, y)

library(readxl)
library(magrittr)

dist <- readxl::read_xls("C:/Users/ТТТ/Dropbox/RGit/RStudy/Lesson 5/table.russia.xls")
dist %<>% as.data.frame()
row.names(dist) <- c("Абакан",	"Ачинск",	"Барнаул",	"Владивосток",	"Екатеринбург",	"Казань",	"Кемерово",	"Красноярск",	"Москва",	"Новосибирск",	"Томск")
colnames(dist) <- c("Абакан",	"Ачинск",	"Барнаул",	"Владивосток",	"Екатеринбург",	"Казань",	"Кемерово",	"Красноярск",	"Москва",	"Новосибирск",	"Томск")

# библиотека superheat
library(superheat)

# тепловая карта
superheat(dist)
# добавление дендрограмы
superheat(dist, row.dendrogram = TRUE)

# библиотека ggcorrplot
library(ggcorrplot)

# матрица корреляций
corr <- round(cor(dist), 1)
corr

# диаграмма корреляций
ggcorrplot(corr)
# использование иерархической кластеризации
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# отображение значений корреляций
ggcorrplot(corr, type = "lower",
           lab = TRUE)
# p-значения матрицы корреляций (статистическая значимость)
ggcorrplot(corr, type = "lower", p.mat = p.mat <- cor_pmat(dist))

# библиотека corrplot
library(corrplot)
# эллипсы
corrplot(corr, method = "ellipse")
# значения коэффциентов корреляции
corrplot(corr, method = "number") 
# смешанный вариант
corrplot.mixed(corr, lower.col = "black", number.cex = .7)
# три кластера
corrplot(corr, order = "hclust", addrect = 3)

# библиотека GGally
library(GGally)
ggpairs(dist)