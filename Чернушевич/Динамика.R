library(tidyverse)
library(readxl)
library(magrittr)
library(gghighlight)
library(ggpubr)

data <- read_xls("C:/Users/ТТТ/Dropbox/RGit/RStudy/Чернушевич/Динамика.xls")
data <- read_xls("/Users/materov/Dropbox/RGit/RStudy/Чернушевич/Динамика.xls")

data$Год      %<>% as.factor()
data$Значение %<>% as.integer()
data$Причина  %<>% as.factor()

# здесь указывается путь к файлам
setwd("C:/Users/ТТТ/Dropbox/RGit/RStudy/Чернушевич/")
setwd("/Users/materov/Dropbox/RGit/RStudy/Чернушевич/")

cairo_pdf("Highlight_no.pdf", width = 12, height = 6)

data %>%
  group_by(Год) %>%
  ggplot(.,aes(x = Год, y = Значение, fill = Причина)) + 
  geom_bar(stat="identity") + 
  # подсветка серым
  #gghighlight() +
  facet_grid(. ~Причина) +
  labs(title="Распределение количества погибших по основным причинам гибели", 
       subtitle = "Динамика с 2014 г. по 2017 г.",
       caption = "по данным ФГБУ ВНИИПО МЧС России") +
  xlab("\nГод") + ylab("Значение\n") +
  ggsci::scale_fill_d3() +
  #ggpubr::theme_pubclean() +
  #ggthemes::theme_fivethirtyeight()
  ggridges::theme_ridges() +
  theme(legend.position = "none") +
  NULL

dev.off()

#--------------------------------------------
# другой вариант
# сделаем все графики по отдельности
# и соберем вместе
# тогда каждый график будет в своем масштабе
#--------------------------------------------

# первый график
graph_1 <-
  data %>%
  filter(Причина == "Воздействие высокой температуры") %>%
  group_by(Год) %>%
  ggplot(.,aes(x = Год, y = Значение, fill = Причина)) + 
  geom_bar(stat="identity") + 
  facet_grid(. ~Причина) +
  xlab("\nГод") + 
  ylab("Значение\n") +
  ggsci::scale_fill_d3() +
  ggpubr::theme_pubclean() +
  theme(legend.position = "none") +
  NULL

# второй график
graph_2 <-
  data %>%
  filter(Причина == "Отравление токсичными продуктами") %>%
  group_by(Год) %>%
  ggplot(.,aes(x = Год, y = Значение, fill = Причина)) + 
  geom_bar(stat="identity") + 
  facet_grid(. ~Причина) +
  xlab("\nГод") + 
  ylab("Значение\n") +
  ggsci::scale_fill_nejm() +
  ggpubr::theme_pubclean() +
  theme(legend.position = "none") +
  NULL

# третий график
graph_3 <-
  data %>%
  filter(Причина == "Пониженная концентрация кислорода") %>%
  group_by(Год) %>%
  ggplot(.,aes(x = Год, y = Значение, fill = Причина)) + 
  geom_bar(stat="identity") + 
  facet_grid(. ~Причина) +
  xlab("\nГод") + 
  ylab("Значение\n") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "none") +
  NULL

#  собираем все вместе
my_graph <- ggarrange(graph_1, graph_2, graph_3, 
                      ncol = 3, nrow = 1) 

cairo_pdf("Graph.pdf", width = 14, height = 6)

# аннотируем
annotate_figure(my_graph,
                top = text_grob("Распределение количества погибших по основным причинам гибели\n", face = "bold"),
                bottom = text_grob("\nисточник данных: ФГБУ ВНИИПО МЧС России ", 
                                   hjust = 1, x = 1, face = "italic", size = 11)
)

dev.off()
