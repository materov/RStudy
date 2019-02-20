library(tidyverse)
library(readxl)
library(magrittr)
library(gghighlight)

data <- read_xls("C:/Users/ТТТ/Dropbox/RGit/RStudy/Чернушевич/Динамика.xls")

data$Год      %<>% as.factor()
data$Значение %<>% as.integer()
data$Причина  %<>% as.factor()

# data %>%
#   group_by(Год) %>%
#   mutate(per = 100*Значение/sum(Значение)) %>%
#   ggplot(.,aes(x = Год, y = per)) + 
#   geom_bar(stat="identity") + 
#   facet_grid(. ~Причина)

setwd("C:/Users/ТТТ/Dropbox/RGit/RStudy/Чернушевич/")

cairo_pdf("Highlight_no.pdf", width = 12, height = 6)

data %>%
  group_by(Год) %>%
  ggplot(.,aes(x = Год, y = Значение, fill = Причина)) + 
  geom_bar(stat="identity") + 
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
