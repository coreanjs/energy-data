library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(showtext)
library(extrafont)
library(ggtext)

showtext_auto()

font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")



#R 버전 알려주는 코드 
R.version

setwd("C:/R/Rproject/Energy&Data/")

price_raw <- read_excel("230821_생활물가지수.xlsx", sheet ="Sheet1")


unique(price_raw$type)


price_tidy<- price_raw %>% 
  pivot_longer(-type, values_to ="value", names_to = "date") %>% 
  mutate(date = as.Date(zoo::as.yearmon(date, "%Y.%m")),
         color_label = case_when(type =='경유'~ type,
                                 type =="도시가스" ~ type,
                                 TRUE ~ '기타'))


price_tidy

range(price_tidy$date)


price_tidy_img <- price_tidy %>% 
  #ggplot(aes(x = date, y = value, group = type, color = color_label))+
  ggplot() +
  geom_line(data =. %>% filter(!type %in% c(
    #"도시가스", 
    "경유", 
    "휘발유"
    #"전기료"
    )), 
            aes(x = date, y = value, group = type), size = 1.2,  color ="gray")+
  geom_line(data =. %>% filter(type =="경유"), aes(x = date, y = value), size = 1.5,  color ="red")+
  #geom_line(data =. %>% filter(type =="도시가스"), aes(x = date, y = value), size = 1.5,  color ="blue")+
  geom_line(data =. %>% filter(type =="휘발유"), aes(x = date, y = value), size = 1.5,  color ="gold")+
  #geom_line(data =. %>% filter(type =="전기료"), aes(x = date, y = value), size = 1.5,  color ="brown")+
  scale_x_date(date_breaks = "1 year", date_labels =paste0("'", "%y"))+
    theme_bw()+
  theme_minimal()+
  theme(plot.title = element_text(size = 24, family="nanumgothic"),
        plot.subtitle =element_markdown(),
        plot.background =element_rect(fill="white"),
        panel.grid.minor = element_blank())+
  labs(title ="통계청 생활물가지수를 통해서 보는 에너지 물가",
       subtitle ="물가지수에서 <span style = 'color:red;'>경유</span>와 <span style = 'color:gold;'>휘발유</span>만 표기",
       caption = "Source: 통계청, Graphic: Jiseok")

price_tidy_img

ggsave(plot = price_tidy_img, "경유와 휘발유.png",  width= 630, height = 700, units ="px", dpi = 100)



library(gghighlight)


price_tidy %>% 
  #ggplot(aes(x = date, y = value, group = type, color = color_label))+
  ggplot(aes(x = date, y = value, group = type, color =type)) +
  geom_line(linewidth = 1.2)+
  gghighlight(type %in% c(
    #"도시가스", 
    #"경유", 
    #"휘발유", 
    "전기료"))+
  scale_x_date(date_breaks = "1 year", date_labels =paste0("'", "%y"))+
  theme_bw()+
  theme_minimal()+
  theme(plot.title = element_text(size = 24, family="nanumgothic"),
        plot.background =element_rect(fill="white"),
        panel.grid.minor = element_blank())+
  labs(title ="물가지수에서 경유와 도시가스만 표기",
       subtitle ="물가지수에서 에너지가 차지하는 정도를 보기 위해 만들었다",
       caption = "Source: 통계청, Graphic: Jiseok")
