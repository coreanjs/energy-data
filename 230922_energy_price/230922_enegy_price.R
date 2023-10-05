library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(showtext)
library(extrafont)
library(ggtext)
library(gghighlight)

showtext_auto()
font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')
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
  geom_line(data =. %>% filter(type =="경유"), aes(x = date, y = value), size = 1.5,  color ="#04cf98")+
  #geom_line(data =. %>% filter(type =="도시가스"), aes(x = date, y = value), size = 1.5,  color ="blue")+
  geom_line(data =. %>% filter(type =="휘발유"), aes(x = date, y = value), size = 1.5,  color ="#f7c93a")+
  #geom_line(data =. %>% filter(type =="전기료"), aes(x = date, y = value), size = 1.5,  color ="brown")+
  scale_x_date(date_breaks = "1 year", date_labels =paste0("'", "%y"))+
    theme_bw()+
  scale_y_continuous(limits = c(0, 200))+
  theme_minimal()+
  theme(plot.title = element_text(size = 24, family="nanumgothic"),
        plot.subtitle =element_markdown(size = 20),
        plot.background =element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x =element_blank())+
  labs(y = "물가지수",
       x ='연도',
       title ="통계청 생활물가지수를 통해서 보는 에너지 물가",
       subtitle ="물가지수에서 <span style = 'color:#04cf98;'>경유</span>와 <span style = 'color:#f7c93a;'>휘발유</span>만 표기",
       caption = "Source: 통계청, Graphic: Jiseok")

price_tidy_img

ggsave(plot = price_tidy_img, "경유와 휘발유.png",  width= 630, height = 700, units ="px", dpi = 100)


price_tidy %>% 
  #ggplot(aes(x = date, y = value, group = type, color = color_label))+
  ggplot() +
  geom_line(data =. %>% filter(!type %in% c(
    #"도시가스", 
    #"경유", 
    #"휘발유"
    "전기료"
  )), 
  aes(x = date, y = value, group = type), size = 1.5,  color ="gray")+
  geom_line(data =. %>% filter(type =="전기료"), aes(x = date, y = value), size = 2,  color ="#f7c93a")+
  #geom_line(data =. %>% filter(type =="전기료"), aes(x = date, y = value), size = 1.5,  color ="brown")+
  scale_x_date(date_breaks = "1 year", date_labels =paste0("'", "%y"))+
  theme_bw()+
  scale_y_continuous(limits = c(0, 200))+
  theme_minimal()+
  theme(plot.title = element_text(size = 24, family="nanumgothic"),
        plot.subtitle =element_markdown(size = 20),
        plot.background =element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x =element_blank())+
  labs(y = "물가지수",
       x ='연도',
       title ="통계청 생활물가지수를 통해서 보는 에너지 물가",
       subtitle ="물가지수에서 <span style = 'color:#f7c93a;'>전기료</span>만 표기",
       caption = "Source: 통계청, Graphic: Jiseok")

ggsave("전기요금.png",  width= 630, height = 700, units ="px", dpi = 100)

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
        panel.grid.minor = element_blank(),
        panel.grid.major.x =element_blank())+
  labs(title ="물가지수에서 경유와 도시가스만 표기",
       subtitle ="물가지수에서 에너지가 차지하는 정도를 보기 위해 만들었다",
       caption = "Source: 통계청, Graphic: Jiseok")



price_tidy %>% 
  #ggplot(aes(x = date, y = value, group = type, color = color_label))+
  ggplot(aes(x = date, y = value, group = type)) +
  geom_line(linewidth = 1.2, color ='#f4481a')+
  gghighlight()+
  scale_x_date(date_breaks = "1 year", date_labels =paste0("'", "%y"))+
  theme_bw()+
  theme_minimal()+
  facet_wrap(~type)+
  theme(plot.title = element_text(size = 24, family="nanumgothic"),
        plot.background =element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x =element_blank())+
  labs(title ="물가지수",
       subtitle ="물가지수에서 에너지가 차지하는 정도를 보기 위해 만들었다",
       caption = "Source: 통계청, Graphic: Jiseok")












############ 새로운 파일 230922



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


setwd("C:/R/Rproject/Energy&Data/230922_energy_price")


price_fig1 <- read_excel("230922_소비자물가지수.xlsx", sheet ="대분류별", skip=18) %>% 
  na.omit() %>% 
  pivot_longer(-type, names_to = "date", values_to ="index") %>% 
  mutate(type = as.factor(type),
         date = ym(date),
         type_label = if_else(date =="2023-08-01", type , NA_character_))


range(price_fig1$date)


unique(price_fig1$type)
## 첫번째 그래프 
##총지수, 석유류, 전기/가스수도, 화장품, 농축수산물 강조



price_fig1 %>% 
  ggplot(aes(x = date, y = index, group = type, color = type))+
  geom_line(color = "gray80")+
  geom_line(data =. %>% filter(type =="총지수"), color ="#ec111a", size = 1.5)+
  geom_line(data =. %>% filter(type %in% c('석유류')), color ="gold", size = 1.5)+
  geom_line(data =. %>% filter(type %in% c('전기 · 가스 · 수도')), color ="#1f5c99", size = 1.5)+
  scale_y_continuous(limits = c(80, 160))+
  #gghighlight(type %in% c('총지수', '석유류', '전기 · 가스 · 수도', '화장품', '농축수산물'))+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "대한민국 17개 광역지자체별 전력자립도 변화<br> Changes in electricity dependency in South Korea by regions",
       subtitle ="전력자립도는 전략 발전량을 소비량으로 나눈 백분율로, <br> <span style='color:#142f38'>**2011년**</span>부터 <span style ='color:#4ea267'>2020년</span>까지 대한민국 광역지자체별 전력자립도의 변화를 나타내었음",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")



str(price_fig1)



### ver for 이화랑

library(ggrepel)
price_fig1 %>% 
  ggplot(aes(x = date, y = index, group = type, color = type))+
  geom_line(linewidth = .5, alpha = .3)+
  geom_line(data =. %>% filter(type %in% c("총지수", "석유류")), size = 1.7)+
  scale_y_continuous(limits = c(40, 160))+
  #scale_color_brewer(palette ="Paired")+
  scale_x_date(limits = as.Date(c('2017-12-01', 
                                 '2024-06-01'), format ="%Y"))+
  geom_text_repel(aes(color = type, label = type_label), 
                  family = 'Nanum Myeongjo',
                  size = 4,
                  direction = "y",
                  xlim = c(2020.8, NA),
                  hjust = -.3,
                  segment.size = .5,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20)+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "물가지수",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")


setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230922_energy_price/")
ggsave("energy_price_fig1.png",  width= 800, height = 800, units ="px", dpi = 100)




# facet wrap


max_date<- range(price_fig1$date)[2]

type_level<-price_fig1 %>% 
  filter(date == max_date) %>% 
  arrange(desc(index)) %>% pull(type) %>% as.character
  

type_level



range(price_fig1$date)

price_fig1 %>% 
  ggplot(aes(x = date, y = index, group = type))+
  geom_line(linewidth = 1)+
  scale_y_continuous(limits = c(35, 165), breaks = c(40, 100, 160))+
  gghighlight(use_direct_label = FALSE)+
  scale_x_date(limits = as.Date(c('2017-12-01', 
                                  '2023-08-01'), format ="%Y"))+
  geom_vline(xintercept = as.Date(c('2020-01-01', '2022-01-02')), linetype="dotted")+

  facet_wrap(~fct_relevel(type, type_level), nrow =4)+
  geom_line(data =. %>% filter(type %in% c("총지수")), aes(x = date, y = index), color ="#1f5c99", size = 1.5)+
  geom_line(data =. %>% filter(type %in% c("석유류")), aes(x = date, y = index), color ="brown", size = 1.5)+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "물가지수",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")




setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230922_energy_price/")
ggsave("energy_price_fig1_ver2.png",  width=1000, height =800, units ="px", dpi = 100)



## 총지수, 석유류, 등유 강조


setwd("C:/R/Rproject/Energy&Data/230922_energy_price")
price_fig2 <- read_excel("230922_소비자물가지수.xlsx", sheet ="석유류", skip=12) %>% 
  na.omit() %>% 
  pivot_longer(-type, names_to = "date", values_to ="index") %>% 
  mutate(type = as.factor(type),
         date = ym(date))


price_fig2 %>% 
  ggplot(aes(x = date, y = index, group = type, color = type))+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 200))+
  scale_color_manual(values = c("burlywood4","firebrick2", "#1f5c99"))+
  
  #geom_line(data =. %>% filter(type =="총지수"), color ="#ec111a", size = 1.5)+
  #geom_line(data =. %>% filter(type %in% c('전기 · 가스 · 수도')), color ="#1f5c99", size = 1.5)+
  #scale_y_continuous(limits = c(80, 160))+
  gghighlight(
    type %in% c('석유류','등유')
              )+
  
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "물가지수",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")











max_date<- range(price_fig2$date)[2]

fig2_level<-price_fig2 %>% 
  filter(date == max_date) %>% 
  arrange(desc(index)) %>% pull(type) %>% as.character


total_fig2<-price_fig2 %>% 
  filter(type =='총지수')


price_fig2 %>% 
  ggplot(aes(x = date, y = index, group = type))+
  geom_line(data =. %>% filter(!type  %in% c("총지수", '석유류')), linewidth = 1, color =  "brown")+
  scale_y_continuous(limits = c(0, 200), breaks = c(40, 100, 160))+
  gghighlight(use_direct_label = FALSE)+
  scale_x_date(limits = as.Date(c('2017-12-01', 
                                  '2023-08-01'), format ="%Y"))+
  geom_vline(xintercept = as.Date(c('2020-01-01', '2022-01-02')), linetype="dotted")+
  
  facet_wrap(~fct_relevel(type, fig2_level), nrow =4)+
  #geom_line(data =total_fig2, aes(x = date, y = index), color ="#1f5c99", size = 1.5)+
  #geom_line(data =. %>% filter(type %in% c("석유류")), aes(x = date, y = index), color ="brown", size = 1.5)+
  #geom_line(data =. %>% filter(type %in% c("등유")), aes(x = date, y = index), color ="brown", size = 1.5)+
    theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "석유류 물가지수",
       subtitle ="석유류 물가지수는 등유, 부탄가스, 경유, 휘발유, 취사용LPG, 자동차용LPG로 구성됨",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")



xl    = c( 4,  1)
yl    = c( 1,  4)
type =rep(LETTERS[1:6], each=60)

type
line2 = data.frame(x=xl,y=yl,type)


line2

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230922_energy_price/")
ggsave("energy_price_fig2.png",  width= 600, height = 800, units ="px", dpi = 100)






## 총지수, 전기가스수도


setwd("C:/R/Rproject/Energy&Data/230922_energy_price")
price_fig3 <- read_excel("230922_소비자물가지수.xlsx", sheet ="전기가스수도", skip=10) %>% 
  na.omit() %>% 
  pivot_longer(-type, names_to = "date", values_to ="index") %>% 
  mutate(type = as.factor(type),
         date = ym(date))


unique(price_fig3$type)

price_fig3 %>% 
  ggplot(aes(x = date, y = index, group = type, color = type))+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 200))+
  
  #geom_line(data =. %>% filter(type =="총지수"), color ="#ec111a", size = 1.5)+
  #geom_line(data =. %>% filter(type %in% c('전기 · 가스 · 수도')), color ="#1f5c99", size = 1.5)+
  #scale_y_continuous(limits = c(80, 160))+
  gghighlight(type %in% c('총지수', '전기 · 가스 · 수도'))+
  scale_color_manual(values = c("firebrick2", "#1f5c99"))+
  
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "물가지수",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")









max_date<- range(price_fig3$date)[2]

fig3_level<-price_fig3 %>% 
  filter(date == max_date) %>% 
  arrange(desc(index)) %>% pull(type) %>% as.character

fig3_level

price_fig3 %>% 
  ggplot(aes(x = date, y = index, group = type))+
  geom_line(linewidth = 1)+
  scale_y_continuous(limits = c(0, 200), breaks = c(40, 100, 160))+
  gghighlight(use_direct_label = FALSE)+
  scale_x_date(limits = as.Date(c('2017-12-01', 
                                  '2023-08-01'), format ="%Y"))+
  geom_vline(xintercept = as.Date(c('2020-01-01', '2022-01-02')), linetype="dotted")+
  
  facet_wrap(~fct_relevel(type, fig2_level), nrow =4)+
  geom_line(data =. %>% filter(type %in% c("총지수")), aes(x = date, y = index), color ="#1f5c99", size = 1.5)+
  geom_line(data =. %>% filter(type %in% c("전기 · 가스 · 수도")), aes(x = date, y = index), color ="brown", size = 1.5)+  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "물가지수",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")



setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230922_energy_price/")
ggsave("energy_price_fig3.png",  width= 600, height = 600, units ="px", dpi = 100)







## 총지수, 석유류


setwd("C:/R/Rproject/Energy&Data/230922_energy_price")
price_fig4 <- read_excel("230922_소비자물가지수.xlsx", sheet ="석유교통요금", skip=10) %>% 
  na.omit() %>% 
  pivot_longer(-type, names_to = "date", values_to ="index") %>% 
  mutate(type = as.factor(type),
         date = ym(date))


unique(price_fig4$type)

price_fig4 %>% 
  ggplot(aes(x = date, y = index, group = type, color = type))+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 200))+
  
  #geom_line(data =. %>% filter(type =="총지수"), color ="#ec111a", size = 1.5)+
  #geom_line(data =. %>% filter(type %in% c('전기 · 가스 · 수도')), color ="#1f5c99", size = 1.5)+
  #scale_y_continuous(limits = c(80, 160))+
  gghighlight(type %in% c('총지수', '석유류'))+
  theme_bw()+
  theme_minimal()+
  scale_color_manual(values = c("firebrick1", "#1f5c99"))+
  
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "총지수, 석유류",
       subtitle ="물가지수",
       x = "",
       y = "물가지수",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")




setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230922_energy_price/")
ggsave("energy_price_fig4.png",  width= 600, height = 600, units ="px", dpi = 100)

