

# 전력자립도(지역) - 출처: KESIS 


## 라이브러리랑 데이터 가져오기
##데이터는 가져오면서 tidy 형태로 변환. 그리고 필요한 전처리

library(readxl)
library(tidyverse)
library(here)
library(extrafont)
library(showtext)

############ 한글 폰트 꺠짐 해별 방법
theme_set(theme_bw(base_family='nanumgothic'))
font_add_google(name =  'Nanum Gothic Coding',
                family = 'Nanum Gothic Coding' )


font_add_google("Noto Serif Korean")

font_add_google(name =  'Noto Serif Korean',
                family = 'Noto Serif Korean')



font_add_google(name =  'Nanum Myeongjo',
                family = 'Nanum Myeongjo')


showtext_auto(TRUE)

setwd("C:/R/Rproject/Energy&Data/20230901_elec_indenendency")


#자료원 :전력거래소
#주석 :1. 상용자가 자가소비량 제외

elec_independency_raw<-read_excel("231006_지역별_발전량_소비량.xlsx") %>% 
  pivot_longer(-c('region', 'type'), values_to = "GWh", names_to = "year") %>% 
  mutate(year = as.numeric(year),
         region = str_remove_all(region, ".(GWh)."))


elec_independency_raw






elec_independency_raw %>% 
  ggplot(aes(x = year, y = GWh, group =type, color = type))+
  geom_line()+
  facet_wrap(~region)



### pivor_wider로 구분



elec_independency<- elec_independency_raw %>% 
  pivot_wider(names_from = type, values_from = GWh, id_cols =c(region, year)) %>% 
  mutate(pct = 발전량/소비량*100) 


elec_independency


elec_independency


head(elec_independency)
str(elec_independency)


elec_independency %>% 
  ggplot(aes(x = year, y = pct, group = region)) +
  geom_line()


elec_independency %>% 
  filter(year==2021) %>% 
  arrange(desc(pct)) %>% pull(region) ->region_order

region_order



library(gghighlight)
library(shadowtext)
library(ggtext)
elec_independency %>% 
  mutate(pct = round(pct, 1)) %>% 
  ggplot(aes(x = year, y = pct, group = as.factor(region))) +
  geom_line(linewidth = 1.2, color ="#282c34")+
  gghighlight(unhighlighted_params = list(linewidth = 1, colour = alpha("gray80", 0.3)))+
  scale_x_continuous(limits = c(1991, 2021), breaks = c(1991, 2000, 2021))+
  scale_y_continuous(limits = c(0, 500),
                     breaks = c(0, 100, 200, 300), 
                     labels = function(x) paste0(x, "%"))+
  facet_wrap(~fct_relevel(region, region_order), ncol =5)+
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
       y = "전력자립도(%)",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")+
  geom_point(data =. %>% filter(year %in% c(2010)))+
  geom_point(data =. %>% filter(year %in% c(2016) & region =="세종"))+
  geom_point(data =. %>% filter(year %in% c(2020)), color ="#4ea267", size = 2)+
  geom_shadowtext(data =. %>% filter(year ==2016 & region =="세종"), aes(label = pct), hjust = 1, vjust = -.4, 
                  bg.color ="white",
                  color ="black",
                  family ="Nanum Myeongjo")+
  
    geom_shadowtext(data =. %>% filter(year ==2011), aes(label = pct), hjust = 0.3, vjust = -.8, 
                  bg.color ="white",
                  color ="black",
                  family ="Nanum Myeongjo")+
  geom_shadowtext(data =. %>% filter(year ==2020), aes(label = pct), hjust = .7, vjust = -.8,  
                  bg.color ="white", 
                  color ="#4ea267", size = 5, face ="bold",
                  family ="Nanum Myeongjo")


setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230904_Elec_independency/")
ggsave("KESIS_elec_independency_facet.png",  width= 800, height = 1000, units ="px", dpi = 100)


setwd("C:/R/Rproject/Energy&Data/20230901_elec_indenendency")
ggsave(filename = file.path("figs", "KESIS_elec_independency_facet.png"),  width= 800, height = 1000, units ="px", dpi = 100)



## beautiful plotting with cedric
library(ggtext)

??element_textbox_highlight

elec_independency %>% 
  mutate(pct = round(pct, 1)) %>% 
  ggplot(aes(x = year, y = pct, group = as.factor(region))) +
  geom_line(linewidth = 1.2, color ="#282c34")+
  gghighlight(unhighlighted_params = list(linewidth = 1, colour = alpha("gray80", 0.3)))+
  scale_x_continuous(limits = c(2010, 2021), breaks = c(2010, 2015, 2021))+
  scale_y_continuous(limits = c(0, 400),
                     breaks = c(0, 100, 200, 300), 
                     labels = function(x) paste0(x, "%"))+
  facet_wrap(~fct_relevel(region, region_order), ncol =4)+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_markdown(size= 22, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face ="bold", size = 16, hjust = 0, vjust = 0)
  )+

  labs(title = "대한민국 17개 광역지자체별 전력자립도 변화<br> Changes in electricity dependency in South Korea by regions",
       subtitle ="전력자립도는 전략 발전량을 소비량으로 나눈 백분율로, <br> <span style='color:#142f38'>**2011년**</span>부터 <span style ='color:#4ea267'>2020년</span>까지 대한민국 광역지자체별 전력자립도의 변화를 나타내었음",
       x = "",
       y = "전력자립도(%)",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")+
  geom_point(data =. %>% filter(year %in% c(2010)))+
  geom_point(data =. %>% filter(year %in% c(2016) & region =="세종"))+
  geom_point(data =. %>% filter(year %in% c(2021)), color ="#4ea267", size = 2)+
  geom_shadowtext(data =. %>% filter(year ==2016 & region =="세종"), aes(label = pct), hjust = 1, vjust = -.4, 
                  bg.color ="white",
                  color ="black",
                  family ="Nanum Myeongjo")+
  
  geom_shadowtext(data =. %>% filter(year ==2011), aes(label = pct), hjust = 0.3, vjust = -.8, 
                  bg.color ="white",
                  color ="black",
                  family ="Nanum Myeongjo")+
  geom_shadowtext(data =. %>% filter(year ==2020), aes(label = pct), hjust = .7, vjust = -.8,  
                  bg.color ="white", 
                  color ="#4ea267", size = 5, face ="bold",
                  family ="Nanum Myeongjo")


setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230904_Elec_independency/")
ggsave("KESIS_elec_independency_facet.png",  width= 800, height = 1000, units ="px", dpi = 100)















elec.independency.wider<-elec_independency %>% 
  pivot_wider(names_from = year,
              values_from = pct,
              id_cols = region,
              names_prefix = 'value_') %>% 
  select(region, value_2011, value_2020) 


elec.independency.wider





elec.independency.wider %>% 
  arrange(desc(value_2020)) %>% 
  mutate(y_position = rev(1:nrow(.))) %>% 
  select(region, y_position) ->y_position_elec

y_position_elec


elec_y_position <- left_join(elec.independency.wider, y_position_elec, by = 'region') %>% 
  arrange(y_position) %>% 
  mutate(bump_2011 = case_when(value_2011 < value_2020 ~ value_2011 -18, 
                               value_2011 > value_2020 ~  value_2011 +18,
                               TRUE~NA_real_),
         bump_2020 = case_when(value_2011 < value_2020 ~ value_2020 +18, 
                               value_2011 > value_2020 ~  value_2020 -18,
                               TRUE~value_2020 + 10)) %>% 
  pivot_longer(-c('region', 'y_position'), 
               names_to = c('.value', 'year'),
               names_sep = "_") %>% 
  mutate(value = round(value, 1),
         bump = ifelse(region =="세종", bump+10, bump))

elec_y_position


arrow_data_elec<-elec_y_position %>% 
  pivot_wider(names_from = year, values_from = value,
              id_cols = c(region, y_position),
              names_prefix = 'year_') %>% 
  filter(abs(year_2011 - year_2020) >10) %>% 
  mutate(midpoint = (year_2011 + year_2020)/2) %>% 
  select(region, year_2011, midpoint, y_position) %>% 
  pivot_longer(-c('region', 'y_position'), names_to = "type", values_to = "x") %>% 
  mutate(x = ifelse(region %in% c('대구'), x +3, x),
         x = ifelse(region %in% c('서울', '광주'), x +2, x))


arrow_data_elec




library(shadowtext)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')


elec_y_position %>%
  ggplot(aes(x = value, y = y_position, color = year, group = y_position))+
  geom_vline(xintercept = 100, linetype ="dotted", color = "blue")+
  
  geom_line( color ="#142f38", linewidth = 1.5)+
  geom_point(size = 3)+
  geom_path(data = arrow_data_elec, aes(x = x, y = y_position, group = y_position),
            color = '#142f38',
            arrow = arrow(angle = 30, length = unit(0.1, 'inch')))+
  scale_color_manual(values =  c('#142f38', '#4ea267'),
                     #c('#727272', '#15607a'),
                     label = c('2011년', '2020년')
  )+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  scale_y_continuous(breaks = c(elec_y_position$y_position, 1,elec_y_position$y_position+1),
                     labels = c(elec_y_position$region, rep("", length(elec_y_position$y_position)+1)))+
  geom_shadowtext(aes(label = value, x = bump, 
                      #  y = y_position -.4
                      family = 'Nanum Myeongjo'),
  bg.colour='white', size = 4)+
  theme_bw()+
  theme_minimal()+
  guides(color = guide_legend("연도"))+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size= 24, face="bold"),
        plot.subtitle = element_markdown(size= 14,lineheight = 1.2),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "top"
  )+
  labs(title = "대한민국 17개 광역지자체별 전력자립도 변화",
       subtitle ="전력자립도는 전략 발전량을 소비량으로 나눈 백분율로, <br> <span style='color:#142f38'>**2011년**</span>과 <span style ='color:#4ea267'>2020년</span> 대한민국 광역지자체별 전력자립도 변화를 <br>덤벨차트를 활용하여 나타내었으며, 2020년 기준 내림차순으로 정렬",
       y = "",
       x = "전력자립도",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")


setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230904_Elec_independency/")
ggsave("KESIS_elec_independency.png",  width= 550, height = 800, units ="px", dpi = 100)


setwd("C:/R/Rproject/Energy&Data/20230901_elec_indenendency")
ggsave(filename = file.path("figs", "KESIS_elec_independency.png"),  width= 800, height = 1800, units ="px", dpi = 100)










elec_independency %>% 
  ggplot(aes(x= as.numeric(year), y = pct, 
             #color = region, 
             group = region))+
  geom_line(data = . %>% filter(region %in% c('인천', '충남', '부산', '전남', '경남')), color ="red")+
  geom_line(data = . %>% filter(region %in% c('울산', '전북', '경기', '강원', '경북')), color ="blue")+
  geom_text(data = . %>% filter(year == 2020), aes(label =region),  hjust = -1, family = 'Nanum Myeongjo')+
  scale_x_continuous(limits = c(2011, 2021), breaks =seq(2011, 2020, 1))+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                            size = 14),
        plot.title = element_text(size= 20, face="bold"),
        plot.subtitle = element_markdown(size= 16,lineheight = 1.2),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x =element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )+
  labs(title = "2011-2020, 시도별 전력자립도",
       subtitle ="<span style='color:#142f38'>**2011년**</span>과 <span style ='color:#4ea267'>2020년</span>, 지자체별 전력자립도",
       y = "",
       x = "전력자립도",
       caption = "Source : KESIS(국가에너지통계 정보시스템),\nGraphic : Jiseok")





