library(tidyverse)
library(readxl)
options(scipen=999)
library(showtext)
showtext_auto()
font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')


##단위 MWh
PV_regional <- read_excel('./230911_PV_regional/PV_regional_selected.xlsx') %>%
  select(-MWh) %>%
  mutate(`2010` = as.numeric(`2010`)) %>% 
  pivot_longer(-c(region, type), names_to = 'year', values_to ="MWh") %>% 
  filter(region!='전국') %>% 
  mutate(MWh = ifelse(is.na(MWh), 0, MWh),
         GWh = MWh/1000,
         TWh = GWh/1000,
         year = as.numeric(year)) 



PV_regional %>% 
  group_by(type,year) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = year, y = TWh, group = type, color = type))+
  geom_line()+
  labs(title = "2005-2020 대한민국 태양광 발전량 사업용/자가용 구분")


PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(region) %>% 
  summarise(TWh = sum(TWh)) %>% 
  arrange(desc(TWh)) %>% pull(region) -> region_order

region_order



PV_regional %>% 
  group_by(region, year) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = year, y = TWh, group = region, color =region))+
  geom_line()+
  theme_bw()+
  theme_minimal()+
  labs(x = '지역',
       title = "2020년 대한민국 지역별 태양광 발전량(사업용/자가용 합쳐서)")+
  theme(text = element_text(family ='Nanum Myeongjo'),
        plot.title = element_text(size = 16))


PV_regional %>% 
  filter(year == 2020) %>% 
  group_by(type, region) %>% 
  summarise(TWh = sum(TWh)) %>% 
  ggplot(aes(x = fct_relevel(region, rev(region_order)), y = TWh, group = type, fill =type))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits = c(0, 4.5))+
  coord_flip()+
  theme_bw()+
  theme_minimal()+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = region), 
               geom = "text",  hjust = -0.3, family ='Nanum Myeongjo')+
  labs(x = '지역',
       title = "2020년 대한민국 지역별 태양광 발전량(사업용/자가용 구분)")+
  theme(text = element_text(family ='Nanum Myeongjo'),
        plot.title = element_text(size = 16))





PV_regional %>% 
  ggplot(aes(x = year,  y =GWh, group = type, color =type))+
  geom_line()+
  theme_bw()+
  #theme_minimal()+
  facet_wrap(~fct_relevel(region, region_order), ncol = 3)+
  scale_x_continuous(breaks = seq(2005, 2020, 5))+
  labs(x = '지역',
       title = "2020년 대한민국 지역별 태양광 발전량(사업용/자가용 구분)")+
  theme(text = element_text(family ='Nanum Myeongjo'),
        plot.title = element_text(size = 16),
        #legend.position ="top"
  )+
  guides(color=guide_legend(title="구분"))



