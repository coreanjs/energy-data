
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(showtext)
library(extrafont)
library(ggtext)
library(gghighlight)
library(scales)

font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

showtext_auto()

options(scipen=999)

setwd("C:/R/Rproject/Energy&Data")



##단위 MWh
EDGAR_monthly <- read_excel('./231028_EDGAR_temproral_profiles_CO2/EDGAR_temporal_profiles_r2_CO2.xlsx', skip = 1) %>%
  select(-Substance, -`EDGAR Sector`) %>%  # Substance는 모두 CO2이다
  rename(country = Country_code_A3) %>% 
  pivot_longer(-c(country, description, Year), names_to = 'month', values_to ="value") %>% 
  rename(year = Year) %>% 
  mutate(month = recode(month,
                        Jan = 1,
                        Feb = 2,
                        Mar = 3,
                        Apr = 4,
                        May = 5,
                        Jun = 6,
                        Jul = 7,
                        Aug = 8,
                        Sep = 9,
                        Oct = 10,
                        Nov = 11,
                        Dec = 12),
         date = as.Date(as.yearmon(paste(year, month, sep="-"))))


head(EDGAR_monthly)

range(EDGAR_monthly$year)


unique(EDGAR_monthly$country)

unique(EDGAR_monthly$description)

##대한민국 KOR

EDGAR_monthly %>% 
  filter(country =="KOR") %>% 
  ggplot(aes(x = month, y = value, group = year))+
  geom_line()+
  facet_wrap(~description)



library(ggdist)


EDGAR_monthly %>% 
  filter(country =="KOR") %>% 
  ggplot(aes(x = month, y = value, group = year))+
  scale_x_continuous(limits = c(1,12), breaks =seq(1, 12, 1))+
  stat_lineribbon()+
  facet_wrap(~description)



monthly_elec_gen %>% 
  ggplot(aes(x = month, y = TWh))+
  stat_lineribbon(
    data = . %>% filter(!year %in% c(2022, 2021)),
    .width = c(.5, .8, .95), linetype ="dashed", linewidth = .2, color = "black", alpha = .8)+
  geom_line(data = . %>% filter(year == 2022), 
            color ="#e3000b",
            linewidth = .8)+
  geom_line(data = . %>% filter(year == 2021), color ="#f7a102", linewidth = .8)+
  scale_fill_brewer()+
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1))+
  scale_y_continuous(limits = c(1, 23), breaks = seq(0, 20, 5))+
  facet_wrap(~fct_relevel(type, type_level), nrow=2, scales="free_x")+
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(family = 'Nanum Myeongjo',
                             size = 14),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
        axis.ticks.x = element_line(linewidth = .2,
                                    color = 'black'),
        axis.ticks.length = unit(.08, "cm"),
        axis.line.x = element_line(colour = "gray80", 
                                 size = .5, linetype = "solid"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 14),
        legend.position ="none"
        )+
  labs(title = "2017~2022 주요 발전원별 발전량 변화",
       subtitle = "한국전력통계 발전량 추이 데이터-사업자(종합) 기준-를 활용하여<br>
     주요 발전원의 2017~2020년 월별 발전량의 분포(점선은 평균)를 나타내었고,<br> 
    <span style = 'color:orange;'><b>2021년</b></span>과<span style = 'color:red;'>
       <b>2022년</b></span>은 개별 그래프로 나타내었음.<br>
       유연탄 발전량은 감소 추세이며, 원자력과 신재생 발전량은 증가 추세임",
       x ="월",
       caption = "Source : KEPCO, Graphic : Jiseok")


#setwd("./231027_elec_generation_by_month_KEPCO")
ggsave("gen.png",  width=800, height =1000, units ="px", dpi = 100, bg='white')


