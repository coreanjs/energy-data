library(tidyverse)
library(RColorBrewer)
library(scales)
library(lubridate)
library(ggh4x)
library(plotly)
library(zoo)
library(extrafont)
library(showtext)                                     
library(gghighlight)
library(echarts4r)
library(ggh4x)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(viridis)
library(calendR)
library(sugrrants)
library(patchwork)
library(markdown)
library(ggtext)
library(TTR)
library(ggExtra)
library(ggbeeswarm)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggdist) 
library(readxl)
news.keyword
library(tidytext)




## 사전 설치


#install.packages("remotes")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
useNIADic()
useSejongDic()








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



setwd("C:/R/Rproject/Energy&Data/20230838_Energy_Issues_Bigkinds")

news.raw<- read_excel("climate change.xlsx")

str(news.raw)

news.selected<- news.raw %>% 
    select(일자, 언론사, `특성추출(가중치순 상위 50개)`) %>% 
    #  mutate(키워드 = str_replace_all(키워드, ',', ' ')) %>% 
    rename(keyword = `특성추출(가중치순 상위 50개)`,
           date = 일자,
           source = 언론사) %>% 
    mutate(date = as.Date(date, format="%Y%m%d"),
           president = case_when(date >= as.Date('2008-02-05') & date <= as.Date('2013-02-24') ~ '이명박',
                                 date >= as.Date('2013-02-25') & date <= as.Date('2017-03-10')~ '박근혜',
                                 date >= as.Date('2017-05-10') & date <= as.Date('2022-05-09') ~ '문재인',
                                 date >= as.Date('2022-05-10')  ~ '윤석열'),
           president = factor(president, levels = c('이명박', '박근혜', '문재인', '윤석열')))


news.selected

## complete.cases를 활용해 결측행만 보기

news.selected[!complete.cases(news.selected),]

## complete.cases를 활용해 박근혜와 문재인 사이 빈 기간 날리기

news.selected <- news.selected %>% filter(complete.cases(news.selected))



unique(news.selected$source)

head(news.selected)


tail(news.selected)



## 2357개 사설
nrow(news.selected)


news.selected %>% 
    group_by(president) %>% 
    count(n = n()) %>%
    ungroup() %>% 
    mutate(pct = nn/n*100)



range(news.selected$date)




### 월별로 정리해서 보기 - 모든 기간
news.selected %>% 
    mutate(ym = as.yearmon(date)) %>% 
    group_by(ym, president) %>% 
    count( n = n()) %>% 
    ggplot(aes(x = as.Date(ym), y = nn))+
    geom_col(fill ="#1f5c99")+
    scale_x_date(date_breaks = "1 year", date_labels = "'%y")+ 
    geom_vline(xintercept = as.Date('2008-02-05'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2013-02-25'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2017-05-10'), color ="red", linetype ="dashed")+
    geom_vline(xintercept = as.Date('2022-05-10'), color ="red", linetype ="dashed")+
    theme_minimal()+
    theme_bw()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        plot.title = element_text(size = 30, face ="bold"),
        plot.subtitle = element_text(size =24),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x =element_text(size = 16),
        axis.title.y =element_text(size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position = "none")+
    labs(y = '사설 수',
         x = '연도',
         title = "분석기간 동안의 탄소중립 관련 사설의 수",
         subtitle ="분석기간은 2008년 2월 29일부터 2023년 7월 10일이며, \n사설은 주요 언론사 및 지역 언론사를 포함한 46개 언론사의 검색 결과를 포함")

setwd("C:/R/Rproject/Energy&Data/20230838_Energy_Issues_Bigkinds/img for poster")
#setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")

ggsave('number_of_column_trend.png',   width =1200, height = 700, units ="px", dpi = 100)




### 월별로 정리해서 보기 - 정권별로 구분

news.selected %>% 
    mutate(ym = as.yearmon(date)) %>% 
    group_by(ym, president) %>% 
    count( n = n()) %>% 
    ggplot(aes(x = as.Date(ym), y = nn))+
    geom_col(fill ="#1f5c99")+
    scale_x_date(date_breaks = "1 year", date_labels = "'%y. %b.")+ facet_wrap(~president, ncol = 1)+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none")+
  labs(y = '사설 수',
       x = '연도',
       title = "분석기간 동안의 탄소중립 관련 사설의 수",
       subtitle ="분석기간은 2008년 2월 29일부터 2023년 7월 10일이며, \n사설은 주요 언론사 및 지역 언론사를 포함한 46개 언론사의 검색 결과를 포함")+
    geom_smooth(color ="red")

ggsave('number_of_column_trend_facet.png',   width =1200, height = 1200, units ="px", dpi = 100)



### 공통키워드를 찾아볼까???





############ 230710

##unnest tokens는 tidytext 라이브러링

library(tidytext)



news.selected

news.selected %>% 
    select(keyword) %>% 
    separate_rows(keyword, sep= ",")


news.keyword<- news.selected %>% 
    separate_rows(keyword, sep= ",") %>% 
    rename(word = keyword) %>% 
    filter(str_length(word) > 1) %>% 
    count(president, word, sort = TRUE) %>%
    mutate(word = str_replace_all(word, '우리나라', '한국/우리나라'),
         word = str_replace_all(word, '코로나19', '코로나'),
         word = ifelse(word %in% c("택소", "노미"), "택소노미", word)) %>% 
    filter(!word %in% c('문재인', '윤석열', '가능성', '이명박', '박근혜', '역대급', '만큼', '그동안', '각국'))


news.keyword

news.keyword %>% 
  filter(word =="택소")



### 어짜피 키워드로 구분되어있으니까 unnest_tokens 안 써도 된다
#news.keyword<- news.selected %>% 
#  select(date, source, keyword, president) %>% 
#  unnest_tokens(input = keyword,
#                output = word,
#  ) %>% 
#  filter(str_length(word) > 1) %>% 
#  count(president, word, sort = TRUE) %>% 
#  filter(!word %in% c('만큼', '그동안', '각국'))
#


news.keyword %>% 
    filter(president =="문재인")




## slice 활용해서 상위 n개만 보기
news.keyword %>% 
    group_by(president) %>%
    mutate(row_number = row_number(),
           word = reorder_within(word, n, president)) %>% 
    slice(1:20) %>% 
    ggplot(aes(x = word, y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    scale_x_reordered()+
    facet_wrap(~president, scales = "free_y", nrow = 1)+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        plot.title =element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none",
        plot.title.position = "plot")+
    labs(y = '사설 수',
         x = '날짜',
         title ="전체 키워드 상위 20개(예를 들어)를 보여주는데, 정권별로 구분")



ggsave('number_of_keyword_by_president_keyword50.png',   width =800, height = 600, units ="px", dpi = 100)






## unnest_token으로 분류
library(tidytext)




news.keyword %>% 
    group_by(word) %>% 
    summarise(n = sum(n)) %>% 
    top_n(20) %>% 
    ggplot(aes(x = reorder(word, n), y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    theme(   plot.title =element_text(size = 16),
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none",
        plot.title.position="plot")+
    labs(y = '단어 수',
         x = '단어',
         title ="전체 키워드 상위 20개(예를 들어)를 보여주는데, 정권별로 구분 없이")

setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")


ggsave('keyword_keyword50.png',   width =500, height = 700, units ="px", dpi = 100)



### 특정 키워드만 ('미국) 보기  - 미국이 들어간 컬럼만 보기
news.selected %>% 
    mutate(keyword_detect = ifelse(str_detect(keyword, '미국'), 'Y', 'N')) %>% 
    filter(keyword_detect =='Y') %>% 
    separate_rows(keyword, sep= ",") %>% 
    rename(word = keyword) %>% 
    filter(str_length(word) > 1) %>% 
    count(president, word, sort = TRUE) %>% 
    group_by(president) %>%
    mutate(row_number = row_number(),
           word = reorder_within(word, n, president)) %>% 
    slice(1:20) %>% 
    ggplot(aes(x = word, y = n))+
    geom_col(fill ="#1f5c99")+
    coord_flip()+
    scale_x_reordered()+
    facet_wrap(~president, scales = "free_y", nrow = 1)+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 20),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        legend.position = "none")+
    labs(title = '미국이 들어간 칼럼만 추출한 결과',
         y = '사설 수',
         x = '날짜')




### 특정 키워드만 ('요금) 보기  - 미국이 들어간 컬럼만 보기
news.selected %>% 
  mutate(keyword_detect = ifelse(str_detect(keyword, '요금'), 'Y', 'N')) %>% 
  filter(keyword_detect =='Y') %>% 
  separate_rows(keyword, sep= ",") %>% 
  rename(word = keyword) %>% 
  filter(str_length(word) > 1) %>% 
  count(president, word, sort = TRUE) %>% 
  group_by(president) %>%
  mutate(row_number = row_number(),
         word = reorder_within(word, n, president)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = word, y = n))+
  geom_col(fill ="#1f5c99")+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~president, scales = "free_y", nrow = 1)+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 20),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 11),
    legend.position = "none")+
  labs(title = '요금이 들어간 칼럼만 추출한 결과',
       y = '사설 수',
       x = '날짜')

keyword_one<- "태양광"
news.selected %>% 
  mutate(keyword_detect = ifelse(str_detect(keyword, keyword_one), 'Y', 'N')) %>%
  filter(keyword_detect =='Y') %>% 
  separate_rows(keyword, sep= ",") %>% 
  rename(word = keyword) %>% 
  filter(str_length(word) > 1) %>% 
  count(president, word, sort = TRUE) %>% 
  group_by(president) %>%
  mutate(row_number = row_number(),
         word = reorder_within(word, n, president)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = word, y = n))+
  geom_col(fill ="#1f5c99")+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~president, scales = "free_y", nrow = 1)+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 20),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 11),
    legend.position = "none")+
  labs(title = paste0(keyword_one, '(이/가) 들어간 칼럼만 추출한 결과'),
       y = '사설 수',
       x = '날짜')




keyword_one<- "태양광"
news.selected %>% 
 # mutate(keyword_detect = ifelse(str_detect(keyword, keyword_one), 'Y', 'N')) %>%
#  filter(keyword_detect =='Y') %>% 
  select(keyword) %>% 
  rowid_to_column() %>% 
  unnest_tokens(input = keyword,
                output = word,
                to_lower = FALSE) %>% 
  filter(str_length(word) > 1) %>%
  add_count(word) %>% 
  filter(n >=20) %>%   ## 절대적 기준 없음
  ## pairwise_count가 아니고 pairwise_cor 임
  pairwise_cor(item = word,      
               feature = rowid,
               sort = T) %>% 
  filter(correlation >= 0.2) %>% ## 이거 꼭 넣어야함 안 그럼 개판(절대적 기준 없음) 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
         group = as.factor(
           group_louvain()
           #group_infomap()
         ))  %>%
  
  ggraph(layout = "fr") +      # 레이아웃
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
  
  geom_node_point(aes(size = centrality,
                      color = group,
                      alpha = .3),
                  show.legend = F) +
  scale_size(range = c(2, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 4,
                 family = "nanumgothic") +
  
  theme_graph()+                          # 배경 삭제
  labs(title = "1990-2023 신문 사설을 활용한 단어 간 상관관계(phi-coefficient) 네트워크")





########### 함수로 만들어서



keyword_top5<- c('기후변화', '미국', '한국', '우리나라', '중국', '배출량', 
                 '온실가스', '전문가', '가능성', '녹색성장', '코로나19',
                 '창조경제', '탈원전')


keyword_top5



for (i in keyword_top5) {
    
    keyword_top5_by_president = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('특정 키워드가 포함된 (키워드 :', i ,  ')칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    
    #setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue/keyword_by_president")

    ggsave(plot = keyword_top5_by_president, file =paste0("keyword_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}


getwd()







### 정권별로 구분해서?   - count로 
news.keyword %>% 
    group_by(word) %>% 
    mutate(total = sum(n),
           pct = round(n/total*100, 0)) %>% 
    #top_n(20) %>% 
    filter(n>=50) %>% 
    ggplot(aes(x = reorder(word, n, sum), y = n, fill = president))+
    geom_col()+
    coord_flip()+
    theme(
        text = element_text(family = 'Nanum Myeongjo'),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #    panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 11),
        #legend.position = "none"
    )+
    labs(y = '단어 수',
         x = '단어')







### 정권별로 구분해서?   - pct로 
news.keyword  %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.48) %>%
    #filter(pct >.3) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    plot.title.position = "plot",
    #legend.position = "none"
    )+
    labs(
        x = '단어',
        y = '빈도(%)',
        title ="단어별 출연 빈도(%)를 정권별로 나타내며, 상위 20개 단어만 제시")

ggsave('keyword_keyword20.png',   width =1200, height = 700, units ="px", dpi = 100)



news.keyword  %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.48) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none")+
  labs(
    x = '',
    y = '빈도(%)',
    title ="단어별 출연 빈도(%)를 정권별로 나타내며 최고 출연 단어 20개만 제시")+
  facet_wrap(~president, nrow =1)








#####patchwork용

keyword_20 <-news.keyword  %>% 
  group_by(president) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n/total*100) %>% 
  #top_n(20) %>% 
  filter(pct >.48) %>%
  #filter(pct >.3) %>% 
  ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
  geom_col()+
  coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = c(0.8, 0.7)
  )+
  labs(
    x = '단어',
    y = '빈도(%)')


keyword_20_facet<- news.keyword  %>% 
  group_by(president) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = n/total*100) %>% 
  #top_n(20) %>% 
  filter(pct >.48) %>% 
  ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
  geom_col()+
  coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none")+
  labs(
    x = '',
    y = '빈도(%)')+
  facet_wrap(~president, nrow =1)




keyword_20+keyword_20_facet+
  plot_annotation(
    title = '단어별 출연 빈도(%)를 정권별로 나타냄 - 상위 단어 20개만 제시',
    subtitle = '왼쪽 그래프는 누적 비중을 확인할 수 있으며, 오른쪽 그래프는 정권별 특징을 보여줌',
    caption = 'Disclaimer: None of these plots are insightful') &
  theme( text = element_text(family = 'Nanum Myeongjo'),
         plot.title = element_text(size = 30, face ="bold"),
         plot.subtitle = element_text(size =24))

ggsave('keyword_keyword20_patchword.png',   width =1200, height = 700, units ="px", dpi = 100)















########## pct

news.keyword.is.na<- news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    select(president, word, pct) %>% 
    mutate(row_id = row_number(), .by=c('president', 'word')) %>%  ### https://stackoverflow.com/questions/76366516/is-there-an-r-function-in-pivot-wider-to-spread-out-list-columns-data-from-a-piv
    pivot_wider(names_from= 'president', values_from = 'pct') %>%  ## pivot wider list-column 문제 해결
    mutate(pct = rowSums(.[2:5], na.rm=T),
           is_na = rowSums(is.na(.[2:5]))) %>% 
    arrange(-pct)




news.keyword.is.na 


news.keyword.is.na %>% 
    select(-pct) %>% 
    pivot_longer(-c(word, is_na), names_to="president", values_to = 'pct') %>% 
    slice(1: 100) %>% 
    ggplot(aes(x=word, y = pct))+
    geom_col()




### 두 정권에서만 나온 키워드 5개
news.keyword.is.na %>% 
    filter(is_na ==2) %>% 
    slice(1:5) %>%  pull(word) -> keyword_is_na_2


keyword_is_na_2



for (i in keyword_is_na_2) {
    
    keyword_is_na_2_graph = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('두 정권에서만 나타난 키워드 (', i ,  ') 칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    

    
    ggsave(plot = keyword_is_na_2_graph, file =paste0("keyword_is_na_2_graph_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}





## 한 정권에서만 나온 단어

news.keyword.is.na %>% 
    filter(is_na ==3) %>% 
    slice(1:5) %>%  pull(word) -> keyword_is_na_1


keyword_is_na_1



for (i in keyword_is_na_1) {
    
    keyword_is_na_1_graph = news.selected %>% 
        mutate(keyword_detect = ifelse(str_detect(keyword, i), 'Y', 'N')) %>% 
        filter(keyword_detect =='Y') %>% 
        separate_rows(keyword, sep= ",") %>% 
        rename(word = keyword) %>% 
        filter(str_length(word) > 1) %>% 
        count(president, word, sort = TRUE) %>% 
        group_by(president) %>%
        mutate(row_number = row_number(),
               word = reorder_within(word, n, president)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x = word, y = n))+
        geom_col(fill ="#1f5c99")+
        coord_flip()+
        scale_x_reordered()+
        facet_wrap(~president, scales = "free_y", nrow = 1)+
        theme(
            text = element_text(family = 'Nanum Myeongjo'),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 20),
            #panel.grid.major.x = element_blank(),
            #    panel.grid.major.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_blank(),
            strip.text.x = element_text(size = 11),
            legend.position = "none")+
        labs(title = paste('한 정권에서만 나타난 키워드 (', i ,  ') 칼럼만 추출하여 빈도 분석'),
             y = '사설 수',
             x = '날짜')
    
    #setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue/keyword_by_president")
    setwd("C:/R/Rproject/2023_project_KIER/20230838_Energy_Issues_Bigkinds")
    
    ggsave(plot = keyword_is_na_1_graph, file =paste0("keyword_is_na_1_graph_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
    
}




### 모든 키워드 포함해서
### 모든 키워드 포함해서
### 모든 키워드 포함해서
### 모든 키워드 포함해서


news.keyword %>% 
    group_by(president) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n/total*100) %>% 
    #top_n(20) %>% 
    filter(pct >.4) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none")+
    labs(
        x = '단어')+
    facet_wrap(~president, scales= "free_y", nrow = 1)


## 모든 정권에서 나타난 단어는 제외하고



####
news.keyword.is.na %>% 
    arrange(desc(pct)) %>% 
    filter(is_na>1) %>%  ################################ 여기
    mutate(row_number = row_number()) %>% 
    select(-is_na) %>% 
    rename(value = pct) %>% 
    pivot_longer(-c(word, value, row_number), names_to = "president", values_to = 'pct') %>% 
    filter(row_number<= 50) %>% 
    mutate(president = factor(president, levels = c('이명박', '박근혜', '문재인', '윤석열'))) %>% 
    ggplot(aes(x = reorder(word, pct, sum), y = pct, fill = president))+
    geom_col()+
    coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme_bw()+
  theme(
    text = element_text(family = 'Nanum Myeongjo'),
    plot.title = element_text(size = 30, face ="bold"),
    plot.subtitle = element_text(size =24),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 18),
    axis.title.x =element_text(size = 16),
    axis.title.y =element_text(size = 16),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = "none")+
    labs(
        x = '단어')+
    facet_wrap(~president, scales= "free", nrow = 1)






library(ggraph)
library(tidygraph)  ## 네트워크 그래프
library(widyr)   ##pairwise_count




### 함수로 만들기 전에 원본?



news.keyword


news.selected


news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>%
    separate_rows(keyword, sep= ",") %>% 
    #  unnest_tokens(input = keyword,
    #                output = word,
    #                to_lower = FALSE) %>% 
    #  filter(str_length(word) > 1) %>%  
    #  filter(!word %in% c('만큼', '그동안')) %>%  
    pairwise_count(item = keyword,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >= 30) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = 'Nanum Myeongjo') +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    theme( text = element_text(family = 'Nanum Myeongjo'))


## 이미지 저장 폴더
setwd("C:/Users/User/OneDrive - 한국에너지기술연구원/안지석(개인폴더)/230125_energydata_샘플_가이드_png/resources/images/230607_Energy_Issue")








news.keyword






news.keyword

1+1

news.selected%>% 
    select(keyword, president) %>% 
    rowid_to_column() %>% 
    filter(president =="이명박") %>% ############################# 
separate_rows(keyword, sep= ",") %>% 
    pairwise_count(item = keyword,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >= 20) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(
        #layout = "fr"
    )+      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = 'Nanum Myeongjo') +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    theme( text = element_text(family = 'Nanum Myeongjo'))




















######## 함수로 만들어서 ggssave!!!!!!!!!!!!!


number <- c(100, 150, 200, 250, 300, 350, 400)
number

?tibble

for (i in number) {
    
    test = news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE) %>% 
        filter(str_length(word) > 1) %>%  
        pairwise_count(item = word,     ## pairwise_count  : 단어 동시 출현 빈도
                       feature = rowid,
                       sort = T) %>% 
        filter(n >= i) %>%  ##
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(group_louvain()))  %>% 
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                       alpha = 0.2) +             # 엣지 명암
        
        geom_node_point(aes(size = centrality,    # 노드 크기
                            color = group,
                            alpha =0.5),       # 노드 색깔
                        show.legend = F) +        # 범례 삭제
        scale_size(range = c(3, 15)) +            # 노드 크기 범위
        geom_node_text(aes(label = name),         # 텍스트 표시
                       repel = T,                 # 노드밖 표시
                       size = 5,                  # 텍스트 크기
                       family = 'Nanum Myeongjo') +  # 폰트
        
        theme_graph()+                             # 배경 삭제
        theme( text = element_text(family = 'Nanum Myeongjo'))+
        labs(title = paste("1990-2023 신문 사설을 활용한 키워드 동시출현빈도 네트워크", "(filter, n >=", i, ")"))
    
    
    
    ggsave(plot = test, file =paste0("pairwise_count_", i, ".png"),  width =1000, height = 700, units ="px", dpi = 100)
    
}













## 연결중심성   pairwise_count  : 단어 동시 출현 빈도


pairwise_count_1<- news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>%  
    pairwise_count(item = word,     ## pairwise_count  : 단어 동시 출현 빈도
                   feature = rowid,
                   sort = T) %>% 
    filter(n >=50) %>%  ##
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(group_louvain()))  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(edge_color = "gray50",          # 엣지 색깔
                   alpha = 0.2) +             # 엣지 명암
    
    geom_node_point(aes(size = centrality,    # 노드 크기
                        color = group,
                        alpha =0.5),       # 노드 색깔
                    show.legend = F) +        # 범례 삭제
    scale_size(range = c(3, 15)) +            # 노드 크기 범위
    geom_node_text(aes(label = name),         # 텍스트 표시
                   repel = T,                 # 노드밖 표시
                   size = 5,                  # 텍스트 크기
                   family = "nanumgothic") +  # 폰트
    
    theme_graph()+                             # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 키워드 동시출현빈도 네트워크")


pairwise_count_1

ggsave(plot =pairwise_count_1, "pairwise_count_1.png",  width =1000, height = 700, units ="px", dpi = 100)








## centrality_edge_betweenness() 


centrality_edge_betweenness_1<- news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>% 
    pairwise_count(item = word,    
                   feature = rowid,
                   sort = T) %>% 
    filter(n >=280) %>% 
    as_tbl_graph(directed = F) %>% 
    activate(edges) %>% ## betweenness 하려면 edge랑 node active 해야함
    mutate(centrality = centrality_edge_betweenness(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           # group = as.factor(group_louvain())
    )  %>% 
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_diagonal(aes(edge_width = centrality, label = round(centrality, 2)), 
                       angle_calc = "along", alpha = 0.5, vjust = -1,
                       color = "lightblue") +
    geom_node_circle(aes(r = 0.2), fill = "lightblue", color = "navy") +
    geom_node_text(aes(label = name)) +
    coord_equal() +
    theme_graph()+              # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 centrality_edge_betweenness()  네트워크")


ggsave(plot =centrality_edge_betweenness_1, "centrality_edge_betweenness_1.png",  width =1200, height = 700, units ="px", dpi = 100)



library(ggraph)
## 상관계수 phi-coefficient 









## 20231010 미팅
## 특정 단어 분석


## 20231010 미팅
## 20231010 미팅
## 20231010 미팅
## 20231010 미팅
## 20231010 미팅
## 20231010 미팅
## 20231010 미팅








president_name <-c("윤석열")
president_name <-c("이명박")
president_name <-c("박근혜")
president_name <-c("문재인")

filter_number = 20
corr = 0.15

news.selected%>% 
   filter(president == president_name) %>% 
    select(keyword, president) %>%
  #  filter(str_detect(keyword, keyword_name)) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE) %>% 
    filter(str_length(word) > 1) %>%
    mutate(word =str_replace_all(word, '제주도', '제주'),
           word =str_replace_all(word, '19', '코로나'),
           word =str_replace_all(word, '광주시', '광주'),
           word =str_replace_all(word, '전남도', '전남'),
           word =str_replace_all(word, '전문가들', '전문가'),
           word =str_replace_all(word, '울산시', '울산'),
           word =str_replace_all(word, '서울시', '서울'),
           word =str_replace_all(word, '충남도', '충남'),
           word =str_replace_all(word, '경남도', '경남'),
           word =str_replace_all(word, '부산시', '부산'),
           ) %>% 
    add_count(word) %>% 
   filter(n >= filter_number) %>%   ## 절대적 기준 없으나 꼭 필요
    ## pairwise_count가 아니고 pairwise_cor 임
    pairwise_cor(item = word,      
                 feature = rowid,
                 sort = T) %>% 
    #filter(item1 %in% c("태양광")) %>% 
    filter(correlation >= corr) %>% ## 이거 꼭 넣어야함 안 그럼 개판(절대적 기준 없음) 
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(
               group_louvain()
               #group_infomap()
           ))  %>%
    
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(color = "gray50",
                   aes(edge_alpha = correlation,   # 엣지 명암
                       edge_width = correlation),  # 엣지 두께
                   show.legend = F) +              # 범례 삭제
    scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
    
    geom_node_point(aes(size = centrality,
                        color = group,
                        alpha = .3),
                    show.legend = F) +
    scale_size(range = c(2, 10)) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "Nanum Myeongjo") +
    
    theme_graph()+                          # 배경 삭제
    labs(title = paste0(president_name, " 정권",  "- 단어 간 상관관계(phi-coefficient) 네트워크"))



ggsave(file =paste0(president_name, "_phi_", corr, "_filter_",filter_number, ".png"),  width =1200, height = 700, units ="px", dpi = 100)




correlation_coef <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
correlation_coef

## 미팅 회의 최지영 

for (i in correlation_coef) {
    
    phi = news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE) %>% 
        filter(str_length(word) > 1) %>%
        add_count(word) %>% 
        filter(n >=50) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        pairwise_cor(item = word,      
                     feature = rowid,
                     sort = T) %>% 
        filter(correlation >= i) %>% ## 이거 꼭 넣어야함 안 그럼 개판(절대적 기준 없음) 
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(edge_alpha = correlation,   # 엣지 명암
                           edge_width = correlation),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group,
                            alpha = .3),
                        show.legend = F) +
        scale_size(range = c(2, 10)) +
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "Nanum Myeongjo") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("1990-2023 신문 사설을 활용한 단어 간 상관관계(phi-coefficient) 네트워크", "(correlation coeff=", i, ")"))
    
    
    
    ggsave(plot = phi, file =paste0("phi_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}















########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것
########### 여기부터는 활용 안할 것




## 바이그램

ngram_1 <-news.selected%>% 
    select(keyword) %>% 
    rowid_to_column() %>% 
    unnest_tokens(input = keyword,
                  output = word,
                  to_lower = FALSE,
                  token ="ngrams", ### ngrams 토큰
                  n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
    separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
    count(word1, word2, sort = T) %>%
    na.omit() %>% 
    filter(n >=50) %>%   ## 절대적 기준 없음
    ## pairwise_count가 아니고 pairwise_cor 임
    as_tbl_graph(directed = F) %>% 
    mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
           group = as.factor(
               group_louvain()
               #group_infomap()
           ))  %>%
    
    ggraph(layout = "fr") +      # 레이아웃
    geom_edge_link(color = "gray50",
                   aes(color = "gray50",            # 엣지 색깔
                       alpha = 0.5),  # 엣지 두께
                   show.legend = F) +              # 범례 삭제
    scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
    
    geom_node_point(aes(size = centrality,
                        color = group),
                    show.legend = F) +
    scale_size(range = c(2, 7)) +
    
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 4,
                   family = "nanumgothic") +
    
    theme_graph()+                          # 배경 삭제
    labs(title = "1990-2023 신문 사설을 활용한 ngram 단어쌍 빈도 네트워크")

ggsave(plot =ngram_1, "ngram_1.png",  width =1200, height = 700, units ="px", dpi = 100)







ngram_coef <- c(50, 100, 150, 200, 250, 300, 350, 400)
ngram_coef

for (i in ngram_coef) {
    
    ngram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("바이그램(ngreams, n = 2) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = ngram, file =paste0("ngram_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}





ngram_coef <- c(50, 100, 150, 200, 250, 300, 350, 400)
ngram_coef

for (i in ngram_coef) {
    
    bigram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =2) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("바이그램(n = 2) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = bigram, file =paste0("bigram_n=2_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}



for (i in ngram_coef) {
    
    trigram =  news.selected%>% 
        select(keyword) %>% 
        rowid_to_column() %>% 
        unnest_tokens(input = keyword,
                      output = word,
                      to_lower = FALSE,
                      token ="ngrams", ### ngrams 토큰
                      n =3) %>%    ### n = 2 바이그램, n = 3 트라이그램
        separate(word, c('word1', 'word2'), sep = ' ') %>%  ## 두 단어 분리
        count(word1, word2, sort = T) %>%
        na.omit() %>% 
        filter(n >= i ) %>%   ## 절대적 기준 없음
        ## pairwise_count가 아니고 pairwise_cor 임
        as_tbl_graph(directed = F) %>% 
        mutate(centrality = centrality_degree(),        # 연결 중심성 or centrality_edge_betweenness()  참고: https://tidygraph.data-imaginist.com/
               group = as.factor(
                   group_louvain()
                   #group_infomap()
               ))  %>%
        
        ggraph(layout = "fr") +      # 레이아웃
        geom_edge_link(color = "gray50",
                       aes(color = "gray50",            # 엣지 색깔
                           alpha = 0.5),  # 엣지 두께
                       show.legend = F) +              # 범례 삭제
        scale_edge_width(range = c(0.5, 2)) +            # 엣지 두께 범위
        
        geom_node_point(aes(size = centrality,
                            color = group),
                        show.legend = F) +
        scale_size(range = c(2, 7)) +
        
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 4,
                       family = "nanumgothic") +
        
        theme_graph()+                          # 배경 삭제
        labs(title = paste("트라이그램(n = 3) 네트워크", "(filter>=", i, ")"))
    
    
    
    ggsave(plot = trigram, file =paste0("trigram_n=3_", i, ".png"),  width =1200, height = 700, units ="px", dpi = 100)
    
}









