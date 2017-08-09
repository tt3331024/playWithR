library(rvest)
library(dplyr)

store_url <- read_html('https://tw.tp-tea.com/store/')
county_list <- read_html('https://tw.tp-tea.com/store/') %>%
  html_nodes('#data_city option') %>%
  html_text()
county_list <- county_list[-1]

TP_tea <- data.frame()
for(c in county_list){
  county_url <- paste0('https://tw.tp-tea.com/store/?index_m_id=1&city=', c, '#map_location') %>% read_html()
  .m <- county_url %>%
    html_nodes('.responsive td') %>%
    html_text()
  .d <-matrix(.m, ncol = 4, byrow = T) %>% as.data.frame(stringsAsFactors = F)
  TP_tea <- bind_rows(TP_tea, .d)
}
TP_tea$V4 <- substr(TP_tea$V3, 1,3)
names(TP_tea) <- c('店名', '電話', '地址', '所在縣市')

lng <- vector()
lat <- vector()
for(i in 1:nrow(TP_tea)){
  address <- strsplit(TP_tea[i, 3], '號')[[1]][1] %>% paste0('號')
  addresses <- paste0('http://maps.googleapis.com/maps/api/geocode/xml?address=',
           address, '&sensor=false') %>% read_html()
  
  .lat <- addresses %>% html_nodes('geometry location lat') %>% html_text()
  if(length(.lat) == 0) .lat <- NA
  lat[i] <- .lat
  
  .lng <- addresses %>% html_nodes('geometry location lng') %>% html_text()
  if(length(.lng) == 0) .lng <- NA
  lng[i] <- .lng
}
TP_tea$經度 <- lng
TP_tea$緯度 <- lat


write.csv(TP_tea, file = paste0(getwd(), '/TP_tea.csv'), row.names = F)





