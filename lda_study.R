#인코딩 문제 해결 위해 read.any 함수 생성
library(readr)

read.any <- function(text, sep = "", ...) {
  
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
  
}

library(dplyr)
library(readxl)
df_vaccine = read_excel("0704인구수대비접종률데이터.xlsx",
                        range = cell_rows(1:19))
df_vaccine = read.any("0704인구수대비접종률데이터.csv", header=T)
df_vaccine = df_vaccine[, c(3, 4)]
df_vaccine

install.packages("stats")
library(stats)
km = kmeans(df_vaccine, sqrt(18/2))
km

km$centers

install.packages("factoextra")
library(factoextra)
fviz_cluster(km, data = df_vaccine$접종률, stand = F)

#normalizing
normalize <- function(x){
  return((x-min(x)) / (max(x) - min(x)))
}
df_vaccine$총인구수 = normalize(df_vaccine$총인구수)
df_vaccine


plot(df_vaccine, col = km$cluster)


#유입인원과 확진자수의 상관관계 파악
corona = read.any("0704인구수대비코로나감염률데이터.csv", header = T)
corona = corona %>% select(-X) %>% filter(!(행정기관 == '전국'))
corona

v2_group = read.any("df_immunisation - df_immunisation.csv", header=T)
nw = v2_group %>% select(-X) %>% group_by(유입대상지역) %>% summarise(sum(유입인구))
colnames(nw) = c("행정기관", "유입인구")
nw


newData = merge(corona, nw)


cor.test(newData$"감염률", newData$"유입인구")
#p-value = 0.144, cor = 0.3697855이므로 관련 없음


# 화장실 데이터 가져오기
toilet = read.any("시도별공공화장실수.csv", header = T)
toilet = toilet %>% select(-X)
v2_group = read.any("df_immunisation - df_immunisation.csv", header=T)
nw = v2_group %>% select(-X) %>% group_by(유입대상지역) %>% summarise(sum(유입지역.접종률))
colnames(nw) = c("행정기관", "접종률")

newData = merge(newData, toilet)
newData = merge(newData, nw)
new = newData %>% select(감염률, 화장실수, 접종률)
new$화장실수 = as.numeric(new$화장실수)

km = kmeans(new, sqrt(18/2))
plot(new, col = km$cluster)

