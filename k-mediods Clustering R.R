install.packages("fpc")
library(fpc)

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



#유입과 각 지역별 확진률의 상관관계 파악
corona = read.any("0704인구수대비코로나감염률데이터.csv", header = T)
corona = corona %>% select(-X) %>% filter(!(행정기관 == '전국'))
corona

v2_group = read.any("df_immunisation.csv", header = T)
nw = v2_group %>% select(-X) %>% group_by(유입대상지역) %>% summarise(sum(유입인구))
colnames(nw) = c("행정기관", "유입인구")
nw

#newData = 행정기관, 감염자수, 총인구수, 감염률, 유입인구
newData = merge(corona, nw)
newData

cor.test(newData$"감염률", newData$"유입인구")


#p-value = 0.144, cor = 0.3697855이므로 관련 없음


# 화장실 데이터 가져오기
toilet = read.any("시도별공공화장실수.csv", header = T)
toilet = toilet %>% select(-X)

immun = read.any("df_total_immunisation.csv", header = T)
immun
nw = immun %>% select(-X)
nw
colnames(nw) = c("행정기관", "접종률")

#newData = 행정기관, 감염자수, 총인구수, 감염률, 유입인구, 화장실수
newData = merge(newData, toilet)
newData

#newData = 행정기관, 감염자수, 총인구수, 감염률, 유입인구, 화장실수, 접종률
newData = merge(newData, nw)
newData

#clustering 위한 열 선별 및 정규화
new = newData %>% select(감염률, 화장실수, 접종률)
new$화장실수 = as.numeric(new$화장실수)
new$화장실수 = normalize(new$화장실수)
new
new = new %>% select(접종률, 감염률)
new = new %>% select(감염률, 화장실수)


pamk.result = pamk(new)
pamk.result$nc
pamk.result$pamobject$clusterin

table(pamk.result$pamobject$clustering, newData$행정기관)    # 테이블을 보자. 

layout(matrix(c(1,2), 1, 2))  # 한 화면에 두개의 그래프가 나오도록 설정.

plot(pamk.result$pamobject)     # plot을 그리기.

layout(matrix(1)) # 한 화면에 한그림만 나오도록(원래대로) 재설정 


#pam
library(cluster)
pam.result <- pam(new, 3)          # pam 함수를 적용
table(pam.result$clustering, newData$행정기관) 



layout(matrix(c(1,2), 1,2))            # 한 화면에 두개의 그래프가 나오도록 설정.
plot(pam.result)

