r_data = read.any("합친데이터.csv", header = T)
r_data

library(dplyr)
immun = read.any("df_total_immunisation.csv", header = T)
immun
nw = immun %>% select(-X)
colnames(nw) = c("행정기관", "접종률")
nw

preData = read.any("합친데이터.csv", header = T)
preData = preData %>% select(-X) %>% select(-접종률) 

newData = merge(nw, preData)

write.csv(newData, "시도별데이터정리.csv")
