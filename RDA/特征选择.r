library(tidyverse)
library(vegan)
# 读取数据

otu_table <- read.csv('20240307-菌种.txt', header = TRUE, sep = '\t', row.names = 1) %>% t() %>% as.data.frame()
envdata <- read.table('全部环境因子.txt', sep = '\t', header = TRUE, row.names = 1)

# 不适用任何特征拟合
rda.0 <-rda(otu_table ~ 1, envdata,scale=F)

# 使用全部特征
rda.all <-rda(otu_table ~., envdata,scale=F)


mod <- ordistep(
  rda.0, 
  scope = formula(rda.all), 
  direction = 'both', 
  R2scope=0.99, 
  permutations = 999,  
  Pin = 0.05)

summary(mod)
r2 <- RsquareAdj(mod)$adj.r.squared

# 筛选后得到的关键变量 ,可以使用这些重新去做RDA

envdata[,rownames(mod$CCA$biplot)] %>% mutate(sample= rownames(.)) %>% 
  select("sample", everything()) %>%
  write_tsv("筛选的环境因子.txt")

