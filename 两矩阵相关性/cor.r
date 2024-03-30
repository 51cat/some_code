library(corrplot)
library(tidyverse)
library(psych)
library(pheatmap)

spec <- read.table("B20240315spe_table.txt", header = T, row.names = 1) %>% t()
env <- read.table("20240315env_table.txt", header = T, row.names = 1) %>% t()
env

corr.res <- psych::corr.test(spec, env, method = 'pearson')

cor.mtx <- t(corr.res$r)
p.mtx <- t(corr.res$p)


# 这个地方可以自己调p的阈值
p.mtx[p.mtx > 0.05] <- NA
p.mtx[p.mtx <= 0.001] <- "***"
p.mtx[p.mtx <= 0.01 & p.mtx>0.001] <- "**"
p.mtx[(p.mtx <= 0.05 & p.mtx>0.01)] <- "*"
p.mtx[is.na(p.mtx)] <- ''

# 改热图颜色
# 后面随便填颜色，颜色数量大于等于2就行
color = colorRampPalette(c("navy", "white", "firebrick3"))(50)


# 显示星号
pheatmap(
  cor.mtx,
  border = F,
  display_numbers = p.mtx,
  number_color = "black",
  fontsize_number = 12,
  # 去掉下面的注释修改颜色
  #color = color
  )

# 显示r值
pheatmap(
  cor.mtx,
  border = F,display_numbers = T,
  number_color = "black",
  fontsize_number = 12,
  # 去掉下面的注释修改颜色
  #color = color
  )

