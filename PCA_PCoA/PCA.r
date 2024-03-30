library(tidyverse)
library(vegan)
library(ggsci)


# # 设置参数和读入数据 -------------------------------------------------------------
# 默认使用PC1和PC2, 如果要用PC3, PC4改一下就好
use <- c("PC1", "PC2")

# 是否标准化数据, 默认进行标准化
is.scale <- T

# 读入数据, 每行是一个样本, 样本列名必须是sample
lh <- read.table("LH.csv", header = T, sep = ",", row.names = 1) 

# 样本分组信息第一列是sample, 第二列是group信息
lh.gr <- read_csv("LH_group.csv")


# pca 分析  -------------------------------------------------------------------------

pca_lh <- rda(lh,scale = is.scale)

#plot(pca_lh$CA$eig,type = "b")
#abline(h=mean(pca_lh$CA$eig),lwd=1,col="blue")

# 提取坐标信息
pc.df <- pca_lh$CA$u[,c(use[1], use[2])] %>% 
  as.data.frame() %>%  
  mutate(sample = rownames(.)) %>% 
  left_join(lh.gr, by = "sample")

# 计算每个主成分的贡献度
conb <- map_df(pca_lh$CA$eig, function(x){round(100* x/sum(pca_lh$CA$eig),2)})

p <- ggplot(pc.df, aes_string(use[1], use[2], color = "group")) + geom_point(size =3.5)+
  geom_vline(xintercept = 0,color = "gray") +
  geom_hline(yintercept = 0,color = "gray") + 
  labs(x = str_glue("{use[1]}: {conb[use[1]]}%"), y = str_glue("{use[2]}: {conb[use[2]]}%"))+
  scale_color_aaas() + 
  theme(
    axis.text = element_text(color = "black", size = 10, face = "bold"),
    axis.title = element_text(color = "black", size = 10, face = "bold"),
    legend.text = element_text(color = "black", size = 9),
    legend.title = element_text(color = "black", size = 9, face = "bold"),
    panel.grid.major = element_line(color = "#EAEAEA", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(color = "black", size = 9, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "black", size = 9, hjust = 0.5),
    plot.caption = element_text(color = "black", size = 8, hjust = 1),
    plot.background = element_rect(fill = "white"),
    strip.text = element_text(color = "black", size = 8),
    strip.background = element_rect(fill = "#EAEAEA", color = NA),
    legend.position = "top"
  ) 
# 输出结果
if (!dir.exists("./out/pca/")) {
  dir.create("./out/pca/", recursive = T)
}

ggsave(filename = "./out/pca//pca.pdf", plot = p, device = "pdf", width = 5, height = 5)
write_tsv(conb, "./out/pca//eig.tsv")
write_tsv(pc.df,  "./out/pca//pca_table.tsv")
p