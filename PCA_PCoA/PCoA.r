library(tidyverse)
library(vegan)
library(ggsci)


# # 设置参数和读入数据 -------------------------------------------------------------
# 默认使用PCoA1和PCoA1, 如果要用PCoA3, PCoA4改一下就好
use <- c("PCoA1", "PCoA2")
dist.use <- "euclidean"

# 是否标准化数据, 默认进行标准化
is.scale <- T

# 读入数据, 每行是一个样本, 样本列名必须是sample
lh <- read.table("LH.csv", header = T, sep = ",", row.names = 1) 

# 样本分组信息第一列是sample, 第二列是group信息
lh.gr <- read_csv("LH_group.csv")

# # PCoA分析 ----------------------------------------------------------------

#lh <- lapply(lh, scale) %>% as.data.frame()

if (is.scale) {
  for (col in colnames(lh)){
    lh[[col]] <- scale(lh[[col]])
  }
}

lh.dist <- vegdist(lh, method = dist.use) %>% as.matrix()
pcoa <- cmdscale(lh.dist,k = nrow(lh.dist)-1,eig = T)

pcoa.df <- pcoa$points %>% as.data.frame()
colnames(pcoa.df) <- str_c("PCoA", c(1:ncol(pcoa.df))) 

conb <- map(pcoa$eig, function(x){round(100* x/sum(pcoa$eig),2)}) %>% as.data.frame()
colnames(conb) <- str_c("PCoA", c(1:ncol(conb))) 

pcoa.df <- pcoa.df %>% 
  mutate(sample = rownames(.))%>%
  left_join(lh.gr, by = "sample")

p <- ggplot(pcoa.df, aes_string(use[1], use[2], color = "group")) + geom_point() + geom_point(size =3.5)+
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
p
# 输出结果
if (!dir.exists("./out/PCoA/")) {
  dir.create("./out/PCoA/", recursive = T)
}

ggsave(filename = "./out/PCoA//PCoA.pdf", plot = p, device = "pdf", width = 5, height = 5)
write_tsv(conb, "./out/PCoA/eig_PCoA.tsv")
write_tsv(pcoa.df,  "./out/PCoA/PCoA_table.tsv")
