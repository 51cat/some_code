library(tidyverse)
library(vegan)
library(ggsci)
# 读取数据

otu_table <- read.csv('20240307-菌种.txt', header = TRUE, sep = '\t', row.names = 1) %>% t() %>% as.data.frame()
envdata <- read.table('筛选的环境因子.txt', sep = '\t', header = TRUE, row.names = 1)
group <- read_tsv("20240307-group2.txt")

#如果前4个轴的所有轴长度均小于3则选择RDA分析，如果4个轴中存在一个轴的长度大于4则选择CCA分析
decorana(otu_table)

# RDA分析
res <- rda(otu_table ~ ., envdata, scale =F)#RDA分析
res
r2 <- RsquareAdj(res)$adj.r.squared

res.sum <- summary(res)
importance_df <- res.sum$concont$importance %>% as.data.frame()

# 贡献度
proportion <- c(rda1 = round(100 * importance_df$RDA1[2], 2), 
                rda2 = round(100 * importance_df$RDA2[2], 2))
# 提取数据
# 样本
pdat <- res$CCA$u %>% 
  as.data.frame() %>% 
  mutate(sample = rownames(.)) %>% 
  left_join(group, by = "sample")

# 物种
spec <- res$CCA$v %>% 
  as.data.frame() %>% 
  mutate(spec = rownames(.))

# 环境信息
env <- res$CCA$biplot %>% 
  as.data.frame() %>% 
  mutate(feature = rownames(.))

# 画图
p1 <- ggplot(pdat, aes(RDA1, RDA2)) +
  geom_point(aes(color = group),size=3) + 
  scale_color_aaas()+
  geom_hline(aes(yintercept = 0), colour="gray88", size = 1,linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", size = 1,linetype="dashed")  +
  
  geom_segment(data = env,aes(x=0, xend= RDA1, y=0, yend= RDA2 ),size = 1,arrow = arrow(length = unit(0.5, "cm")), colour = 'gray20', alpha = 1) +
  
  geom_text(data = env,aes(x = RDA1*1.1, y = RDA2*1.1, label = feature), size = 5, colour = 'red', check_overlap = TRUE) +
  labs(x = str_glue("RDA1: {proportion['rda1']}%"), y =str_glue("RDA2: {proportion['rda2']}%")) + 
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

p1

p2 <- ggplot(pdat, aes(RDA1, RDA2)) +
  geom_point(aes(color = group),size=3) + 
  scale_color_manual(values = c('red', 'orange', 'green3',"black","green","red","blue")) +
  geom_hline(aes(yintercept = 0), colour="gray88", size = 1,linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", size = 1,linetype="dashed")  +
  
  geom_point(data = spec, aes(x=RDA1, y=RDA2),size = 0.5, colour = 'pink', pch = 3) +#删除, colour = season, shape = foreststructure
  geom_segment(data = spec,aes(x=0, xend= RDA1, y=0, yend= RDA2 ), size = 1, arrow = arrow(length = unit(0.0, "cm")), colour = 'blue') +
  geom_text(data = spec,aes(x = RDA1*1.1, y = RDA2*1.1, label = spec), size = 4, colour = 'blue') +
  
  geom_segment(data = env,aes(x=0, xend= RDA1, y=0, yend= RDA2 ),size = 1,arrow = arrow(length = unit(0.3, "cm")), colour = 'red') +
  geom_text(data = env,aes(x = RDA1*1.1, y = RDA2*1.1, label = feature), size = 5, colour = 'red', check_overlap = TRUE) +
  
  #geom_point(data = samples, aes(x=RDA1, y=RDA2),size = 3)  +#删除, colour = season, shape = foreststructure
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) + 
  
  labs(x = str_glue("RDA1: {proportion['rda1']}%"), y =str_glue("RDA2: {proportion['rda2']}%"))
p2


ggsave(filename = "./out//rda1.pdf", plot = p1, device = "pdf",width = 5, height = 5)
ggsave(filename = "./out//rda2.pdf", plot = p2, device = "pdf",width = 6, height = 5)



