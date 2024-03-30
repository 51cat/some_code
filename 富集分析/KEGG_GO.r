
library(clusterProfiler)
library(tidyverse)
library(org.Hs.eg.db)

to.id <- function(gene.table){
  eg <- bitr(
    geneID = gene.table$gene, 
    fromType = "SYMBOL",
    toType=c("ENTREZID","ENSEMBL"), 
    OrgDb="org.Hs.eg.db")
  return(eg)
}

go <- function(gene.table, out, sameple.name, sufx) {
  dir.create(str_glue("{out}/{sameple.name}/go/"))
  eg <- to.id(gene.table)
  
  go <- enrichGO(
    eg$ENTREZID, 
    OrgDb = org.Hs.eg.db, 
    ont='ALL',
    pAdjustMethod = 'BH',
    pvalueCutoff = 0.05, 
    qvalueCutoff = 0.2, 
    keyType = 'ENTREZID')
  res.df <- go@results
  if (nrows(go@results %>% filter(p_adj <= 0.05)) != 0) {
    p.go <- dotplot(go,showCategory = 30)
    ggsave(plot = p.go, filename = str_glue("{out}/{sample.name}/{sample.name}_{sufx}_go.txt"))
  }else{
    warning(str_glue("NOT Found any GO pathways in {sameple.name} {sufx}"))
  }
  write_tsv(res.df, str_glue("{out}/{sample.name}/{sample.name}_{sufx}_GO.txt"))
  
}

kegg <- function(gene.table, out, sameple.name, sufx) {
  dir.create(str_glue("{out}/{sameple.name}/kegg/"))
  eg <- to.id(gene.table)
  
  kegg <- enrichKEGG(
    gene = eg$ENTREZID, 
    organism = "hsa", 
    keyType = "kegg", 
    pvalueCutoff = 0.05,
    pAdjustMethod = "BH",
    minGSSize = 10, 
    maxGSSize = 500,
    qvalueCutoff = 0.2,
    use_internal_data = FALSE)
  
  res.df <- kegg@results
  if (nrows(kegg@results %>% filter(p_adj <= 0.05)) != 0) {
    p.kegg <- dotplot(kegg,showCategory = 30)
    ggsave(plot = p.kegg, filename = str_glue("{out}/{sample.name}/{sample.name}_{sufx}_go.txt"))
  }else{
    warning(str_glue("NOT Found any KEGG pathways in {sameple.name} {sufx}"))
  }
  write_tsv(res.df, str_glue("{out}/{sample.name}/{sample.name}_{sufx}_KEGG.txt"))
}