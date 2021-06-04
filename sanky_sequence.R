#REF
# Sanky plot
# https://qiita.com/flatsilver/items/df0543ff442c49bf8120
# Save HTML 
# https://stackoverflow.com/questions/56086341/how-to-save-simplenetwork-output-from-networkd3-in-pdf-jpeg-tiff-format

library(tibble)
library(dplyr)
library(tidygraph)
library(networkD3)
require(htmlwidgets)
require(bio3d)


args=commandArgs(T)
#f=read.fasta("~/tst.fasta")
f=read.fasta(args[1])
df=data.frame(f$ali)
len=length(df[1,])
es=list()
for (i in seq(1,len)){
  df[,i]=paste0(df[,i],i)
}

for (i in seq(1,len-1)){ 
  evaled=paste0("count(df,X",i,",X",i+1,") %>% as_tbl_graph()")
  es[[i]]=eval(parse(text=evaled)) 
}

for (i in seq(1,len-1)){

  if (i==1){
    e=es[[i]]
  }else{
    e <- e %>% 
      graph_join(es[[i]])
  }
}

edge <- e %>% 
  activate(edges) %>%
  as_tibble() %>%
  mutate(
    from = from - 1,
    to = to - 1
  )


node <- e %>% 
  activate(nodes) %>% 
  as_tibble()

p2=sankeyNetwork(
  Links = edge,
  Nodes = node,
  Source = "from",
  Target = "to",
  Value = "n",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30
)

saveWidget(p2, file=paste0(args[1],".html"))

