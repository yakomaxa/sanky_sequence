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
  #df[,i+1]=paste0(df[,i+1],i+1)
  evaled=paste0("count(df,X",i,",X",i+1,") %>% as_tbl_graph()")
  es[[i]]=eval(parse(text=evaled)) 
}
#e2=df %>% count(X2,X3) %>% as_tbl_graph()
# bind_graph()ではnodeが重複するのでこの場合はgraph_join()を利用する
#e=c()
for (i in seq(1,len-1)){
  #es[[i]]=eval(parse(text=evaled)) 
  if (i==1){
    e=es[[i]]
  }else{
    e <- e %>% 
      graph_join(es[[i]])
  }
}


# networkD3をつかうのでindexを0始まりに変更
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

#jpeg(file = paste0(args[1],".pdf"))

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


#print(p2)
#dev.off()

saveWidget(p2, file=paste0(args[1],".html"))

