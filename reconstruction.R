require(ape)
nexustree = read.nexus("./data/paup_ML_allbest.trees")
elevation = read.csv(file.choose())
elevation$la_name = as.character(elevation$la_name)

spclist = nexustree$tip.label
spclist = sub("_"," ",spclist)

mean.ele = matrix(0,nrow = length(spclist),ncol = 1)
var.ele = matrix(0,nrow = length(spclist),ncol = 1)

for(i in 1:(length(spclist))){
  temp = elevation[elevation$la_name==spclist[i],]
  if(nrow(temp)!=0 & sum(!is.na(temp$ele))>0){
  mean.ele[i] = mean(na.omit(temp$ele))
  var.ele[i] = var(na.omit(temp$ele))
  }
}

ou.model = lapply(nexustree,function(x,tree){tryCatch(compar.ou(x,tree),error = function(e){NA})},x=as.vector(var.ele))
aces = lapply(nexustree,function(tree,x){tryCatch(ace(x,tree),error = function(e){NA})},x=as.vector(mean.ele))

aces = ace(as.vector(mean.ele),nexustree)

plot(nexustree)
nodelabels(text = as.character(ceiling(aces$ace)),bg = "transparent",frame = "none")
tiplabels(text = as.character(ceiling(mean.ele)),bg = "transparent",frame = "none",adj = -10)
