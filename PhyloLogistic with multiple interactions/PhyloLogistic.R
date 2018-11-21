phyloLogistic.logPL = function(theta, X, treeM, Graphs, Z)
{
    p = length(theta)
	n_thr = ncol(X)
	beta = theta[1:n_thr]
	nmatrix = length(Graphs)
    Ives_alpha = theta[nthr + 1]
	eta_tree = theta[nthr + 2]
	eta_other = theta[nthr+2+1:nmatrix]
	
	A = eta_tree * exp(-abs(Ives_alpha) * treeM)
	for(i in 1:nmatrix){
		A = A + eta_other[i] * Graphs[[i]]
	}
	
    # eta = theta[p]
    Xbeta = X %*% beta
    mu = exp(Xbeta) / (1 + exp(Xbeta))
    logPL = Xbeta + eta * A %*% (Z - mu)
    logPL = t(Z) %*% logPL - sum(log(1 + exp(logPL)))
    -logPL
}

phyloLogistic.fit = function(Z,phylotree,data,Graphs){
	treeM = Ives_standardizetree(phylotree)
	stopifnot(sum(!sapply(Graphs,isSymmetric.matrix))==0)
	stopifnot(sum(sapply(Graphs,nrow)!=length(Z)))
	stopifnot(sum(sapply(Graphs,ncol)!=length(Z)))
	X = cbind(matrix(1,nrow = nrow(data),ncol = 1),as.matrix(data))
	p = ncol(X) + 2 + length(Graphs)
	theta_ini = runif(p)
	res = optim(theta_ini,phyloLogistic.logPL,X,treeM,Graphs,Z)

}