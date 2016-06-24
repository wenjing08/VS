library(glmvsd)

VS <- function(x,y,cutoff=0.5,weight.method,n.sim=100,n.train){
  threshold <- 0.3
  if(missing(weight.method))
    stop('missing weight.method')
  if(weight.method=='ARM' && missing(n.train))
    stop('missing n.train')
  object <-getModel(x,y,weight.method=weight.method,n_sim=n.sim,n_train=n.train)
  cadidate_model <- object$model
  w <- diag(object$weight)
  model <- w %*% object$model
  m <- colSums(model)
  best <- as.numeric(m>cutoff)
  o <- glmvsd(x,y,method='customize',model_check=best,candidate_models=object$model,weight_type = 'BIC',prior=TRUE)
  VSD <- o$VSD
  VSD_minus <- o$VSD_minus
  VSD_plus <- o$VSD_plus
  model <- as.numeric(m>threshold)
  list(VSD=VSD,VSD_minus=VSD_minus,VSD_plus=VSD_plus,best=best,model=model,m=m,weights=object$weight,cadidate_model=cadidate_model)
}
