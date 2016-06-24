getModel <-function(x,y,psi=1,weight.method='BIC',n_sim,n_train){

  if(weight.method=='ARM')
    if(missing(n_sim) || missing(n_train)){
      stop('missing n.sim or n.train')
    }
  n <- length(y)#sample number
  np <- dim(x)
  p <- np[2]#sample dimensonality
  if(is.factor(y)){
    #for classification
    #get candidate model by Lasso,MCP,SCAD
    lassofit <- glmnet(x = x, y = y, family = "binomial", alpha = 1, maxit = 1e+06) #nlambda = 100 默认选100个模型
    scadfit <- ncvreg(X = x, y = y, family = "binomial", penalty = "SCAD",
        warn = FALSE, max.iter = 1e+04)
    mcpfit <- ncvreg(X = x, y = y, family = "binomial", penalty = "MCP",
        warn = FALSE, max.iter = 1e+04)
    lasso.path <- as.matrix(lassofit$beta)
    scad.path <- as.matrix(scadfit$beta[-1, ])
    mcp.path <- as.matrix(mcpfit$beta[-1, ])
    beta.path <- t(cbind(lasso.path, scad.path, mcp.path))
    candidate_models <- (1 - (beta.path == 0))
    candidate_models <- unique(candidate_models)#candidate models
    rownames(candidate_models) <- NULL
    candidate_models <- candidate_models[order(rowSums(candidate_models)),]
    candidate_models <- candidate_models[which(rowSums(candidate_models)!=0),]
  }
  else{
    #for regression
    #get candidate model by Lasso,MCP,SCAD
    lassofit <- glmnet(x = x, y = y, family = "gaussian", alpha = 1, maxit = 1e+06)
    scadfit <- ncvreg(X = x, y = y, family = "gaussian", penalty = "SCAD",
        warn = FALSE, max.iter = 1e+04)
    mcpfit <- ncvreg(X = x, y = y, family = "gaussian", penalty = "MCP",
        warn = FALSE, max.iter = 1e+04)
    lasso.path <- as.matrix(lassofit$beta)
    scad.path <- as.matrix(scadfit$beta[-1, ])
    mcp.path <- as.matrix(mcpfit$beta[-1, ])
    beta.path <- t(cbind(lasso.path, scad.path, mcp.path))
    candidate_models <- (1 - (beta.path == 0))

    candidate_models <- candidate_models[which(rowSums(candidate_models)!=0),]
    candidate_models <- unique(candidate_models)#candidate models
    candidate_models <- candidate_models[order(rowSums(candidate_models)),]
  }


  if(is.factor(y)){
	if(weight.method != 'ARM'){
		weight = logitIC(x,y,candidate_models,psi,type=weight.method,prior = TRUE)
	}
	else{
		weight = logitARM(x,y,candidate_models, n_train, n_sim, psi, prior = TRUE)
	}
  }
  else{
	if(weight.method != 'ARM'){
		weight = lsIC(x,y,candidate_models,psi,prior=TRUE,type=weight.method)
	}
	else if(weight.method == 'ARM'){
		weight = lsARM(x,y,candidate_models,n_train=n_train,no_rep=n_sim,psi=psi,prior = TRUE)
	}
  }



  #
  candidate_models <- weight$candidate_models


  index <- vector()
  for(i in 1:dim(candidate_models)[1]){
    index <- c(index,which(candidate_models[i,]!=0))
  }
  index <- unique(index)
  list(weight=weight$weight,model=candidate_models,index=index)
}
