# I have adjusted the code of the kknn package (http://cran.r-project.org/web/packages/kknn/index.html) to quickly compute
# the neighbors from 1 to MAXK for a set of input x.

dyn.load("dm.so")

fastkknn <- function (formula = formula(train), train, test, na.action = na.omit(), 
k = 7, distance = 2, inc = 2, allk = FALSE) 
{
	maxk <- max(k)
    mf <- model.frame(formula, data = train)
    mt <- attr(mf, "terms")
    formula2 <- as.formula(mf)
    formula2[[2]] <- NULL
    mt2 <- terms.formula(formula2, data = train)
    cl <- model.response(mf)
	
    if (distance <= 0) 
	stop("distance must >0")
    if (any(k <= 0)) 
	stop("k must >0")
    learn <- model.matrix(mt, mf)
    valid <- model.matrix(mt2, test)
	
	
    m <- dim(learn)[1]
    p <- dim(valid)[1]
    q <- dim(learn)[2]
    D <- matrix(0, nrow = p, ncol = maxk + 1)
    CL <- matrix(0, nrow = p, ncol = maxk + 1)
    ind <- attributes(valid)$assign
    d.sd <- numeric(length(ind)) + 1
    we <- numeric(length(ind)) + 1
    for (i in 1:max(ind)) {
        d.sd[ind == i] = sqrt(mean(diag(cov(as.matrix(learn[, 
													  ind == i])))))
        we[ind == i] = 1/sum(ind == i)
    }
    we[d.sd == 0] = 0
    d.sd[d.sd == 0] = 1
    learn <- t(t(learn)/d.sd)
    valid <- t(t(valid)/d.sd)
    dmtmp <- .C("dm", as.double(learn), as.double(valid), as.integer(m), 
				as.integer(p), as.integer(q), dm = as.double(D), cl = as.integer(CL), 
				k = as.integer(maxk + 1), as.double(distance), as.double(we))
    D <- matrix(dmtmp$dm, nrow = p, ncol = maxk + 1)
    C <- matrix(dmtmp$cl, nrow = p, ncol = maxk + 1)
	
	
	res <- matrix(NA,nrow=k,ncol=p)
	W2 <- matrix(0,nrow=nrow(D),ncol=ncol(D))
	Output <- matrix(cl[C+1], nrow = nrow(C), ncol = ncol(C))
	
	tot <- .C("knnpred",as.double(Output),as.double(D),as.double(W2),as.double(res),as.integer(p),as.integer(k),as.integer(inc),as.integer(allk),NAOK = TRUE)
	fit <- matrix(tot[[4]],nrow=k,ncol=p)
			
	fit
}



# NA in res + NAOK=TRUE do the same	:
#	if(allk ){
#		fit[-seq(1,maxk,by=inc),] <- NA
#	}

#	if(!allk && maxk>1){
#		fit[seq(1,maxk-1),] <- NA
#	}
