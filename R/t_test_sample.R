#' t test
#'
#' t test
#'
#' @export

t_test<-function(geno,pheno) {

	reject<-0
	pvalue<-rep(NA,nrow(geno))


	for(n in 1:nrow(geno))
	    pvalue[n] <- summary(lm(pheno~geno[n,]))$coefficients[2,4]

	for(n in 1:nrow(geno))
		if(pvalue[n]<0.05)	reject<-reject+1

	print(c("reject count in t-test is : ",reject))
	return(pvalue)
}
