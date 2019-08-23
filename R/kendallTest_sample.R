#' kendall test
#'
#' kendall test
#'
#' @export
#' @import Kendall

kendallTest<-function(geno,pheno) {
        reject<-0
	pvalue<-rep(NA,nrow(geno))

	for(n in 1:nrow(geno))
	   pvalue[n] <- Kendall(geno[n,], pheno)$sl

	for(n in 1:nrow(geno))
		if(pvalue[n]<0.05)	reject<-reject+1


	print(c("reject count in kendall.R is : ",reject))
	return(pvalue)
}
