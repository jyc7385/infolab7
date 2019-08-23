#' genotype data
#'
#' genotype data
#'
#' @export

genotype <- function(n,samp,p)
{
	geno <- matrix(data=1, nrow=n, ncol=samp)

  for(i in 1: n)
	   geno[i,] <-sample(-1:1,samp, replace=TRUE,prob=c((1-p)*(1-p),2*p*(1-p),p*p))


  write.table(geno, file="sample_genotype.txt", row.names=F, col.names=F, quote=F,sep="\t")

	return(geno)
}
