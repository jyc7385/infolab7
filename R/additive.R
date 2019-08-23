#' adiitive
#'
#' additive
#'
#' @export

additive <- function(geno, n, samp, p, pi)
{
  pheno <- matrix(data=1, nrow=samp, ncol=1)

  pheno<- sqrt(1-pi)*rnorm(n=samp, mean=0, sd=1)+geno[1,]*sqrt(pi/(2*p*(1-p)))
  write.table(pheno, file="sample_pheno_add.txt", row.names=F, col.names=F, quote=F,sep="\t")

  return (pheno=pheno)
}
