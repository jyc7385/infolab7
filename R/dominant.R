#' dominant
#'
#' dominant
#'
#' @export

dominant <- function(geno, n, samp, p, pi)
{
  geno <- matrix(data=1, nrow=n, ncol=samp)
  pheno_dom <- matrix(data=1, nrow=samp, ncol=1)

  geno_dom <- geno
  geno_dom<-apply(geno_dom,2,function(x) {ifelse(x==-1, x <- 0, x <- 1)})
  pheno_dom<- sqrt(1-pi)*rnorm(n=300, mean=0, sd=1)+geno_dom[1,]*sqrt(pi/(p*(1-p)))

  write.table(pheno_dom, file="sample_phenotype_dom.txt", row.names=F, col.names=F, quote=F,sep="\t")

	return (pheno_dom=pheno_dom)
}
