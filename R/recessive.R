#' recessive
#'
#' recessive
#'
#' @export

recessive <- function(geno, n, samp, p, pi)
  {

  geno <- matrix(data=1, nrow=n, ncol=samp)
  pheno_rec<- matrix(data=1, nrow=samp, ncol=1)

  geno_rec <- geno
  geno_rec<-apply(geno_rec,2,function(x) {ifelse(x==-1, x < -1, x <- 0)})
  pheno_rec<- sqrt(1-pi)*rnorm(n=300, mean=0, sd=1)+geno_rec[1,]*sqrt(pi/(p*(1-p)))

  write.table(pheno_rec, file="sample_phenotype_rec.txt", row.names=F, col.names=F, quote=F,sep="\t")

  return (pheno_rec=pheno_rec)
}
