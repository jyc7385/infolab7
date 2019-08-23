#' adjgeno
#'
#' adjgeno
#'
#' @export

adjgeno<-function(geno,evec,pc_num) {
	evec<-as.matrix(evec)

	geno1<-geno%*%evec[,1:pc_num]%*%t(evec[,1:pc_num])

	geno <- geno - geno1;

	return(geno)
}
