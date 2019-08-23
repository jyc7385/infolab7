#' pca
#'
#' pca
#'
#' @export

pca <- function(geno, pc_num)
{
	pc <-prcomp(geno, center=T, scale=T)

        print("eigenvalue sum is ")
		print(sum(pc$sdev*pc$sdev))

	pca_eval <-pc$sdev
	write.table(pca_eval,file="PCA_eigenvalues.txt",row.names=F,col.names=F,quote=F,append=F,sep="\t")

	pca_evec<-pc$rotation[,1:pc_num]

	write.table(pca_evec,file="PCA_eigenvectors.txt",row.names=F,col.names=F,quote=F,append=F,sep="\t")

	return(list(value=pca_eval, vec=pca_evec))
}

