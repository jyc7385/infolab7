#' GWAS main
#'
#' GWAS main
#'
#' @export

GWAS_main<-function(Y,G,pc_num) {
	if(is.null(Y))	stop("Phenotypes must exist.")
	if(is.null(G))	stop("Genotypes must exist.")

	geno_num <-as.matrix(G)
	geno_pos <-1:nrow(geno_num)
	geno_chr <-rep(1,nrow(geno_num))

	pheno_num<-as.matrix(Y)
	print("Genotype encoding complete.")

	pca_evec <- pca(geno_num, pc_num)
	print("PCA complete.")


	adj_geno<-adjgeno(geno=geno_num,evec=pca_evec$vec,pc_num)
        write.table(adj_geno,file="adjusted_genotype.txt",row.names=F,col.names=F,quote=F,append=F,sep="\t")

	print("Adjusted Genotype matrix complete.")

	kpv<-kendallTest(geno=adj_geno,pheno=pheno_num)
	ken_pv <-cbind(geno_chr,geno_pos,kpv)
        write.table(ken_pv,file="kendall_test.txt",row.names=F,col.names=F,quote=F,append=F)
	print("Kendall Test complete.")

	tpv<- t_test(geno=adj_geno,pheno=pheno_num)
	t_pv <-data.frame(geno_chr,geno_pos, tpv)
        write.table(t_pv,file="t_test.txt",row.names=F,col.names=F,quote=F, append=F)
	print("t-test Test complete.")

	adj_kpv<-p.adjust(kpv,method="BH")
        print(c("reject count using adjusted p-value is ",sum(adj_kpv<0.05)))

	adj_tpv<-p.adjust(tpv,method="BH")
        print(c("reject count using adjusted p-value is ",sum(adj_tpv<0.05)))

	adj_kpv <-data.frame(geno_chr,geno_pos, adj_kpv)
	adj_tpv <-data.frame(geno_chr,geno_pos, adj_tpv)

	write.table(adj_kpv,file="adj_kendall_test.txt",sep="\t",row.names=F,col.names=F,quote=F,append=F)
	write.table(adj_tpv,file="adj_t_test.txt",sep="\t",row.names=F,col.names=F,quote=F,append=F)
	print("FDR Test complete.")
}
