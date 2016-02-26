count_dat <- df.cor.rev[,5:14]

count_mat <- matrix(data = NA, ncol = ncol(count_dat),nrow=ncol(count_dat))
colnames(count_mat) <- colnames(count_dat)
rownames(count_mat) <- colnames(count_dat)

for(i in 1:nrow(count_mat)){
  row_met = rownames(count_mat)[i]
  sub_ves <- count_dat[which(!is.na(count_dat[,i])),]
  for(j in i:ncol(count_mat)){
  count_mat[i,j] <-  length(which(!is.na(sub_ves[,j])))
  }
}
