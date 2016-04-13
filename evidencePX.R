evidencePX <- function(likelihood, prior){
	
	prodofLLandPrior <- data.frame(likelihood$likelihood1*prior[1], likelihood$likelihood2*prior[2], likelihood$likelihood3*prior[3])
	rownames(prodofLLandPrior) <- c("X1", "X2", "X3", "X4")
	temp <- t(prodofLLandPrior)
	finalPX <- 0
	for(p in 1:dim(temp)[1]) {
		for(q in 1:dim(temp)[1]){
			for(r in 1:dim(temp)[1]) {
				for(s in 1:dim(temp)[1]) {
					finalPX = finalPX + temp[p,1]*temp[q,2]*temp[r,3]*temp[s,4]
				}
			}
		} 
	}
	print(paste("The final P(X) value is:", finalPX))
}