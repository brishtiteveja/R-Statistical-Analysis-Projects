multiply_P_Xj_y_1 <- function(){
	P_X12.._y_1 <- 1
	for(j in 1:53){
		P_X12.._y_1 <- P_X12.._y_1 * P_Xj_y_1(j)
	}
	
	return(P_X12.._y_1)
}

multiply_P_Xj_y_0 <- function(){
	P_X12.._y_0 <- 1
	for(j in 1:53){
		P_X12.._y_0 <- P_X12.._y_0 * P_Xj_y_0(j)
	}
	
	return(P_X12.._y_0)
}