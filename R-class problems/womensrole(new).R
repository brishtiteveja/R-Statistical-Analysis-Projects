	data("womensrole",package = "HSAUR2")
	womensrole
	fm1 <-  cbind(agree,disagree) ~ gender + education
	womensrole[1:30,]
	womensrole_glm_1 <- glm(fm1,data = womensrole,family=binomial())
	summary(womensrole_glm_1)
	coef(womensrole_glm_1)
	
	 myplot <- function(role.fitted){
 		f <-womensrole$gender ==  "Female"
 		plot(womensrole$education,role.fitted,type = "n",ylab = "Probability of agreeing",xlab= "Education",ylim = c(0,1))
 		lines(womensrole$education[!f],role.fitted[!f],lty=1)
 		lines(womensrole$education[f],role.fitted[f],lty=2)
 		lgtxt <- c("Fitted (Males)","Fitted (Females)")
 		legend("topright",lgtxt,lty = 1:2,bty = "n")
 		y <- womensrole$agree/ (womensrole$agree + womensrole$disagree)
 		text(womensrole$education,y,ifelse(f,"\\VE","\\MA"),family = "HersheySerif",cex = 1.25)
 	}

	fm2 <- cbind(agree,disagree) ~ gender * education
 	womensrole_glm_2 <- glm(fm2,data = womensrole,family = binomial())

	fm3 <- cbind(agree,disagree) ~ gender + education + gender*education
	womensrole_glm_3 <- glm(fm3,data = womensrole,family = binomial())


	role.fitted1 <- predict(womensrole_glm_1,type = "response")
	myplot(role.fitted1)
	role.fitted2 <- predict(womensrole_glm_2,type = "response")
	myplot(role.fitted2)
	


   