	data("womensrole",package = "HSAUR2")
	fm1 <-  cbind(agree,disagree) ~ gender + education
	womensrole[1:30,]
	womensrole_glm_1 <- glm(fm1,data = womensrole,family=binomial())
	summary(womensrole_glm_1)
	coef(womensrole_glm_1)
	?glm
	
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
	role.fitted1
	myplot(role.fitted1)
	role.fitted2 <- predict(womensrole_glm_2,type = "response")
	myplot(role.fitted2)
	
	bgender = ifelse(womensrole$gender == "Female", 0, 1)
	bgender
	womensrole$bgender = bgender
	fm4 = cbind(agree,disagree) ~ bgender + education + bgender * education 
	womensrole_glm_4 <- glm(fm4,data = womensrole,family = binomial())
	role.fitted4 <- predict(womensrole_glm_4,type="response")
	myplot(role.fitted4)
	
	summary(womensrole_glm_2)
	
	summary(womensrole_glm_4)

   