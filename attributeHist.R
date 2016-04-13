
attributeHist <- function(name){
	house <- read.csv("housetype_data.txt")
	house$age <- factor(house$age)
	levels(house$age) <- c("14 thru 17", "18 thru 24", "25 thru 34", "35 thru 44", "45 thru 54", "55 thru 64", "65 and Over")
	house$eth <- factor(house$eth)
	levels(house$eth) <- c("American Indian", "Asian", "Black", "East Indian", "Hispanic","Pacific Islander", "White", "Other")
	
	#Maintaining a List Dictionary to plot the exact TITLE and X-LABEL
	attributes <- list(ht = "TYPE OF HOME", sex = "SEX", ms = "MARITAL STATUS", age = "AGE", edu = "EDUCATION", ocu = "OCCUPATION", inc = "ANNUAL INCOME OF HOUSEHOLD", ba = "HOW LONG SOMEONE HAS LIVED IN BAY AREA", di = "DUAL INCOMES", hs = "No of PERSONS IN THE HOUSEHOLD", hs2 = "PERSONS IN HOUSEHOLD UNDER 18", hhs = "HOUSEHOLDER STATUS", eth = "ETHNIC CLASSIFICATION", lang = "LANGUAGE MOST OFTEN SPOKEN IN HOME")
	
	#Checking if the passed argument matches the column name in data frame
	if (name %in% names(attributes)){
		
		#Checking the type of the attribute - If a 'factor' we need to plot(), 
		#if a 'numeric', we can use hist()
		if(class(house[[name]])=="numeric"){
			hist(house[[name]], main = paste("Histogram of", attributes[[name]]), xlab = attributes[[name]], col = 3)	
			print("Reached inside the numeric if condition")
		}
		if (class(house[[name]])=="factor"){
			print("Reached inside the factor if condition")
			plot(house[[name]], main = paste("Histogram of", attributes[name]), xlab = attributes[[name]], col = 4)
		}
		
		#calculating the number of missing values in the column
		miss <- sum(is.na(house[[name]]))
		
		#Displaying the missing values message on to the R console
		#if it is greater than 0
		if ( miss > 0){
			print(paste(miss, "missing values"))
		}
	}
	
	#In the case that the attribute passed is incorrect, the following message is 
	#printed on to the R console
	else {
		print(paste("There is no such attribute as ", name, ". Kindly pass the correct attribute", sep = ""))
	} 
	
	
}


