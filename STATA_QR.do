/*
	Title:		Quantile Regression STATA Code
	Author:		Aayush Khadka, Jilly Hebert, Amanda Irish, and Anusha Vable
	Institution: University of California, San Francisco
*/
	
	****************************************************************************
	clear all
	
	use QRWorkshop.dta, clear
	
	****************************************************************************
	
	** Mean Model
	
	****************************************************************************
	
	** Create OLS results sheet
	putexcel set qr_results, modify sheet("OLS")
	putexcel A1 = "Quantile"	
	putexcel B1 = "Estimate" //Saves school year estimate
	putexcel C1 = "Lower"	
	putexcel D1 = "Upper"	
	
	** Bootstrap 95% CI's
	regress sbp c.schlyrs c.age c.age2 i.gender i.race c.rameduc c.rafeduc //
	i.southern i.year, vce(boostrap, reps(500))
	
	** Extracting results
	matrix param = r(table)	
	
	** Storing results
	putexcel A2 = -0.10
	putexcel B2 = param[1,1]
	putexcel C2 = param[5,1]
	putexcel D2 = param[6,1]
	
	
	****************************************************************************
	
	** Conditional Quantile Regression (CQR)
	
	****************************************************************************
	
	** Create CQR results sheet
	putexcel set qr_results.xlsx, modify sheet("CQR")
	putexcel A1 = "Quantile"			
	putexcel B1 = "Estiamte" //Saves school year estimate
	putexcel C1 = "Lower"	
	putexcel D1 = "Upper"	
	
	** Creating a counter
	local c = 2
	
	**Bootstrap 95% CIs
	forval i=0.01(0.01)0.99 {
		
		bsqreg sbp c.schlyrs c.age c.age2 i.gender i.race //
		c.rameduc c.rafeduc i.southern i.year, quantile(`i') reps(500)
		
		** Extracting results
		matrix param = r(table)
		
		** Storing results
		putexcel A`c' = `i'*100
		putexcel B`c' = param[1,1]
		putexcel C`c' = param[5,1]
		putexcel D`c' = param[6,1]
		
		** Updating counter
		local c = `c' + 1		
		
	}
	
	
	****************************************************************************
	
	** Unconditional Quantile Regression (UQR)
	
	****************************************************************************
	
	** Create UQR results sheet
	putexcel set qr_results.xlsx, modify sheet("UQR")
	putexcel A1 = "Quantile"			
	putexcel B1 = "Estimate" //Saves school year estimate
	putexcel C1 = "Lower"	
	putexcel D1 = "Upper"	
	
	** Creating a counter
	local c = 2
	
	**Bootstrap 95% CI's
	forval i=1(1)99 {
		
		rifhdreg sbp c.schlyrs c.age c.age2 i.gender i.race c.rameduc //
		c.rafeduc i.southern i.year, rif(q(`i')) vce(bootstrap, reps(500))
		
		** Extracting results
		matrix param = r(table)
		
		** Storing results
		putexcel A`c' = `i'
		putexcel B`c' = param[1,1]
		putexcel C`c' = param[5,1]
		putexcel D`c' = param[6,1]
		
		** Updating counter
		local c = `c' + 1		
		
	}
	
	
	