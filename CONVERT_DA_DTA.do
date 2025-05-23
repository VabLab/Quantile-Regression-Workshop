clear all

* Convert
	clear all
	infile using "TRK2022TR_R.dct", using("TRK2022TR_R.da")
	save TRK2022TR_R.dta, replace
	
*dct files come from stata folder on HRS and da files come from data files in HRS
