# Society for Epidemiologic Research (SER) Quantile Regression Workshop
#### By Jilly Hebert, Amanda Irish, and Anusha Vable

Quantile regression is a powerful method of evaluating how an exposure affects the entire outcome distribution; this is distinct from analyses on how exposures impact the means, which are more common in the epidemiological literature. However, quantile regression remains underused in epidemiology. Our workshop has two aims: 1) introduce participants to quantile regressions with a focus on distinguishing between estimators targeted at the conditional versus marginal outcome distribution; and 2) equip participants to conduct quantile regression analyses in statistical packages such as R or Stata. Our workshop has three phases. Phase 1 will be theoretically oriented and will provide participants with knowledge of quantile regression theory. Phase 2 will be empirically oriented and will provide participants with hands-on experience of fitting quantile regressions to a dataset we will provide. Phase 3 will sketch extensions to quantile regressions, such as quantile regression estimators for longitudinal data. Phases 1 and 2 will form the bulk of the workshop. All sessions will be interactive to encourage hands-on experience and learning. Our workshop is targeted at beginners, i.e., individuals with no prior experience of using quantile regressions. However, we do presume that participants have a working knowledge of linear regressions since we will use mean models (ordinary least squares regression) as the starting point to develop ideas about quantile regressions. 

While no longer with our team, this workshop would not be possible without Aayush Khadka, first author of our didactic paper.


# Repository Content

- `README.md`: This file with an explanation of the workshop.
- `2024 SER Workshop.pptx`: This is a copy of the 2024 SER Workshop presentation.
- `QRHandout.Rmd`: This is an R markdown file that creates the handout (also provided in HTML and PDF format) with code and descriptions. Knit to HTML **only** (due to the animation plot) and allow 10-15 minutes to compile; if the markdown fails to compile to HTML, check that all packages are installed and that you are using the most up to date version of R and R Studio.
- `QRHandout.html`: This is the HTML version of the QR Handout when knit in R.
- `Pull SPB Measures.R`: This is an R script to pull and clean SPB measures from the Health and Retirement Study RAND data.
- `Create Dataset.R`: This is an R script to pull, merge, and clean covariate data from the Health and Retirement Study Tracker and RAND datasets.
- `R_QR.R`: An example script file implementing quantile regression in R.
- `STATA_QR.do`: An example Do file implementing quantile regression in STATA.
- `2023 SER Workshop` Folder: Contains the PowerPoint presentation of the 2023 SER workshop, the R markdown Handout file, and the PDF version of the Handout.

Note that no datasets will be made available on this repository due to data usage restrictions. All data can be accessed through the Health and Retirement Study (HRS) public survey files. Files include the Cross-Wave Tracker file (STATA) and the 2018 RAND HRS Longitudinal File (STATA). Tracker data has been update past 2018, so please note your compiled dataset might look different since we use 2018 Tracker data. 


# Contact Information

Jilly Hebert jilly.hebert@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco

Amanda Irish amanda.irish@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco

Sachi Taniguchi sachi.taniguchi@ucsf.edu
VabLab Research Community Operations Manager
Department of Family and Community Medicine, University of California, San Francisco


# References

Health and Retirement Study, (Tracker and RAND) public use dataset. Produced and distributed by the University of Michigan with funding from the National Institute on Aging (grant number NIA U01AG009740). Ann Arbor, MI, (2023). URL https://hrs.isr.umich.edu/data-products

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/

StataCorp. 2021. Stata Statistical Software: Release 17. College Station, TX: StataCorp LLC.

