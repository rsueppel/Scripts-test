
#1.	Preparation

#1.1. Call packages
	
	rm(list=ls())
	
	library(rJava)
	options(java.parameters = "-Xmx1200m" )
	
	require(Management)
	
	require(zoo)
	require(xlsx)
	require(chron)
	Sys.setenv(TZ="GMT")
	
	enddate=as.Date("2016-08-31")
	
#1.2. Folder paths

	f_setup()
	folder_macro <- "M:/Trading/EXA/1. Macro Data/"
	
#1.3. Workbooks and sheets 
	
	l_wrk<-list(0)
	l_deets<-list(0)
	
	# Macro folder
	
	sub_folder<-"Indicators/Macro/"
	
	workbook<-"InterestRates_WM"
	n_from<-1
	sheet1<-"Output_D"
	sheet2<-"Output_M"
	l_wrk[[1]]<-c(workbook,sub_folder,n_from)
	l_deets[[1]]<-array(c(sheet1,sheet2,"d","m"),dim=c(2,2))
	
	workbook<-"MoneyReserves"
	n_from<-10
	sheet1<-"PrivateCredit"
	sheet2<-"FXReserves"
	l_wrk[[2]]<-c(workbook,sub_folder,n_from)
	l_deets[[2]]<-array(c(sheet1,sheet2,paste(sheet1,"m",sep="_"),paste(sheet2,"m",sep="_")),dim=c(2,2))
	
	workbook<-"ExternalBalances_WM"
	n_from<-3
	sheet_m<-"ExportM"
	l_wrk[[3]]<-c(workbook,sub_folder,n_from)
	l_deets[[3]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	workbook<-"Openness"
	n_from<-1
	sheet1<-"Openness"
	l_wrk[[4]]<-c(workbook,sub_folder,n_from)
	l_deets[[4]]<-array(c(sheet1,"m"),dim=c(1,2))
	
	workbook<-"Prices"
	n_from<-1
	sheet_m<-"ExportM"	
	l_wrk[[5]]<-c(workbook,sub_folder,n_from)
	l_deets[[5]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	workbook<-"MonetaryPolicyBiasMonitor"
	n_from<-1
	sheet_m<-"ExportM"	
	l_wrk[[6]]<-c(workbook,sub_folder,n_from)
	l_deets[[6]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	# Market folder
	
	sub_folder<-"Indicators/Market/"
	
	workbook<-"CommodityIndicators"
	n_from<-1
	sheet_m<-"ForDB"
	l_wrk[[7]]<-c(workbook,sub_folder,n_from)
	l_deets[[7]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	workbook<-"EquityMultiples_WM"
	n_from<-3
	sheet_m<-"Monthly"	
	l_wrk[[8]]<-c(workbook,sub_folder,n_from)
	l_deets[[8]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	workbook<-"Banks Lending Conditions"
	n_from<-1
	sheet_m<-"ExportM"
	l_wrk[[9]]<-c(workbook,sub_folder,n_from)
	l_deets[[9]]<-array(c(sheet_m,"m"),dim=c(1,2))
	
	workbook<-"EquilibriumEquityMultiples"
	n_from<-3
	sheet_m<-"All"
	l_wrk[[10]]<-c(workbook,sub_folder,n_from)
	l_deets[[10]]<-array(c(sheet_m,"m"),dim=c(1,2))

	workbook<-"NewEquityMultiples"
	n_from<-1
	sheet1<-"ForRMSCI"
	sheet2<-"ForRIBES"
	l_wrk[[11]]<-c(workbook,sub_folder,n_from)
	l_deets[[11]]<-array(c(sheet1,sheet2,paste(sheet1,"m",sep="_"),paste(sheet2,"m",sep="_")),dim=c(2,2))
	
#2.	Data files creation
	
	for(i in 1:length(l_wrk)){
		
		wrk<-l_wrk[[i]]
		deets<-l_deets[[i]]
		file_xls<-paste(folder_macro, wrk[2], wrk[1], ".xls",sep="")
		
		for(j in 1:dim(deets)[1]){
			
			sheet<-deets[j,1]
			df<-f_readbigexcel(fileName = file_xls, sheetName = sheet, startRow = as.numeric(wrk[3]), colClasses = c("character",rep("numeric",250)))
			df_export<-window(zoo(apply(df,c(1,2),as.numeric),as.Date(rownames(df),"%d/%m/%Y")),end=enddate)
			coredata(df_export)<-replace(coredata(df_export),is.nan(coredata(df_export)),NA)	# subsistute NAs instead of NaN
			
			for(k in 1:dim(df_export)[2]){
				df_check<-na.trim(df_export[,k],sides="left")
				if(sum(is.na(df_check))!=0){
					print(paste("UPDATE ERROR: the variable",names(df_export)[k],"is not update on the below date(s):",sep=" "))
					print(index(df_check)[which(is.na(df_check))],sep=" ")
				}
			}
			save(df_export,file=paste(folder.data, paste(wrk[1], deets[j,2], sep="_"), ".RData", sep=""))			
			gc()		#reinstates heap memery after each file
		}
		
	}
	