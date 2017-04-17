#
# Inflation Pressure Table for G3
#
	
	#A. Preliminary preparations
	
	Sys.setenv(TZ="GMT")
	rm(list=ls())
		
	require(zoo)
	
	require(Management)
	
	f_setup()
	
	#m_sfu<-"M:/Trading/EXA/2. Quantitative Research/2. Support functions/"
	#m_data<-"M:/Trading/EXA/1. Macro Data/AllData/"	
	#folder_data<-m_data
	#folder_sfu<-m_sfu
	
	#source(paste(folder_sfu,"SeriesTransformer.r",sep=""))
	#source(paste(folder_sfu,"AdvancedTransformer.r",sep=""))
	#source(paste(folder_sfu,"Transparency.r",sep=""))
	#source(paste(folder_sfu,"DataLoader.r",sep=""))
	
	id_g3 <- c("USD","EUR","JPY")
	cat_cpi <- c("CPIHO_SA","CPICO_SA","CPIHN_SA","CPICN_SA")	#seasonally-adjsuted CPI series
	cat_inf <- c("INFHO","INFHN","INFCO","INFCN")				#inflation series (headline/core, unadjusted/net)
	cat_oth <- c("COM_EN","FOOD_GC")							# food and energy price index series
	
	cat_xtra <- c(
		"USD_HHEXPINF",		#1y household inflaitn expectation (consumer survey)
		"USD_NFAVGHEAR",	#%oya average hourly earning
		"USD_LABCOSTYOY",	#%oya labor cost
		"USD_BRKEVN5Y",		#breakeven 5y
		"USD_BRKEVN10Y",	#breakeven 10y
		"USD_INFLSW5Y",		#inflation swap 5y
		"USD_INFLSW10Y",	#inflation swap 10y
		"USD_INFL5Y5Y",		#inflation fwd 5y5Y
		
		"EUR_INFEXPNET",	#household inflation expectation (net balance)
		"EUR_INFLFRCST2Y",	#forcaster inflaiton expectation (2y ahead)
		"EUR_INFLFRCST5Y",	#forcaster inflaiton expectation (5y ahead)
		"EUR_COMPYOY",		#compensation per emplyee (%oya)
		"EUR_WAGEYOY",		#wage growth (%oya)
		"DEM_BRKEVN10Y",	#Germany 10y breakeven
		"FRF_BRKEVN10Y",	#France 10y breakeven
		"EUR_INFLSW5Y",		#inflation swap 5y
		"EUR_INFLSW10Y",	#inflation swap 10y
		"EUR_INFL5Y5Y",		#inflation fwd 5y5Y
		
		"JPY_INFLFRCST1Y",	#forcaster consensus 1y inflation expectation
		"JPY_HHINFLFRCST",	#reconstructed average household infaltion exepcation 1y ahead
		"JPY_CASHEARMON",	#cash earnings (chg %oya)
		"JPY_SCHCASHEARMON",#scheduled cash earnings (%oya)
		"JPY_BRKEVN3Y",		#breakeven 3y
		"JPY_BRKEVN10Y",	#breakeven 10y
		"JPY_INFLSW5Y",		#inflation swap 5y
		"JPY_INFLSW10Y",	#inflation swap 10y
		"JPY_INFL5Y5Y")		#inflation fwd 5y5Y
		
	l_tick <- list()
	l_tick[[1]] <- outer(id_g3,c(cat_inf,cat_cpi),paste,sep="_")
	l_tick[[2]] <- cat_oth
	tickers <- do.call(c,l_tick)
	dfm_all <- f_loaddata_all(folder=folder.data,freq="m",enddate=as.Date(Sys.time()))
	dfm0 <- dfm_all[,intersect(names(dfm_all),tickers)]
	rm(dfm_all)
	
	#### !!!! ### NOTE: the below will be removed one the inlfation series are added to the official inflaitn workbook (i.e. next week)
	df_extra <- f_loaddata(folder.data,"ExtraInflationSeries",freq="m",enddate=as.Date(Sys.time())) 
	dfm0 <- merge(dfm0,df_extra)
	
	names(dfm0) <- gsub("COM_EN","NRG_CPI",names(dfm0))
	names(dfm0) <- gsub("FOOD_GC","FOOD_CPI",names(dfm0))
	dfm <- dfm0
	
	#B. Table structure
	
	### Below, the use can define for each country, a sketch of the table structure
	l_usd_cell <- list(	paste("USD",c("INFHN","CPIHN_SA_QOQ_SAAR","CPIHN_SA_MOM_SAAR_T6M","INFCN","CPICN_SA_QOQ_SAAR","CPICN_SA_MOM_SAAR_T6M"),sep="_"),
						c("USD_HHEXPINF","USD_LABCOSTYOY","USD_NFAVGHEAR"),
						paste("USD",c("BRKEVN5Y","BRKEVN10Y","INFLSW5Y","INFLSW10Y","INFL5Y5Y"),sep="_"))
						
	l_eur_cell <- list(	paste("EUR",c("INFHN","CPIHN_SA_QOQ_SAAR","CPIHN_SA_MOM_SAAR_T6M","INFCN","CPICN_SA_QOQ_SAAR","CPICN_SA_MOM_SAAR_T6M"),sep="_"),
						c("EUR_INFLFRCST2Y","EUR_INFLFRCST5Y","EUR_INFEXPNET","EUR_COMPYOY","EUR_WAGEYOY"),
						c("DEM_BRKEVN10Y","FRF_BRKEVN10Y","EUR_INFLSW5Y","EUR_INFLSW10Y","EUR_INFL5Y5Y"))
						
	l_jpy_cell <- list(	paste("JPY",c("INFHO","INFHN","CPIHN_SA_QOQ_SAAR","CPIHN_SA_MOM_SAAR_T6M","INFCO","INFCN","CPICN_SA_QOQ_SAAR","CPICN_SA_MOM_SAAR_T6M"),sep="_"),
						c("JPY_INFLFRCST1Y","JPY_HHINFLFRCST","JPY_CASHEARMON","JPY_SCHCASHEARMON"),
						paste("JPY",c("BRKEVN3Y","BRKEVN10Y","INFLSW5Y","INFLSW10Y","INFL5Y5Y"),sep="_"))
	
	l_commo_cell <- list(c("FOOD_POYA_NSA","FOOD_QOQ_NSA"),c("NRG_POYA_NSA","NRG_QOQ_NSA"))
	l_all <- list(l_usd_cell, l_eur_cell, l_jpy_cell, l_commo_cell)

	v_back = c(12,6,3,1) # the look back month intervalls to use
	
	#C. Data transformations
	
	vsuffix <- c("POYA_SA","QOQ_SAAR","MOM_SAAR_T6M")			#suffixes for %oya, %3m/3m saar, %m/m 6mt
	vtrpars <- c(12,3,1)										#time interval to for the aformentioned series
	vtrim = c(0,0,1/6)											#trimming (6mtr = 1/6th on top and same on bottom
	vmav <- c(1,1,6)											#MA lookback
	dfm_infsaar <- NULL
	for(k in 1:length(vsuffix)){
		for(j in 1:length(cat_cpi)){
			outname <- paste(cat_cpi[j],vsuffix[k],sep="_")		#output name
			df_trans <- setNames( 
			(12/vtrpars[k]) * f_pch(dfm[, paste(id_g3, cat_cpi[j], sep="_")], vtrpars[k]), paste(id_g3, outname, sep="_")) # transformation+annualisation
			df_trans <- rollapply(df_trans,vmav[k],mean,trim=vtrim[k],fill=NA,align="right") # applies the trim mean when required (will be added in the new f_trsvdf)
			if(j==1 && k==1) dfm_infsaar <- df_trans
			else dfm_infsaar <- merge(dfm_infsaar,df_trans)
		}
	}
	dfm <- merge(dfm,dfm_infsaar)	#merging the transformed series with the main dataframe
	
	dfm <- merge(	dfm,
				setNames(f_pch(dfm[, paste(c("NRG","FOOD"), "CPI", sep="_")], 12), paste(c("NRG","FOOD"), "POYA_NSA", sep="_")), #forming %oya change in commodities price indices
				setNames(f_pch(dfm[, paste(c("NRG","FOOD"), "CPI", sep="_")], 3), paste(c("NRG","FOOD"), "QOQ_NSA", sep="_"))) #forming %qoq change in commodities price indices	
	dfm<-merge(dfm,NA*dfm[,1]) ; names(dfm)[ncol(dfm)]<-"DUMMY"	#dummy column (very useful for formatting
		
	#D. Initial formatting
	
	k<-1;nrows<-0
	while(k<=length(l_all)){nrows<-nrows+length(do.call(c,l_all[[k]]))+length(l_all[[k]])+1;k<-k+1}
	nrows<-nrows+(length(l_all)-1)								# total number of rows in the talbe (including empty rows, etc.)
	
	v_names<-do.call(c,lapply(l_all,function(x){do.call(c,x)}))	#named rows
	n_names<-length(v_names)
	
	v_pos<-rep(0,n_names)
	v_pos[cumsum( unlist( lapply( l_all,function(x){unlist(lapply(x,length))})))+1]<-1
	v_pos[cumsum( unlist( lapply( lapply( l_all, function(x){ unlist( lapply(x, length))}),sum)))+1]<-3
	v_pos[1]<-1;v_pos<-v_pos[-length(v_pos)]
	v_pos<-cumsum(v_pos)+(1:n_names)							# position of named rows in the table ([1...n_names] -> [1... n_rows] )
	
	namerow<-rep("DUMMY",nrows)
	namerow[v_pos]<-v_names
	namecol<-c(paste(v_back,"m ago",sep=""),"lastest","")
	m_output<-array("",dim=list(nrows,length(v_back)+2),dimnames=list(namerow,namecol))	#arrary containing the final table
	
	dftail<-tail(dfm0,12)
	coredata(dftail)<-replace(coredata(dftail),!is.na(dftail),0)
	coredata(dftail)<-replace(coredata(dftail),is.na(dftail),1)
	col_latest<-index(dfm)[rep(length(index(dfm0)),length(namerow))-colSums(dftail)[namerow]]	#this is the last column of our table = latest available non-NA value for the base series
	
	dfm_spe<-dfm[,namerow]						#dataframe with columns ordered like the rows of the final table
	dfm_spe<-na.locf(dfm_spe,fromLast=FALSE)	#fill in the latest NA values by the latest available
	m_output[,length(v_back)+2]<-as.character(as.Date(col_latest,"YYYY-MM-DD"))	#formatting the lastest-avaiable column as dates
	m_output[,length(v_back)+1]<-tail(dfm_spe,1)	#adding the latest avialable values
	for(k in 1:length(v_back)){								
		m_output[,k]<-tail(lag(dfm_spe,k=-v_back[k]),1)	#adding the n-month backward avialable value
	}
	m_output<-replace(m_output,is.na(m_output),"")	#replace NAs by empty value, m_output is ready to be sent to Excel


	## For quality check (this can draw the table direclty in R to check all is in place)
	# f_drawtable(m_output,title=NULL,subtitle=NULL,l_notes=NULL,in_bold=NULL,ndecs=2,altern_color=TRUE,row_spe_col=NULL,reg_break=5)
	
	