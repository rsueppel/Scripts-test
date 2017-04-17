#
# Hedge Basket Generator
#
#A.Preparations

	rm(list=ls())
	enddate = as.Date("2016-08-31")	
	require(Management)
	f_setup()
	setwd(folder.data)
	
#B. Loading and manual input of required data

	#B.1. Define id vectors
	
	id_eq <- c("USD", "EUR","JPY","GBP","SEK","AUD","CAD","TWD","INR","ZAR","CNY")		#equity cross-section identifiers ISSUE: HSI missing
	eq_wgt <- c(0.3 ,0.15 ,0.13 ,0.09 ,0.03 ,0.04 ,0.05 ,0.03 ,0.04 ,0.03 ,0.11)
	id_fx <- c("AUD","NZD","SEK","NOK","KRW","INR","ZAR","ILS","RUB","TRY","PLN","BRL","MXN")
																					    #FX cross-section identifiers
	id_cr <- outer(c("EUR","USD"), c("IG","HY"), paste,sep="_")							#CDS cross-section index identifiers

	id_uni <- unique(c(id_eq, id_fx))													#unified "all countries" character vector

	id_eqc <- paste(id_eq, "EQ", sep="_")												#equity contract identifiers
	id_crc <- paste(id_cr, "CR", sep="_")												#credit contract identifiers
	id_fxc <- paste(id_fx, "FX", sep="_")												#FX contract identifiers (only for transaction cost)

	id_all <- c(id_eqc, id_fxc, id_crc)													#all asset identifiers (for performance data import)
	cat_ret <- c("XR", "CRY")
	
	#B.2. Import data series from Access and rename
	
	#a. Monthly data import

 	l_all <- list()
	l_all[[1]] <- outer(id_all, cat_ret, paste,sep="_")
	tickers <- do.call(c, l_all)
	m_files <- c("FXReturns", "EquityReturns", "OtherReturns")
	dfm <- f_loaddata(folder = folder.data, m_files, "m", enddate=enddate)
	
	#b. Daily data import
	
	d_files <- m_files
	dfd <- f_loaddata(folder = folder.data, d_files, "d", enddate=enddate)	
		
#C. Basket formation

	#C.1. Asset class sub-baskets
	
 	l_eq_bkt <- f_basket(dfm, dfd, id_eqc, wgts=eq_wgt, xr="XR", cry="CRY", sd_win=18)
	l_fx_bkt <- f_basket(dfm, dfd, id_fxc, wgts=NULL, xr="XR", cry="CRY", sd_win=18)
	l_cr_bkt <- f_basket(dfm, dfd, id_crc, wgts=c(0.35,0.35,0.15,0.15), xr="XR", cry="CRY", sd_win=18)
	
	dfm_bkt <- dfm[,1]*NA															#dataframe of monthly sub-basket performance data
	for(i in 1:2){dfm_bkt <- merge(dfm_bkt, merge(l_eq_bkt[[i]], l_fx_bkt[[i]], l_cr_bkt[[i]]))}
	
	l_id<-list(id_eqc,id_fxc,c(id_crc[1:2],id_crc[3:4]))
	l_bkts<-c("l_eq_bkt","l_fx_bkt","l_cr_bkt")
	id_bkt<-c("GLB_EQ","GLB_FX","GLB_CR")											#name the dataframe series
	
	dfm_bkt<-dfm_bkt[,-1]
	
	dfd_bkt<-dfd[,1]*NA																#dataframe of daily sub-basket performance data
	for(i in c(4,6)){dfd_bkt<-merge(dfd_bkt,merge(l_eq_bkt[[i]],l_fx_bkt[[i]],l_cr_bkt[[i]]))}
	dfd_bkt<-dfd_bkt[,-1]
	names(dfm_bkt)[1:6]<-names(dfd_bkt)<-as.vector(outer(id_bkt,c("XR","CRY"),paste,sep="_"))
	
	#C.2. Global directional basket
	
	l_gdb<-f_basket(dfm_bkt,dfd_bkt,id_bkt,wgts=NULL,xr="XR",cry="CRY",sd_win=18)
	
	dfm_gdb<-merge(l_gdb[[1]],l_gdb[[2]])											#monthly performance data
	dfd_gdb<-merge(l_gdb[[4]],l_gdb[[6]])											#daily performance data
	dfm_hbw<-merge(l_eq_bkt[[5]]*l_gdb[[5]][,1],l_fx_bkt[[5]]*l_gdb[[5]][,2],l_cr_bkt[[5]]*l_gdb[[5]][,3])
																					#weights for assets
	names(dfm_gdb)<-names(dfd_gdb)<-paste("GLB_DRB",c("XR","CRY"),sep="_")
	
	#C.3. Vector of average trading costs
	
	tcm_av<-rcm_av<-ccm_av<-0*tcm
 	for(k in 1:dim(tcm)[1]){
		for(n in 1:dim(tcm)[2]){
			area<-dimnames(tcm)[[1]][k]
			asset<-dimnames(tcm)[[2]][n]
			name<-paste(area,asset,sep="_")
			if(is.element(name,names(dfm_hbw))){
				tcm_av[area,asset]<-mean(dfm_hbw[,name])*tcm[area,asset]
				rcm_av[area,asset]<-mean(dfm_hbw[,name])*rcm[area,asset]
				ccm_av[area,asset]<-mean(dfm_hbw[,name])*ccm[area,asset]
			}
		}
	}
	v_costs<-c(sum(tcm_av),sum(rcm_av),sum(ccm_av))									#vector costs per USD notional
	
#D. Output
	dfm_wgt<-dfm_hbw
	colnames(dfm_wgt)<-paste(colnames(dfm_wgt),"WT",sep="_")
   	dfm_wgt<-zoo(dfm_wgt,as.character(index(dfm_wgt)))
	
	dfd_cry<-dfd[,paste(c(id_eqc,id_crc,id_fxc),"CRY",sep="_")]
	dfd_cry<-zoo(dfd_cry,as.character(index(dfd_cry)))
	
	l_gdb<-vector("list",6)
	l_gdb[[1]]<-dfm_gdb
	l_gdb[[2]]<-dfd_gdb
	l_gdb[[3]]<-dfm_hbw
	l_gdb[[4]]<-v_costs
	l_gdb[[5]]<-dfm_bkt
	l_gdb[[6]]<-dfd_bkt
	
	save(l_gdb,file="GDB.RData")
	
#E. Write outputs for scorecard                                                          
	
	output_path<-folder.out                                                        
	write.xlsx2(dfm_wgt, file=paste(output_path, "MONTHLY WEIGHTS.xls",sep=""), col.names=TRUE, row.names=TRUE, showNA=TRUE) 
	write.xlsx2(dfd_cry, file=paste(output_path, "DAILY CARRIES.xls",sep=""), col.names=TRUE, row.names=TRUE,showNA=TRUE)    
	
	dfm_gdb<-zoo(dfm_gdb,as.character(index(dfm_gdb)))
	write.xlsx2(dfm_gdb, file=paste(output_path,"MONTHLY CARRIES.xls",sep=""), col.names=TRUE, row.names=TRUE, showNA=TRUE)
	