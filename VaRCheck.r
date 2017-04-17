#
# Trading Strategies Consolidation Exercise
#

#A. Preparations

    Sys.setenv(TZ="GMT")
    library(Management)
    f_setup(path="gcm")
    enddate <- as.Date("2016-05-31")  # set last working day for which data are updated
    graphics.off()
    options(digits=2)

#B. Import files and set general parameters

		id_dm<-c("AUD","CAD","CHF","EUR","GBP","JPY","SEK","NOK","NZD","USD")
		id_em<-c("BRL","CLP","CNY","COP","CZK","HUF","IDR","ILS","INR","KRW","MXN","MYR","PEN","PHP","PLN","RON","RUB","THB","TRY","TWD","ZAR")
		id_sdc<-c("AUD","CAD","GBP","SEK","NOK","NZD")
		id_xco<-c("BRT","ALM","CPR","LED","NIC","ZNC","GLD","SIV","CFE","COR","CTN","SGR","SOY","WHT","CAT","HOG")
		id_all<-unique(c(id_dm,id_em,id_sdc,id_xco))
		cat_fx<-c("FX_XR")
		cat_eq<-c("EQ_XR")
		cat_cr<-c("IG_CR_XR","HY_CR_XR")
		cat_du<-c("2YIRS_XR","5YIRS_XR","10YIRS_XR","5YXCS_XR")
		cat_xco<-c("CO_XR")
		cat_all<-c(cat_fx,cat_eq,cat_cr,cat_du,cat_xco)
		
		l_id<-list(id_all)
		l_cat<-list(cat_all)
		l_all<-list()
		for (i in 1:length(l_id)) l_all[[i]]<-outer(l_id[[i]],l_cat[[i]],paste,sep="_")
		
		tickers_d<-do.call(c,l_all)
		dfd_all<-f_loaddata_all(folder=folder.data,freq="d",enddate=enddate)
		dfd0<-dfd_all[,intersect(names(dfd_all),tickers_d)]
		rm(dfd_all)
		print(paste("Missing:",sort(setdiff(tickers_d,names(dfd0)))))
		
		tickers_m<-tickers_d
		dfm_all<-f_loaddata_all(folder=folder.data,freq="m",enddate=enddate)
		dfm0<-dfm_all[,intersect(names(dfm_all),tickers_m)]
		rm(dfm_all)
		print(paste("Missing:",sort(setdiff(tickers_m,names(dfm0)))))
		
# C. Risk management indicators

	#C.1. VaR estimates for strategic markets
	
		#Local plot function
		mini_plot=function(df,id,id_extra=NULL,freq="years",start=index(df)[1]){
			#dev.new()
			df_plot<-window(df[,id],start=start)
			yrg<-NULL
			if(!is.null(id_extra)){df_plotextra<-window(df[,c(id,id_extra)],start=start);yrg<-range(df_plotextra)}
			plot(df_plot,xaxt='n',main=id,ylim=yrg)
			if(!is.null(id_extra)){for(j in 1:length(id_extra)){lines(df_plotextra[,id_extra[j]],col=2*j)}}
			axis.Date(1, at = seq(from=index(df_plot)[1], to=tail(index(df_plot),1), by=freq) , format = "%b-%Y" )
			lines(0*df_plot+mean(df_plot),col="purple")
			abline(v=seq(from=index(df_plot)[1], to=tail(index(df_plot),1), by="years"),col="darkgray", lty=3)
			if(!is.null(id_extra)){legend("topright",c(id,id_extra),col=c("black",2*(1:length(id_extra))),lty=rep(1,1+length(id_extra)))}
		}
		
		#a. Equity markets (US,EU,Jap,EM)
		
		eq_em<-c("BRL","INR","KRW","MXN","MYR","THB","TRY","TWD","ZAR")
		eq_g3<-c("USD","EUR","JPY")
		eq_sdc<-c("AUD","CAD","CHF","GBP","SEK")
		
		dfd_eq_xr<-na.trim(merge(dfd0[,paste(eq_g3,"EQ_XR",sep="_")],rowMeans(dfd0[,paste(eq_em,"EQ_XR",sep="_")],na.rm=TRUE),rowMeans(dfd0[,paste(eq_sdc,"EQ_XR",sep="_")],na.rm=TRUE)))
		names(dfd_eq_xr)[ncol(dfd_eq_xr)+(-1:0)]<-c("EM_EQ_XR","SDC_EQ_XR")
		dfd_eq_xr<-na.trim(merge(dfd_eq_xr,rowMeans(dfd_eq_xr)))
		names(dfd_eq_xr)[ncol(dfd_eq_xr)]<-"GLB_EQ_XR"
		dfd_eq_xr<-na.trim(merge(dfd_eq_xr,zoo(rowMeans(dfd0[,paste(c(eq_g3,eq_sdc),"EQ_XR",sep="_")],na.rm=TRUE),index(dfd0))))
		names(dfd_eq_xr)[ncol(dfd_eq_xr)]<-"DM_EQ_XR"
		
		dfd_eq_var<-f_estimVaR(dfd_eq_xr,decay=0.94,pct_cutoff=1.00,quant=0.975)
		names(dfd_eq_var)<-gsub("XR","VaR",names(dfd_eq_xr))
		
		graphics.off()
		for (i in 1:ncol(dfd_eq_var)){
			mini_plot(dfd_eq_var,names(dfd_eq_var)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
			
		#b. Rates markets (US,EU,Jap,DM,EM)
		
		du_2y<-paste(c("BRL","CLP","HUF"),"2YIRS_XR",sep="_")
		du_5y<-paste(c("CZK","ILS","INR","KRW","MXN","MYR","PLN","THB","ZAR","AUD","CAD","CHF","GBP","NZD","SEK"),"5YIRS_XR",sep="_")
		du_10y<-paste(c("AUD","CAD","CHF","EUR","GBP","JPY","SEK","NOK","USD"),"10YIRS_XR",sep="_")
		du_xcs<-paste(c("TRY","RUB"),"5YXCS_XR",sep="_")
		
		dfd_xdu<-zoo(rowMeans(dfd0[,c(du_2y,du_5y,du_xcs)],na.rm=TRUE),index(dfd0))
		dfd_du_dm<-zoo(rowMeans(dfd0[,du_10y],na.rm=TRUE),index(dfd0))
		
		dfd_du_xr<-na.trim(merge(dfd0[,paste(c("USD","JPY","EUR"),"10YIRS_XR",sep="_")],dfd_xdu,dfd_du_dm),na.rm=TRUE)
		names(dfd_du_xr)[ncol(dfd_du_xr)+(-1:0)]<-c("XDU_DU_XR","DM_DU_XR")
		names(dfd_du_xr)<-gsub("10YIRS","DU",names(dfd_du_xr))
		
		dfd_du_var<-f_estimVaR(dfd_du_xr,decay=0.94,pct_cutoff=1.00,quant=0.975)
		names(dfd_du_var)<-gsub("XR","VaR",names(dfd_du_xr))
		
		graphics.off()
		for (i in 1:ncol(dfd_du_var)){
			mini_plot(dfd_du_var,names(dfd_du_var)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
			
		#c. CDS markets (US,EU)
		
		area_cr<-c("USD_IG","EUR_IG","USD_HY","EUR_HY")
		dfd_cr_xr<-dfd0[,paste(area_cr,"CR_XR",sep="_")]
		dfd_cr_xr<-na.trim(merge(dfd_cr_xr,rowMeans(dfd_cr_xr)))
		names(dfd_cr_xr)[ncol(dfd_cr_xr)]<-"GLB_CR_XR"
		
		dfd_cr_var<-f_estimVaR(dfd_cr_xr,decay=0.94,pct_cutoff=1.00,quant=0.975)
		names(dfd_cr_var)<-gsub("XR","VaR",names(dfd_cr_xr))
		
		graphics.off()
		for (i in 1:ncol(dfd_cr_var)){
			mini_plot(dfd_cr_var,names(dfd_cr_var)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
		
		#d. FX markets (EM,SDCexCH,EUR,JPY)
		
		dfd_fx_em<-zoo(rowMeans(dfd0[,paste(id_em,"FX_XR",sep="_")],na.rm=TRUE),index(dfd0[,1]))
		dfd_fx_sdc<-zoo(rowMeans(dfd0[,paste(setdiff(id_sdc,"CHF"),"FX_XR",sep="_")],na.rm=TRUE),index(dfd0[,1]))
		dfd_fx_xr<-window(na.trim(merge(dfd_fx_em,dfd_fx_sdc,dfd0[,paste(c("JPY","EUR"),"FX_XR",sep="_")])),start="2001-01-01")
		names(dfd_fx_xr)<-paste(c("EM","SDC","JPY","EUR"),"FX_XR",sep="_")	
		
		dfd_fx_var<-f_estimVaR(dfd_fx_xr,decay=0.94,pct_cutoff=1.00,quant=0.975)
		names(dfd_fx_var)<-gsub("XR","VaR",names(dfd_fx_xr))
		
		graphics.off()
		for (i in 1:ncol(dfd_fx_var)){
			mini_plot(dfd_fx_var,names(dfd_fx_var)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
			
		#e. Commodities markets (GLB)
		
		zoo_co_xr<-zoo(rowMeans(dfd0[,paste(id_xco,"CO_XR",sep="_")],na.rm=TRUE),index(dfd0[,1]))
		zoo_co_xr[is.nan(zoo_co_xr)]<-0
		zoo_co_var<-f_estimVaR(zoo_co_xr,decay=0.94,pct_cutoff=1.00,quant=0.975)
		
		dfd_co_xrvar<-merge(zoo_co_xr,zoo_co_var)
		names(dfd_co_xrvar)<-c("GLB_CO_XR","GLB_CO_VaR")
		
		graphics.off()
		for (i in 1:ncol(dfd_co_xrvar)){
			mini_plot(dfd_co_xrvar,names(dfd_co_xrvar)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
		
		#f. Hybrids markets (G3 EQDU, G2 CRDU, and combined 50%-50%, extra= Japan only)
				
		eqdu_g3<-c("USD","EUR","JPY")
		crdu_g2<-c("EUR_HY","EUR_IG","USD_HY","USD_IG")
		
		dfm0_addhy<-dfm0_addig<-dfm0[,paste(c("USD","EUR"),"10YIRS_XR",sep="_")]
		names(dfm0_addhy)<-gsub("10YIRS_XR","HY_10YIRS_XR",names(dfm0_addhy))
		names(dfm0_addig)<-gsub("10YIRS_XR","IG_10YIRS_XR",names(dfm0_addig))
		dfm0<-merge(dfm0,dfm0_addhy,dfm0_addig)
		
		dfd0_addhy<-dfd0_addig<-dfd0[,paste(c("USD","EUR"),"10YIRS_XR",sep="_")]
		names(dfd0_addhy)<-gsub("10YIRS_XR","HY_10YIRS_XR",names(dfd0_addhy))
		names(dfd0_addig)<-gsub("10YIRS_XR","IG_10YIRS_XR",names(dfd0_addig))
		dfd0<-merge(dfd0,dfd0_addhy,dfd0_addig)
		
		dfd_eqdu_xrn <- f_normal(dfm0,dfd0,eqdu_g3,astr=c("EQ","10YIRS"),pstr=c("XR"),v_freq=c("m","d"),wgts=c(0.2,0.8),v_lbps=c(24,36),bm="USD_EQ_XR",dcry=FALSE)[[5]]
		dfd_eqdu <- f_ptrans(dfd_eqdu_xrn, eqdu_g3, "EQDU_XRN = f_avg( EQ_XRN + 10YIRS_XRN , 1)")
		dfd_eqdu<-na.trim(merge(dfd_eqdu,rowMeans(dfd_eqdu,na.rm=TRUE)))
		names(dfd_eqdu)[ncol(dfd_eqdu)]<-c("G3_EQDU_XRN")
		
		dfd_crdu_xrn<-f_normal(dfm0,dfd0,crdu_g2,astr=c("CR","10YIRS"),pstr=c("XR"),v_freq=c("m","d"),wgts=c(0.99,0.01),v_lbps=c(24,36),bm="USD_EQ_XR",dcry=FALSE)[[5]]
		dfd_crdu <- f_ptrans(dfd_crdu_xrn, crdu_g2, "CRDU_XRN = f_avg( CR_XRN + 10YIRS_XRN , 1)")
		dfd_crdu<-na.trim(merge(dfd_crdu,rowMeans(dfd_crdu,na.rm=TRUE)))
		names(dfd_crdu)[ncol(dfd_crdu)]<-c("G2_CRDU_XRN")
		
		dfd_hyb<-na.trim(merge(dfd_eqdu,dfd_crdu[,"G2_CRDU_XRN"]),na.rm=TRUE)
		dfd_hyb<-na.trim(merge(dfd_hyb,rowMeans(dfd_hyb)),na.rm=TRUE)
		names(dfd_hyb)[ncol(dfd_hyb)+(-1:0)]<-c("G2_CRDU_XRN","50%EQDU+50%CRDU_HYB_XRN")
		
		dfd_hyb_var<-f_estimVaR(dfd_hyb,decay=0.94,pct_cutoff=1.00,quant=0.975)
		names(dfd_hyb_var)<-gsub("XRN","VaR",names(dfd_hyb))
		
		graphics.off()
		for (i in 1:ncol(dfd_hyb_var)){
			mini_plot(dfd_hyb_var,names(dfd_hyb_var)[i],freq="6 months",start=as.Date("2010-01-01"))
			}
			
		#Comparing EQ, DU and HYB VaR series
		mini_plot( merge(dfd_eq_var,dfd_du_var,dfd_hyb_var), id="JPY_EQDU_VaR", id_extra=c("JPY_EQ_VaR","JPY_DU_VaR"), freq="6 months", start=as.Date("2002-01-01"))
		mini_plot( merge(dfd_eq_var,dfd_du_var,dfd_hyb_var), id="USD_EQDU_VaR", id_extra=c("USD_EQ_VaR","USD_DU_VaR"), freq="6 months", start=as.Date("2002-01-01"))
		mini_plot( merge(dfd_eq_var,dfd_du_var,dfd_hyb_var), id="G3_EQDU_VaR", id_extra=c("DM_EQ_VaR","DM_DU_VaR"), freq="6 months", start=as.Date("2002-01-01"))
		
#D. VaR-based indicators

		#a. Daily VaR shocks
		
		dfd_all_var<-merge(dfd_fx_var,dfd_du_var,dfd_eq_var,zoo_co_var)
		names(dfd_all_var)[ncol(dfd_all_var)]<-"GLB_CO_VaR"
		
		dfd_all_xr<-merge(dfd_fx_xr,dfd_du_xr,dfd_eq_xr,zoo_co_xr)
		names(dfd_all_xr)[ncol(dfd_all_xr)]<-"GLB_CO_XR"
		
		dfd_all<-merge(lag(dfd_all_var,k=-1),dfd_all_xr)						#shocks are daily drawdowns beyond estimated 1 x VaR
		dfd_lvar<-dfd_all[,names(dfd_all_var)]
		dfd_xr<-dfd_all[,names(dfd_all_xr)]
		dfd_all_shock<-dfd_xr/dfd_lvar
		coredata(dfd_all_shock)[coredata(dfd_all_shock)>(-1)]<-0
		dfd_all_shock<-(-1)*dfd_all_shock
		names(dfd_all_shock)<-gsub("XR","SHOCK",names(dfd_all_shock))
		
		graphics.off()
		for (i in 1:ncol(dfd_all_shock)){
			mini_plot(dfd_all_shock,names(dfd_all_shock)[i],freq="6 months",start=as.Date("2012-01-01"))
			}		
		
		#b. VaR % increases (1-month average of 1-month increase in VaR)
	
		dfd_all_varpch<-100*(dfd_all_var/lag(dfd_all_var,k=-21)-1)
		names(dfd_all_varpch)<-gsub("VaR","VARPCH",names(dfd_all_varpch))
		dfd_all_varp1m<-rollapply(dfd_all_varpch,21,mean,fill=NA,align="right")	#monthly VaR percent changes
		coredata(dfd_all_varp1m)[coredata(dfd_all_varp1m)<0]<-0					#only increases represent disturbances
		
		graphics.off()
		for (i in 1:ncol(dfd_all_varp1m)){
			mini_plot(dfd_all_varp1m,names(dfd_all_varp1m)[i],freq="6 months",start=as.Date("2010-01-01"))
			}		
		
		#c. VaR % over 3-years average
		
		dfd_varma<-rollapply(dfd_all_var,3*252,median,align="right",fill=NA)
		dfd_all_varel<-100*(dfd_all_var/dfd_varma-1)
		names(dfd_all_varel)<-gsub("VaR","VAREL",names(dfd_all_varel))
		coredata(dfd_all_varel)[coredata(dfd_all_varel)<0]<-0					#only increases represent disturbances
		
		graphics.off()
		for (i in 1:ncol(dfd_all_varel)){
			mini_plot(dfd_all_varel,names(dfd_all_varel)[i],freq="6 months",start=as.Date("2005-01-01"))
			}		
			
