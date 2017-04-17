#
# Trading Strategies Consolidation Exercise
#

#A. Preparations

	#A.1. Load all required external packages

		Sys.setenv(TZ="GMT")
		rm(list=ls())
		graphics.off()
		require(zoo)
		require(PerformanceAnalytics)
		options(digits=2)
		
	#A.2. Source all custom-made functions

		walid_out<-"C:/Users/wdif/Desktop/Work/Aggregator/"
		walid_sfu<-"C:/Users/wdif/Desktop/Work/Technology/Support functions_backup/"
		walid_qmd<-"C:/Users/wdif/Desktop/Work/Infrastructure/"

		ralph_sfu<-"C:/Users/rsueppel/Documents/5. PKH_originals/R solutions/2. Support functions/"
		ralph_out<-"C:/Users/rsueppel/Documents/5. PKH_originals/R solutions/4. Output/"
		
		m_sfu<-"M:/Trading/EXA/2. Quantitative Research/2. Support functions/"
		m_out<-"M:/Trading/EXA/2. Quantitative Research/4. Output/"			

		folder_sfu<-m_sfu
		folder_out<-m_out

		setwd(folder_out)
		# source(paste(folder_sfu,"ConnectAccess.r",sep=""))
		# source(paste(folder_sfu,"FrequencyConverter.r",sep=""))

#B. Import files and set general parameters

	#B.1. Import all backtested strategies

		cv_sns<-c("xef","xdx","xdu","xco","xeq","eqdu","crdu","eqfx")				#character vector of all strategy names
		cv_pnames<-c("PNL_NP","PNL_ADJ")											#names of non-penalized and penalized PnLs
		
		cv_pofs<-paste(cv_sns,"output_oos.RData",sep="_")							#character vector of PnL output files
		for(i in 1:length(cv_pofs)){
			load(cv_pofs[i])
			if(i==1){dfd_pnl0<-get(paste(cv_sns[i],"sd1pnl",sep="_"))
			}else{dfd_pnl0<-merge(dfd_pnl0,get(paste(cv_sns[i],"sd1pnl",sep="_")))}
		}
		names(dfd_pnl0)<-toupper(cv_sns)
		dfd_pnl0<-window(dfd_pnl0,start=as.Date("2002-01-02"),end=index(dfd_pnl0)[length(index(dfd_pnl0))])
		
	#B.2. Set parameters

		vol<-4.5																	#volatility target
		diverslev<-2																#diversification leverage
		penalty<-0.3																#drift penality as ratio of target SD (15% OF 4.5% gives 67.5bps p.a.)
		dog_wgt<-0.1								#0.2								#set weight of dog		
		
		v_wgt<-rep(NA,length(cv_sns)+1);names(v_wgt)<-c(cv_sns,"dog")				#set up strategy and weight vector
		v_wgt["xef"]<-0.35
		v_wgt["xdx"]<-0.075
		v_wgt["xdu"]<-0.05
		v_wgt["xco"]<-0.15
		v_wgt["xeq"]<-0.175
		v_wgt["eqdu"]<-0.125
		v_wgt["crdu"]<-0.025
		v_wgt["eqfx"]<-0.05
		
		#ndgs<-length(v_wgt)-1;v_wgt[1:ndgs]<-rep(1/ndgs,ndgs)						#line for equal weighting
		
	#B.3. Adjusted vector of strategy weights (including "dog")	

		v_wgt<-v_wgt*(1-dog_wgt)/sum(v_wgt,na.rm=T)									#ensure sum of 1  and correct dog weight
		v_wgt["dog"]<-dog_wgt
				
	#B.4. PnL creation including dog
	
		dfd_pnl<-na.trim(100*dfd_pnl0*vol*diverslev,is.na="all")					#PnL in bps with assumption of vol and leverage	
		
		dfd_pnl0_al<-dfd_pnl0; dfd_pnl0_al[is.na(dfd_pnl0_al)]<-0					#alternative PnL df with zeroes
		dfd_pnl_cum<-cumsum(dfd_pnl0_al)											#cumulative PnLs	
		dfa_pnl_cum<-dfd_pnl_cum[index(dfd_pnl0)[as.numeric(format(index(dfd_pnl0),"%Y"))-as.numeric(format(index(lag(dfd_pnl0,k=-1)),"%Y"))!=0]]
																					#annual frequency cumulative PnLs
		dfa_pnl<-rbind(dfa_pnl_cum[1,],diff(dfa_pnl_cum))							#annual frequency annual PnLs		
		dfa_worst=apply(dfa_pnl,1, function(x) min(x))								#combined series of worst PnLs
		
		dfa_wcol<-(dfa_pnl==dfa_worst)*1											#identify column number of dog strategy
		dfd_wcol<-na.locf(merge(dfd_pnl[,1]*NA,dfa_wcol)[,-1],fromLast=T)			#convert to daily
		zoo_dog_pnl<-zoo(rowSums(dfd_pnl*dfd_wcol,na.rm=T),index(dfd_pnl))			#create and append annualy changing dog strategy
		dfd_pnl<-merge(dfd_pnl,zoo_dog_pnl);names(dfd_pnl)[ncol(dfd_pnl)]<-"DOG"
		
#C. Create weighted PnLs

	#C.1. Dataframe of adjusted weights
	
		dfd_wgt<-dfd_pnl
		dfd_wgt[!is.na(dfd_wgt)]<-1													#set the dates to 1 if the trade exists, 0 otherwise
		dfd_wgt[is.na(dfd_wgt)]<-0													#this enables to determine the equal weighting simply by summing the columns
		for(j in 1:length(v_wgt)){
			dfd_wgt[,j]<-v_wgt[j]*dfd_wgt[,j]										#df of original weights and zeroes
			}
		dfd_awgt<-dfd_wgt/rowSums(dfd_wgt)											#weights re-inflated to add up to one
		
	#C.2. Dataframe of weighted PnLs
	
		dfd_pnl[is.na(dfd_pnl)]<-0
		dfd_wpnl<-dfd_pnl*dfd_awgt													#dataframe of individual weighted PnLs
		zoo_pnl<-zoo(rowSums(dfd_wpnl),index(dfd_wpnl))								#aggregate weighted PnL
	
	#C.3. Adjust for penalty

		pen<-penalty*100*vol/252													#daily penalty on drift
		df_aggpnl<-merge(zoo_pnl,zoo_pnl-pen)										#subtract the penality from the daily return, the NA trick avoids to add penalty on non-live dates
		names(df_aggpnl)<-cv_pnames
		
# D. Charts and Statistics

	#D.1 Basic table and charts
	
		#i. Daily returns and statistics based on it
	
		zoo_pnl_np<-df_aggpnl[,cv_pnames[1]]
		zoo_pnl_p<-df_aggpnl[,cv_pnames[2]]

		table.AnnualizedReturns(df_aggpnl,geometric=F,digits=2)
		
		dfd_pnl_ind<-dfd_pnl[,1:dim(dfd_pnl)[2]]/100
		table.AnnualizedReturns(dfd_pnl_ind,geometric=F,digits=2)

		#ii. Monthly data

		dev.new()
		loc_dfm<-zoo(0,index(df_aggpnl)[which(!(months(index(df_aggpnl))==months(index(lag(df_aggpnl,k=-1)))))])
		dfm_aggpnl<-f_convfreq(df_aggpnl,loc_dfm,from="d",to="m",meth="a")
		chart.CumReturns(dfm_aggpnl,geometric=F,main="Weighted adjusted PnL, cumulative monthly returns")		
		tail(dfm_aggpnl,36)
		
		#iii. Sub-strategy performances
		
		dfm_pnl<-f_convfreq(dfd_pnl,loc_dfm,from="d",to="m",meth="a")
		plot(cumsum(dfd_pnl),main="Sub-strategy PnLs")
		
		dfm_pnl<-f_convfreq(dfd_pnl,loc_dfm,from="d",to="m",meth="a")
		
		dfm_pnlc<-dfm_pnl*rep(v_wgt,each=nrow(dfm_pnl))
		dfm_pnlc<-merge(dfm_aggpnl,dfm_pnlc)
		tail(dfm_pnlc)
		
	#D.2. Returns and histogram
	
		dev.new()
		chart.CumReturns(df_aggpnl,geometric=F,main="Weighted adjusted PnL, cumulative returns")

		#WALID#
		dev.new()
		plot(cumsum(dfd_pnl_ind),main="Cumulative returns of individual trades")
		#WALID#
		
		dev.new()
		chart.Histogram(zoo_pnl_p/100,main="Weighted adjusted PnL, Daily Returns",method="add.normal")
		
	#D.3. Drawdown charts
		
		dev.new()
		ptt_draw<-cumsum(zoo_pnl_p)-cummax(cumsum(zoo_pnl_p))						# peak-to-trough drawdown graph
		plot(ptt_draw,main="Weighted adjusted PnL, peak-to-trough drawdowns")
		
		dev.new()
		xr20<-cumsum(zoo_pnl_p)-lag(cumsum(zoo_pnl_p),k=-20)						# 20d P&L drawdown
		xr20[xr20>0]<-0
		plot(xr20,main="Weighted adjusted PnL, 20d drawdowns")

	#D.4. Worst drawdowns

		xr1_ver1<-zoo_pnl_np														# daily reutrns
		xr3_ver1<-cumsum(zoo_pnl_np)-lag(cumsum(zoo_pnl_np),k=-3)					# 3d returns
		xr5_ver1<-cumsum(zoo_pnl_np)-lag(cumsum(zoo_pnl_np),k=-5)					# 5d returns

		xr1_ver2<-zoo_pnl_p															# daily reutrns
		xr3_ver2<-cumsum(zoo_pnl_p)-lag(cumsum(zoo_pnl_p),k=-3)						# 3d returns
		xr5_ver2<-cumsum(zoo_pnl_p)-lag(cumsum(zoo_pnl_p),k=-5)						# 5d returns

		df_draw<-array(NA,dim=c(3,2),dimnames=list(c("Worst 1% daily drawdowns","Worst 1% 3d drawdowns","Worst 1% 5d drawdowns"),c("NP","Adjusted")))
		df_draw[1,1]<-quantile(xr1_ver1,0.01)
		df_draw[2,1]<-quantile(xr3_ver1,0.01)
		df_draw[3,1]<-quantile(xr5_ver1,0.01)
		df_draw[1,2]<-quantile(xr1_ver2,0.01)
		df_draw[2,2]<-quantile(xr3_ver2,0.01)
		df_draw[3,2]<-quantile(xr5_ver2,0.01)
		df_draw
		
	#D.5. 12-Mth Rolling Sharpes

		k=252
		zoo_sharpe<-sqrt(252)*rollapply(zoo_pnl_p,k,mean,alig="right")/rollapply(zoo_pnl_p,k,sd,align="right")
		dev.new()
		plot(zoo_sharpe,ylim=range(zoo_sharpe),col="red",main="Strategies 12M Rolling Sharpe Ratio")
		lines(zoo_sharpe*0+1,col="magenta",lty=2)
		lines(zoo_sharpe*0,col="cyan",lty=2)
		
		#WALID#
		df_sharpe<-array(NA,dim=c(4,1),dimnames=list(c("Sharpe LT Mean","% occurr. Sharpe<1","% occurr. Sharpe<0.5","% occurr. Sharpe<0"),c("Sharpe table")))
		df_sharpe[1,1]<-mean(zoo_sharpe)
		df_sharpe[2,1]<-length(zoo_sharpe[zoo_sharpe<1])/length(zoo_sharpe)*100
		df_sharpe[3,1]<-length(zoo_sharpe[zoo_sharpe<0.5])/length(zoo_sharpe)*100
		df_sharpe[4,1]<-length(zoo_sharpe[zoo_sharpe<0])/length(zoo_sharpe)*100
		df_sharpe
		#WALID#
		
	#D.6. Cross-Correlation Tables
	
		correl<-cor(dfd_pnl)*NA
		for(i in 1:dim(dfd_pnl)[2]){
			for(j in 1:dim(dfd_pnl)[2]){
				if(i>=j){
					loc_pnl<-na.trim(dfd_pnl[,c(i,j)])
					correl[i,j]<-100*cor(loc_pnl)[1,2]
				}
			}
		}
		correl
		
	#D.7. 40d P&L vs. GDB
	
		pcma<-40
		bmarks<-"GDB"
		load("GDB.RData")
		zoo_bm<-l_gdb[[2]][,1]
		zoo_bma<-rollapply(zoo_bm,pcma,mean,align="right",na.fill=TRUE)				# MA of the benchmark
		zoo_pnl_ma2<-rollapply(zoo_pnl_p,pcma,mean,align="right",na.fill=TRUE)		# MA of the global P&L of the strategy
																					# rescaling, for graphical reasons
		zoo_pnl_ma2<-zoo_pnl_ma2*max(abs(range(na.trim(zoo_bma))/range(na.trim(zoo_pnl_ma2))))
		dev.new()
		df_agg2<-na.trim(merge(zoo_bma,zoo_pnl_ma2))
		plot(df_agg2[,1],ylim=range(df_agg2),col="red",main=paste("Strategy (Version 2) Pro-Cyclicality, ",pcma,"d Moving Average vs.Benchmark ",bmarks,sep=""))
		lines(df_agg2[,2],col="blue")
		legend("topleft",c(paste("MA[Benchmark =",bmarks,"]",sep=""),"MA[Trade]"),col=c("red","blue"),lty=c(1,1),pch=c(1,1),merge=TRUE)
		
		legend("topleft",c(paste("MA[Benchmark =",bmarks,"]",sep=""),"MA[Trade]"),col=c("red","blue"),lty=c(1,1),pch=c(1,1),merge=TRUE)
		
		#WALID#
		zoo_pnl_gdb<-na.trim(merge(zoo_pnl_p,dfd_pnl_ind,zoo_bm))
		names(zoo_pnl_gdb)<-c(cv_pnames[2],names(dfd_pnl_ind),"GDB")
		xr5<-cumsum(zoo_pnl_gdb)-lag(cumsum(zoo_pnl_gdb),k=-5)
		df_pnl_gdb<-array(NA,dim=c(length(names(xr5))-1,2),dimnames=list(c("Aggreg. PnL",names(dfd_pnl_ind)),c("Correl. PnL vs GDB","Correl. 5d PnL vs GDB")))
		for(i in 1:length(names(xr5))-1){
			df_pnl_gdb[i,1]<-cor(zoo_pnl_gdb[,i],zoo_pnl_gdb[,dim(zoo_pnl_gdb)[2]])*100
			df_pnl_gdb[i,2]<-cor(xr5[,i],xr5[,dim(zoo_pnl_gdb)[2]])*100
		}
		df_pnl_gdb
		#WALID#
		