#
# Trading strategies combination tool
#


#A. Preparations

rm(list=ls())

require(zoo)
require(PerformanceAnalytics)

home_out<-"C:/Users/rsueppel/Documents/5. PKH_originals/R solutions/4. Research output/"
ralph_out<-"C:/Users/rsueppel/Documents/2. Quant Research/Research output/"
folder_out<-ralph_out

setwd(folder_out)


#B. Files to load and preparation of dataframes

sids<-c("ccef","ccdx","cceq","eqdu","hydu","igdu","eqfx","coeq")				#all strategy ids
vos<-paste(sids,"sd1pnl.RData",sep="_")											#vector of strategies' names
for(i in 1:length(vos)) load(vos[i])											#upload all strategies

dfd<-get(paste(sids[1],"sd1pnl",sep="_"))										#convert to datframe of strategy SD1 PnLs 
for (i in 2:length(sids)){
	sd1pnl<-get(paste(sids[i],"sd1pnl",sep="_"))
	dfd<-merge(dfd,sd1pnl)
	}
names(dfd)<-paste(toupper(sids),"SD1PNL",sep="_")
dfd<-na.trim(dfd,is.na="any")


vots<-c("CFX","CEQ","HYB","RAC")												#vector of TYPES of strategies
lots<-vector("list",length(vots))												#list of types' constituents and weights
names(lots)<-vots
lots[[1]]<-list(c("CCDX","CCEF"),c(0.8,0.2))
lots[[2]]<-list("CCEQ",1)
lots[[3]]<-list(c("EQDU","HYDU","IGDU"),c(0.5,0.3,0.2))
lots[[4]]<-list(c("EQFX","COEQ"),c(0.7,0.3))

for (i in 1:length(lots)){														#create type of strategy dataframes
	sl<-lots[[i]]																#pick strategy type's names and weights
	dft<-dfd[,paste(sl[[1]],"SD1PNL",sep="_")]									#extract dataframe with strategies belonging to type
	nc<-ncol(as.matrix(dft))													#count number of strategies
	if(nc>1){sd1pnl<-dft%*%lots[[i]][[2]]}else{sd1pnl<-dft}						#calculate aggregate strategy PnL
	dft<-merge(dft,sd1pnl)														#add it to extracted dataframe
	names(dft)[length(dft[1])]<-paste(vots[i],"SD1PNL",sep="_")					#name aggregate type of strategy PnL
	if(nc>1){dev.new()															#open new device for chart if strategy has components
			chart.CumReturns(dft,geometric=F,legend.loc="bottom")				#cumulated PnL chart
			}
	assign(paste("dfd",tolower(vots[i]),sep="_"),dft)	 
	}

	#graphics.off()

#C. Strategy type analysis


#D. Aggregate strategy analysis

wots<-c(0.5,0.05,0.25,0.2)														#weigts of types of strategies

wots<-rep(1/length(wots),length(wots))											#equal weighting
tsd<-6																			#annual SD target on individual strategy level
dlv<-1.6																		#diversification leverage on the aggregate level
pen<-0.1/252																	#daily PnL penalty as ratio of targeted DAILY SD
	
#D.1. Series and dataframe creation
	
dfd_agg<-dfd[,1]																#create dataframe of SD1 PnLs for all types of strategies
for(i in 1:length(vots)){
	dft<-get(paste("dfd",tolower(vots[i]),sep="_"))
	dfd_agg<-merge(dfd_agg,dft[,length(dft[1])])
	names(dfd_agg)[length(dfd_agg[1])]<-paste(vots[i],"SD1PNL",sep="_")
	}
dfd_agg<-na.trim(dfd_agg[,-1])
dfd_exa<-dfd_agg%*%wots															#add aggregate PnL
dfd_agg<-merge(dfd_agg,dfd_exa)
names(dfd_agg)[length(dfd_agg[1])]<-"EXA_SD1PNL"

dfd_agg<-dfd_agg-rep(pen,length(wots)+1)

woas<-c(wots,1)
dfd_arc<-zoo(t(t(dfd_agg)*woas)*tsd*dlv,index(dfd_agg))							#return dataframe = SD1PnLs * weights * targetSD * diversification leverage
names(dfd_arc)<-gsub("SD1PNL","XRC",names(dfd_arc))								#name as excess return contribution

#D.2. Graphs

dev.new()
chart.CumReturns(dfd_agg[,1:length(wots)],geometric=F,legend.loc="bottom")		#cumulative SD1 PnLs by types of strategies
dev.new()
chart.CumReturns(dfd_arc,geometric=F,legend.loc="topleft",colorset=(5:1))		#cumulative returns and type of strategies contribution
dev.new()
chart.CumReturns(tail(dfd_arc$EXA_XRC,n=252),geometric=F,main="EXA cumulative return")						
																				#cumulative returns and type of strategies contribution


#D.2. Statistics

table.AnnualizedReturns(dfd_arc,geometric=F,digits=2)
range(dfd_arc$EXA_XRC)
mean(abs(dfd_arc$EXA_XRC))
dev.new()
chart.Histogram(dfd_arc$EXA_XRC,main="EXA Daily Returns",method="add.normal")
skewness(dfd_arc$EXA_XRC)
kurtosis(dfd_arc$EXA_XRC,method="excess")


250*length(dfd_arc$EXA_XRC[dfd_arc$EXA_XRC<=-1])/length(dfd_arc$EXA_XRC)			#days per year with >-1 drawdowns

draw<-cumsum(dfd_arc$EXA_XRC)-cummax(cumsum(dfd_arc$EXA_XRC))
min(draw)
dev.new()
plot(draw,main="EXA peak-to-trough drawdowns")

xr20<-cumsum(dfd_arc$EXA_XRC)-lag(cumsum(dfd_arc$EXA_XRC),k=-20)				#20-day returns
xr20[xr20<=(-3.5)]																#gauge >3.5% 20-day drawdowns
xr252<-cumsum(dfd_arc$EXA_XRC)-lag(cumsum(dfd_arc$EXA_XRC),k=-252)				#1-year returns
length(xr252[xr252<=0])/length(xr252)









	