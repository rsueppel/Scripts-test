#Utilities

# Test 19/04/2017 11:40 Review Change

# ===
# Convenience functions to analyze 'tail' of dataframes
# ===

f_tn <- function(df, n=50) tail(names(df), n)
f_tdf <- function(df, n=30) tail(df[, (ncol(df) - n):ncol(df)])

# ===
# Extract time window from dataframe
# ===

dfm[index(dfm) >= as.Date("2008-01-01") & index(dfm) < as.Date("2009-01-01"), 1:2]

# ===
# Eliminate category panel in a dataframe
# ===

df.tmp <- dfm[, setdiff(names(dfm), paste(id.xdx, "BPBEL_BN", sep="_"))]


# ===
# Check existence of indicators in panel dataframe
# ===

cv_xsecs <- id.xeq
cv_inds <- names(l.mf)
cv_check <- as.vector(outer(cv_xsecs, cv_inds, paste, sep="_"))
cv_check[!is.element(cv_check,names(dfm))]

# ===
# Check all functions for unintended global variables
# ===

cv_fcheck <- apropos("f_", mode="function")
for (i in 1:length(cv_fcheck)){
	cv_globals <- codetools::findGlobals(get(cv_fcheck[i]))
	print(list(cv_fcheck[i], cv_globals))
}

# ===
# Show all names of panel variables in alphabetical order
# ===

df.tmp <- dfm
cs.tmp <- "AUD_"
sort(gsub(cs.tmp, "", names(df.tmp)[grep(cs.tmp, names(df.tmp))]))

# ===
# Extract names legend from factor list
# ===

l.leg <- l.fcg
sort(paste(names(l.leg), unlist(lapply(l.leg, function(x) x[1])), sep=":"))

# ===
# Update packages from local file
# ===

path <- "M:/Trading/EXA/2. Quantitative Research/2. Support functions/Packages/Binary packages"
cv.pkgs <- c("Management_0.1.zip", "Panel_0.1.zip", "Analysis_0.1.zip", "Signals_0.1.zip") 
install.packages(paste(path, cv.pkgs, sep="/") , repos = NULL, type ="binary")



#
# Investigate zn-scores 
#

#a. Descriptive analysis of specific variable/zn-score

m_specs																			#view specs table
id_sel=id_xeq
df_sel=dfm

varn<-"BWSTEEP_3MAR"																#variable name														
graphics.off()
f_2lines(df_sel,id_sel,varn,paste(varn,"Z",sep=""))								#check zn-score consistency
f_describe(df_sel,id_sel,varn)
f_describe(df_sel,id_sel,paste(varn,"Z",sep=""))

#c. [Optional] Exploratory correlation/signficance table

f_pcor(df_sel,id_sel,paste(rownames(m_specs),"Z",sep=""),dname=dep)				#print correlation tables

#
# Explore factors
#

xpl_sel<-"EQ_XRDIELNRBZ"    #"FX_CRRDV3Y" 
id_sel<-id_xdx
df_sel<-window(dfm,start=as.Date("1999-01-29")) #"1999-01-29","2007-01-31","2009-01-30"
dep_sel<-"FX_XR_H2"   #"FX_XRNR" #"FX_XR_HR" 
neg=F
bms=c("GLB_DRB_XR")

graphics.off()
f_describe(df_sel,id_sel,xpl_sel ,line_bm="mean",balpan=F)

if (neg) {
  exdf<-(-1)*df_sel[,paste(id_sel,xpl_sel,sep="_")];names(exdf)<-paste(names(exdf),"N",sep="")
  df_sel<-merge(df_sel,exdf)
  xpl_sel<-paste(xpl_sel,"N",sep="")
}
graphics.off()
f_graphview(df_sel,id_sel,dep_sel,xpl=xpl_sel,dma=1,xma=1,intv="years",showcs=F)

graphics.off()							
f_xexpl(df_sel,id_sel,dep=dep_sel,deptrans=c(1,1),xpl=xpl_sel,xpltrans="MAV",bms=bms,xplpv=c(1),start=as.Date("1999-01-29"))

f_hovstat(df_sel,id_sel,dep_sel,xpl_sel,sub=3)

f_pcor(df_sel,id_sel,c(dep_sel,xpl_sel),paste(names(l_mf),"R",sep=""),fps=1)
f_pcor(df_sel,id_sel,c(xpl_sel,dep_sel),paste(names(l_mf),"R",sep=""),fps=0)


#
# Compare consistency of monthly/daily returns/signals in E section
#

#A. Compare signals in daily frequency

id_x<-id_creq
cv_x<-c("RSIG",cv_zn)
cs="EUR"
dfm_ex<-merge(dfm,dfm_rsig)
dfd_ex<-merge(dfd,dfd_rsig)

dfm_check<-dfm_ex[,paste(cs,cv_x,sep="_")]
dfd_check<-dfd_ex[,paste(cs,cv_x,sep="_")]

dfd_m_check<-lag(f_convfreq(dfm_check,dfd_check,from="m",to="d",meth="s",fnp=T),k=1)

graphics.off()
for (c in 1:length(cv_x)){
  dfd_check2<-na.trim(merge(dfd_m_check[,c],dfd_check[,c]))
  dev.new()
  plot(dfd_check2[,2],type="l")
  lines(dfd_check2[,1],col="red")
  title(main=cv_x[c])
}

#B. Compare returns in monthly frequency

ret<-dep
dfd_rcheck<-dfd[,paste(id_x,ret,sep="_")]
dfm_rcheck<-dfm[,paste(id_x,ret,sep="_")]

dfm_d_rcheck<-f_convfreq(dfd_rcheck,dfm_rcheck,from="d",to="m",meth="a")

graphics.off()
for (c in 1:length(id_x)){
  dfm_check2<-na.trim(merge(dfm_rcheck[,c],dfm_d_rcheck[,c]))
  dev.new()
  plot(cumsum(dfm_check2[,1]),type="l")
  lines(cumsum(dfm_check2[,2]),col="red")
  title(main=id_x[c])
}


