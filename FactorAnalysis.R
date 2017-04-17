#
# Generic code for factor analysis
#

# A. Specify analysis

  id.sel <- id.xef  # cross-sectional identifiers
  startd <- as.Date("2001-12-31") #start date, usually "1999-01-29","2007-01-31",or "2009-01-30"
  df.sel <- window(merge(dfm, dfmr), start=startd) # required zoo dataframes  window(merge(dfm, dfmr), start=startd)

  dep.sel <- "FX_XR_HR"  # dependent variable/ target return
  xpl.sel <- "MIRGS_1MAR"  # primary explanatory variable
  zn=F  # set to TRUE is primary explanatory variable is already zn-scored

  l.fcc <- l.fcg # list of factor groups for correlation analysis (else set to NULL)  l.fcg
  lz = T # set to TRUE is the factor constituents and composites are already available as zn-scores (else internally calculated)
  cv.bms <- c("GLB_DRB_XR")  # benchmarks for directional correlation analysis

  rel <- F  #if true, both dependent and explanatory are converted to relative terms (necessary of they do not exist)
  bname <- NULL  # blacklisting applied to relative terms (default is NULL)


# B. Series creation or transformation

  # B.1. If required: calculation of relative variables


  if(rel) {
    for (sn in c(dep.sel, xpl.sel)){ # loop through dependent/ explanatory
      if(!all(is.element(paste(id.sel, paste(sn, "R", sep=""), sep="_"), names(df.sel)))){
        df.ser <- f_relbm(df.sel, id.sel, list(sn), blname=bname)
        df.sel <- merge(df.sel, df.ser)
        print(paste(sn, "R had to be created", sep=""))
      }
    }
    xpl.self <- paste(xpl.sel, "R", sep="")  # re-name explanatory factor
    dep.self <- paste(dep.sel, "R", sep="")  # re-name dependent return
  }else{
    xpl.self <- xpl.sel # re-name explanatory factor
    dep.self <- dep.sel # re-name dependent return
  }

  # B.2. Zn-scoring (if new or unavailable)

  if(!zn){ # primary explanatory not yet z-scored
    if(!is.element(paste(id.sel[1], paste(xpl.self,"Z",sep=""), sep="_"), names(df.sel))){ #no previously calculated z-score
      df.szn <- f_znscore(df.sel, id.sel, xpl.self, imean=0, isd=0, inwin=36,
                          strict=T ,maxwin=120, itrim=0.1, ctrim=0.1, neutral=0, trabs=3)[[1]]
      df.sel<-merge(df.sel,df.szn)
      print(paste(xpl.self, "Z had to be created", sep=""))
    }
    xpl.selfz <- paste(xpl.self, "Z", sep="")
  } else{
    xpl.selfz <- xpl.self
  }

  if(!lz){ # correlation factor/constituents z-scores not available
    cv.fzn <- unique(unlist(lapply(l.fcc, function(x) x[[2]])))  # all factors that may need z-sc
    for (i in 1:length(cv.fzn)){
      if(!is.element(paste(id.sel[1], paste(cv.fzn[i],"Z",sep=""), sep="_"), names(df.sel))){ #no previously calculated z-score
        df.szn <- f_znscore(df.sel, id.sel, cv.fzn[i], imean=0, isd=0, inwin=36,
                            strict=F ,maxwin=120, itrim=0.1, ctrim=0.1, neutral=0, trabs=3)[[1]]
        df.sel <- merge(df.sel,df.szn)
      }
    }
    suppressWarnings(rm(l.fcz))
    for (i in 1:length(l.fcc)) f_fgroup("l.fcz", names(l.fcc)[i], l.fcc[[i]]$const, zscore=T, descr=l.fcc[[i]]$descr)
    for (i in 1:length(l.fcz)){
      df.wz <- f_wz(df.sel, id.sel, l.fcz[[i]][[2]], outn=paste(names(l.fcz)[i], "Z", sep=""))[[1]]
      df.sel <- merge(df.sel, df.wz)
    }
  }

# C. All analyses

# C.1. Descriptive analysis	of final explanatory variable

  graphics.off()
  f_describe(df.sel, id.sel, xpl.self, balpan=F, bwts=0.7)
  graphics.off()
  f_describe(df.sel, id.sel, xpl.selfz, balpan=F, bwts=0.7)

# C.2. Ad-hoc illustrative line charts

  graphics.off()
  f_2line(df.sel, id.sel, c("BPB", "BPBU"), bm="GLB", dax=F, title=NULL,
            leg1=NULL, leg2=NULL)

# C.3. Correlation with other indicators

  cv.sel <- l.fcg$MIRGS[[2]]   # l.nwz$BPBACKR[[2]]
  cv.oth <- paste(names(l.fcg), "Z", sep="") #  paste(names(l.nwz), "Z", sep="")
  m.sel <- f_pcor(df.sel, id.sel, cv.names=cv.sel, cv.bms=cv.oth, fps=0, single=TRUE)
  graphics.off()
  f_heatmap(m.sel, gtitle="Indicator/factor correlations")

# C.4. Correlation and co-movements with returns

  graphics.off()
  f_graphview(df.sel, id.sel, dep.self, xpl=xpl.selfz, dma=3, xma=3, intv="years", showcs=F)

# C.5. Naive PnL

  graphics.off()
  f_qexpl(df.sel, id.sel, dep=dep.self,  xpl=xpl.selfz, xpltrans="MAV", bms=cv.bms, xplpv=c(1), start=startd, cspnl=F)

# C.6. Check for neutral level

  graphics.off()
  f_hovstat(df.sel, id.sel, dep.self, xpl.self, sub=1)

# D. Cumulative return/ persistence analysis

  if(F){
    ret.sel <- c("FX_XR", "FX_XR_H")
    cv.cum <- as.vector(outer(id.xef, ret.sel, paste, sep="_"))
    dfm.cum <- do.call(merge,lapply(df.sel[, cv.cum], function(x) cumprod(1+na.trim(x/100))))
    names(dfm.cum) <- paste(cv.cum, "CUM", sep="_")
    df.sel1 <- merge(df.sel, dfm.cum)
    graphics.off()
    f_2line(df.sel1, id.sel, paste(ret.sel, "CUM", sep="_"), bm="GLB", dax=F, t=NULL, leg1=NULL, leg2=NULL)
  }




