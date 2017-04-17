#
# Generic Comprehensive Factor Analysis
#

#A. General preparation

    startd   <- as.Date("2000-01-01") # start date, usually from strategy variable "sdt" outline
    id.sel   <- id.ighy #id.fxdu  #id.xef #id.eqdu # cross-sectional identifiers
    cat.sel  <- cat.ighy #cat.xef #cat.eqdu #cat.xdu 
    df.sel   <- window(merge(dfm), start = startd) # required zoo dataframes
    dfd.ref  <- window(merge(dfd), start = startd) # daily dataframe for cfrequency onversion
    dep.sel  <- "IGHY_XRN" # dependent variable/target return
    xpl.sel  <- "HYIG_RRATE_D3V1YMA"
    xpl.fcg  <- NULL # factor group to which the explanatory belongs
    cv.bms   <- c("GLB_DRB_XR") # benchmarks for directional correlation analysis
    bname    <- NULL # blacklisting applied to relative terms (default is NULL)
    xpl_is_z <- FALSE # set to TRUE is primary explanatory variable is already zn-scored
    need_rel <- FALSE # if TRUE, both dependent and explanatory are converted to relative terms (necessary if they do not exist)
    ishyb    <- FALSE # if TRUE, the strategy is treated as a hybrid in the position function
    whichRSD <- "RSDL" # can be "RSD" or NULL depending on whether we ned RSD in the position function 
    
#B. Dataset and factor group analysis

  #B.1. Specificy dataframe and factor groups
    
    all_zn <- FALSE  # logical, if TRUE the zn-scores are already in the dataframe (else internally calculated)
    suffix <- character(0) # can be character(0), "Z", "RZ", etc. # ending to be removed from list constituent names
    l.fcc  <- l.awz #l.fcg #l.fcg #l.owz # #
    cv.fzn <- unique(unlist(lapply(l.fcc, function(x) x[[2]])))
    cv.fzn <- substr(cv.fzn, start = 1, stop = nchar(cv.fzn)-length(suffix))

    #B.2. Create factor and constituent zn-scores
    
    if(!all_zn){  # correlation factor/constituents z-scores not available
      for (i in 1:length(cv.fzn))
        df.sel <- f_merge(df.sel, setNames(f_zni(df.sel[, paste(id.sel, cv.fzn[i], sep = "_")], trabs = 3), 
                                           paste(id.sel, paste(cv.fzn[i], "Z", sep = ""), sep = "_")), print = TRUE)
      suppressWarnings(rm(l.fcz))
      for (i in 1:length(l.fcc)) 
        f_fgroup("l.fcz", names(l.fcc)[i], substr(l.fcc[[i]]$const, start = 1, stop = nchar(l.fcc[[i]]$const)-length(suffix)), zscore = T)
      l.fgwz <- f_wznfgl(l.fcz, id.sel, df_m = df.sel, df_d = NULL)
      df.sel <- f_merge(df.sel, l.fgwz[[1]], print = TRUE)
    } else l.fcz <- l.fcc

    
  #B.3. Full cross-factor correlation heatmap

    graphics.off()
    f_heatmap(m.sel <- f_pcor(df.sel, id.sel, cv.names <- paste(names(l.fcc), "Z", sep=""), cv.bms = cv.names, fps=0, single=TRUE, print=FALSE),
              gtitle="Full cross-factor correlations", cluster = "both")

  #B.4. Individual factor cross-correlation heatmap

    cv.sel <- l.fcc[[xpl.fcg]][[2]]
    graphics.off()
    f_heatmap(f_pcor(df.sel, id.sel, cv.names = cv.sel, cv.bms = paste(names(l.fcc), "Z", sep=""), fps=0, single=TRUE, print=FALSE),
              gtitle="Individual factor cross-correlations")

#C. Predictor analysis

  #C.1. If required: calculation of relative variables

    if(need_rel){
      v.reladded <- setdiff(as.vector(outer(id.sel, paste(c(dep.sel, xpl.sel), "R", sep=""), paste, sep="_")), names(df.sel))
      if(length(v.reladded)>1){ print("The following relative series had to be created") ; print(v.reladded) }
      
      df.sel <- f_merge(df.sel, f_relbm(df.sel, id.sel, c(dep.sel, xpl.sel), blname=bname))
    }
    xpl.self <- paste(xpl.sel, ifelse(need_rel, "R", ""), sep="")  # re-name explanatory factor
    dep.self <- paste(dep.sel, ifelse(need_rel, "R", ""), sep="")  # re-name explanatory factor
    
  #C.2. Zn-scoring (if new or unavailable)

    if(!xpl_is_z) # primary explanatory not yet z-scored
      df.sel <- f_merge(df.sel, setNames(f_zni(df.sel[, paste(id.sel, xpl.self, sep = "_")], trabs = 3), 
                                         paste(id.sel, paste(xpl.self, "Z", sep = ""), sep = "_")), print = TRUE)
    xpl.selfz <- paste(xpl.self, ifelse(!xpl_is_z, "Z", ""), sep="")  # re-name explanatory factor

  #C.3. Descriptive analysis of final explanatory variable

    graphics.off() ; f_describe(df.sel, id.sel, xpl.self, balpan=F, bwts=0.5)

    graphics.off() ; f_describe(df.sel, id.sel, xpl.selfz, balpan=F, bwts=0.5)

    graphics.off()  # ah-hoc descriptive line charts
    f_2line(df.sel, id.sel, c(dep.sel, xpl.sel), bm="GLB", dax=F, title=NULL, leg1=NULL, leg2=NULL)

  #C.4. Correlation and co-movements with returns

    graphics.off() ; f_explortest(df.sel, id.sel, dep.self, xpl=xpl.selfz, vdma=c(1, 3), xma=1)
    
    graphics.off() ; f_hovstat(df.sel, id.sel, dep.self, xpl.self, sub=4)

  #C.5. Naive PnL
    
    graphics.off() ; f_qexpl(df.sel, id.sel, dep=dep.self, vxpl = xpl.selfz, bms=cv.bms)
    
    graphics.off() ; f_qexpl(df.sel, id.sel, dep=dep.self, vxpl = c(xpl.selfz, "DUEQ_PRNI_3Mv6MZ"), bms=cv.bms)
    
#D. Daily PnL with test for slippage effects

    dfd.sel <- f_merge(dfd.ref, f_convfreq(df.sel[, as.vector(outer(id.sel, c(xpl.selfz, dep.self), paste, sep="_"))], dfd.ref, 
                          from = "m", to = "d", meth="s", fnp = FALSE))
    dfd.rsig <- f_wzn(dfd.sel, id.sel, cv_zn = xpl.selfz, v_wgts = NULL, rezn = TRUE, trabs = 3, outname = "RSIGZ")
    l.pos <- f_pos(merge(dfd.sel, dfd.rsig), id.sel, cat.sel, sig = "RSIGZ", v_rd = 1, 
                   rsd = whichRSD,
                   cashn = ifelse(length(cat.sel)>1, F,T), 
                   hyb= ishyb)
    l.pnl.ist <- f_pnl(merge(dfd.sel, l.pos[[1]]), id.sel, cat.sel, posn="POS", ret="XR", rblag = 2, gname="IST")
    l.pnl.nip <- f_pnl(merge(dfd.sel, l.pos[[2]]), id.sel, "NIP", posn="POS", ret="XR", rblag = 2, gname="NIP")
    dfd.pnl <- merge(l.pnl.nip[[3]], l.pnl.ist[[1]]) # dataframe of exact instrument/global PnLs and proxy NIP Pnls
    graphics.off()
    f_xpnleval(dfd.pnl, id.sel, pnlname="NIP_PNLX", gpnlname="GLB_IST_PNL", bmarks=NULL, start=sdt)
    
#E. Cumulative return/ persistence analysis

    df.sel1 <- merge(df.sel, setNames(do.call(merge, lapply(df.sel[, as.vector(outer(id.sel, dep.sel, paste, sep="_"))], 
                                                            function(x) log(cumprod(1+na.trim(x/100))))),
                                      paste(as.vector(outer(id.sel, dep.sel, paste, sep="_")), "CUM", sep="_")))
    graphics.off()
    f_2line(df.sel1, id.sel, paste(dep.sel, "CUM", sep="_"), bm="GLB", dax=F, t=paste("Log of ", dep.sel, "_CUM", sep=""), leg1=NULL, leg2=NULL)
    