
# Indicators creation
#

#A. Basic preparation, including sourcing of packages and custom functions

  rm(list=ls()) # cannot be called from inside a function
  
  library(Management)
  f_setup(path="rdt", exclscript = c("SeriesTransformer.r", "TempScript.r")) # f_setup called on the paths defined above
  d.lastwd <- as.Date("2017-02-28")  # set last working day for which data are updated
  live_mode <-  FALSE # if FALSE, the script will shunt the slower parts of the code (no SDs and BETAs created)
  
  suppressWarnings(rm(list=c("l.mi", "l.mrd")))  # removal of any list of indicators

#B. Loading or manual input of required data

  #B.1. Identifiers (all available cross-sections)

  id.fxrus <- c("BRL", "CLP", "CNY", "COP", "IDR", "ILS", "INR", "KRW", "MXN", "MYR", "PEN", "PHP", "SGD", "THB", "TWD", "ZAR",
                "AUD", "CAD", "EUR", "JPY", "NZD") # USD crosses
  id.fxreu <- c("CZK", "HUF", "PLN", "RON", "CHF", "NOK", "SEK")  # EUR crosses
  id.fxred <- c("RUB", "TRY", "GBP")  # basket crosses
  id.allfx <- c(id.fxrus, id.fxreu, id.fxred)
  id.ez <- c("DEM", "ESP", "FRF", "ITL", "NLG")
  id.allcs <- c(id.allfx, id.ez, "USD")

  id.co.sa <- c("CFE", "COR", "CTN", "SGR", "SOY", "WHT", "CAT", "HOG") # Ags and softs (high-storage-cost commos) use SA carries
  id.co.nsa <- c("BRT", "ALM", "CPR", "LED", "NIC", "ZNC", "GLD", "SIV") # Industials, PM and energy (easier-to-store commos) use non-SA carries
  id.commo <- c(id.co.sa, id.co.nsa)
  id.comfx <- c("ARS", "AUD", "BRL", "CAD", "COP", "CLP", "CNY", "EUR", "IDR", "INR", "JPY", "KZT", "KRW", "MXN", "NGN", 
                "NOK", "PEN", "PHP", "RUB", "THB", "UAH", "VND", "ZAR")
  id.cred <- c("EUR_HY", "EUR_IG", "USD_HY", "USD_IG")
  id.all <- unique(c(id.allcs, id.commo, id.cred, "GLB", id.comfx))

  #B.2. Categories

  cat.ass <- c("2YIRS", "5YIRS", "5YXCS", "10YIRS", "FX", "EQ", "CO", "DRB", "IG_CR", "HY_CR", "COM")
  cat.mes <- c("XR", "CRY_NSA", "CRY_SA", "CRY", "SPOT", "XRD", "CRYD", "NA", "IV", "WT")
  l.compo <- outer(cat.ass, cat.mes, paste, sep="_")
  cat.compo <- c(cat.mes, as.vector(l.compo))

  cat.inf <- c("INFHO", "INFHN", "INFCO", "INFCN", "CPIHO_SA", "CPICO_SA", "CPIHN_SA", "CPICN_SA", "INFT_LB", "INFT_UB", "INFTO", "INFLSW10Y")
  cat.yld <- c("2YIRS_YLD", "5YIRS_YLD", "5YXCS_YLD", "10YIRS_YLD", "5YGOV_YLD", "2YR_YLD")
  cat.gwth <- c("EMG3MA", "EMG3MA_D3M", "EMG_D1M","PTGDP", "EWPG", "IP_POYA", "PMI",
                "EMG", "EWEMG", "EWEMG_D1M", "EWEMG3MA", "EWEMG3MA_D3M", "VEH_SAL", "UMR_SA", "NAIRU", "TWPMI", "EGIZ", "EGIZ_D3M")
  cat.cred <- c("PC_POYA", "PC_DGDP")
  cat.ext <- c("OPENNESS", "FDR", "CAR", "CAR_OilAdj",
               "NIR", "CRN", "NPIR",
               "IB_TEPS", "IB_TPER", "IB_FEPS", "IB_FPER", "IB_PADJ", "TPBR", "TDYR",
               "DS_FEPS", "DS_TPER", "DS_TPBR", "DS_TDYR", "DS_TCYR",
               "EYT", "BPT", "DVT", "EYRSX", "BPRSX", "DVRSX",
               "MSCI_FEPS", "MSCI_FPER", "MSCI_TEPS", "MSCI_TPER", "MSCI_TPBR", "MSCI_TDYR",
               "PC_POYA", "PC_DGDP", "PCGD", "M3_POYA",
               "FXR_DMB", "MB_POYA", "FXR_DGDP", "FXR_PMOM", "BXE_DMB",
               "RTG", "LC",
               "NEL", "REL", "RES")
  cat.econ <- c(cat.compo, cat.inf, cat.yld, cat.gwth, cat.cred, cat.ext)
  l.all <- outer(id.all, cat.econ, paste,sep="_")
  cv.import <- as.vector(l.all)
  cat.oth <- c("COM_EN", "FOOD_GC", "USD_CJC", "VIX", "EIX", "GLB_MAN_PMI", "GLB_CMP_PMI", "HFRXGL_XR", "HFRXM_XR", "G3_LCZ",
               "USD_ISMJOB_MFG_SA", "USD_ISMJOB_SER_SA", "USD_ISMJOBMFG_SURVNET", "USD_ISMJOBSER_SURVNET", 
               "USD_CONFJOB_CURRNET_SA", "USD_COFCJOB_ETI_SA", "USD_INJC_SA")
  cv.import <- c(cv.import, cat.oth)

  #B.3. Import all external data

  dfm0 <- f_dataloader(folder=folder.data, freq="m", csids=cv.import, enddate=d.lastwd, useIndic = FALSE)
  dfd0 <- f_dataloader(folder=folder.data, freq="d", csids=cv.import, enddate=d.lastwd, useIndic = FALSE)

  #B.4. Basic renamings

  cv.old <- c("COM_EN", "FOOD_GC")#, paste(id.co.sa, "CO_CRY_SA", sep="_"), paste(id.co.nsa, "CO_CRY_NSA", sep="_")) # Andy modified, Mar_16
  cv.new <- c("ENGY", "FOOD")#, paste(id.co.sa, "CO_CRY", sep="_"), paste(id.co.nsa, "CO_CRY", sep="_")) # Andy modified, Mar_16
  for(i in 1:length(cv.old)){
    names(dfm0) <- gsub(cv.old[i], cv.new[i], names(dfm0))
    names(dfd0) <- gsub(cv.old[i], cv.new[i], names(dfd0))
  }

  dfd <- f_wdadd(window(dfd0, end=d.lastwd), 1)
  dfm <- window(dfm0, end=d.lastwd)

  f_addlist("l.mi", "FX_XR", "FX forward return against dominant cross/basket")
  f_addlist("l.mi", "FX_CRY", "1M FX forward implied carry against dominant cross/basket")
  f_addlist("l.mi", "FX_NA", "FX nominal appreciation against dominant cross/basket")
  f_addlist("l.mi", "FX_IV", "1M ATMF FX implied volatility, for pairs against dominant cross and 
                              quoted in accordance with gneneral market convention. 
                              e.g. AUD_FX_IV is the 1M ATMF implied vol for AUDUSD, not USDAUD")
  f_addlist("l.mi", "FX_SPOT", "USDXXX spot FX rate")
  
  f_addlist("l.mi", "2YIRS_XR", "2YIRS return")
  f_addlist("l.mi", "2YIRS_CRY", "2YIRS monthly carry")
  f_addlist("l.mi", "5YIRS_XR", "5YIRS return")
  f_addlist("l.mi", "5YIRS_CRY", "5YIRS monthly carry")
  f_addlist("l.mi", "5YXCS_XR", "5YXCS return")
  f_addlist("l.mi", "5YXCS_CRY", "5YXCS monthly carry")
  f_addlist("l.mi", "10YIRS_XR", "10YIRS return")
  f_addlist("l.mi", "10YIRS_CRY", "10YIRS monthly carry")
  
  f_addlist("l.mi", "EQ_XR", "Equity future return, in local currency")
  f_addlist("l.mi", "EQ_CRY", "Earnings yield-based carry, funded in local currency")

  f_addlist("l.mi", "EQ_XRD", "Equity future return, in USD")
  f_addlist("l.mi", "EQ_CRYD", "arnings yield-based carry, funded in USD")

  f_addlist("l.mi", "CO_XR", "Commodity futures return")
  f_addlist("l.mi", "CO_CRY", "Commodity futures curve steepness based carry b/w first and second contract, SA/NSA adjusted")
  
  f_addlist("l.mi", "CR_XR", "CDS indices returns")
  f_addlist("l.mi", "CR_CRY", "CDS indices carry, ie annualized index spread")
  
  f_addlist("l.mi", "GLB_MAN_PMI", "Global Manufacturing PMI, NGDP weighted average of 
                                    G10, CNY, SGD, INR, ILS, RUB, PLN, TRY, ZAR and MXN")
  f_addlist("l.mi", "GLB_SER_PMI", "Global Service PMI, NGDP weighted average of USD, EUR, GBP, AUD and MXN")
  f_addlist("l.mi", "GLB_CMP_PMI", "Global Composite PMI, NGDP weighted average of 
                                    G10, CNY, SGD, INR, ILS, RUB, PLN, TRY, ZAR and MXN")
  
  f_addlist("l.mi", "ENGY", "Thomson Reuters Energy Commodity Index, equally weighted USD prices
                             (crude oil, heating oil and natural gast)  ")
  f_addlist("l.mi", "FOOD", "In-house food price index, based on weighted monthly changes in USD prices (10 food items)")
  f_addlist("l.mi", "VIX", "CBOE Market Volatility Index, popular measure of the implied volatility of SnP 500 index options")

  #B.5. Pseudo-daily carries

  id.cry <- unlist(sapply(c("YIRS_CRY", "YXCS_CRY"), FUN=function(x) names(dfm)[grepl(x, names(dfm))])) #"CO_CRY"
  dfm.cry <- dfm[, id.cry]
  dfd.cry <- lag(f_convfreq(dfm.cry ,dfd, from="m" ,to="d", meth="s", fnp=T), k=1)
  dfd <- merge(dfd, dfd.cry)

  #B.6. Spot renamings

  dfd.eurx <- setNames(dfd[, paste(c("GBP", "NOK", "SEK"), "FX_SPOT", sep="_")] / dfd[, "EUR_FX_SPOT"], paste(c("EURGBP", "EURNOK", "EURSEK"), "FX_SPOT", sep="_"))
  dfd.usdx <- setNames(dfd[, paste(c("AUD", "CAD", "GBP", "JPY", "NZD"), "FX_SPOT",sep="_")], paste(c("USDAUD", "USDCAD", "USDGBP", "USDJPY", "USDNZD"), "FX_SPOT", sep="_"))
  dfd.spot <- merge(dfd.eurx, dfd.usdx)
  dfm.spot <- f_convfreq(dfd.spot, dfm, from="d", to="m", meth="l")
  dfd <- merge(dfd, dfd.spot)
  dfm <- merge(dfm, dfm.spot)
  
  #B.7. PMI adjustment
  
  names(dfm) <- gsub("GLB_CMP_PMI", "GLB_PMI", names(dfm))
  f_addlist("l.mi", "GLB_PMI", "Global Composite PMI, NGDP weighted average of G10, CNY, SGD, INR, ILS, RUB, PLN, TRY, ZAR and MXN")
  
  id.pmi <- substr(names(dfm[, grepl("PMI",names(dfm))]), start=1, stop=3)  # id for countries with PMI...
  id.pmix <- setdiff(id.allcs, id.pmi)  # ...and for those without
  
  id.pmic <- c("THB", "TWD")  # id of countries where PMI is non-stationary and is transformed into change
  dfm.pmic <- f_ptrans(dfm, id.pmic, "PMI00 = f_pch( PMI , 12 )")
  names(dfm.pmic) <- paste(id.pmic, "PMI", sep="_")
  
  for(jj in 1:length(id.pmic)) dfm[, paste(id.pmic[jj], "PMI", sep="_")] <- dfm.pmic[, jj]
  
  f_addlist("l.mi", "PMI", "either manufacturing or composite PMI (where available)")
  
  #B.8. Inflation adjustment
  
  dfm[, "AUD_INFHN"] <- ifelse(is.na(dfm[,"AUD_INFHN"]), dfm[,"AUD_INFHO"], dfm[,"AUD_INFHN"])
  dfm[, "AUD_INFCN"] <- ifelse(is.na(dfm[,"AUD_INFCN"]), dfm[,"AUD_INFCO"], dfm[,"AUD_INFCN"])  # fill up 'missing' early AUD data
  
  #B.9. External balances adjustment
  
  # Chile
  dfm[, paste("CLP", "FDR", sep="_")] <- na.locf(dfm[, paste("CLP","FDR",sep="_")],fromLast=T)  # ISSUE: criminal fix of missing data
  
  # Norway
  zom.ncar <- dfm$NOK_CAR
  dfm <- merge(dfm, zom.ncar)
  names(dfm)[ncol(dfm)] <- "NOK_CARO" # original Norway CAR stored separately
  dfm$NOK_CAR <- dfm$NOK_CAR_OilAdj # replace Norway's CAR with oil-adjusted balance
  
  #B.10. 5-Day average FX carries
  
  id.fx5d <- unique(substring(names(dfd)[grepl("FX_CRY", names(dfd))], 1, 3))
  dfd.cry5 <- f_ptrans(dfd, id.fx5d, "FX_CRY5 = f_avg( FX_CRY , 5)")
  dfm.cry5 <- f_convfreq(dfd.cry5, dfm, from = "d", to = "m", meth = "l")
  dfd <- merge(dfd, dfd.cry5)
  dfm <- merge(dfm, dfm.cry5)
  
  f_addlist("l.mi", "FX_CRY5", "5d average FX carry")
  
  #B.11. Hard currency equity returns
  
  id.g3bm <- c(id.ez, "EUR", "JPY", "USD")  # countries with presumed zero FX risk to benchmark (exaggeration)
  dfd.efxr <- merge( f_ptrans(dfd, setdiff(id.allcs, id.g3bm), "EF_XR = FX_XR + EQ_XR"), 
                     f_ptrans(dfd, id.g3bm, "EF_XR = EQ_XR")) # hard-currency equity returns
  dfd <- merge(dfd, dfd.efxr)
  
  dfm.efxr <- merge( f_ptrans(dfm, setdiff(id.allcs, id.g3bm), "EF_XR = FX_XR + EQ_XR"), 
                     f_ptrans(dfm, id.g3bm, "EF_XR = EQ_XR")) # hard-currency equity returns
  dfm <- merge(dfm, dfm.efxr)
  
  f_addlist("l.mi", "EF_XR", "Equity return in benchmark currency terms")

  #B.12. Commodity-specific currency appreciation
  
  m.cfx <- f_readexcel(fileName=paste(folder.data, "TransactionCosts.xls", sep=""), sheetName="COM")
  id.cfx <- rownames(m.cfx)
  dfm.fa1y <- f_ptrans(dfm, id.cfx, "CFXA_1Y = f_pch( 1 / FX_SPOT , 12 )")  # USDXXX commodity currencies appreciation over past year
  dfm.ca1y <- setNames(zoo(as.matrix(dfm.fa1y) %*% as.matrix(m.cfx), index(dfm.fa1y)), paste(colnames(m.cfx), "CFXA_1Y", sep="_"))
  dfm <- merge(dfm, dfm.ca1y)
  
  f_addlist("l.mi", "CFXA_1Y", "Commodity currencies' FX appreciation over past year")
  
  #B.13. Pseudo monthly and daily euro countries series
  
  cat.eurm <- c("MB_POYA", "CAR", "FDR",
                "5YIRS_XR", "5YIRS_CRY", "5YIRS_YLD",
                "10YIRS_XR", "10YIRS_CRY", "10YIRS_YLD",
                "FX_XR", "FX_CRY",
                "NIR", "NPIR", "2YR_YLD",
                "REL", "RES", "NEL",
                "FXR_DMB", "BXE_DMB")  # euro area monthly data used for member countries
  dfm.eurm <- f_panels(dfm, id.ez, paste("EUR", cat.eurm, sep="_"), cv.on = cat.eurm)
  dfm <- merge(dfm, dfm.eurm)
  
  cat.eurd <- c("FX_XR", "FX_CRY",
                "5YIRS_XR", "10YIRS_XR", "NPIR")  # euro area daily data used for member countries
  dfd.eurd <- f_panels(dfd, id.ez, paste("EUR", cat.eurd, sep="_"), cv.on = cat.eurd)
  dfd <- merge(dfd, dfd.eurd)
  
  #B.14. QE-related monetary base growth
  
  dfm.eubm <- dfm[, paste(id.ez, "BXE_DMB", sep="_")]
  coredata(dfm.eubm[index(dfm) < as.Date("2014-12-31"), ]) <- rep(0, length(index(dfm)[index(dfm) < as.Date("2014-12-31")]) * length(id.ez))
  dfm.g3bm <- dfm[, paste(c("GBP", "JPY", "USD"), "BXE_DMB", sep="_")]
  coredata(dfm.g3bm) <- replace(coredata(dfm.g3bm), is.na(dfm.g3bm), 0)
  
  id.qebm <- c(id.ez, c("GBP", "JPY", "USD"))
  dfm.qebm <- setNames(merge(dfm.eubm, dfm.g3bm)[, paste(id.qebm, "BXE_DMB", sep="_")], paste(id.qebm, "BXE_DMBA", sep="_"))
  id.nqbm <- setdiff(substr(grep("FXR_DMB", names(dfm), value=T), 1, 3), id.qebm)
  dfm.nqbm <- setNames(zoo(matrix(0, nrow(dfm), length(id.nqbm)), index(dfm)), paste(id.nqbm, "BXE_DMBA", sep="_"))
  dfm <- merge(dfm, dfm.qebm, dfm.nqbm)
  
  f_addlist("l.mrd", "BXE_DMB", "Central bank balance sheet expansion as % of monetary base 12m ago
                     (only available for the U.S., the U.K., eurozone and Japan)")
  f_addlist("l.mrd", "BXE_DMBA", "QE-related central bank balance sheet expansion as % of monetary base 12m ago")
  
dfmB <- dfm
dfdB <- dfd

#C. Asset related measures (Std Dev and Betas)

  #C.1. Annualized stadard deviation for set of half-times
  
#  cat.ass <- c(c("FX", "EF"), setdiff(cat.ass, "DRB"))
  cat.ass <- c("FX", "EF", "EQ")
  cv.hft <- c(21, 90, 200, 500)
  cv.pct <- c(15, 15, 30, 30)

  if(live_mode){
    for(j in 1:length(cat.ass)){
      cids <- unique(substring(names(dfd)[grepl(paste(cat.ass[j], "XR", sep="_"), names(dfd))], 1, 3))
      for(k in 1:length(cv.hft)){
	    dfd.locsd <- sqrt(252) * setNames(f_estimVaR(dfd[, paste(cids, cat.ass[j], "XR", sep="_")], half=cv.hft[k], pct=cv.pct[k], sd=T),
		             paste(cids, paste(cat.ass[j], "XR", paste(cv.hft[k], "SD", sep=""), sep="_"), sep="_"))
        if(j==1 && k==1) dfd.asds <- dfd.locsd else dfd.asds <- merge(dfd.asds, dfd.locsd)
        f_addlist("l.mi", paste(cat.ass[j], "XR", paste(cv.hft[k], "SD", sep=""), sep="_"), 
          paste(paste(cat.ass[j], "XR", sep="_"), "annualized std dev with halftime =", cv.hft[k], "days"))
      }
    }
    dfd <- merge(dfd, dfd.asds)
    dfm.asds <- f_convfreq(dfd.asds, dfm, from="d", to="m", meth="l", fnp=FALSE)	
    dfm <- merge(dfm, dfm.asds)
  } else {
      for(j in 1:length(cat.ass)){
        cids <- unique(substring(names(dfd)[grepl(paste(cat.ass[j], "XR", sep="_"), names(dfd))], 1, 3))
        for(k in 1:length(cv.hft)){
          f_addlist("l.mi", paste(cat.ass[j], "XR", paste(cv.hft[k], "SD", sep=""), sep="_"), 
            paste(paste(cat.ass[j], "XR", sep="_"), "annualized std dev with halftime =", cv.hft[k], "days"))
        }
      }  
   }
 
dfmC1 <- dfm
dfdC1 <- dfd

  #C.2. Hedged normalized returns

  cat.ass <- setdiff(cat.ass, "EF")

  cv.fs <- c("m","w")  # frequencies used for estimation
  v.flm	<- c(36, 7)  # frequencies' lookback windows
  v.fsw <- c(0.2, 0.8)  # weights of frequencies
  bmr <- c("GLB_DRB_XR")  # hedge benchmark return
  bmc <- c("GLB_DRB_CRY")  # hedge benchmark carry

  if(live_mode){
    for(j in 1:length(cat.ass)){
      cids <- unique(substring(names(dfm)[grepl(paste(cat.ass[j], "CRY", sep="_"), names(dfm))], 1, 3))
      l.h <- f_hedge(dfm, dfd, cids, xr=paste(cat.ass[j], "XR", sep="_"), cry=paste(cat.ass[j], "CRY", sep="_"), 
                     bm_xr=bmr, bm_cry=bmc, cv_freq=cv.fs, v.flm, v_hfm=NULL, v_wgts=v.fsw, trim=0, meth="LS", adj_beta=T, anchor=F, alim=2)
      dfm <- merge(dfm, do.call(merge, l.h[1:3]))  # add hedged normalized returns and carries
      dfd <- merge(dfd, do.call(merge, l.h[5:6]))
      f_addlist("l.mi", paste(cat.ass[j] , "XR_H", sep="_"), paste("GDRB-hedged", cat.ass[j], "return"))
      f_addlist("l.mi", paste(cat.ass[j] , "CRY_H", sep="_"), paste("GDRB-hedged", cat.ass[j], "carry"))
      f_addlist("l.mi", paste(cat.ass[j] , "XR_BETA1", sep="_"), paste("Directional beta for", cat.ass[j]))    
    }
  } else {
      for(j in 1:length(cat.ass)){
      cids <- unique(substring(names(dfm)[grepl(paste(cat.ass[j], "CRY", sep="_"), names(dfm))], 1, 3))
      f_addlist("l.mi", paste(cat.ass[j] , "XR_H", sep="_"), paste("GDRB-hedged", cat.ass[j], "return"))
      f_addlist("l.mi", paste(cat.ass[j] , "CRY_H", sep="_"), paste("GDRB-hedged", cat.ass[j], "carry"))
      f_addlist("l.mi", paste(cat.ass[j] , "XR_BETA1", sep="_"), paste("Directional beta for", cat.ass[j]))    
    }
  }

dfmC <- dfm
dfdC <- dfd
l.asset <- l.mi

#D. Inflation series definitions

  #D.1. Inflation series creation

  f_addlist("l.mi", "GLB_INFHE", "Global headline inflation of countries with early release")
  f_addlist("l.mi", "GLB_INFCE", "Global core inflation of countries with early release")

  f_addlist("l.mi", "INFHO", "Official headline inflation")
  f_addlist("l.mi", "INFHN", "Net headline inflation (adjusted for noticeable (>1%) one-off effects) or evident fiscal effects)")
  f_addlist("l.mi", "INFCO", "Official core inflation (predominantly CPI ex food and energy; unweighted average where more than 
                              one core measures is monitored)")
  f_addlist("l.mi", "INFCN", "Net core inflation (adjusted for noticeable (>1%) one-off effects or evident fiscal effects)")
  f_addlist("l.mi", "CPIHO_SA", "Official headline CPI, seasonally adjusted")
  f_addlist("l.mi", "CPICO_SA", "Official core CPI , seasonally adjusted")
  f_addlist("l.mi", "CPIHN_SA", "Net headline CPI, adjusted for noticeable (>1%) one-off effects or evident fiscal effects, seasonally adjusted")
  f_addlist("l.mi", "CPICN_SA", "Net core CPI, adjusted for noticeable (>1%) one-off effects or evident fiscal effects, seasonally adjusted")
  f_addlist("l.mi", "INFT_LB", "Inflation target, lower bound")
  f_addlist("l.mi", "INFT_UB", "Inflation target, upper bound")
  f_addlist("l.mi", "INFTO", "Officially targeted inflation rate (whatever calculation)")
  
  f_addlist("l.mi", "INFLSW10Y", "10Y zero-coupon inflation swap par coupon")
  
  id.allinf <- id.allcs
  
  dfm.inf <- f_ptrans(dfm, id.allinf, c(
    "INFT = 0.5 * ( INFT_LB + INFT_UB )",  # inflation target (mid-point of range)
    "INFB = 0.5 * ( INFHN + INFCN )",  # broad (net) inflation
    "INFH = 0.5 * ( INFHO + INFHN )",  # single headline inflation
    "INFB_1YMA = f_avg( INFB , 12 )",  # 1-year moving averages of broad (net) inflation
    "INFTE = 0.5 * ( INFT + INFB_1YMA )",  # effective target

    "XINF = INFTO - INFT",  # Targeted annual inflation excess over target
    "XINFC = INFCO - INFT",  # Official core inflation excess over target
    "XINFCN = -1 * ( INFCN - INFT )", # for EQDU: Excess core inflation, i.e. difference from target, net of tax effects
    "XINFB = INFB - INFT", # Broad (net) inflation excess over target
    "XINFBN = -1 * ( INFB - INFT )")) # Broad (net) inflation excess over target, negative
  dfm <- merge(dfm, dfm.inf)

  f_addlist("l.mi", "INFT", "Inflation target; mid-point of range")
  f_addlist("l.mi", "INFB", "Broad inflation, net of evident tax and administered price effects")
  f_addlist("l.mi", "INFH", "Headline inflation, average of official and net inflation")
  f_addlist("l.mi", "INFB_1YMA", "1-year moving averages of broad (net) inflation")
  f_addlist("l.mi", "INFTE", "Effective inflation target")
  
  f_addlist("l.mi", "XINF", "Officially targeted inflation rate over target")
  f_addlist("l.mi", "XINFC", "Official core inflation over target")
  f_addlist("l.mi", "XINFCN", "Net core inflation over target")
  f_addlist("l.mi", "XINFB", "Broad (net) inflation over target")
  f_addlist("l.mi", "XINFBN", "Broad (net) inflation over target, negative")

  #D.2. Annual inflation base effect (based on adjusted inflation rates) i.e. short-term inflation trends

  pch <-"pch"
  dfm.stinf <- f_ptrans(dfm, id.allinf, c(
    "CPIHO_P1M_SAAR = 100 * ( ( ( 1 + f_elv ( CPIHO_SA , 1 , pch ) / 100 ) ^ 12 ) - 1 )",
    "CPICN_P1M_SAAR = 100 * ( ( ( 1 + f_elv ( CPICN_SA , 1 , pch) / 100 ) ^ 12 ) - 1 )",
    "CPIHN_P1M_SAAR = 100 * ( ( ( 1 + f_elv ( CPIHN_SA , 1 , pch) / 100 ) ^ 12 ) - 1 )",
    "CPIHN_FBEF = 100 * ( ( ( 1 + ( CPIHN_P1M_SAAR - f_lag( CPIHO_P1M_SAAR , 11) ) / 100 ) ^ ( 1 / 12 ) ) -1 )"))
  dfm <- merge(dfm, dfm.stinf) # Headline inflation, 1-m forward base effect (latest change minus next report's year ago monthly change)

  f_addlist("l.mi", "CPIHO_P1M_SAAR", "Official CPI increase, % over one month, annualized rate")
  f_addlist("l.mi", "CPICN_P1M_SAAR", "Net core CPI increase, % over one month, annualized rate")
  f_addlist("l.mi", "CPIHN_P1M_SAAR", "Net headline CPI increase, % over one month, annualized rate")
  f_addlist("l.mi", "CPIHN_FBEF", "Net headline CPI, 1-month forward base effect (latest change minus next report's year ago monthly change)")
  
  #D.3. Inflation vs. base country
    
  l.scfx <- list(c(id.fxrus, id.ez, "USD"), id.fxreu, id.fxred)  # list of small countries
  l.bmfx <- list("USD", "EUR", c("USD", "EUR"))  # corresponding list of benchmark countries
  cv.infrb <- c("INFTO", "INFB", "INFH", "INFTE", "XINF", "XINFC", "XINFCN", "XINFB", "XINFBN")
  dfm <- merge(dfm, do.call(merge, lapply(cv.infrb, function(x) f_rlbk(dfm, x, l.scfx, l.bmfx))))
   
  f_addlist("l.mi", "INFTORB", "Officially targeted inflation rate in country relative to same rate in FX base country/countries")
  f_addlist("l.mi", "INFBRB", "Broad (net) inflation in country relative to same rate in FX base country/countries")
  f_addlist("l.mi", "INFHRB", "Headline inflation in country relative to same rate in FX base country/countries")
  f_addlist("l.mi", "INFTERB", "Effective inflation target in country relative to same rate in FX base country/countries")
  f_addlist("l.mi", "XINFRB", "Excess officially targeted inflation rate relative to same rate in FX base country/countries")
  f_addlist("l.mi", "XINFCRB", "Excess official core inflation relative to same rate in FX base country/countries")
  f_addlist("l.mi", "XINFCNRB", "Excess net core inflation relative to same rate in FX base country/countries")
  f_addlist("l.mi", "XINFBRB", "Excess broad (net) inflation relative to same rate in FX base country/countries")
  f_addlist("l.mi", "XINFBNRB", "Excess broad (net) inflation over target, negative, relative to same rate in FX base country/countries")

  dfmD <- dfm
  l.inflation <- setdiff(l.mi, l.asset)
  names(l.inflation) <- setdiff(names(l.mi), names(l.asset))

#E. Growth & activity related series definitions

  #E.1. Growth & productivity

  f_addlist("l.mi", "EMG", "Estimated GDP growth (%oya) based on high-frequency indicators")
  f_addlist("l.mi", "EWEMG", "Export-weighted estimated GDP growth (%oya) of trade partners")
  f_addlist("l.mi", "EMG_D1M", "Estimated change in GDP growth over 1 month")
  f_addlist("l.mi", "EWEMG_D1M", "Estimated change in export-weighted GDP growth of trade partners over 1 month")
  f_addlist("l.mi", "EWEMG3MA", "Export-weighted estimated GDP growth of trade partners, 3-month moving average")
  f_addlist("l.mi", "EWEMG3MA_D3M", "Change in export-weighted GDP growth trend of trade partners, 3-month moving average over 3 month")
  f_addlist("l.mi", "VEH_SAL", "Motor vehicle sales levels")
  f_addlist("l.mi", "UMR_SA", "Seasonally-adjusted unemployment rate")
  f_addlist("l.mi", "NAIRU", "Non-accelerating inflation rate of unemployment, estimated by OECD")
  f_addlist("l.mi", "TWPMI", "Trade-weighted foreign PMIs of country based on euro area, the U.S., and China")
  f_addlist("l.mi", "EGIZ", "Early growth indicator (out-of-sample z-score)")
  f_addlist("l.mi", "EGIZ_D3M", "Change in early growth indicator (out-of-sample z-score) over 3 months")

  f_addlist("l.mi", "EMG3MA", "Estimated monthly GDP growth, 3-month moving average")
  f_addlist("l.mi", "EMG3MA_D3M", "Change estimated monthly GDP growth trend, 3-month moving average over 3 months")
  f_addlist("l.mi", "PTGDP", "A. Potential output of economy from OECD where available
                              B. Average of 10Y trimmed mean and 6Y median of annual GDP growth from IMF for
                                 CNY, COP, PEN, RON, RUB, ZAR, BRL, INR, IDR, THB, TWD, PHP, MYR and SGD")
  
  f_addlist("l.mi", "EWPG", "Eligible workforce population growth")
  f_addlist("l.mi", "IP_POYA", "Industrial production, %oya")
  
  id.allgdp <- id.allcs
  
  dfm.open <- f_panels(dfm, id.ez, cv.sn="EUR_OPENNESS", cv.on="OPENNESS")
  dfm <- merge(dfm, dfm.open)
  dfm[, paste(id.allgdp, "OPENNESS", sep="_")] <- na.locf(dfm[, paste(id.allgdp, "OPENNESS", sep="_")], na.rm=F)

  dfm.egp <- f_ptrans(dfm, id.allinf, c(
    "EMG3MA_DD3M = f_dif( EMG3MA_D3M , 3 )", #second difference in change in GDP growth
    "EPG3MA = EMG3MA - EWPG",  # Estimated productivity growth
    "EGVP3MA = EMG3MA - PTGDP",  # Estimated annual GDP growth trend versus potential
    "IP_POYA_3MA = f_avg( IP_POYA , 3 )"))  # Industrial production growth trend
  dfm <- merge(dfm, dfm.egp)

  f_addlist("l.mi", "EPG3MA", "Estimated productivity growth trend; estimated growth trend minus workforce growth trend, %oya, 3-month moving average")
  f_addlist("l.mi", "EGVP3MA", "Estimated GDP growth trend versus potential growth estimate, 3-month moving average")
  f_addlist("l.mi", "IP_POYA_3MA", "Industrial production growth trend")
  
  cv.emgs <- c("EMG3MA", "EMG3MA_D3M", "EMG3MA_DD3M", "EGVP3MA", "EPG3MA", "IP_POYA_3MA")
  dfm.emgb <- do.call(merge, lapply(cv.emgs, function(x) f_rlbk(dfm, x, l.scfx, l.bmfx)))
  dfm <- merge(dfm, dfm.emgb)

  f_addlist("l.mi", "EMG3MARB", "Estimated GDP growth trend, 3-month moving average, relative to FX base country/countries.")
  f_addlist("l.mi", "EMG3MA_D3MRB", "Change in estimated GDP growth trend, over 3 month, 3-month moving average, relative to FX base country/countries.")
  f_addlist("l.mi", "EPG3MARB", "Estimated productivity growth trend, 3-month moving average, relative to FX base country/countries.")
  f_addlist("l.mi", "EGVP3MARB", "Estimated GDP growth trend versus potential, 3-month moving average, relative to FX base country/countries.")
  f_addlist("l.mi", "IP_POYA_3MARB", "Industrial production growth trend, 3-month moving average, relative to FX base country/countries.")
  
  cv.emgr <- c(cv.emgs, paste(cv.emgs, "RB", sep=""))
  dfm.egoa <- f_ptrans(dfm, id.allgdp, c(
    "EMG3MA_AO = EMG3MA / OPENNESS",
    "EMG3MA_D3M_AO = EMG3MA_D3M / OPENNESS",
    "EGVP3MA_AO = EGVP3MA / OPENNESS",
    "EPG3MA_AO = EPG3MA / OPENNESS",
    "IP_POYA_3MA_AO = IP_POYA_3MA / OPENNESS",

    "EMG3MARB_AO = EMG3MARB / OPENNESS",
    "EMG3MA_D3MRB_AO = EMG3MA_D3MRB / OPENNESS",
	"EMG3MA_DD3MRB_AO = EMG3MA_DD3MRB / OPENNESS",
    "EGVP3MARB_AO = EGVP3MARB / OPENNESS",
    "EPG3MARB_AO = EPG3MARB / OPENNESS",
    "IP_POYA_3MARB_AO = IP_POYA_3MARB / OPENNESS"))
  dfm <- merge(dfm, dfm.egoa)

  f_addlist("l.mi", "EMG3MA_AO", "Estimated GDP growth trend, 3-month moving average, divided by openness.")
  f_addlist("l.mi", "EMG3MA_D3M_AO", "Change in estimated GDP growth trend (3-month moving average) over 3 months, divided by openness.")
  f_addlist("l.mi", "EPG3MA_AO", "Estimated productivity growth trend, 3-month moving average, divided by openness.")
  f_addlist("l.mi", "EGVP3MA_AO", "Estimated GDP growth trend versus potential, 3-month moving average, divided by openness.")
  f_addlist("l.mi", "IP_POYA_3MA_AO", "Industrial production growth trend, 3-month moving average, divided by openness.")
  f_addlist("l.mi", "EMG3MARB_AO", "Estimated GDP growth trend, 3-month moving average, relative to FX base country/countries, divided by openness.")
  f_addlist("l.mi", "EMG3MA_D3MRB_AO", "Change in estimated GDP growth trend (3-month moving average) over 3 months, 
                                        relative to change in FX base country/countries, divided by openness.")
  f_addlist("l.mi", "EPG3MARB_AO", "Estimated productivity growth trend, 3-month moving average, 
                                    relative to FX base country/countries, divided by openness.")
  f_addlist("l.mi", "EGVP3MARB_AO", "Estimated GDP growth trend versus potential, 3-month moving average, 
                                     relative to FX base country/countries, divided by openness.")
  f_addlist("l.mi", "IP_POYA_3MARB_AO", "Industrial production growth trend, 3-month moving average,
                                         relative to FX base country/countries, divided by openness.")
  
  #E.2. Employement

  f_addlist("l.mi", "USD_CJC", "US Continuing Jobless Claims")
  
  dfm.ucjc <- f_panels(dfm, id.allgdp, cv.sn="USD_CJC", cv.on="UCJC")
  dfm.ujcc <- f_ptrans(merge(dfm, dfm.ucjc), id.allgdp, "UCJC_P1MN = -1 * f_pch( UCJC , 1 )")
  dfm <- merge(dfm, dfm.ujcc, dfm.ucjc)
  
  f_addlist("l.mi", "UCJC", "U.S. continuous jobless claims, level, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "UCJC_P1MN", "U.S. continuous jobless claims, % over one month, negative, pseudo panel consisting only of U.S. series.")
  
  f_addlist("l.mi", "USD_ISMJOB_MFG_SA", "ISM U.S. manufacturers survey index: employment component, same=50, SA")
  f_addlist("l.mi", "USD_ISMJOB_SER_SA", "ISM U.S. non-manufacturers survey index, employment component, same=50, SA")
  f_addlist("l.mi", "USD_ISMJOBMFG_SURVNET", "ISM U.S. manufacturers survey, employment component, higher/same/lower [+1/0/-1], net pct respondents, NSA")
  f_addlist("l.mi", "USD_ISMJOBSER_SURVNET", "ISM U.S. non-manufacturers survey, employment component, higher/same/lower [+1/0/-1], net pct respondents, NSA")
  f_addlist("l.mi", "USD_CONFJOB_CURRNET_SA", "The Conference Board, Consumer Confidence Survey, current job sentiment:
                     jobs are plentiful/not so plentiful/hard to find [+1/0/-1], net pct respondents, SA")
  f_addlist("l.mi", "USD_COFCJOB_ETI_SA", "The Conference Board, Employment Trends Index (ETI), level")
  f_addlist("l.mi", "USD_INJC_SA", "Department of Labor, U.S. initial jobless claims, level, SA")
  
  dfm.ismjmfg <- f_panels(dfm, id.allgdp, cv.sn="USD_ISMJOB_MFG_SA", cv.on="ISM_JMFG")
  dfm.ismjser <- f_panels(dfm, id.allgdp, cv.sn="USD_ISMJOB_SER_SA", cv.on="ISM_JSER")
  dfm.ismjmfgnet <- f_panels(dfm, id.allgdp, cv.sn="USD_ISMJOBMFG_SURVNET", cv.on="ISM_JMFGNET")
  dfm.ismjsernet <- f_panels(dfm, id.allgdp, cv.sn="USD_ISMJOBSER_SURVNET", cv.on="ISM_JSERNET")
  dfm.confjnet <- f_panels(dfm, id.allgdp, cv.sn="USD_CONFJOB_CURRNET_SA", cv.on="CONF_JCURNET")
  dfm.confeti <- f_panels(dfm, id.allgdp, cv.sn="USD_COFCJOB_ETI_SA", cv.on="CONF_ETI")
  dfm.uijc <- f_panels(dfm, id.allgdp, cv.sn="USD_INJC_SA", cv.on="UIJC")
  
  f_addlist("l.mi", "ISM_JMFG", "ISM manufacturers survey, employment index, unch=50, SA, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "ISM_JSER", "ISM non-manufacturers survey, employment index, unch=50, SA, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "ISM_JMFGNET", "ISM manufacturers survey, employment survey, net pct higher-to-lower respondents, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "ISM_JSERNET", "ISM non-manufacturers survey, employment survey, net pct higher-to-lower respondents, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "CONF_JCURNET", "The Conference Board, U.S. Consumer Survey, net pct respondents finding jobs plentiful vs. jobs hard to find, 
                                     pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "CONF_ETI", "The Conference Board, Employment Trends Index (ETI), level, pseudo panel consisting only of U.S. series.")
  f_addlist("l.mi", "UIJC", "Department of Labour, U.S. initial jobless claims, level, pseudo panel consisting only of U.S. series.")
  
  dfm <- merge(dfm, dfm.ismjmfg, dfm.ismjser, dfm.ismjmfgnet, dfm.ismjsernet, dfm.confjnet, dfm.confeti, dfm.uijc)
  
dfmE <- dfm


#F. External basic balance

  f_addlist("l.mi", "MTR",    "Mechandise trade balance ratio: 
                               12mma of annualized merchandise trade balance divided by 12mma of annualized GDP")
  f_addlist("l.mi", "MEX",    "Merchandise trade exports, USD billion, annualized")
  f_addlist("l.mi", "USDGDP", "Nominal GDP in USD billion, annualized and extrapolated 
                               (12mma of quarter averages, updated once per quarter)")
  f_addlist("l.mi", "FDR",    "Net foreign direct investment ratio: 
                               24m trimmed mean (1/8 off each end) of annualized net FDI/USDGDP")
  f_addlist("l.mi", "CAR",    "Adjusted current account balance ratio: 
                               12mma of annualized current account balance(updated once per quarter)/USDGDP 
                               - concurrent MTR + latest MTR)")
  id.allbbr <- c(id.allfx, "USD")
  
  dfm.xbbr <- f_ptrans(dfm, id.allbbr, "BBR = CAR + FDR")  # External basic balance, % of GDP, 12-month average
  dfm <- merge(dfm, dfm.xbbr)
  f_addlist("l.mi", "BBR", "External basic balance, % of GDP, 12-month average")

  dfmF <- dfm
  l.activity <- setdiff(l.mi, c(l.asset, l.inflation))
  names(l.activity) <- setdiff(names(l.mi), c(names(l.asset), names(l.inflation)))

# G. Additional raw data documentation

  #Interest rates markets
  f_addlist("l.mrd", "NIR", "1-month LIBOR rate or closes equivalent (nominal interest rate)")
  f_addlist("l.mrd", "CRN", "nominal interest rate differential to dominant cross")
  f_addlist("l.mrd", "NPIR", "1-month LIBOR or better representation monetary policy controlled rate")
  f_addlist("l.mrd", "NPIRMEC", "Average market-implied policy rate bias (curve and forecasts)")
  f_addlist("l.mrd", "LTIR", "2-year swap yield")
  f_addlist("l.mrd", "5YGOV_YLD", "Generic 5-year government bond yield")
  f_addlist("l.mrd", "2YR_YLD", "2-year interest rate swap yield")
  
  #Equity markets

  f_addlist("l.mrd", "IB_TEPS",   "IBES 12m trailing EPS 
                                   A. Constituent TEPS:  
                                      (M*EPS[0]+(12-M)*EPS[1])/12 where
                                      EPS[0] = EPS for last fiscal year
                                      EPS[1] = consensus EPS estimates for current fiscal year
                                      M = number of months reamaining before current fiscal year end
                                      IBES uses both actual and estimated earnings and negative earnings are included
                                   
                                   B. Index TEPS:  
                                      SUM(index shares[i]*constituent TEPS[i])/SUM(index shares[i]*price[i])*index level
                                   
                                   C. Note the unit for Index TPES is NOT local currency  
                                      as a multiplication with the index level is applied")
  
  f_addlist("l.mrd", "IB_TPER",   "IBES 12m trailing PE ratio, NOT directly available from I/B/E/S 
                                   calculated in workbook using: IB_PADJ/IB_TEPS")
            
  f_addlist("l.mrd", "IB_FEPS",   "IBES 12m forward EPS, directly available from I/B/E/S via DataStream
                                   A. Constituent FEPS
                                      (M*EPS[1]+(12-M)*EPS[2])/12 where,
                                      EPS[1] = consensus EPS estimates for current fiscal year
                                      EPS[2] = consensus EPS estimates for next fiscal year
                                      M = number of months reamaining before current fiscal year end
                                      IBES uses estimated earnings and negative earnings are included
                                   
                                   B. Index FEPS:  
                                      SUM(index shares[i]*constituent FEPS[i])/SUM(index shares[i]*price[i])*index level
  
                                   C. Note the unit for Index FPES is NOT local currency  
                                      as a multiplication with the index level is applied")
  
  f_addlist("l.mrd", "IB_FPER",   "IBES 12m forward PE ratio, directly available from I/B/E/S via DataStream
                                   SUM(index shares[i]*price[i])/ SUM(index shares[i]*constituent FEPS[i])")
                                   
  f_addlist("l.mrd", "IB_PADJ",   "Price adjustment factor for weekly IBES price series: 
                                   SUM(shares[i]*price[i])/sum(shares[i])
                                   Daily price level from Bloomberg/Weekly price level directly from IBES")
  
  f_addlist("l.mrd", "TPBR",      "12m trailing price to book ratio
                                   A. Calcualtion methodolgy is the same as TPER
                              
                                   B. BPS is expressed as common equity divided by number of shares outstanding.
                                   This includes stock held by common shareholders, as well as a corporation's 
                                   retained earnings and any contributions that exceed the stock's normal value, 
                                   which is called additional paid-in capital.

                                   C. A source prefix is absent because a number of trailing price to book ratios
                                   come from Bloomberg rather than IBES)")
  
  f_addlist("l.mrd", "TDYR",      "12m trailing dividend yields
                                   A. Calcualtion methodolgy is the same as TPER
                              
                                   B. DPS is expressed as a corporation's common stock dividends on an annualised basis, 
                                   divided by the weighted average number of common shares outstanding for the year.
                              
                                   C. A source prefix is absent because a number of trailing dividend yields 
                                   come from Bloomberg rather than IBES)")
  
  f_addlist("l.mrd", "DS_FEPS", "DS 12m forward EPS, calculated using the same method as IB_FEPS")
  
  f_addlist("l.mrd", "DS_TPER", "DS 12m trailing PE ratio, calculated using the same method as IB_TPER, except
                                 DS uses actual earnings and negative earnings are treated as zero")
  
  f_addlist("l.mrd", "DS_TPBR", "DS 12m trailing price to book ratio, calculated using the same method as TPBR")
  
  f_addlist("l.mrd", "DS_TDYR", "DS 12m trailing dividend yield, calculated using the same method as TDYR")
  
  f_addlist("l.mrd", "DS_TCYR", "DS 12m trailing cash earnings yield")
  
  # For equity sector relative value strategy
  f_addlist("l.mrd", "EYT", "Theoretical US-equivalent country earnings yield")
  f_addlist("l.mrd", "BPT", "Theoretical US-equivalent country book yield")
  f_addlist("l.mrd", "DVT", "Theoretical US-equivalent country dividend yield")
  f_addlist("l.mrd", "EYRSX", "Country sector excess earnings yield")
  f_addlist("l.mrd", "BPRSX", "Country sector excess book yield")
  f_addlist("l.mrd", "DVRSX", "Country sector excess dividend yield")
  
  #Not in use
  f_addlist("l.mrd", "MSCI_FEPS", "MSCI 12m forward EPS")
  f_addlist("l.mrd", "MSCI_FPER", "MSCI 12m forward price to earnings ratio")
  f_addlist("l.mrd", "MSCI_TEPS", "MSCI 12m trailing EPS")
  f_addlist("l.mrd", "MSCI_TPER", "MSCI 12m trailing price to earnings ratio")
  f_addlist("l.mrd", "MSCI_TPBR", "MSCI 12m trailing price to book ratio")
  f_addlist("l.mrd", "MSCI_TDYR", "MSCI 12m trailing dividend yield")
  
  #Money, credit, and reserve dynamics
  f_addlist("l.mrd", "PC_POYA", "Change in private credit, %oya")
  f_addlist("l.mrd", "PC_DGDP", "Change in private credit as % of NGDP 12M ago")
  f_addlist("l.mrd", "PCGD", "12mma of private credit/12mma of NGDP")
  f_addlist("l.mrd", "M3_POYA", "Change in broad money supply, %oya")

  f_addlist("l.mrd", "FXR_DMB", "Change in FX reserve as % of monetary base 12m ago(converted to USD using end-of-month FX rate)")
  f_addlist("l.mrd", "MB_POYA", "Change in monetary base, %oya")
  f_addlist("l.mrd", "FXR_DGDP", "Change in FX reserves  as % of NGDP 12M ago")
  f_addlist("l.mrd", "FXR_PMOM", "Change in FX reserve, %m/m")

  #Composite sovereign foreign-currency debt ratings
  f_addlist("l.mrd", "RTG", "A numerical representation of the average rating (including outlooks)")

  #Bank lending surveys
  f_addlist("l.mrd", "G3_LCZ", "G3 loan officer survey:
                                NGDP-weighted normalised and lagged lending conditions of the U.S., eurozone and Japan")
  f_addlist("l.mrd", "LC",     "Lending conditions indices (constrant intra-quarter) for the U.S., eurozone and Japan")

  #Effective Exchange Rates
  f_addlist("l.mrd", "NEL", "JPMorgan nominal effective exchange rate
                            (monthly series are based on average exchange rates) ")
  f_addlist("l.mrd", "REL", "JPMorgan CPI-based real effective exchange rate level
                            (monthly series are based on average exchange rates)")
  f_addlist("l.mrd", "RES", "Real exchange rate strength: latest REL/average of the REL of the preceding 60 months*100")

  #Openness
  f_addlist("l.mrd", "OPENNESS", "100*(Annualised Export + Annualised Import)/Annualised Nominal GDP, 5Y (2007-2011) Average")   

  #Terms of Trade
  f_addlist("l.mrd", "OTOT", "Official terms of trade")
  f_addlist("l.mrd", "CTOT", "Citi terms of trade: The Citi Commodity Terms of Trade indices measurethe relative performance of 
                              commodity export and import prices for the major world economies. They are based on exports and 
                              imports of 21 commodities. All commodities are priced in US dollars and therefore exclude local 
                              currency conversion effects. Trade data give annual flows (in USD) in a detailed list of commodities.
                              A chained price index id computed for exports and for imports for each country. While the index is 
                              restricted to commodities, their relative share in a country's trade flows is taken into account: 
                              to illustrate, if exports are half manufacturing and half commodities, and commodities prices rise 
                              by 20%, the price index for exports will rise by 10% only. The indices therefore maintain the 
                              imbalance between net commodity trade, which is particularly useful for economies that are either 
                              large commodity exporters or importers.
                              The indices are calculated as the log of the ratio of the price index for commodity exports over the 
                              price index for commodity imports, multiplied by 100. Positive readings of +5 means that export 
                              prices have out-performed import prices by 5% since the index start date. A change of +5 between 
                              any two dates means that export prices have out-performed import prices by 5% over that time span.
                              Given there is no trade data for Taiwan from IMF, Citi does not produce an index for Taiwan. 
                              As a result, we use a lagged (1 month) ToT index from Ministry of Finance, Taiwan.")

  #Auxiliary indices
  f_addlist("l.mrd", "HFRXGL_XR", "Return of HFRX Index, representative of the overall HF industry.")
  f_addlist("l.mrd", "HFRXM_XR", "Return of HFRX Macro/CTA Index, representative of macro trading.")
  
  #H. GDRB/GFX related indicators (Andy added the following section for XDX)
  
  id.gdb.fx <- c("AUD", "NZD", "SEK", "NOK", "KRW", "INR", "ZAR", "ILS", "RUB", "TRY", "PLN", "BRL", "MXN") 
  l.gfxpm <- list(c("AUD", "BRL", "EUR", "ILS", "INR", "KRW", "MXN", "NZD", "USD", "ZAR"), c("NOK", "PLN", "SEK"), c("RUB", "TRY"))  # list of primary currencies in GFX
  l.gfxbm <- list("USD", "EUR", c("USD", "EUR"))                                                                                     # corresponding list of benchmark countries
   
  #re-weight constituents in GFX   
  dfm.gfx.rwt <- dfm[, paste(id.gdb.fx, "FX_WT", sep="_")]/zoo(rowSums(dfm[, paste(id.gdb.fx, "FX_WT", sep="_")], na.rm = T), index(dfm))

  #prepare datafram for calculating GFX related indicators
  dfm.2yy.naf <- f_nafill(dfm, c("RUB", "TRY"), "2YR_YLD", "NIR", adjust.method = "DIF", adjust.intv = 1)
  dfm[, paste(c("RUB", "TRY"), "2YR_YLD", sep="_")] <- dfm.2yy.naf 
  dfm.gfx <- f_rlbk(dfm, "2YR_YLD",  l.gfxpm, l.gfxbm, out="Y2YIRRB")
  
  dfm.egizda <- f_ptrans(dfm, c(id.gdb.fx, "USD", "EUR"), "EGIZ_D3M_3MA = f_avg( EGIZ_D3M , 3)")
  dfm.egizdarb <- f_rlbk(dfm.egizda, "EGIZ_D3M_3MA",  l.gfxpm, l.gfxbm)
  dfm.gfx <- merge(dfm.gfx, dfm.egizda, dfm.egizdarb)
  
  cv.gfxin <- c("INFBRB", "XINFBRB", "XINFCRB", "INFTERB", "EMG3MARB_AO", "EGVP3MARB_AO", "EGVP3MARB")
  cv.gfxon <- c("INFBRB", "XINFBRB", "XINFCRB", "INFTERB", "EMGRB_AO",    "EGVPRB_AO",    "EGVP3MARB")
  for (i in 1: length(cv.gfxin)){
    dfm.gfxx <- setNames(dfm[, paste(id.gdb.fx, cv.gfxin[i], sep="_")], paste(id.gdb.fx, cv.gfxon[i], sep = "_"))
	dfm.gfx <- merge(dfm.gfx, dfm.gfxx)
  }
  
  cv.gfx <- unique(substr(colnames(dfm.gfx),5,nchar(colnames(dfm.gfx))))
  for(i in 1: length(cv.gfx)){
    zoo.gfxx <- window(zoo(rowSums(dfm.gfx.rwt * dfm.gfx[, paste(id.gdb.fx, cv.gfx[i], sep="_")], na.rm = T), index(dfm.gfx)), start= as.Date("2001-02-28"))
	zoo.gfxx[zoo.gfxx==0] <- NA
	dfm.gfx <- merge(dfm.gfx, zoo.gfxx)
	colnames(dfm.gfx)[ncol(dfm.gfx)] <- paste("GLB_FX", cv.gfx[i], sep="_")
    }  
  
  dfm.gfx.export <- dfm.gfx[, grep("GLB_FX", colnames(dfm.gfx))]
  
# H. RData files creation

  rdata.file <- "CleanRawSeries"  # R Data files with all base series, after renaming, completion, etc. being done
  df_export <- merge(dfmB, dfm.gfx.export)
  save(df_export, file=paste(folder.data, paste(rdata.file, "m", sep="_"), ".RData", sep=""))
  df_export <- dfdB
  save(df_export, file=paste(folder.data, paste(rdata.file, "d", sep="_"), ".RData", sep=""))

  if(live_mode){
    rdata.file <- "AssetSDBetas"  # R Data files with all base series, after renaming, completion, etc. being done
    df_export <- dfm[, setdiff(names(dfmC), names(dfmB))]
    save(df_export, file=paste(folder.data, paste(rdata.file, "m", sep="_"), ".RData", sep=""))
    df_export <- dfd[, setdiff(names(dfdC), names(dfdB))]
    save(df_export, file=paste(folder.data, paste(rdata.file, "d", sep="_"), ".RData", sep=""))
  }
  
  rdata.file <- "InflationMeasures"
  df_export <- dfm[, setdiff(names(dfmD), names(dfmC))]
  save(df_export, file=paste(folder.data, paste(rdata.file, "m", sep="_"), ".RData", sep=""))

  rdata.file <- "GrowthBalances"
  df_export <- dfm[, setdiff(names(dfmF), names(dfmD))]
  save(df_export, file=paste(folder.data, paste(rdata.file, "m", sep="_"), ".RData", sep=""))

  rdata.file <- "DataDefinition"
  l.mx <- c(l.mi, l.mrd)
  m.indic <- matrix(NA, length(l.mx), 4)
  m.indic[, 1] <- names(l.mx)
  m.indic[, 2] <- as.vector(unlist(l.mx))
  for(i in 1:length(l.mx)) 
    m.indic[i, 3] <- paste(sort(intersect(id.all, 
                                          unique(substr(names(dfm)[grepl(names(l.mx)[i], names(dfm))], start=1, stop=3)))), collapse=", ")
  m.indic[match(names(l.asset), m.indic[,1]), 4] <- "ASSET"
  m.indic[match(names(l.inflation), m.indic[,1]), 4] <- "INFLATION"
  m.indic[match(names(l.activity), m.indic[,1]), 4] <- "ACTIVITY"
  m.indic[match(names(l.mrd), m.indic[,1]), 4] <- "OTHER"
  tab.indic <- m.indic
  colnames(tab.indic) <- c("Acronym", "Definition", "Availability", "Allocation")
  save(tab.indic, file=paste(folder.data, rdata.file, ".RData", sep=""))
  
