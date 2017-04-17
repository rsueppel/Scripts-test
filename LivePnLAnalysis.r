
  #A. Basic preparation, including sourcing of packages and custom functions

  rm(list=ls()) # cannot be called from inside a function
  
  d.lastwd <- as.Date("2015-07-31")  # set last working day for which data are updated  
  loc <- "gcm" # "gcm", "rdt"
  cv.paths.file <- c("M:/Trading/EXA/2. Quantitative Research/", "C:/Users/rsueppel/Documents/5. PKH_originals/R solutions/")
  names(cv.paths.file) <-c("gcm", "rdt")  # other locations and names can be added  
  folder.sfu <- paste(cv.paths.file[loc], "2. Support functions/",sep="")  
  source(paste(folder.sfu, "DataLoader.r", sep=""))  
  f_setup(path=loc) # f_setup called on the paths defined above

  #B. Loading returns data

  id.strat <- c("EXA", "XEF", "HYB", "EQFX", "XEQ", "XCO", "XDU", "XDX")
  cat.strat <- c("MMPNL", "PNL", "ALLOC")
  
  cv.import <- c(outer(id.strat, cat.strat, paste,sep="_"), "USD_EQ_XR", "EUR_EQ_XR")
  dfm <- f_dataloader(folder=folder.data, freq="m", csids=cv.import, enddate=d.lastwd) # using f_dataloader function
  dfd <- f_dataloader(folder=folder.data, freq="d", csids=cv.import, enddate=d.lastwd)
	
  #C. Return simple statisitcs
  
  sdt <- NULL  # NULL, as.Date("2013-09-01")
  edt <- NULL  # NULL, as.Date("2013-09-01")
  dfd.sel <- window(na.trim(dfd, is.na="all"), start=sdt, end = edt)
  
  dfd_pnl <- dfd.sel[, paste(id.strat, "PNL", sep="_")] # based on % per EXA (implicit weighting by allocation)
  table.AnnualizedReturns(dfd_pnl, geometric=F, digits=3)
  
  dfd_ac_pnl <- 100 * dfd.sel[, paste(id.strat, "MMPNL", sep="_")] / dfd.sel[, paste(id.strat, "ALLOC", sep="_")]
  table.AnnualizedReturns(dfd_ac_pnl, geometric=F, digits=3)  # based on % of strategy allocation
  
  #D. Return charts and histogram
  
  cv.snames <- c("Overall EXA book", "EM FX relative value", "Equity/Credit/IRS relative value", "Equity-FX relative value", 
                 "Global equity indices relative value", "Commodity futures relative value", "Global interest rate swaps relative value",
                 "Developed market FX")
  
  graphics.off()
  for(k in 1:length(id.strat)){
    dev.new()
    chart.CumReturns(dfd_ac_pnl[, k], geometric=F, main=paste(cv.snames[k], ": cumulative return, % of allocated capital", sep=""))
  }
  
  graphics.off()
  chart.Histogram(dfd_ac_pnl[, "EXA_MMPNL"]/100,main="EXA PnL, Daily Returns",method="add.normal")
  
  #E. Drawdown charts
  
  graphics.off()
  for(k in 1:length(id.strat)){
    dev.new()
    df.loc <- na.trim(dfd_ac_pnl[, k])
    trange <- range(index(df.loc))
    zoo.draw <- cumsum(df.loc) - cummax(cumsum(df.loc))
    plot(zoo.draw, xlim=trange, col="blue", type="l", ann=FALSE, xaxt='n')
    title(main=paste(names(dfd_ac_pnl)[k], ", peak-to-trough drawdown", sep=""), sub="based on % of allocated strategy")
    axis.Date(1,at=seq(from=trange[1],to=trange[2],by="6 months"),format="%b-%Y")
    abline(v=seq(from=trange[1],to=trange[2],by="3 months"),col="darkgray",lty=3)
  }
  
  