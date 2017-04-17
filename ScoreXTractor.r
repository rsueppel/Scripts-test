#
# XTractor for verification
# Part I
# Position and basic signal information
#
# N.B. the function f_basicinfo is called from Transparency.r

# Main strategies

# XEF
f_basicinfo(dfmr, id.xef, cv.zn, alloc=1031, strat="XEF", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# XDX
f_basicinfo(dfm, id.xdx, cv.zn, alloc=171.6, strat="XDX", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# XDU
f_basicinfo(dfmr, id.xdu, cv.zn, alloc=206.3, strat="XDU", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# XCO
f_basicinfo(dfmr, id.xco, cv.zn, alloc=229.2, strat="XCO", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)

# XEQ
f_basicinfo(dfmr, id.xeq, cv.zn, alloc=366.7, strat="XEQ", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# EQDU
f_basicinfo(dfm, id.eqdu, cv.zn, alloc=46.1, strat="EQDU", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# CRDU
f_basicinfo(dfm, id.crdu, cv.zn, alloc=44.8, strat="CRDU", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# EQFX
f_basicinfo(dfm, id.eqfx, cv.zn, alloc=45.8, strat="EQFX", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# Contingent strategies

# CREQ
f_basicinfo(dfm, id.creq, cv.zn, alloc=46.1, strat="CREQ", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# IGHY
f_basicinfo(dfm, id.ighy, cv.zn, alloc=68.8, strat="IGHY", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# COFI
f_basicinfo(dfm, id.cofi, cv.zn, alloc=68.8, strat="COFI", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)


# FXDU
f_basicinfo(dfm, id.fxdu, cv.zn, alloc=68.8, strat="FXDU", cap_factor=1.333,
            dfrsig=dfm.rsig, dfmfsig=dfm.fsig, dfdfsig=dfd.fsig, dfhh=dfd.hh, dfcc=dfd.cc, dfch=dfd.ch, lvgchoice = "cc", dfdposnip=dfd.pos.nip, dfdposist=dfd.pos.ist)

#===============================================================================================================================================
#===============================================================================================================================================
#===============================================================================================================================================
# Part II
# Cross-sectional factor tables

# ========================
# CS Factor Tables for XEF
# ========================

# Manual data creation
dfmT <- merge(dfm, dfmr)
l.composite <- lapply(l.wz, function(x) x[[2]])
cv_factors <- c("O_XBD_ZC", "O_TOTD_ZC", "O_GROWTH_ZC", "O_IRD_ZC", "O_CRRH_ZC", "CISIRB_ZC", "O_BPBACK_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(paste(strsplit(cv_factors, "_ZC"), "R", sep="")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- as.vector(do.call(cbind, strsplit(l.composite[[kj]], "RZ")))
}
l.ind[[6]] <- "CISIRB"
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: XEF"
f_factortable(dfmT, id.xef, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3) , selid="TRY", m.specs=m_specs, mtitle=title, useRZC=TRUE)


# ==================
# Application to XDX
# ==================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_XFXDEP_ZC", "O_XBR_ZC", "O_TOTD_ZC", "O_EGID_ZC", "O_EQEFF_ZC", "O_INFEFF_ZC", "O_CRY_ZC", "O_ECORB_ZC", "O_VOLEFF_ZC", "O_CTA_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- substr(l.composite[[kj]]$constituents, 1, nchar(l.composite[[kj]]$constituents)-1)
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: XDX"
f_factortable(dfmT, id.xdx, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3) , selid=id.xdx[1], m.specs=m_specs, mtitle=title)


# ==================
# Application to XEQ
# ==================

# Manual data creation
dfmT <- merge(dfm, dfmr)
l.composite <- l.owz
cv_factors <- c("O_AFEY_ZC", "O_EPSD_ZC", "O_WXGFXA_ZC", "O_NELDNA_ZC", "O_XINBN_ZC", "O_XRYLD_ZC", "O_DIST_ZC", "O_XBIN_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(paste(strsplit(cv_factors, "_ZC"), "R", sep="")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- as.vector(do.call(cbind, strsplit(l.composite[[kj]]$constituents, "RZ")))
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: XEQ"
f_factortable(dfmT, id.xeq, cv_factors, v.wg/sum(v.wg), l.ind, l.indwgt, l.mf, v.back=c(1, 3) , selid=id.xeq[1], m.specs=m_specs, mtitle=title, useRZC=TRUE)


# ==================
# Application to XDU
# ==================

# Manual data creation
dfmT <- merge(dfm, dfmr)
names(dfmT) <- gsub("CRYAN_HIZ", "CRYAN_HI", names(dfmT))

l.composite <- l.owz
cv_factors <- c("O_XRYLDS_ZC", "O_CPID_ZC", "O_PCGD_ZC", "O_CRYA_ZC", "O_EXRA_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(paste(strsplit(cv_factors, "_ZC"), "R", sep="")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- as.vector(do.call(cbind, strsplit(l.composite[[kj]]$constituents, "RZ")))
}
names(l.ind) <- cv_factors

l.ind[[4]][3] <- "DU_CRYAN_HI"
rownames(m_specs)[match("DU_CRYAN_HIZR", rownames(m_specs))] <- "DU_CRYAN_HIR"
names(l.mf)[match("DU_CRYAN_HIZ", names(l.mf))] <- "DU_CRYAN_HI"

# Factor table creation
title <- "Cross-sectional factors/indicators table: cross-duration (XDU)"
f_factortable(dfmT, id.xdu, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3), selid=id.xdu[1], m.specs=m_specs, mtitle=title, useRZC=TRUE)


# ==================
# Application to XCO
# ==================

# Manual data creation
dfmT <- merge(dfm, dfmr)
l.composite <- l.owz
cv_factors <- c("O_CRN_ZC", "O_DRIFT_ZC", "O_xEGID_ZC", "MRGCHN_ZC", "SPOSZN_ZC", "CO_GBPB_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(paste(strsplit(cv_factors, "_ZC"), "R", sep="")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- as.vector(do.call(cbind, strsplit(l.composite[[kj]]$constituents, "RZ")))
}
l.ind[[4]] <- "MRGCHN"
l.ind[[5]] <- "SPOSZN"
l.ind[[6]] <- "CO_GBPB"
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: cross-commodities (XCO)"
f_factortable(dfmT, id.xco, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.xco[1], m.specs=m_specs, mtitle=title, useRZC=TRUE)


# ===================
# Application to EQFX
# ===================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_CRRN_ZC", "O_FX_GROWTHRN_ZC", "O_FX_XBALN_ZC", "O_EQFX_EASE_ZC", "O_FX_CRYDN_ZC", "O_XINFLN_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- substr(l.composite[[kj]]$constituents, 1, nchar(l.composite[[kj]]$constituents)-1)
  #if(!is.na(kj)) l.ind[[k]] <- l.composite[[kj]]$constituents
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: EQFX"
f_factortable(dfmT, id.eqfx, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3), selid=id.eqfx[1], m.specs=m_specs, mtitle=title, useRZC=FALSE)


# ===========================
# Transparency tables fo EQDU
# ===========================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_XVY_ZC", "O_DISINFL_ZC", "O_XHCRY_ZC", "O_UNEMP_ZC", "O_XECGN_ZC", "O_INFN_ZC", "O_HFCORN_ZC", "O_FCTIGHT_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- substr(l.composite[[kj]]$constituents, 1, nchar(l.composite[[kj]]$constituents)-1)
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Cross-sectional factors/indicators table: Equity-Duration (EQDU) "
f_factortable(dfmT, id.eqdu, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.eqdu[1], 
              m.specs=m_specs, mtitle=title, useRZC=FALSE)


# ============================
# Transparency tables for CRDU
# ============================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_DISIG_ZC", "O_XCRYA_ZC", "O_INSUBIN_ZC", "O_CORFLIP_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)){
    l.ind[[k]] <- substr(l.composite[[kj]]$constituents, start = 1, stop=nchar(l.composite[[kj]]$constituents) - length("Z"))
    l.indwgt[[k]] <- l.composite[[kj]]$weights
  }
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Factors/indicators table: Hybrid Credit-Duration (CRDU)"
f_factortable(dfmT, id.crdu, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3), selid=id.crdu[1], 
              m.specs=m_specs, mtitle=title, useRZC=FALSE)


# ============================
# Transparency tables for CREQ
# ============================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_CRXCRYN_ZC", "O_XIVOL_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- substr(l.composite[[kj]]$constituents, 1, nchar(l.composite[[kj]]$constituents)-1)
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Factors/indicators table: Credit-equity (CREQ)"
f_factortable(dfmT, id.creq, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.creq[1], m.specs=m_specs, mtitle=title)


# ============================
# Transparency tables for IGHY
# ============================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_CRATED_ZC", "O_PMIDN_ZC", "O_CRYN_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)) l.ind[[k]] <- substr(l.composite[[kj]]$constituents, 1, nchar(l.composite[[kj]]$constituents)-1)
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Factors/indicators table: Calibrated IG-HY CDS indices (IGHY)"
f_factortable(dfmT, id.ighy, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.ighy[1], m.specs=m_specs, mtitle=title)


# ============================
# Transparency tables for COFI
# ============================

# Manual data creation
dfmT <- dfm
l.composite <- l.owz
cv_factors <- c("O_EINFD_ZC", "O_INFL_ZC", "O_XPMI_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)){
    l.ind[[k]] <- substr(l.composite[[kj]]$constituents, start = 1, stop=nchar(l.composite[[kj]]$constituents) - length("Z"))
    l.indwgt[[k]] <- l.composite[[kj]]$weights
  }
}
names(l.ind) <- cv_factors
l.ind[[3]] <- c("GLB_XMPMI", "CNY_XPMI")
# Factor table creation
title <- "Factors/indicators table: Commodities-financial risk (COFI)"
f_factortable(dfmT, id.cofi, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.cofi[1], m.specs=m_specs, mtitle=title)


# ============================
# Transparency tables for FXDU
# ============================

# Manual data creation
dfmT <- dfm
l.composite <- l.wz
cv_factors <- c("O_XCRN_ZC", "O_FXEGRB_ZC", "O_CRYD_ZC")
l.ind <- l.indwgt <- vector("list", length(cv_factors))
for(k in 1:length(cv_factors)){
  kj <- match(strsplit(cv_factors, "_ZC")[k], names(l.composite))
  if(!is.na(kj)){
    l.ind[[k]] <- substr(l.composite[[kj]]$constituents, start = 1, stop=nchar(l.composite[[kj]]$constituents) - length("Z"))
    l.indwgt[[k]] <- l.composite[[kj]]$weights
  }
}
names(l.ind) <- cv_factors

# Factor table creation
title <- "Factors/indicators table: Calibrated FX-duration (FXDU)"
f_factortable(dfmT, id.fxdu, cv_factors, v.wg, l.ind, l.indwgt, l.mf, v.back=c(1, 3, 6), selid=id.fxdu[1], m.specs=m_specs, mtitle=title)
