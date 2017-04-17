# Insertable lines for beta-balanced signal generation

# to be inserted before NAs in final signals are removed

#XEF = Runs beta-balanced with 25% damage to long-term performance

#E.2.a.

		dfm_check<-dfm_bb<-merge(dfmr,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>1&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_ef,sname="FSIG",bname="FX_XR_BETA1R",outname="SIGB")
			dfmr_x<-merge(dfmr,dfm_sig)
			dfm_fsig<-f_fsig(dfmr_x,id_ef,rsname="SIGB",mass=0.15,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfm_bb<-merge(dfmr,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_ef,"FSIG",sep="_")]*dfm_bb[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_ef,"FSIG",sep="_")]*dfm_check[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_ef,"FSIG",sep="_")]*dfm_bb[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")	
		
#E.2.b.

		dfd_check<-dfd_bb<-merge(dfdr,dfd_fsig)	
		ln=0;maeb=1000													
		while(maeb>1&ln<5){																
			dfd_sig<-f_betbal(dfd_bb,id_ef,sname="FSIG",bname="FX_XR_BETA1R",outname="SIGB")
			dfdr_x<-merge(dfdr,dfd_sig)
			dfd_fsig<-f_fsig(dfdr_x,id_ef,rsname="SIGB",mass=0.15,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfd_bb<-merge(dfdr,dfd_fsig)
			maeb<-max(abs(rowSums(dfd_bb[,paste(id_ef,"FSIG",sep="_")]*dfd_bb[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfd_check[,paste(id_ef,"FSIG",sep="_")]*dfd_check[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfd_bb))
		zoo_agb2<-zoo(rowSums(dfd_bb[,paste(id_ef,"FSIG",sep="_")]*dfd_bb[,paste(id_ef,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfd_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")	

#XFX = runs without hedge basket with 30-50% loss of long-term performance
		
		#E.2.a.

		dfm_check<-dfm_bb<-merge(dfmr,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>1&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_xfx,sname="FSIG",bname="FX_XR_BETA1R",outname="SIGB")
			dfmr_x<-merge(dfmr,dfm_sig)
			dfm_fsig<-f_fsig(dfmr_x,id_xfx,rsname="SIGB",mass=0.15,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfm_bb<-merge(dfmr,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_xfx,"FSIG",sep="_")]*dfm_bb[,paste(id_xfx,"FX_XR_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_xfx,"FSIG",sep="_")]*dfm_check[,paste(id_xfx,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_xfx,"FSIG",sep="_")]*dfm_bb[,paste(id_xfx,"FX_XR_BETA1R",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")
		
		dfm_xpl<-merge(dfmr[,paste(id_xfx,dep,sep="_")],dfm_fsig)

		graphics.off()							
		f_xexpl(dfm_xpl,id_xfx,dep=dep,deptrans=c(1,1),xpl="FSIG",xpltrans="MAV",xplpv=c(1),start=sdt)		
		
		

#XDU 

#E.2.a.

		dfm_check<-dfm_bb<-merge(dfmr,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>1&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_xdu,sname="FSIG",bname="DU_XRN_BETA1R",outname="SIGB")
			dfmr_x<-merge(dfmr,dfm_sig)
			dfm_fsig<-f_fsig(dfmr_x,id_xdu,rsname="SIGB",mass=0.07,zmean=T,csnorm=T,vweight=T,vrnames=dep)[[1]]
			dfm_bb<-merge(dfmr,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_xdu,"FSIG",sep="_")]*dfm_bb[,paste(id_xdu,"DU_XRN_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_xdu,"FSIG",sep="_")]*dfm_check[,paste(id_xdu,"DU_XRN_BETA1R",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_xdu,"FSIG",sep="_")]*dfm_bb[,paste(id_xdu,"DU_XRN_BETA1R",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")


			
#XCO = Runs balanced beta at marginal loss in long-term value generation

#E.2.a.
		
		dfm_check<-dfm_bb<-merge(dfmr,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>0.5&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_com,sname="FSIG",bname="CO_BETANR",outname="SIGB")
			dfmr_x<-merge(dfmr,dfm_sig)
			dfm_fsig<-f_fsig(dfmr_x,id_com,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfm_bb<-merge(dfmr,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_com,"FSIG",sep="_")]*dfm_bb[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_com,"FSIG",sep="_")]*dfm_check[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_com,"FSIG",sep="_")]*dfm_bb[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")		
			
		
#E.2.b.

		
		dfd_check<-dfd_bb<-merge(dfd_fsig,dfdr)	
		ln=0;maeb=1000																			#OPTIONAL: Iterative beta balancing
		if(TRUE){
		while(maeb>0.5&ln<10){																
			dfd_sig<-f_betbal(dfd_bb,id_com,sname="FSIG",bname="CO_BETANR",outname="SIGB")
			dfdr_x<-merge(dfdr,dfd_sig)
			dfd_fsig<-f_fsig(dfdr_x,id_com,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfd_bb<-merge(dfdr,dfd_fsig)
			maeb<-max(abs(rowSums(dfd_bb[,paste(id_com,"FSIG",sep="_")]*dfd_bb[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}}
		zoo_agb1<-zoo(rowSums(dfd_check[,paste(id_com,"FSIG",sep="_")]*dfd_check[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T),index(dfd_bb))
		zoo_agb2<-zoo(rowSums(dfd_bb[,paste(id_com,"FSIG",sep="_")]*dfd_bb[,paste(id_com,"CO_BETANR",sep="_")],na.rm=T),index(dfd_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")

#XEQ = Runs balanced beta at cost of 20-25% of long-term value generation

#E.2.a.
		
		dfm_check<-dfm_bb<-merge(dfmr,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>0.5&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_eq,sname="FSIG",bname="EQ_XRN_BETA1R",outname="SIGB")
			dfmr_x<-merge(dfmr,dfm_sig)
			dfm_fsig<-f_fsig(dfmr_x,id_eq,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfm_bb<-merge(dfmr,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_eq,"FSIG",sep="_")]*dfm_bb[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_eq,"FSIG",sep="_")]*dfm_check[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_eq,"FSIG",sep="_")]*dfm_bb[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")		
			
		
#E.2.b.

		
		dfd_check<-dfd_bb<-merge(dfd_fsig,dfdr)	
		ln=0;maeb=1000																			#OPTIONAL: Iterative beta balancing
		if(TRUE){
		while(maeb>1&ln<10){																
			dfd_sig<-f_betbal(dfd_bb,id_eq,sname="FSIG",bname="EQ_XRN_BETA1R",outname="SIGB")
			dfdr_x<-merge(dfdr,dfd_sig)
			dfd_fsig<-f_fsig(dfdr_x,id_eq,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfd_bb<-merge(dfdr,dfd_fsig)
			maeb<-max(abs(rowSums(dfd_bb[,paste(id_eq,"FSIG",sep="_")]*dfd_bb[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}}
		zoo_agb1<-zoo(rowSums(dfd_check[,paste(id_eq,"FSIG",sep="_")]*dfd_check[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T),index(dfd_bb))
		zoo_agb2<-zoo(rowSums(dfd_bb[,paste(id_eq,"FSIG",sep="_")]*dfd_bb[,paste(id_eq,"EQ_XRN_BETA1R",sep="_")],na.rm=T),index(dfd_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")	

#EQFX = Runs balanced beta at cost of 10-20% of long-term value generation

#E.2.a.
		
		dfm_check<-dfm_bb<-merge(dfm,dfm_fsig)	
		ln=0;maeb=1000												
		while(maeb>0.5&ln<10){																
			dfm_sig<-f_betbal(dfm_bb,id_qf,sname="FSIG",bname="EQFX_XRN_BETA1",outname="SIGB")
			dfm_x<-merge(dfm,dfm_sig)
			dfm_fsig<-f_fsig(dfm_x,id_qf,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfm_bb<-merge(dfm,dfm_fsig)
			maeb<-max(abs(rowSums(dfm_bb[,paste(id_qf,"FSIG",sep="_")]*dfm_bb[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}
		zoo_agb1<-zoo(rowSums(dfm_check[,paste(id_qf,"FSIG",sep="_")]*dfm_check[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T),index(dfm_bb))
		zoo_agb2<-zoo(rowSums(dfm_bb[,paste(id_qf,"FSIG",sep="_")]*dfm_bb[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T),index(dfm_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")

#E.2.b.
		
		dfd_check<-dfd_bb<-merge(dfd_fsig,dfd)	
		ln=0;maeb=1000																			#OPTIONAL: Iterative beta balancing
		if(TRUE){
		while(maeb>0.5&ln<10){																
			dfd_sig<-f_betbal(dfd_bb,id_qf,sname="FSIG",bname="EQFX_XRN_BETA1",outname="SIGB")
			dfd_x<-merge(dfd,dfd_sig)
			dfd_fsig<-f_fsig(dfd_x,id_qf,rsname="SIGB",mass=0.3,zmean=F,csnorm=T,vweight=F,vrnames=dep)[[1]]
			dfd_bb<-merge(dfd,dfd_fsig)
			maeb<-max(abs(rowSums(dfd_bb[,paste(id_qf,"FSIG",sep="_")]*dfd_bb[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T)))
			print(maeb)
			ln=ln+1	
			}}
		zoo_agb1<-zoo(rowSums(dfd_check[,paste(id_qf,"FSIG",sep="_")]*dfd_check[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T),index(dfd_bb))
		zoo_agb2<-zoo(rowSums(dfd_bb[,paste(id_qf,"FSIG",sep="_")]*dfd_bb[,paste(id_qf,"EQFX_XRN_BETA1",sep="_")],na.rm=T),index(dfd_bb))	
		plot(zoo_agb1,type="line")
		lines(zoo_agb2,col="red")		