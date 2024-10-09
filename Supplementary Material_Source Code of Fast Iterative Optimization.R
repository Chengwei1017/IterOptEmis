#!/usr/bin/env Rscript
options(error=function() traceback(3))
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RNetCDF)

print("Fast Iterative Optimization for FMEmT")
print("                         lcw@cdaes.cn")
print("                             20231207") 

fmtnum=function(xi,dig=2){
  return(paste0(paste0(rep("0",dig-nchar(xi)),collapse = ""),xi))
}

workdir="/home/lcw/LCW/Models/RMODELS/OPENEMISOPT/"
steptag=1
emisyrm="202301"
emisdom="1"
withcot=T
emispct="CDF01"
cotname="COT_1"

Args=commandArgs()

if(length(Args)>=5){
  steptag=as.numeric(Args[6])
  print(paste0("Doing STEP ",steptag,"..."))
}

if(length(Args)>=6){
  emisyrm=Args[7]
  print(paste0("Using emisyrm ",emisyrm,"..."))
}

emismon=paste0(emisyrm,emisdom)

if(steptag==1){
  BASETAG="STEP00"
  SCALETAG=paste0("STEP",fmtnum(steptag))
}else{
  BASETAG=paste0("STEP",fmtnum(steptag-1))
  SCALETAG=paste0("STEP",fmtnum(steptag))
}


region_map=read.csv(file=paste0(workdir,"OBSFILES/RegionMap.csv"),fileEncoding = "GB2312")
if(withcot) region_map=region_map[-nrow(region_map),]
obsdir="/home/lcw/LCW/Models/RMODELS/E_ALCHEMY/OBSFILES/"
moddir=paste0("/home/lcw/LCW/MODELOUT/AIRCSV/CMAQSIMU/",emismon,"/",BASETAG,"/")
emisdir="/home/lcw/LCW/Models/RMODELS/MEIC2CMAQ/BASEEMIS/"
patch_region=T

merge_aq=function(){
  aqdata=data.frame()
  obs=readRDS(paste0(workdir,"OBSFILES/OBS.RDs"))
  obs$AQI=as.numeric(obs$AQI)
  obs$PM25=as.numeric(obs$PM25)
  obs$PM10=as.numeric(obs$PM10)
  obs$SO2=as.numeric(obs$SO2)
  obs$NO2=as.numeric(obs$NO2)
  obs$O3=as.numeric(obs$O3)
  obs$CO=as.numeric(obs$CO)
  
  for(aql in 1:nrow(region_map)){
    isite=region_map[aql,]
    oname=isite$OBSNAME
    mname=isite$EMISNAME
    iobs=subset(obs,Point==oname)
    print(paste0("Reading ",oname,"..."))
    mfile=paste0(moddir,"/PointData/",mname,".csv")
    print(paste(mfile,file.exists(mfile)))
    mod=read.csv(file=mfile,header=T,fileEncoding = "UTF-8",stringsAsFactors = F)
    mod$Time=substr(mod$Time,1,16)
    print(head(iobs))
    print(head(mod))
    tmpdata=merge(mod,iobs,by.x="Time",by.y="Time")
    #print(head(tmpdata))
    
    taqdata=data.frame(POINT=isite$EMISREG,ONAME=oname,MNAME=mname,日期=tmpdata$Time,
                       O_NO2=tmpdata$NO2.y,O_SO2=tmpdata$SO2.y,O_PM10=tmpdata$PM10.y,O_PM25=tmpdata$PM25.y,O_O3=tmpdata$O3.y,O_CO=tmpdata$CO.y,
                       M_NO2=tmpdata$NO2.x,M_SO2=tmpdata$SO2.x,M_PM10=tmpdata$PM10.x,M_PM25=tmpdata$PM25.x,M_O3=tmpdata$O3.x,M_CO=tmpdata$CO.x)

    aqdata=rbind(aqdata,taqdata)
  }
  return(aqdata)
}

est_data=function(){
  est=rbind(data.frame(POL="PM25",modStats(aqdata,mod="M_PM25",obs="O_PM25",type="POINT")),
            data.frame(POL="PM10",modStats(aqdata,mod="M_PM10",obs="O_PM10",type="POINT")),
            data.frame(POL="NO2",modStats(aqdata,mod="M_NO2",obs="O_NO2",type="POINT")),
            data.frame(POL="O3",modStats(aqdata,mod="M_O3",obs="O_O3",type="POINT")),
            data.frame(POL="SO2",modStats(aqdata,mod="M_SO2",obs="O_SO2",type="POINT")),
            data.frame(POL="CO",modStats(aqdata,mod="M_CO",obs="O_CO",type="POINT"))
  )
  return(est)
}

gen_fix_parm=function(){
  avgdata=as.data.frame(summarize(group_by(aqdata,POINT),
                                  O_NO2=mean(O_NO2,na.rm=T),M_NO2=mean(M_NO2,na.rm=T),
                                  O_SO2=mean(O_SO2,na.rm=T),M_SO2=mean(M_SO2,na.rm=T),
                                  O_CO=mean(O_CO,na.rm=T),M_CO=mean(M_CO,na.rm=T),
                                  O_PM10=mean(O_PM10,na.rm=T),M_PM10=mean(M_PM10,na.rm=T),
                                  O_PM25=mean(O_PM25,na.rm=T),M_PM25=mean(M_PM25,na.rm=T),
                                  O_O3=max(O_O3,na.rm=T),M_O3=max(M_O3,na.rm=T)
  ))
  
  scale_fac=data.frame(POINT=avgdata$POINT,
                       VOC=avgdata$O_O3/avgdata$M_O3,
                       NOx=avgdata$O_NO2/avgdata$M_NO2,
                       CO=avgdata$O_CO/avgdata$M_CO,
                       SO2=avgdata$O_SO2/avgdata$M_SO2,
                       PM10=avgdata$O_PM10/avgdata$M_PM10,
                       PM2p5=avgdata$O_PM25/avgdata$M_PM25,
                       NH3=1
                       )
  
  scale_fac=rbind(scale_fac,data.frame(POINT="COT",t(colMeans(scale_fac[,2:8]))))
  
  return(scale_fac)
  
}

aqdata=merge_aq()

dir.create(paste0(workdir,"RESULTS/",emisyrm,"/"),recursive=T)
saveRDS(file=paste0(workdir,"RESULTS/",emisyrm,"/RAW_ALLAQPNT_",SCALETAG,".rds"),aqdata)

MEIC_TYPE=c("AGR","IND","POW","RES","TRA","SOL","BMA","SHP")
VARBASEEMI=c("VOC","NOx","CO","SO2","PM10","PM2p5","NH3")

gen_scale_file=function(withCOT=T){
  scale_factor=gen_fix_parm()
  print("scale factors:")
  print(scale_factor)
  tscaled=data.frame()
  for(xi in 1:nrow(region_map)){
    iregion=region_map[xi,]
    iscalef=subset(scale_factor,POINT==iregion$EMISREG)
    if(nrow(iscalef)==0){
      iscalef=subset(scale_factor,POINT=="COT")
    }
    iscaled=data.frame(NAME=MEIC_TYPE,VOC=iscalef$VOC,NOx=iscalef$NOx,CO=iscalef$CO,SO2=iscalef$SO2,PM10=iscalef$PM10,PM2p5=iscalef$PM2p5,NH3=iscalef$NH3,REGION=iregion$EMISREG)
    tscaled=rbind(tscaled,iscaled)
  }
  if(withCOT){
    iscalef=subset(scale_factor,POINT=="COT")
    iscaled=data.frame(NAME=MEIC_TYPE,VOC=iscalef$VOC,NOx=iscalef$NOx,CO=iscalef$CO,SO2=iscalef$SO2,PM10=iscalef$PM10,PM2p5=iscalef$PM2p5,NH3=iscalef$NH3,REGION=cotname)
    tscaled=rbind(tscaled,iscaled)
  }
  return(tscaled)
  
}

PatchEMIS=function(EDIR="FNLF01",UPDATEEMIS=T){
  patch_emis_rep=data.frame()
  print(paste0("Creating folder ",workdir,"EMISFILES/",emisyrm,"/",SCALETAG,"/",EDIR))
  dir.create(paste0(workdir,"EMISFILES/",emisyrm,"/",SCALETAG,"/",EDIR),recursive = T)

  if(BASETAG=="STEP00"){
    basepath=paste0(workdir,"EMISFILES/",BASETAG,"/",EDIR,"/")
  }else{
    basepath=paste0(workdir,"EMISFILES/",emisyrm,"/",BASETAG,"/",EDIR,"/")
  }
  print(basepath)
  casepath=paste0(workdir,"EMISFILES/",emisyrm,"/",SCALETAG,"/",EDIR,"/")

  efiles=list.files(path=basepath,pattern="*.nc*")
  print(efiles)

  for(ifile in efiles){
    print(paste0("Removing ",workdir,"EMISFILES/",emisyrm,"/",SCALETAG,"/",EDIR,"/",ifile,"..."))
    file.remove(paste0(casepath,ifile))
    file.copy(from=paste0(basepath,ifile),
              to=paste0(casepath,ifile))
  }
  
  print(paste0("REGION: ",casepath,"regions.nc"))
    
  SFFILE=paste0(workdir,"RESULTS/",emisyrm,"/SCALE_FACTOR_",SCALETAG,".rds")
  
  print(paste0("Reading scale factor file ",SFFILE,"..."))
  SF_DF=readRDS(file=SFFILE)
  SF_RG=as.vector(unique(SF_DF$REGION))
  
  if(is.null(SF_RG[1])){
    SF_RG="ALL"
    SF_DF$REGION="ALL"
  }
  rgmp=open.nc(paste0(casepath,"regions.nc"))
  COLS=dim.inq.nc(rgmp,"COL")$length
  ROWS=dim.inq.nc(rgmp,"ROW")$length
  region_sf=array(0,dim=c(COLS,ROWS,length(SF_RG)))
  
  for(rgid in 1:length(SF_RG)){
    try({
      region_sf[,,rgid]=var.get.nc(rgmp,paste0(SF_RG[rgid],""))
    },silent=T)
  }
  
  close.nc(rgmp)
  
  SF_RG_FACTOR=array(1,dim=c(COLS,ROWS))
  SF_RG_NAMETG=array("NA",dim=c(COLS,ROWS))
  print(paste0("Reading region info from file..."))
  print(SF_RG)
  
  for(EMI in 1:length(MEIC_TYPE)){
    BSF=paste0(casepath,"BASE_",MEIC_TYPE[EMI],"_M.ncf")
    print(paste0("READING EMFILE: ",BSF,"."))
    em=open.nc(BSF,write = T)
    if(EMI==1){
      EROWS=dim.inq.nc(em,"ROW")$length
      ECOLS=dim.inq.nc(em,"COL")$length
      if(EROWS!=ROWS | ECOLS!=COLS){
        print(paste("Grid Info from EMIS: COL:",ECOLS,", ROW:",EROWS,"."))
        print(paste("Grid Info from REGF: COL:",COLS,", ROW:",ROWS,"."))
        print("Grid mismatch between MET and EMIS.")
        quit()
      }
    }
    for(VN in 1:length(VARBASEEMI)){
      sctmp=subset(SF_DF,NAME==MEIC_TYPE[EMI])
      for(rgid in 1:length(SF_RG)){
        sctmp_rg=subset(sctmp,REGION==SF_RG[rgid])
        SCALEFAC=as.vector(sctmp_rg[[VARBASEEMI[VN]]])
        #print(paste0("Scale factor read from file for ",MEIC_TYPE[EMI]," ",VARBASEEMI[VN]," at region ",SF_RG[rgid],": ",SCALEFAC,"."))
        if(SF_RG[rgid]=="ALL"){
          SF_RG_FACTOR[,]=SCALEFAC
          SF_RG_NAMETG[,]="ALL"
        }else{
          SF_RG_FACTOR[region_sf[,,rgid]==1]=SCALEFAC
          SF_RG_NAMETG[region_sf[,,rgid]==1]=SF_RG[rgid]
        }
      }
      tmpemis=var.get.nc(em,VARBASEEMI[VN])
      tmpfact=array(1,dim=c(dim(SF_RG_FACTOR),dim(tmpemis)[3]))
      tmpfact[,,]=SF_RG_FACTOR
      fixedemis=array(0,dim=c(dim(tmpemis)[1:2],1,dim(tmpemis)[3]))
      fixedemis[,,1,]=tmpemis*tmpfact
      
      print(paste0("For cat ",MEIC_TYPE[EMI]," and emis ",VARBASEEMI[VN], ", Raw emis: ",round(sum(tmpemis[,,13]),2),", Fixed emis: ",round(sum(fixedemis[,,1,13]),2),"."))
      rgsum=data.frame(REGION=as.vector(SF_RG_NAMETG),EMISRAW=as.vector(tmpemis[,,13]),EMISNEW=as.vector(fixedemis[,,1,13]))
      rgsum_rep=as.data.frame(summarize(group_by(rgsum,REGION),EMISRAW=sum(EMISRAW,na.rm=T),EMISNEW=sum(EMISNEW,na.rm=T)))
      print(rgsum_rep)
      patch_emis_rep=rbind(patch_emis_rep,
                           data.frame(CAT=MEIC_TYPE[EMI],POL=VARBASEEMI[VN],BEFORE=round(sum(tmpemis[,,13]),2),AFTER=round(sum(fixedemis[,,1,13]),2)))
      var.put.nc(em,variable=VARBASEEMI[VN],data=fixedemis)
    }
    close.nc(em)
  }
  print("Done.")

  if(UPDATEEMIS){
  for(ifile in efiles){
    file.remove(paste0(emisdir,EDIR,"/",ifile))
    file.copy(from=paste0(casepath,ifile),
              to=paste0(emisdir,EDIR,"/",ifile))
  }
  }

  return(patch_emis_rep)
}

scale_data=gen_scale_file()
saveRDS(file=paste0(workdir,"RESULTS/",emisyrm,"/SCALE_FACTOR_",SCALETAG,".rds"),scale_data)
erep=PatchEMIS(emispct)
saveRDS(file=paste0(workdir,"RESULTS/",emisyrm,"/EMISSION_REP_",SCALETAG,".rds"),erep)
print(erep)
