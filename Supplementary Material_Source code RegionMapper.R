#RegionMapper for CMAQ ISAM and Emission Optimization
#           Chengdu Academy of Environmental Sciences
#                   Author: Chengwei LU, lcw@cdaes.cn

create_regions=function(ncdir="D:\\CMAQDOMS\\CP8C",
                        prdom="01",
                        otreg="COT",
                        mapfile="Chengdu.Rds"){
  gridinfo=open.nc(paste0(ncdir,prdom,"\\GRIDCRO2D"))
  lat=var.get.nc(gridinfo,"LAT")
  lon=var.get.nc(gridinfo,"LON")
  col=length(lat[,1])
  row=length(lat[1,])
  
  mapdata=readRDS(paste0(workdir,mapfile))
  mapinfo=data.frame(name=as.vector(unique(mapdata$name)))
  mapinfo$abbr=paste0("D",sapply(1:nrow(mapinfo),fmtnum,dig=2))
  
  ptdf=data.frame(y=as.vector(lat),x=as.vector(lon),pid=NA,pnm=NA)
  grps=mapinfo$abbr
  
  for(grp in 1:length(grps)){
    i_mginfo=mapinfo[grp,]
    ply=subset(mapdata,name==i_mginfo$name)
    ptchk=HRW::pointsInPoly(cbind(ptdf$x,ptdf$y),cbind(ply$long,ply$lat))
    plyn=length(subset(ptchk,ptchk==T))
    if(plyn>0){
      ptdf$pid[ptchk]=fmtnum(grp,2)
      ptdf$pnm[ptchk]=grps[grp]
    }
    
    plot(x=ply$long,y=ply$lat,type="l",col="green",main=paste(i_mginfo$name,"(",grps[grp],")"))
    points(x=ptdf$x[ptchk],y=ptdf$y[ptchk],col="red")
    Sys.sleep(2)
    print(paste0("Processed samap with ",plyn," grids(",grps[grp],")."))
  }
  
  ptdf$pnm[is.na(ptdf$pnm)]=otreg
  
  mnames=cmaq_matix(ptdf$pnm,row,col)
  unames=as.vector(unique(ptdf$pnm))
  
  ncvarlist=gen_vnames(unames,"")
  nv=length(unames)+1
  
  
  write.csv(file=paste0(ncdir,prdom,"\\REGIONREF.csv"),mapinfo)
  sa=create.nc(paste0(ncdir,prdom,"\\regions.nc"))
  
  dim.def.nc(sa,"TSTEP",1) #0
  dim.def.nc(sa,"DATE-TIME",2)   #1
  dim.def.nc(sa,"LAY",1)         #2
  dim.def.nc(sa,"VAR",nv)         #3
  dim.def.nc(sa,"ROW",dim.inq.nc(gridinfo,"ROW")$length)  #4
  dim.def.nc(sa,"COL",dim.inq.nc(gridinfo,"COL")$length)  #5
  if(!ifchem){
    att.copy.nc(gridinfo,"NC_GLOBAL","IOAPI_VERSION",sa,"NC_GLOBAL")
    att.put.nc(sa,"NC_GLOBAL","EXEC_ID","NC_CHAR","SAGRIDS")
    att.copy.nc(gridinfo,"NC_GLOBAL","FTYPE",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","CDATE",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","CTIME",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","WDATE",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","WTIME",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","SDATE",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","STIME",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","TSTEP",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","NTHIK",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","NCOLS",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","NROWS",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","NLAYS",sa,"NC_GLOBAL")
    att.put.nc(sa,"NC_GLOBAL","NVARS","NC_INT",as.numeric(nv))
    att.copy.nc(gridinfo,"NC_GLOBAL","GDTYP",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","P_ALP",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","P_BET",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","P_GAM",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","XCENT",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","YCENT",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","XORIG",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","YORIG",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","XCELL",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","YCELL",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","VGTYP",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","VGTOP",sa,"NC_GLOBAL")
    att.put.nc(sa,"NC_GLOBAL","VGLVLS","NC_FLOAT",c(0,0))
    att.copy.nc(gridinfo,"NC_GLOBAL","GDNAM",sa,"NC_GLOBAL")
    att.copy.nc(gridinfo,"NC_GLOBAL","UPNAM",sa,"NC_GLOBAL")
    att.put.nc(sa,"NC_GLOBAL","HISTORY","NC_CHAR","                      ")
    att.put.nc(sa,"NC_GLOBAL","VAR-LIST","NC_CHAR",ncvarlist)
    att.put.nc(sa,"NC_GLOBAL","FILEDESC","NC_CHAR","Created by SAGRIDS Tool(lcw@cdaes.cn).")
  }
  TFTMP=array(0,dim=c(2,nv,1))
  var.def.nc(sa,"TFLAG","NC_INT",c(1,3,0))
  att.put.nc(sa,"TFLAG","long_name","NC_CHAR","TFLAG")
  att.put.nc(sa,"TFLAG","units","NC_CHAR","<YYYYDDD,HHMMSS>")
  att.put.nc(sa,"TFLAG","var_desc","NC_CHAR","Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS")
  var.put.nc(sa,"TFLAG",TFTMP)
  
  
  for(pvar in unames){
    ncvar=paste0(pvar,"")
    var.def.nc(sa,ncvar,"NC_FLOAT",c(5,4,2,0))
    tmdf=array(0,dim=c(dim.inq.nc(gridinfo,"COL")$length,dim.inq.nc(gridinfo,"ROW")$length,1,1))
    tmdf[,,1,1][mnames==pvar]=1
    var.put.nc(sa,ncvar,tmdf)
    att.put.nc(sa,ncvar,"long_name","NC_CHAR",ncvar)
    att.put.nc(sa,ncvar,"units","NC_CHAR","UNKNOWN")
    att.put.nc(sa,ncvar,"var_desc","NC_CHAR",ncvar)
  }
  
  close.nc(sa)
  close.nc(gridinfo)
  print(paste0(ncdir,prdom,"\\regions.nc saved."))
}