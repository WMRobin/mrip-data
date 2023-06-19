MRIP.lenfreq<-function(intdir=NULL,common=NULL, st=NULL,
  styr=NULL,endyr=NULL,dom=NULL,lenunit=1,lenint=1,conv=1,parms=c(NA,NA)){
  # Gary Nelson, Massachusetts Division of Marine Fisheries
  # gary.nelson@mass.gov
  #07_22_2022 - G Nelson coded method to extract the domain labels for better output  
    if(is.null(intdir)) stop("Need main directory location of intercept files.")
    if(is.null(st)) {stop("No state code was specified.")}
    if(is.null(styr)) stop("Starting year is missing.")
    if(is.null(endyr)) stop("Ending year is missing.")
    if(is.null(common)) stop("Enter common name for species.")
    if(conv==2 & any(is.na(parms))) stop("Conversion equation parameter(s) are missing")
  if(length(grep("/",intdir))==1){
        din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("/"),
          c(paste(intdir,"int",sep="")),c(paste(intdir,"/int",sep="")))
     }
    if(length(grep("\\\\",intdir))==1){
        din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("\\"),
          c(paste(intdir,"int",sep="")),c(paste(intdir,"\\int",sep="")))
     }
  common<-tolower(common)
  st<-as.character(st)
  styr<-as.character(styr)
  endyr<-as.character(endyr)
  wave<-as.character(c(1,2,3,4,5,6))
    dom_id<-NULL
  dataset<-NULL
  temp<-NULL
  temp1<-NULL
   rbind2<- function(input1, input2){
   if(!is.null(ncol(input1))){
   n.input1 <- ncol(input1)
   n.input2 <- ncol(input2)
   
   if (n.input2 < n.input1) {
     TF.names <- which(names(input2) %in% names(input1))         
     column.names <- names(input2[, TF.names])
     } 
   else {TF.names <- which(names(input1) %in% names(input2))
     column.names <- names(input1[, TF.names])}
   return(rbind(input1[, column.names], input2[, column.names]))
  }
    if(is.null(ncol(input1))) return(rbind(input1,input2))
 }   
	for(yr in styr:endyr){
 	   for (j in 1:as.numeric(length(wave))){ 
             #Get size
      	     wv<-wave[j] 
             t3<-read.csv(paste(din,yr,"/","size_",yr,wv,".csv",sep=""),
               colClasses=c("character"),na.strings=".")
		t3$ST <- ifelse(t3$ST==12&t3$SUB_REG==6,121,ifelse(t3$ST==12&t3$SUB_REG==7,122,t3$ST))
               t3<-t3[t3$ST %in% c(st),]
               names(t3)<-tolower(names(t3))
             temp<-rbind2(temp,t3)
             
      #get trips
             t4<-read.csv(paste(din,yr,"/","trip_",yr,wv,".csv",sep=""),
              colClasses=c("character"),na.strings=".")
		t4$ST <- ifelse(t4$ST==12&t4$SUB_REG==6,121,ifelse(t4$ST==12&t4$SUB_REG==7,122,t4$ST))
             t4<-t4[t4$ST %in% c(st),]
            names(t4)<-tolower(names(t4))
            temp1<-rbind2(temp1,t4)
 	   }
   
	}
  convtolow<-function(x){
    for(i in 1:ncol(x)) x[,i]<-tolower(x[,i])
    return(x)
  }
  temp<-convtolow(temp)
  temp1<-convtolow(temp1)
  temp<-temp[,c("common","strat_id","psu_id","st","id_code","sp_code",
        "lngth","wgt","lngth_imp","wgt_imp","wp_size","l_in_bin","l_cm_bin")]
  temp<-temp[order(temp$strat_id,temp$psu_id,temp$id_code),]
  temp1<-temp1[order(temp1$strat_id,temp1$psu_id,temp1$id_code),]
  dataset<-merge(temp1,temp,by.x=c("strat_id","psu_id","id_code","st"),
                by.y=c("strat_id","psu_id","id_code","st"),all.x=FALSE,all.y=FALSE)
  dataset$common<-as.character(dataset$common)
  dataset$common<-ifelse(is.na(dataset$common),"",dataset$common)
 
  #Construct Domain
  dom_ids<-NULL
  mainlev<-length(dom)
  if(length(dom)>0){
    for(l in 1:mainlev){
      if (!any(names(dom)[l]==names(dataset))) stop(paste("Variable ",names(dom[l]),"not found in MRIP dataset"))
      if (any(names(dom)[l]==names(dataset))){
          dataset[,ncol(dataset)+1]<-"DELETE"
          names(dataset)[ncol(dataset)]<-c(paste(names(dom)[l],"1",sep=""))
          colpos<-which(names(dataset)==names(dom[l]))
           sublev<-length(dom[[l]])
         for (k in 1:sublev) dataset[,ncol(dataset)]<-ifelse(dataset[,colpos] %in% c(as.character(dom[[l]][[k]])),c(paste(names(dom)[l],k,sep="")),dataset[,ncol(dataset)])  
        dom_ids<-c(dom_ids,names(dom[l]))   
      }
    }
    test<-c("year","wave","st","sub_reg","mode_fx","area_x")
    for(gg in 1:as.numeric(length(dom_ids))){
      if(!any(dom_ids[gg]==test)) test[as.numeric(length(test)+1)]<-c(paste(dom_ids[gg],"1",sep="")) 
      if(any(dom_ids[gg]==test)){
        colpos<-which(test==dom_ids[gg])
        test[colpos]<-c(paste(dom_ids[gg],"1",sep=""))
      }
    }
    ## add Non-domain names to values in columns
      for(gg in 1:as.numeric(length(test))){
        if(substr(test[gg],nchar(test[gg]),nchar(test[gg]))!="1"){
          eval(parse(text=paste("dataset$",test[gg],'<-paste("',as.character(test[gg]),'",dataset$',test[gg],',sep="")',sep="")))
        }
      }
   for(hh in 1:as.numeric(length(test))){
     if(hh==1) texter<-paste("dataset$",test[hh],sep="") 
     if(hh>1) texter<-paste(texter,",","dataset$",test[hh],sep="")  
   }
     eval(parse(text=paste("dataset$dom_id<-c(paste(",texter,",sep=''))",sep="")))
     tempdom<-as.character(texter)
  }# dom>0 
 
  if(length(dom)==0){
   dataset$year<-c(paste("year",dataset$year,sep=""))
   dataset$wave<-c(paste("wave",dataset$wave,sep=""))
   dataset$st<-c(paste("st",dataset$st,sep=""))
   dataset$sub_reg<-c(paste("sub_reg",dataset$sub_reg,sep=""))
   dataset$mode_fx<-c(paste("mode_fx",dataset$mode_fx,sep=""))
   dataset$area_x<-c(paste("area_x",dataset$area_x,sep=""))
   dataset$dom_id<-c(paste(dataset$year,dataset$wave,dataset$st,dataset$sub_reg,dataset$mode_fx,dataset$area_x,sep=""))
   tempdom<-c("dataset$year,dataset$wave,dataset$st,dataset$sub_reg,dataset$mode_fx,dataset$area_x")
  }
    
  if(lenunit==1){
    if(conv==1) dataset$lenbin<-floor((as.numeric(dataset$lngth)*0.03937)/lenint)*lenint    
    if(conv==2) dataset$lenbin<-floor((parms[1]+parms[2]*(as.numeric(dataset$lngth)*0.03937))/lenint)*lenint    
  }
  if(lenunit==2){
    if(conv==1) dataset$lenbin<-floor(as.numeric(dataset$lngth)/lenint)*lenint  
    if(conv==2) dataset$lenbin<-floor((parms[1]+parms[2]*as.numeric(dataset$lngth))/lenint)*lenint  
  }
  dataset$dom_id_add<-ifelse(dataset$common==common,c(paste(dataset$dom_id,common,sep="")),c(paste(dataset$dom_id,"XXXXXXXXXXXX",sep="")))  
  dataset$wp_size<-as.numeric(dataset$wp_size)
  dataset$numlen<-1
  dataset$wp_size<-ifelse(is.na(dataset$wp_size),0,dataset$wp_size)
  dfpc<-svydesign(ids=~psu_id,strata=~strat_id,
      weights=~wp_size,nest=TRUE,data=dataset)
   options(survey.lonely.psu = "certainty")
   results<-svyby(~numlen,
     ~dom_id_add+lenbin,dfpc,svytotal,vartype=c("se","cv"),keep.names=FALSE) 
  names(results)<-c("Domain","Length","Number","SE","PSE")
   if(length(grep("DELETE",results$Domain,fixed=TRUE))>0){
     results<-results[-c(grep("DELETE",results$Domain,fixed=TRUE)),]
   }
    results<-results[substr(results$Domain,as.numeric(nchar(results$Domain)-4),
     nchar(results$Domain))!="XXXXX",]
    results<-results[order(results$Domain),]
    
    if(nrow(results)>0){  
      
      #Make more palatable Domain labels
      tempdom<-gsub("([0-9]+).*$", "",unlist(strsplit(gsub('dataset$', '', tempdom,fixed=TRUE),",")))
      for(i in 1:length(tempdom)){
        eval(parse(text=paste(tempdom[i],"_first_pos<-regexpr(pattern='",tempdom[i],"',results$Domain)",sep="")))
        eval(parse(text=paste(tempdom[i],"_label<-substr(results$Domain,start=",tempdom[i],"_first_pos,stop=attributes(",tempdom[i],
                              "_first_pos)$match.length+",tempdom[i],"_first_pos-1)",sep="")))
        eval(parse(text=paste(tempdom[i],"_first_pos<-regexpr(pattern='",tempdom[i],"',results$Domain)",sep="")))
        eval(parse(text=paste(tempdom[i],"_label<-substr(results$Domain,start=",tempdom[i],"_first_pos,stop=attributes(",tempdom[i],
                              "_first_pos)$match.length+",tempdom[i],"_first_pos-1)",sep="")))
      }
      for(i in 1:c(length(tempdom)-1)) eval(parse(text=paste(tempdom[i],"_value<-substr(results$Domain,start=attributes(",tempdom[i],
                                                             "_first_pos)$match.length+",tempdom[i],"_first_pos,stop=",tempdom[i+1],"_first_pos-1)",sep="")))
      
      #last element
      foo<-gregexpr("([[:digit:]]+)",results$Domain)
      start_positions<-NULL
      length_last_numeric<-NULL
      for(y in 1: length(foo)){
        endval<-length(foo[[y]]) #number of numbers in domain
        start_positions<-c(start_positions,foo[[y]][endval]) # last start position of numeric in string
        length_last_numeric<-c(length_last_numeric,attributes(foo[[y]])$match.length[endval]) # length of last number
      }
      last_pos<-start_positions+length_last_numeric-1
      eval(parse(text=paste(tempdom[length(tempdom)],"_value<-substr(results$Domain,start=start_positions,stop=last_pos)",sep="")))
      
      #species names
      species_names<-substr(results$Domain,start=start_positions+length_last_numeric, nchar(results$Domain))
      eval(parse(text=paste("foo1<-data.frame(",tempdom[1],"=",tempdom[1],"_value)",sep="")))
      for(i in 2:length(tempdom)) eval(parse(text=paste("foo1$",tempdom[i],"<-",tempdom[i],"_value",sep="")))
      foo1$species<-species_names
      outpt<-cbind(foo1,results[,-1])
      outpt$PSE<-round(outpt$PSE*100,1)
      return(outpt)
    }
    if(nrow(results)==0) return(results)
} #function
