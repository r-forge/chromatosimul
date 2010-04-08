## Write a parser to get parameters from object
setGeneric('csParse',function(object,...) standardGeneric('csParse'))
setMethod('csParse','chromatoSimul',function(object,env,...){
  require(Biobase)
  lst <- object@par
  l2e(lst,env)     
})


## Simulator
setGeneric('generate',function(object,...) standardGeneric('generate'))
setMethod('generate','chromatoSimul',function(object,...){
  ## parse parameters defined in the chromatoSimul class
  current.env <- environment()
  csParse(object,env=current.env)
  ## missing value
  if(is.null(missing)) missing <- int_range[1]
  object@par$missing <- missing  
  ## First goal is to generate a profile matrix.
  rt <- seq(rt_range[1],rt_range[2],rt_diff)
  scan_number<- length(rt)
  profile <- matrix(0,nrow=(diff(mz_range)+1),ncol=scan_number)
  ## then coding:
  ## 0 means in common;
  ## 1 means sample one has, sample 2 doesn't; -1 means the opposite
  ## 2 means sample one high-level, sample two low-level; -2 the opposite
  
  coding <- c(rep(0,common),rep(c(1,-1),diff_zero),rep(c(2,-2),diff_low))
  coding_random <- sample(coding,length(coding))
  ## position
  pos <- sample(1:ncol(profile),length(coding))
  while(any(diff(sort(c(1,pos,ncol(profile))))<(50+rt_shift_sd/rt_diff*2))){
    pos <- sample(1:ncol(profile),length(coding))
  }

  ## generate each pos for each replicates after retention time shift
  plst <- list()
  for(i in 1:rep){
    plst$a[[i]] <- round(rnorm(length(pos),pos,rt_shift_sd/rt_diff))
    plst$b[[i]] <- round(rnorm(length(pos),pos,rt_shift_sd/rt_diff))
  }

                                        #  make a function to generate an intensity in given range
  int_sample <- function(min,max){
    max <- max-missing                  #cause I will add missing in the end
    int <- max-min+1
    while(int>(max-min)){
      int <- rbeta(1,0.5,2)*(max-min)}
    int <- int+min
    return(int)
  }


  ## generate h in each cell for the profile matrix
  lst <- list()
  for(i in 1:rep){
    lst$a[[i]]=lst$b[[i]]=profile
    if(i==1){
      lst$a[[i]]=t(apply(lst$a[[i]],1,function(x){
        for(j in plst$a[[i]][coding_random %in% c(0,1,2)]){
          x[j] <- int_sample(int_range[1],int_range[2])
        }
        return(x)
      }))

      
      
      lst$b[[i]]=t(apply(lst$b[[i]],1,function(x){
        for(j in plst$b[[i]][coding_random %in% c(-1,-2)]){
          x[j] <- int_sample(int_range[1],int_range[2])}
        return(x)
      }))

      lst$a[[i]] <- apply(lst$a[[i]],2,function(x){
        if(sum(x)!=0){
          npeaks <- round(rnorm(1,npeaks_mean,npeaks_sd*npeaks_mean))
          idx_none <- which(x!=0)
          zero <- length(idx_none)-npeaks
          if(zero>0){
            idx_zero<- sample(idx_none,size=zero)
            x[idx_zero] <- 0
          }}
        return(x)
      })

      lst$b[[i]] <- apply(lst$b[[i]],2,function(x){
        if(sum(x)!=0){
          npeaks <- round(rnorm(1,npeaks_mean,npeaks_sd*npeaks_mean))
          idx_none <- which(x!=0)
          zero <- length(idx_none)-npeaks
          if(zero>0){
            idx_zero<- sample(idx_none,size=zero)
            x[idx_zero] <- 0
          }}
        return(x)
      })

      for(j in pos[coding_random %in% c(0,-2,2)]){
        if(j%in%pos[coding_random==0]){
          id <- which(pos==j)
          ja <- plst$a[[i]][id]
          jb <- plst$b[[i]][id]
          lst$b[[i]][,jb] <- sapply(lst$a[[i]][,ja],function(x){
            rnorm(1,x,rep_sd*x)
          })
        }
        if(j%in%pos[coding_random==-2]){
          id <- which(pos==j)
          ja <- plst$a[[i]][id]
          jb <- plst$b[[i]][id]
          lst$a[[i]][,ja] <- sapply(lst$b[[i]][,jb],function(x){
            rnorm(1,x*low,rep_sd*x*low)
          })
        }
        if(j%in%pos[coding_random==2]){
          id <- which(pos==j)
          ja <- plst$a[[i]][id]
          jb <- plst$b[[i]][id]
          lst$b[[i]][,jb] <- sapply(lst$a[[i]][,ja],function(x){
            rnorm(1,x*low,rep_sd*x*low)
          })
        }
      }
    }
    if(i>1){
      for(j in pos[coding_random!=-1]){
        id <- which(pos==j)
        ja0 <- plst$a[[1]][id]
        ja <- plst$a[[i]][id]
        lst$a[[i]][,ja] <- sapply(lst$a[[1]][,ja0],function(x){
          rnorm(1,x,rep_sd*x)
        })
      }
      for(j in pos[coding_random!=1]){
        id <- which(pos==j)
        jb0 <- plst$b[[1]][id]
        jb <- plst$b[[i]][id]
        lst$b[[i]][,jb] <- sapply(lst$b[[1]][,jb0],function(x){
          rnorm(1,x,rep_sd*x)
        })
      }
    }
  }



  if(tau_mean==0) {tau_sd=tau_sd} else{tau_sd=tau_sd*tau_mean}
  ## function to fit one slice for mz
  onemz_simul <- function(object,pos){
    for(i in pos){
      tau <- rnorm(1,tau_mean,tau_sd)
      h <- object[i]
      mu <- i
      span <- rnorm(1,span,span_sd*span) 
      if(span<=0)    span <- rnorm(1,span,span_sd*span)
      sigma <- rnorm(1,sigma,sigma_sd*sigma)
      if(sigma<=0)  sigma <- rnorm(1,sigma,sigma_sd*sigma) 
      x = (mu-round(span/2)):(mu+round(span/2))
      object[x]=egh(x=x,mu=mu,h=h,sigma=sigma,t=tau)
    }
    return(object)
  }


  for(i in 1:rep){
    lst$a[[i]] <-t(apply(lst$a[[i]],1,function(x) onemz_simul(x,plst$a[[i]])))+missing
    lst$b[[i]] <-t(apply(lst$b[[i]],1,function(x) onemz_simul(x,plst$b[[i]])))+missing
  }




  ## Add baseline based on an exponential ditribution
  for(i in 1:rep){
    lst$a[[i]] <-t(apply(lst$a[[i]],1,function(x) {
      x+dexp(1:length(x),rnorm(1,0.005,0.0005))*3e5
    }))
    lst$b[[i]] <-t(apply(lst$b[[i]],1,function(x) {
      x+dexp(1:length(x),rnorm(1,0.005,0.0005))*3e5
    }))
  }
  


  ## Background noise
  ## add the noise to the whole stuff
  for(i in 1:rep){
    lst$a[[i]] <- t(apply(lst$a[[i]],1,function(x){
      rnorm(length(x),x,x*back_sd)
    }))
    lst$b[[i]] <- t(apply(lst$b[[i]],1,function(x){
      rnorm(length(x),x,x*back_sd)
    }))
  }

  
  ## fit noise background(required for pipeline analysis in xcms/chromatoplots
  ## for each scan, we assume every scan have peaks
  ## this method the noise is added only to the column has no peaks

  if(FALSE){
    for(i in 1:rep){
      idx <- (1:ncol(profile))[apply(lst$a[[i]],2,sum)==missing*nrow(profile)]
      temp <- matrix(missing,nrow(profile),length(idx))
      temp <- apply(temp,2,function(x){
        num <- round(rnorm(1,nrow(profile)*0.01,nrow(profile)*0.001))
        idx.random <- unique(round(runif(num,1,nrow(profile))))
        x[idx.random] <- x[idx.random]+abs(rnorm(length(idx.random),1,0.1))
        x
      })
      temp <- as.numeric(unlist(temp))
      lst$a[[i]][,idx] <- temp
      
      idx <- (1:ncol(profile))[apply(lst$b[[i]],2,sum)==missing*nrow(profile)]
      temp <- matrix(missing,nrow(profile),length(idx))
      temp <- apply(temp,2,function(x){
        num <- round(rnorm(1,nrow(profile)*0.01,nrow(profile)*0.001))
        idx.random <- unique(round(runif(num,1,nrow(profile))))
        x[idx.random] <- x[idx.random]+abs(rnorm(length(idx.random),10,0.1))
        x
      })
      temp <- as.numeric(unlist(temp))
      lst$b[[i]][,idx] <- temp    
    }
  }

  
  

  ## begin to generate raw CDF file
  simulCDF <- function(dir=dir,model=model,rep=rep){
    require(ncdf)
    setwd(dir)
    for(i in 1:rep){
      name <- paste("a",i,'.CDF',sep="")
      int <- as.numeric(lst$a[[i]])
      int_filt<- int[int>missing]
      point_number <- length(int_filt)
      sizelist <- list(1:2,1:4,1:8,1:16,1:32,1:64,1:128,1:255,
                       1:2,1:point_number,1,1:scan_number,1)
      sizelist <- lapply(sizelist,as.double)
      genCDF(file=name,model=model,sizelist)
      temp <- open.ncdf(name,write=T)
      put.var.ncdf(temp,"intensity_values",int_filt)
      put.var.ncdf(temp, "scan_acquisition_time",rt)
      put.var.ncdf(temp, "mass_range_min",rep(mz_range[1],scan_number))

      put.var.ncdf(temp, "mass_range_max",rep(mz_range[2],scan_number))
      put.var.ncdf(temp, "total_intensity",as.numeric(apply(lst$a[[i]],2,sum)))
      ##  compute scane index
      l <- as.numeric(apply(lst$a[[i]],2,function(x){
        if(length(x[x>missing])>0){return(length(x[x>missing]))}
        else{return(0)}
      }))
      l <- cumsum(l)
      idx <- c(0,l)
      idx <- idx[-(scan_number+1)]
      put.var.ncdf(temp, "scan_index",idx)
      ## compute mass values
      mz <- rep(mz_range[1]:mz_range[2],ncol(profile))
      mass <- mz[which(int>missing)]
      put.var.ncdf(temp, "mass_values",mass)
      close.ncdf(temp)
      ## for sample b
      name <- paste("b",i,'.CDF',sep="")
      int <- as.numeric(lst$b[[i]])
      int_filt<- int[int>missing]
      point_number <- length(int_filt)
      sizelist <- list(1:2,1:4,1:8,1:16,1:32,1:64,1:128,1:255,
                       1:2,1:point_number,1,1:scan_number,1)
      sizelist <- lapply(sizelist,as.double)
      genCDF(file=name,model=model,sizelist)
      temp <- open.ncdf(name,write=T)
      put.var.ncdf(temp,"intensity_values",int_filt)
      put.var.ncdf(temp, "scan_acquisition_time",rt)
      put.var.ncdf(temp, "mass_range_min",rep(mz_range[1],scan_number))
      put.var.ncdf(temp, "mass_range_max",rep(mz_range[2],scan_number))
      put.var.ncdf(temp, "total_intensity",as.numeric(apply(lst$b[[i]],2,sum)))
      l <- as.numeric(apply(lst$b[[i]],2,function(x){
        if(length(x[x>missing])>0){return(length(x[x>missing]))}
        else{return(0)}
      }))
      l <- cumsum(l)
      idx <- c(0,l)
      idx <- idx[-(scan_number+1)]
      put.var.ncdf(temp, "scan_index",idx)
      ## compute mass values
      mz <- rep(mz_range[1]:mz_range[2],ncol(profile))
      mass <- mz[which(int>missing)]
      put.var.ncdf(temp, "mass_values",mass)
      close.ncdf(temp)
    }}
  
  
  genCDF <- function(file,model,sizelist){
   # if(is.null(model)) {temp <- prototype}
    temp <- open.ncdf(model)
    lst <- list()
    for(i in 1:length(names(temp$dim))){
      id <- names(temp$dim)[i]
      lst$dim[[id]] <- dim.def.ncdf(id,temp$dim[[id]]$units,sizelist[[i]])
    }
    for(i in 1:length(names(temp$var))){
      id <- names(temp$var)[i]
      dim <- list()
      for(j in 1:length(temp$var[[id]]$dim)){
        name <- temp$var[[id]]$dim[[j]]$name
        x <- lst$dim[[name]]
        dim[[j]] <- lst$dim[[name]]
      }
      missval <- temp$var[[id]]$missval
      lst$var[[id]] <- var.def.ncdf(id,
                                    temp$var[[id]]$dim[[1]]$units,dim,
                                    missval)
    }
    
    create.ncdf(file,lst$var)
  }

  simulCDF(dir=dir,model=model,rep=rep)
  intest <- list()
  intest$coding <- coding_random
  intest$pos <- plst
  
  rtlst <- lapply(plst,function(lst){
    lapply(lst,function(x){
      rt[x]
    })
  })

  intest$rt <- rtlst
  intest$profile <- lst
  object@result <- intest
  str(object)
  return(object)
})







