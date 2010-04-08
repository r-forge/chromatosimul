## plot profile matrix
setGeneric('csplotProf',function(object,...) standardGeneric('csplotProf'))
setMethod('csplotProf','chromatoSim',function(object,sampleID=NULL,...){
  if(is.null(sampleID)) sampleID <- 1
  p <- img(object,sampleID)
  print(p)
})



img <- function(object,sampleID){
  require(ggplot2)
  l2e(object@par,environment())
  profile <- object@result$profile$a[[1]]
  rt <- seq(rt_range[1],rt_range[2],rt_diff)
  df <- data.frame(time=rep(rt,each=nrow(profile)),
                   mz=rep(mz_range[1]:mz_range[2],ncol(profile)),
                   int=as.numeric(profile))
  df <- df[profile>500,]                #FIXME:change 500 to a proper number
  df[,'int'] <- log(df[,'int'])
  p <- ggplot(df,aes(x=time,y=mz,fill=int))+geom_tile()
  p <- p+scale_fill_gradient(limits=c(0,log(300000)),low='yellow',high='black')
  p <- p+opts(legend.position='none')+scale_x_continuous(limits=c(0,3700))
  p
}

## plot retention time match or drift diagnostic graphics
setGeneric('csplotRt',function(object,...) standardGeneric('csplotRt'))
setMethod('csplotRt','chromatoSim',function(object,...){
  browser()
  profile <- object@result$profile
  l2e(object@par,environment())
  rt <- seq(rt_range[1],rt_range[2],rt_diff)
  pflst <- do.call(c,profile)
  rtlst <- do.call(c,object@result$rt)
  poslst <- do.call(c,object@result$pos)
  lst <- lapply(pflst,function(x) {apply(x,2,sum)})
  par(new=TRUE)
  for(i in 1:length(lst)){
    plot(rt[poslst[[i]]],lst[[i]][poslst[[i]]],type='h')
  }
})









