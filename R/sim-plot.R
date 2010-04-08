## plot profile matrix
setGeneric('csplotProf',function(object,...) standardGeneric('csplotProf'))
setMethod('csplotProf','chromatoSim',function(object,sampleID=NULL,...){
  if(is.null(sampleID)) sampleID <- 1
  p <- img(object,sampleID)
  print(p)
})

## FIXME: With new qtinterface.
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
setGeneric('csplotRt',function(object,xlim=NULL,ylim=NULL,...) standardGeneric('csplotRt'))
setMethod('csplotRt','chromatoSim',function(object,xlim=NULL,ylim=NULL,...){
  profile <- object@result$profile
  Biobase::l2e(object@par,environment())
  rt <- seq(rt_range[1],rt_range[2],rt_diff)
  pflst <- do.call(c,profile)
  pflst <- lapply(pflst,function(x) {x-missing})
  rtlst <- do.call(c,object@result$rt)
  poslst <- do.call(c,object@result$pos)
  lst <- lapply(pflst,function(x) {apply(x,2,sum)})
  n <- length(pflst)
  if(is.null(xlim)) xlim=range(rt)
  if(is.null(ylim)) ylim=range(lst)
  df <- data.frame(rt=rep(rt,n),
                   intensity=unlist(lst),
                   group=rep(1:n,each=length(rt)))
  p <- ggplot(data=df, aes(x=rt,y=intensity,group=factor(group),color=group))
  p <- p+scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=ylim)+
      geom_line()
  print(p)
})











