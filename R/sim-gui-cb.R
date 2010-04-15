## Put all the handler here
defHandler <- function(h,...) print('hi')
## handler for quit
quitHandler <- function(h,...) dispose(win)
## plot var
plotVarHandler <- function(h,...) {
  if(exists("gsum")) delete(g33,gsum)ls
  v <- svalue(g4drop)
  p <- svalue(g4drop2)
  n <- get.var.ncdf(temp,v)
  if(length(dim(n))==1){
    sum <- data.matrix(summary(n))
    df <- data.frame(Statistics=rownames(sum),Values=as.numeric(sum[,1]))
    gsum <<- gtable(df,cont=g33,expand=T)
  }else{
    sum <- data.matrix(summary(n))
    df <- as.data.frame(cbind(sum))
    gsum<<-gtable(df,cont=g33,expand=T)
    
  }
  if(p=='boxplot') boxplot(n)
  if(p=='histogram') hist(n)
  if(p=='contour') {
    test <- melt(n)
    names(test) <- c('x','y','z')
    gp <- ggplot(test,aes(x,y,z=z))+stat_contour()
    print(gp)
  }
  if(p=='contour2') {
                                        #   browser()
    contour(z=n)
  }
  if(p=='heatmap'){
    heatmap(n)
  }
}
## handler for open

                                        #model <- NULL
                                        #browser()

modelHandler <- function(text='choose a model',action='print',type='open',...){
  gf <- gfile(type=type,text=text,...,action=action,handler=function(h,..){
                                        #assign(model,h$file,envir=.GlobalEnv)
    model<<-h$file
    para[1,4] <- gedit(h$file)
  })
}

simulHandler <- function(h,...){
  ## get values for parameters of simulater

  dir <- svalue(dir)
                                        #model <- svalue(model)
  int_range = c(as.numeric(svalue(int_min)),as.numeric(svalue(int_max)))
  back_sd=as.numeric(svalue(back_sd))
  rep_sd=as.numeric(svalue(rep_sd))
  rep=as.numeric(svalue(rep))
  common=as.numeric(svalue(common))
  diff_low=as.numeric(svalue(diff_low))
  diff_zero=as.numeric(svalue(diff_zero))
  low=as.numeric(svalue(low))
  low_sd=as.numeric(svalue(low_sd))
  mz_range = c(as.numeric(svalue(mz_min)),as.numeric(svalue(mz_max)))
  npeaks_mean=as.numeric(svalue(npeaks_mean))
  npeaks_sd=as.numeric(svalue(npeaks_sd))
  rt_range = c(as.numeric(svalue(rt_min)),as.numeric(svalue(rt_max)))
  rt_diff =  as.numeric(svalue(rt_diff))
  rt_shift_sd= as.numeric(svalue(rt_shift_sd))
  span=as.numeric(svalue(span_mean))
  span_sd=as.numeric(svalue(span_sd))
  sigma=as.numeric(svalue(sigma_mean))
  sigma_sd=as.numeric(svalue(sigma_sd))
  tau_mean = as.numeric(svalue(tau_mean))
  tau_sd = as.numeric(svalue(tau_sd))
  missing= svalue(missing)

  if(missing!='NULL') {missing=as.numeric(missing) } else{missing=NULL}

  generate(object) }                  


setdefHandler <- function(h,...){
  ## lst <- object@par
  ## for(i in 1:length(lst)){
  ##   paraName <- names(lst[i])
  ##   v <- lst[[i]]
  ##   svalue()
  ## }
  ## dispost para widget and create a new one
  dispose(para)
}

quitNbHandler <- function(h,...){
  dispose(nb)
}

openHandler <- function(text="Select a file",action="print",type='open',...) {
  gfile(text=text,type=type,...,action=action,handler=function(h,...){
    if(length(h$file)>1) {
      gmessage('Please choose single file')
    }else{
      obj <<- h$file
    }
    
    
  }
        )}

