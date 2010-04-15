## Generic
setGeneric('chromatoSimGui',function(object,col,...) standardGeneric('chromatoSimGui'))
## Method
setMethod('chromatoSimGui','chromatoSim',function(object,col,...) {
  require(gWidgets)
  require(RGtk2)
  options("guiToolkit"="RGtk2")

  ## Main window
  win <<- gwindow('NetCDF browser and simulator',visible=T)
  group <- ggroup(horizontal=FALSE,cont=win)


  ## menubar and toolbar
  mbl <- list(
              File=list(
                openFile=list(handler=openHandler,icon='open'),
                quit=list(handler=quitHandler,icon='cancel')
                ),
              Edit=list(
                paste=list(handler=defHandler),
                copy=list(handler=defHandler)
                )
              )

  tbl <- list(
              open=list(handler=openHandler,icon='open'),
              save=list(handler=defHandler,icon='save'),
              quit=list(handler=quitHandler,icon='quit')
             # quitNb=list(handler=quitNbHandler,icon='cancel')
              )

  mb <- gmenu(mbl,cont=group)
  tb <- gtoolbar(tbl,cont=group)
  g1 <- ggroup(cont=group,expand=T,horizontal=FALSE)
  nb <- gnotebook(cont=g1,expand=T)


  ## parameter setting tab
  g3 <- ggroup(horizontal=F,cont=nb,label='Parameter Settings')
  para <<- glayout(cont=g3)
  g2 <- ggroup(cont=g3,horizontal=TRUE)
  addSpring(g2)
  button1 <- gbutton("Generate",cont=g2,handler=simulHandler)
  button2 <- gbutton("Set to Default",cont=g2,handler=setdefHandler)
  ## should have a layout function here, dynamicly load para defined in class
  ## chromatoSim
  ## layout function
  createLayout(object,col=col)

})

## Goal: Dynamicly load widgets, parameters
setGeneric('createLayout',function(object,col=NULL,...) standardGeneric('createLayout'))
setMethod('createLayout','chromatoSim',function(object,col=NULL,...){
  if(is.null(col)) col <- 4
  lst <- object@par
  n <- 0
  rr <- 1
  for(i in 1:length(lst)){
    if(n==col) {n <- 0;rr <- rr+1}
    par.name <- names(lst[i])
    n <- n+1
    para[rr,n] <<-glabel(par.name)
    n <- n+1
    para[rr,n] <<-gedit(lst[[i]])
   }
})



  

