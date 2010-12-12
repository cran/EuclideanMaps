EuclideanMaps <-
function ()
{
  # LIBRERIAS
	library(RODBC)
	require(tcltk)
	library(tkrplot)
	tclRequire("BWidget")

  # CALCULA LAS COORDENADAS DE LOS MAPAS

  Mapas <- function()
  {
    mapafila<-array(,c(dim(mresiduos)[1],2),)
    for (i in (1:dim(mresiduos)[1]))
      {
      mapafila[i,1]<-round(descomposicion$u[i]*descomposicion$d[1]/sqrt(dim(mresiduos)[2]),2)
      mapafila[i,2]<-round(mediafilas[i]-media,2)
      }
    mapacolumna<<-array(,c(dim(mresiduos)[2],2),)
    for (j in (1:dim(mresiduos)[2]))
      {
      mapacolumna[j,1]<-round(descomposicion$v[j]*descomposicion$d[1]/sqrt(dim(mresiduos)[1]),2)
      mapacolumna[j,2]<-round(mediacolumnas[j]-media,2)
      }
    rowFunction <- function()
      {
      params <- par(bg="white")
      plot(mapafila,main=(gettext("Row Euclidean Map",domain="R-EuclideanMaps")),xlab=gettext("Interaction effect",domain="R-EuclideanMaps"),ylab=gettext("Main effect",domain="R-EuclideanMaps"),col=color,pch=icono)
      }
    columnFunction <- function()
      {
      params <- par(bg="white")
      plot(mapacolumna,main=(gettext("Column Euclidean Map",domain="R-EuclideanMaps")),xlab=gettext("Interaction effect",domain="R-EuclideanMaps"),ylab=gettext("Main effect",domain="R-EuclideanMaps"),col=color,pch=icono)
      }
    winMaps <- tktoplevel()
    tkwm.title(winMaps,gettext("Euclidean Maps",domain="R-EuclideanMaps"))
    img1 <<- tkrplot (winMaps,fun=rowFunction)
    img2 <<- tkrplot (winMaps,fun=columnFunction)
    tkgrid(img1,img2)
  }


  # ABRE LA VENTANA DE SELECCION DE FICHERO

  CargaFichero<- function()
  {
    file <- tclvalue(tkgetOpenFile())
    if (!length(file)) return()
    channel <- odbcConnectExcel(file)
    hojastabla<-sqlTables(channel)$TABLE_NAME
  	whoja<-tktoplevel()
    tkwm.title(whoja,gettext("Choose a table:",domain="R-EuclideanMaps"))
  	tl<-tklistbox(whoja,height=4,selectmode="single",background="white")
	  tkgrid(tklabel(whoja,text=gettext("Choose a table:",domain="R-EuclideanMaps")))
	  tkgrid(tl)
  	for (k in (1:(dim(sqlTables(channel))[1])))
   	  {
    	   tkinsert(tl,"end",hojastabla[k])
  		}
    tkselection.set(tl,0)  #Indexing starts at zero.
    OnOK <- function()
      {
		    hojaChoice <<- hojastabla[as.numeric(tkcurselection(tl))+1]
  			matrizd<<-sqlFetch(channel,hojaChoice)
        odbcClose(channel)
      	tkdestroy(whoja)
  		}
    OK.but <-tkbutton(whoja,text="   OK   ",command=OnOK)
  	tkgrid(OK.but)
    tkfocus(whoja)
  	tkwait.window(whoja)
    tkdestroy(winprincipal)

    # SE CALCULA LA MATRIZ DE RESIDUOS

    # Calcula el vector de medias para las filas y columnas

    mediafilas<<-array(,dim(matrizd)[1])
    mediacolumnas<<-array(,dim(matrizd)[2])
    for (i in (1:dim(matrizd)[1]))
       mediafilas[i]<<-rowMeans(matrizd)[i]
    for (j in (1:dim(matrizd)[2]))
       mediacolumnas[j]<<-colMeans(matrizd)[j]
    media<<-mean(mediafilas)

    # Carga la matriz de residuos

    mresiduos<<-matrix(,dim(matrizd)[1],dim(matrizd)[2])
    for (i in (1:dim(matrizd)[1]))
    {
       for (j in (1:dim(matrizd)[2]))
          mresiduos[i,j]<<-round(matrizd[i,j]-mediafilas[i]-mediacolumnas[j]+ media,2)
    }
    # HACEMOS LA DESCOMPOSICION SVD DE LA MATRIZ DE RESIDUOS

    descomposicion<<-svd(mresiduos,1,1)
    descomposicion$v
    descomposicion$u
    descomposicion$d

    # ABRE LA PANTALLA DE OPCIONES

    winoptions <- tktoplevel()
    tkwm.title(winoptions,gettext("Options",domain="R-EuclideanMaps"))
    canvas <- tkcanvas(winoptions,width="80",height="25",bg="Red")
    Symbols <- c("$","%","*","+","^","¤","#","O","@","-")
    comboBox <- tkwidget(winoptions,"ComboBox",editable=FALSE,values=Symbols)
    ChangeColor <- function() 
      {
        color <<- tclvalue(tcl("tk_chooseColor",initialcolor="red",title="Color"))
        if (nchar(color)>0)
          tkconfigure(canvas,bg=color)
      }
    ChangeSymbol <- function()
      {
        icono <<- Symbols[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
      }

    ChangeColor.button <- tkbutton(winoptions,text=gettext("Change Color",domain="R-EuclideanMaps"),command=ChangeColor)
    ChangeSymbol.button <- tkbutton(winoptions,text=gettext("Change Symbol",domain="R-EuclideanMaps"),command=ChangeSymbol)
    Graph.button <- tkbutton(winoptions,text=gettext("  Graph  ",domain="R-EuclideanMaps"),command=Mapas)
    tkgrid(canvas,ChangeColor.button)
    tkgrid(comboBox,ChangeSymbol.button)
    tkgrid(Graph.button)
    tkfocus(winoptions)
  }

# ABRE LA PANTALLA INICIAL

  winprincipal <- tktoplevel()
  tkwm.title(winprincipal,gettext("  Model diagnosis: Euclidean Maps  ",domain="R-EuclideanMaps"))
  fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
  tkgrid(tklabel(winprincipal,text=gettext("  MODEL DIAGNOSIS: EUCLIDEAN MAPS  ",domain="R-EuclideanMaps"),font=fontHeading))
  tkgrid(tklabel(winprincipal,text=" "))
  OK.CargaFichero <-tkbutton(winprincipal,text=gettext("Upload file",domain="R-EuclideanMaps"),command=CargaFichero)
  tkgrid(OK.CargaFichero)
  tkfocus(winprincipal)
  #tkwait.window(winprincipal)
  color <<- "Red"
  icono <<- 1    
}

