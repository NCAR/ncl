*ndvPlotStyleName : Scatter Xy

*ndvPlotName : xy
*ndvFuncFiles : (/ ../common.ncl /)

*ndvData : (/ data : 1 /)
*ndvObjects : (/ ca : coordArraysClass , \
		xy : xyPlotClass /)

*data@Description : Data Variable
*data@Required : True

*ca@caYArray : $data$
*ca@caXArray : $data$&-1

*xy@xyCoordData: $ca$

*xyMarkLineMode : markers

*vpUseSegments : True

