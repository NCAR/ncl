*ndvPlotStyleName : Line Contour
*ndvPlotName : contour

*ndvFuncFiles : (/ ../common.ncl /)

*ndvData : (/ data : 2 /)
*ndvObjects : (/ sf : scalarFieldClass , \
		cn : contourPlotClass /)

*data@Description : Scalar Field
*data@Required : True

*sf@sfDataArray : $data$
*sf@sfDataArray%Profile : (/ Name : Scalar Field Data Var /)
*sf@sfXArray : $data$!-1
*sf@sfXArray%Profile : (/ Name : X Coordinate /)
*sf@sfYArray : $data$!-2
*sf@sfYArray%Profile : (/ Name : Y Coordinate /)

*cn@cnScalarFieldData : $sf$
*cn@ndvUpdateFunc : SetContourLevels($cn$,0,0,0)
*cn@ndvUpdateFunc%Profile : (/ Name : Contour Level Control /)

*vpUseSegments : True
*MaxLevelCount : 20

