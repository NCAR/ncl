*ndvPlotStyleName : Streamline
*ndvPlotName : streamline
*ndvFuncFiles : (/ ../common.ncl /)

*ndvData : (/ u : 2, v : 2 /)
*ndvData@ConformalGroup : (/ $u$, $v$ /)

*ndvObjects : (/ vf : vectorFieldClass , \
		st : streamlinePlotClass /)

*u@Description : U Component of Vector Field
*u@Required : True
*v@Description : V Component of Vector Field
*v@Required : True

*vf@vfUDataArray : $u$
*vf@vfUDataArray%Profile : (/ Name : U Component /)
*vf@vfVDataArray : $v$
*vf@vfVDataArray%Profile : (/ Name : V Component /)
*vf@vfXArray : $u$&-1
*vf@vfXArray%Profile : (/ Name : X Coordinate /)
*vf@vfYArray : $u$&-2
*vf@vfYArray%Profile : (/ Name : Y Coordinate /)

*st@stVectorFieldData : $vf$


*vpUseSegments : True

