*ndvPlotStyleName : Fill Vector
*ndvPlotName : vector
*ndvFuncFiles : (/ ../common.ncl /)

*ndvData : (/ u : 2, v : 2 /)
*ndvData@ConformalGroup : (/ $u$, $v$ /)

*ndvObjects : (/ vf : vectorFieldClass , \
		vc : vectorPlotClass /)

*u@Description : U Component of Vector Field
*u@Required : True
*v@Description : V Component of Vector Field
*v@Required : True

*vf@vfUDataArray : $u$
*vf@vfUDataArray%Profile : (/ Name : U Component /)
*vf@vfVDataArray : $v$
*vf@vfVDataArray%Profile : (/ Name : V Component /)
*vf@vfXArray : $u$!-1
*vf@vfXArray%Profile : (/ Name : X Coordinate /)
*vf@vfYArray : $u$!-2
*vf@vfYArray%Profile : (/ Name : Y Coordinate /)

*vc@vcVectorFieldData : $vf$
*vc@vcLevelColors : level_colors($vc$,2,-1)
*vc@vcLevelColors%Profile : (/ Name : Level Colors, SaveForCompare : True /)
*vc@ndvUpdateFunc : set_vector_size_params($vc$,0,0.04,0.2,0.02)
*vc@ndvUpdateFunc%Profile : (/ Name : Vector Size/Spacing Control /)

*vcFillArrowsOn : True
*vcFillArrowEdgeColor : Foreground
*vcFillArrowEdgeThicknessF : 0.1

*vpUseSegments : True
*MaxLevelCount : 20
