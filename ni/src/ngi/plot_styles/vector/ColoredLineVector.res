*ndvPlotStyleName : Colored Line Vector

*ndvPlotName : vector
*ndvFuncFiles : (/ ../common.ncl /)

*ndvData : (/ u : 2, v : 2, sfdata : 2 /)
*ndvData@ConformalGroup : (/ $u$, $v$, $sfdata$ /)

*ndvObjects : (/ vf : vectorFieldClass , \
	         sf : scalarFieldClass , \
		 vc : vectorPlotClass /)

*u@Description : U Component of Vector Field
*u@Required : True
*u@Pattern : (/ /.*u.*/i , /.*/ /)
*v@Description : V Component of Vector Field
*v@Required : True
*v@Pattern : (/ /.*v.*/i /)
*sfdata@Description : Scalar Field
*sfdata@Required : False

*vf@vfUDataArray : $u$
*vf@vfUDataArray%Profile : (/ Name : U Component /)
*vf@vfVDataArray : $v$
*vf@vfVDataArray%Profile : (/ Name : V Component /)
*vf@vfXArray : $u$&-1
*vf@vfXArray%Profile : (/ Name : X Coordinate /)
*vf@vfYArray : $u$&-2
*vf@vfYArray%Profile : (/ Name : Y Coordinate /)

*sf@sfDataArray : $sfdata$
*sf@sfDataArray%Profile : (/ Name : Scalar Field  /)

*vc@vcVectorFieldData : $vf$
*vc@vcScalarFieldData : $sf$
!*vc@vcScalarFieldData%Profile : (/ SaveForCompare : True /)

*vc@ndvUpdateFunc : vc_scalar_field_color($vc$,True)
*vc@ndvUpdateFunc%Profile : (/ Name : Color by Scalar Field /)

*vc@vcLevelColors : level_colors($vc$,2,-1)
*vc@vcLevelColors%Profile : (/ Name : Level Colors, SaveForCompare : True /)

*vc@ndvUpdateFunc1 : set_vector_size_params($vc$,0,0.04,0.0,0.02)
*vc@ndvUpdateFunc1%Profile : (/ Name : Vector Size/Spacing Control /)
*vc@ndvUpdateFunc2 : SetVectorLevels($vc$,0,0,0)
*vc@ndvUpdateFunc2%Profile : (/ Name : Vector Level Control /)

*vcMonoLineArrowColor : False
*pmLabelBarDisplayMode : Conditional
*vpXF : 0.12

*vpUseSegments : True
*MaxLevelCount : 20
