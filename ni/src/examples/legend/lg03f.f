	program lg03f
	implicit none

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC/
C
C      File:           lg03f.f
C
C      Author:         Ed Stautler
C		       National Center for Atmospheric Research
C		       PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates a Legend of 5 markers.
C

	external NhlFhluLayerClass
	external NhlFResListLayerClass
	external NhlFAppLayerClass
	external NhlFLegendLayerClass
	external NhlFXWorkstationLayerClass
		
	integer appid, wid, pid
	integer rlist, ierr

        character*8 labels(5)
        integer colors(5)
        integer types(5)
        integer item_ind(5)
        real item_hgt, lnthik(5)
  
C
C Initialize data values
C
	data labels / 'Line_Type_0', 'Line_Type_1', 'Line_Type_2', 
     $                'Line_Type_3', 'Line_Type_4' /

	data colors / 40, 57, 65, 80, 90 /

	data lnthik / 4.0, 4.0, 4.0, 4.0, 4.0 /

	data types / 0, 0, 0, 0, 0 /

	data item_ind / 2, 3, 4, 5, 6 /

C
C Initialize the high level utility library
C
	call NhlFInitialize

C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
        call NhlFRLCreate(rlist,'SETRL')
        call NhlFRLClear(rlist)
	call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
	call NhlFCreate(appid,'lg03',NhlFappLayerClass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
	call NhlFRLClear(rlist)
	call NhlFRLSetInteger(rlist,'wkPause','True',ierr)
	call NhlFCreate(wid,'lg03Work',NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)
C
C Specify the viewport extent of the object.
C
        call NhlFRLClear(rlist)
	call NhlFRLSetFloat(rlist,'vpXF',0.,ierr)
	call NhlFRLSetFloat(rlist,'vpYF',1.,ierr)
	call NhlFRLSetFloat(rlist,'vpWidthF',1.,ierr)
	call NhlFRLSetFloat(rlist,'vpHeightF',1.,ierr)
C
C Specify the line types for the legend.
C
        call NhlFRLSetInteger(rlist,'lgItemCount',5,ierr)
        call NhlFRLSetString(rlist,'lgMonoItemType','False',ierr)
        call NhlFRLSetFloat(rlist,'lgLabelFontHeightF',.03,ierr)
        call NhlFRLSetStringArray(rlist,'lgLabelStrings',labels,
     $       5,ierr)
        call NhlFRLSetIntegerArray(rlist,'lgItemTypes',types,
     $       5,ierr)
C
C Set the dashed lines and the line characters to the same colors.
C
        call NhlFRLSetIntegerArray(rlist,'lgItemColors',colors,
     $       5,ierr)
        call NhlFRLSetIntegerArray(rlist,'lgItemStringColors',
     $       colors,5,ierr)
        call NhlFRLSetIntegerArray(rlist,'lgItemIndexes',item_ind,
     $       5,ierr)
        call NhlFRLSetString(rlist,'lgMonoItemThickness','False',ierr)
        call NhlFRLSetFloatArray(rlist,'lgItemThicknesses',lnthik,
     $       5,ierr)
        call NhlFRLSetFloat(rlist,'lgItemFontHeightF',.03,ierr)
	call NhlFCreate(pid,'Legend',NhlFlegendLayerClass,wid,rlist,ierr)


	call NhlFDraw(pid,ierr)
	call NhlFFrame(wid,ierr)
	call NhlFDestroy(pid,ierr)
	call NhlFDestroy(wid,ierr)
	call NhlFDestroy(appid,ierr)
	call NhlFClose

	stop
	end
