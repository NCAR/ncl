C
C This example demonstrates how to read and manipulate colormaps.
C
C The first frame displays the default colormap.
C The second frame shows how to alter an entry in the colormap.
C The third frame shows how to create a completely new colormap.
C
	program basic05f
	implicit none

        external NhlFappLayerClass
        external NhlFXWorkstationLayerClass
        external NhlFLabelBarLayerClass

	integer i,j,ierr
	integer num_dims,len_dims(100)
	integer appid,wks,lbar,rlist,glist

	character*3 colorindices(114)

	real cmap(1000,1000)
	real newcmap(3,114)

C ###########
C # Frame 1 #
C ###########
C Display the default colormap.
C
C Initialize labels for the color map entries

	data colorindices / 
     $  '  0','  1','  2','  3','  4','  5','  6','  7','  8','  9',
     $  ' 10',' 11',' 12',' 13',' 14',' 15',' 16',' 17',' 18',' 19',
     $  ' 20',' 21',' 22',' 23',' 24',' 25',' 26',' 27',' 28',' 29',
     $  ' 30',' 31',' 32',' 33',' 34',' 35',' 36',' 37',' 38',' 39',
     $  ' 40',' 41',' 42',' 43',' 44',' 45',' 46',' 47',' 48',' 49',
     $  ' 50',' 51',' 52',' 53',' 54',' 55',' 56',' 57',' 58',' 59',
     $  ' 60',' 61',' 62',' 63',' 64',' 65',' 66',' 67',' 68',' 69',
     $  ' 70',' 71',' 72',' 73',' 74',' 75',' 76',' 77',' 78',' 79',
     $  ' 80',' 81',' 82',' 83',' 84',' 85',' 86',' 87',' 88',' 89',
     $  ' 90',' 91',' 92',' 93',' 94',' 95',' 96',' 97',' 98',' 99',
     $  '100','101','102','103','104','105','106','107','108','109',
     $  '110','111','112','113' /

C
C Initialize libraries and create a resource list.
C
        call NhlFInitialize
        call NhlFRLCreate(rlist,'SETRL')

        call NhlFRLClear(rlist)
        call NhlFCreate(appid,'appid',NhlFappLayerClass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
        call NhlFRLClear(rlist)
        call NhlFCreate(wks,'wks',NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)

C
C Create a labelbar object. 
C
        call NhlFRLClear(rlist)

C Assign the labels
        call NhlFRLSetStringArray(rlist,'lbLabelStrings',
     $       colorindices,114,ierr)

C Label every 17th entry 
        call NhlFRLSetInteger(rlist,'lbLabelStride',17,ierr)

C Single pattern used for fill
        call NhlFRLSetString(rlist,'lbMonoFillPattern','True',ierr)

C Set fill pattern to solid
        call NhlFRLSetString(rlist,'lbFillPattern','SolidFill',ierr)

C No lines between colors
        call NhlFRLSetString(rlist,'lbBoxLinesOn','False',ierr)

C Display 114 entries
        call NhlFRLSetInteger(rlist,'lbBoxCount',114,ierr)

C Turn off labelbar perimeter
        call NhlFRLSetString(rlist,'lbPerimOn','False',ierr)

C Plot title
        call NhlFRLSetString(rlist,'lbTitleString',
     $       'Default Colormap',ierr)

C Title font
        call NhlFRLSetString(rlist,'lbTitleFont','Helvetica-bold',ierr)

C Label font
        call NhlFRLSetString(rlist,'lbLabelFont','Helvetica',ierr)

C Set viewport to max size
        call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
        call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
        call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
        call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)

        call NhlFCreate(lbar,'lbar',NhlFlabelBarLayerClass,wks,
     $       rlist,ierr)

C
C Draw and frame the labelbar
C
	call NhlFDraw(lbar,ierr)
	call NhlFFrame(wks,ierr)

C ###########
C # Frame 2 #
C ###########
C Alter a single entry in the colormap.
C
C Get the current colormap for the workstation pointed to by wks.
C The colormap is stored in a 3xN variable where N is the length of
C the colormap.  Each entry in the color map consists of a vector
C of 3 normalized red-green-blue color values.
C
C Note:  At the time of writing this script, most of the NCL functions
C for color map manipulation were not yet available.  So, the rest
C of this example may no longer be the most effective and simplest
C way to change color map entries.

C Set variable to large values previous to GetValues call

	num_dims = 100
	len_dims(1) = 1000
	len_dims(2) = 1000

        call NhlFRLCreate(glist,'GETRL')
        call NhlFRLClear(glist)
	call NhlFRLGetMDFloatArray(glist,'wkColorMap',cmap,num_dims,
     $       len_dims,ierr)
        call NhlFGetValues(wks,glist,ierr)

	do 20,i=1,len_dims(2)
		do 20,j=1,len_dims(1)
			newcmap(j,i) = cmap(j,i)
 20	continue

C
C Change the first entry in the colormap array to red.
C
 	newcmap(1,1) = 1.0
 	newcmap(2,1) = 0.0
 	newcmap(3,1) = 0.0

C
C Assign the new color map to the workstation object.
C
        call NhlFRLClear(rlist)
	call NhlFRLSetMDFloatArray(rlist,'wkColorMap',newcmap,
     $       num_dims,len_dims,ierr) 
        call NhlFSetValues(wks,rlist,ierr)

C
C Add a different title.
C
        call NhlFRLClear(rlist)
	call NhlFRLSetString(rlist,'lbTitleString',
     $       'Entry 0 set to Red',ierr)
	call NhlFSetValues(lbar,rlist,ierr)
   
C
C Draw and frame the labelbar.
C
	call NhlFDraw(lbar,ierr)
	call NhlFFrame(wks,ierr)

C ###########
C # Frame 3 #
C ###########
C Create and assign a new colormap.
C
C Create an array that will contain the new colormap.

C
C Assign new RGB values to each entry of the colormap.
C
	j = 0
	do 10, i=1,114*3,3
		j = j + 1
		newcmap(1,j) = 1.0-(((i-1)/3)/113.0)
		newcmap(2,j) = ((i-1)/3)/113.0
		newcmap(3,j) = ((i-1)/3)/113.0
  10	continue

C
C Assign the new color map to the workstation object.
C
        call NhlFRLClear(rlist)
	call NhlFRLSetMDFloatArray(rlist,'wkColorMap',newcmap,
     $       num_dims,len_dims,ierr)
	call NhlFSetValues(wks,rlist,ierr)

C
C Assign a new title.
C
        call NhlFRLClear(rlist)
	call NhlFRLSetString(rlist,'lbTitleString','New colormap',ierr)
	call NhlFSetValues(lbar,rlist,ierr)

C
C Draw and frame the labelbar
C
	call NhlFDraw(lbar,ierr)
	call NhlFFrame(wks,ierr)

	call NhlFDestroy(lbar,ierr)
	call NhlFClose

	stop
	end
