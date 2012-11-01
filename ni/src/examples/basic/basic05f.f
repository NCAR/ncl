C
C $Id: basic05f.f,v 1.14 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            basic05f.f
C
C      Author:          Tim Scheitlin (converted by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar 20 10:43:42 MST 1995
C
C      Description:     This example demonstrates how to read, display, and
C                       manipulate colormaps.
C
C             The NG 4.x HLU software supports several different predefined
C             colormaps of various sizes.  This example demonstrates how to
C             display each of those colormaps using the labelbar utility.
C             This example also shows how to change entries in a colormap
C             and create a completely new colormap.
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFLabelBarClass
      external NhlFTextItemClass

      integer i, ierr
      integer num_dims,len_dims(2)
      integer appid,wks,lbar,rlist,glist,text
      character*7  wks_type
      character*3 colorindices(255)

      real cmap(3,8)
      real newcmap(3,100)

C
C Define the workstation type
C
      wks_type = "x11"

C Initialize libraries and create a resource list.

      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFCreate(appid,'appid',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkMetaName','./basic05f.ncgm',ierr)

        call NhlFCreate(wks,'wks',
     1        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C Create an XWorkstation object that uses the default colormap.

        call NhlFRLClear(rlist)

        call NhlFRLSetString(rlist,'wkPause','True',ierr)

C Set color mode to private so that there is no contention for colors
C        call NhlFRLSetString(rlist,'wkXColorMode','private',ierr)

        call NhlFCreate(wks,'wks',
     +     NhlFCairoWindowWorkstationClass,0,
     1     rlist,ierr)

      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkPSFileName','./basic05f.ps',ierr)

        call NhlFCreate(wks,'wks',
     1        NhlFPSWorkstationClass,0,rlist,ierr)

      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkPDFFileName','./basic05f.pdf',
     1        ierr)

        call NhlFCreate(wks,'wks',
     1        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     1         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkFileName','./basic05f',
     1        ierr)
        call NhlFRLSetstring(rlist,'wkFormat',wks_type,
     1        ierr)

        call NhlFCreate(wks,'wks',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkFileName','./basic05f',
     1        ierr)
        call NhlFRLSetstring(rlist,'wkFormat',wks_type,
     1        ierr)

        call NhlFCreate(wks,'wks',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif

C Initialize labels for the colormap entries

      do 100 i=1,255
           write (colorindices(i),111) i
 111       format (I3)
 100  continue

C Create a labelbar object. 

      call NhlFRLClear(rlist)

C Assign the labels
      call NhlFRLSetStringArray(rlist,'lbLabelStrings',
     1      colorindices,232,ierr)

C Label every 5th entry 
      call NhlFRLSetInteger(rlist,'lbLabelStride',5,ierr)

C Single pattern used for fill
      call NhlFRLSetString(rlist,'lbMonoFillPattern','True',ierr)

C Set fill pattern to solid
      call NhlFRLSetString(rlist,'lbFillPattern','SolidFill',ierr)

C No lines between colors
      call NhlFRLSetString(rlist,'lbBoxLinesOn','False',ierr)

C Display 255 entries
      call NhlFRLSetInteger(rlist,'lbBoxCount',255,ierr)

C Turn off labelbar perimeter
      call NhlFRLSetString(rlist,'lbPerimOn','False',ierr)

C Plot title
      call NhlFRLSetString(rlist,'lbTitleString',
     1      '(New) Default Colormap',ierr)

C Title font
      call NhlFRLSetString(rlist,'lbTitleFont','Helvetica-bold',ierr)

C Label font
      call NhlFRLSetString(rlist,'lbLabelFont','Helvetica',ierr)

C Set viewport to max size
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)

      call NhlFCreate(lbar,'lbar',NhlFLabelBarClass,wks,
     1      rlist,ierr)

C Create a text label
      call NhlFRLClear(rlist)

C Set the font
      call NhlFRLSetString(rlist,'txFont','Helvetica-bold',ierr)

C Set position and height
      call NhlFRLSetFloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',.03,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',.035,ierr)

C Set the function code to the "*" character so that the 
C default function code character, the colon, can be used
C in the "txString" resource.
      call NhlFRLSetString(rlist,'txFuncCode','*',ierr)

C Set the text value
      call NhlFRLSetString(rlist,'txString','Note: Entry 0 is the backgr
     $ound color',ierr)

      call NhlFCreate(text,'text',NhlFtextItemClass,wks,
     1      rlist,ierr)


C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to the old default colormap
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','default',ierr)
      call NhlFSetValues(wks,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'lbBoxCount',31,ierr)
      call NhlFRLSetString(rlist,'lbTitleString',
     1      '(Old) Default Colormap',ierr)
      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','cyclic',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Cyclic Colormap',ierr)

C Label every entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',1,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',7,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','gscyclic',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Gscyclic Colormap',
     $     ierr)

C Label every entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',1,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',7,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','gsltod',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Gsltod Colormap',
     $     ierr)

C Label every other entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',2,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',32,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','gsdtol',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Gsdtol Colormap',
     $     ierr)

C Label every other entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',2,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',32,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Uniform Colormap',
     $     ierr)

C Label every 10th entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',10,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',112,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)


C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Temp1 Colormap',
     $     ierr)

C Label every 5th entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',5,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',62,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)


C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','psgcap',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Psgcap Colormap',
     $     ierr)

C Label every 15th entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',15,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',230,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)


C Change the colormap to one of the predefined colormaps
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','example',ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Change the labelbar title, annotation, and number of entries.
      call NhlFRLClear(rlist)

C Labelbar title 
      call NhlFRLSetString(rlist,'lbTitleString','Example Colormap',
     $     ierr)

C Label every 15th entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',10,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',114,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)


C  This next example changes three entries in the colormap.  Changing the
C  first entry (colormap index 0) in the colormap, sets the background
C  color for a plot. The second entry (color index 1) sets the foreground
C  color for a plot.
C 
C  The colormap is stored in a 3xN variable where N is the length of
C  the colormap.  Each entry in the color map consists of a vector
C  of 3 normalized red-green-blue color values.

C Assign gray scale colormap
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','gscyclic',ierr)
      call NhlFSetValues(wks,rlist,ierr)

      num_dims = 2
      len_dims(1) = 3
      len_dims(2) = 8

      call NhlFRLCreate(glist,'GETRL')
      call NhlFRLClear(glist)
      call NhlFRLGetMDFloatArray(glist,'wkColorMap',cmap,num_dims,
     1      len_dims,ierr)
      call NhlFGetValues(wks,glist,ierr)

C  Change the first entry in the colormap array to blue, the
C  second to green, and the fourth to red.


C Background color
      cmap(1,1) = 0.0
      cmap(2,1) = 0.0
      cmap(3,1) = 1.0

C Foreground color 
      cmap(1,2) = 0.0
      cmap(2,2) = 1.0
      cmap(3,2) = 0.0

C Colormap entry 3
      cmap(1,4) = 1.0
      cmap(2,4) = 0.0
      cmap(3,4) = 0.0

C Assign the new color map to the workstation object.
      num_dims=2
      len_dims(1)=3
      len_dims(2)=8
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,
     1      num_dims,len_dims,ierr) 
      call NhlFSetValues(wks,rlist,ierr)

C Add a different title.
      call NhlFRLClear(rlist)

C Set the title
      call NhlFRLSetString(rlist,'lbTitleString',
     1      'Changing colormap entries',ierr)

C Label every entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',1,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',7,ierr)

      call NhlFSetValues(lbar,rlist,ierr)

C Change the textual annotation at bottom of frame
      call NhlFRLClear(rlist)

C Set the title for the labelbar 
      call NhlFRLSetString(rlist,'txString','Entry 0 (background) set to
     $ Blue',ierr)

      call NhlFSetValues(text,rlist,ierr)

C Draw and frame the labelbar.
      call NhlFDraw(lbar,ierr)
      call NhlFDraw(text,ierr)
      call NhlFFrame(wks,ierr)

C This example demonstrates how to create and assign a new colormap.
C
C First we will initialize a new array with RGB entries.  For this
C example we are choosing an arbitrary colormap size of 100.
C 
C Assign new RGB values to each entry of the colormap.
C The first entry (background color) is black.  The rest of
C the colormap is a smooth table that ranges from red to blue.


      newcmap(1,1) = 0.
      newcmap(2,1) = 0.
      newcmap(3,1) = 0.
      do 10, i=2,100
         newcmap(1,i) = 1.-(i/100.)
         newcmap(2,i) = i/100.
         newcmap(3,i) = i/100.
 10   continue

C Assign the new color map to the workstation object.
      call NhlFRLClear(rlist)
      num_dims=2
      len_dims(1)=3
      len_dims(2)=100
      call NhlFRLSetMDFloatArray(rlist,'wkColorMap',newcmap,
     1      num_dims,len_dims,ierr)
      call NhlFSetValues(wks,rlist,ierr)

C Assign a new title.
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'lbTitleString','New colormap',ierr)

C Label every 10th entry
      call NhlFRLSetInteger(rlist,'lbLabelStride',10,ierr)

C Number of entries to display
      call NhlFRLSetInteger(rlist,'lbBoxCount',99,ierr)
      call NhlFSetValues(lbar,rlist,ierr)

C Draw and frame the labelbar
      call NhlFDraw(lbar,ierr)
      call NhlFFrame(wks,ierr)

C Cleanup
      call NhlFDestroy(lbar,ierr)
      call NhlFDestroy(text,ierr)
      call NhlFDestroy(wks,ierr)
      call NhlFClose

      stop
      end
