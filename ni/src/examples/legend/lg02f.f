CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lg02c.c
C
C      Author:         Ed Stautler
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates a Legend of 5 markers.
C
      external NhlFAppClass
      external NhlFLegendClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr

      character*8 labels(5)
      integer colors(5)
      integer item_ind(5)
      real mkthik(5)
  
C
C Initialize data values
C
      data labels / 'Marker_0', 'Marker_1', 'Marker_2', 
     1     'Marker_3', 'Marker_4' /

      data colors / 3, 5, 7, 9, 11 /

      data mkthik / 2.0, 3.0, 4.0, 5.0, 6.0 /

      data item_ind / 2, 3, 4, 5, 6 /

      integer NCGM, X11, PS
C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context.  By default the application looks
C for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'lg02',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lg02f.ncgm',ierr)
         call NhlFCreate(wid,'lg02Work',
     1       NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (X11.eq.1) then
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'lg02Work',NhlFXWorkstationClass,0,
     1        rlist,ierr)
      else if (PS.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./lg02f.ps',ierr)
         call NhlFCreate(wid,'lg02Work',
     1       NhlFPSWorkstationClass,0,rlist,ierr) 
      endif
C
C Specify the viewport extent of the object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',0.,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',1.,ierr)
C
C Specify the type of markers for the legend.
C
      call NhlFRLSetinteger(rlist,'lgItemCount',5,ierr)
      call NhlFRLSetfloat(rlist,'lgLabelFontHeightF',.03,ierr)
      call NhlFRLSetstringarray(rlist,'lgLabelStrings',labels,
     1      5,ierr)
      call NhlFRLSetinteger(rlist,'lgMonoItemType',1,ierr)
      call NhlFRLSetinteger(rlist,'lgItemType',1,ierr)
      call NhlFRLSetintegerarray(rlist,'lgMarkerColors',colors,
     1      5,ierr)
      call NhlFRLSetintegerarray(rlist,'lgMarkerIndexes',item_ind,
     1      5,ierr)
      call NhlFRLSetinteger(rlist,'lgMonoMarkerThickness',0,ierr)
      call NhlFRLSetfloatarray(rlist,'lgMarkerThicknesses',mkthik,
     1      5,ierr)
      call NhlFRLSetfloat(rlist,'lgMarkerSizeF',.05,ierr)
      call NhlFCreate(pid,'Legend',NhlFLegendClass,wid,rlist,
     1      ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
