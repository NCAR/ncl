CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lb02f.f
C
C      Author:         Bob Lackman
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 mdt 1995
C
C      Description:    Demonstrates the LabelBar Object
C                      Creates color bars with every 5th index of the
C                      131 different colors in the default colormap.
C
      external NhlFLabelBarClass
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr
      integer colors(22)
      character*15 line_labels(22)

      data colors / 1,7,13,19,25,31,37,43,49,55,61,67,73,79,85,91,97,
     $     103,109,115,121,127/

      data line_labels / 'Color Index 1 ','Color Index 7 ',
     $     'Color Index 13 ','Color Index 19 ',
     $     'Color Index 25 ','Color Index 31 ',
     $     'Color Index 37 ','Color Index 43 ',
     $     'Color Index 49 ','Color Index 55 ',
     $     'Color Index 61 ','Color Index 67 ',
     $     'Color Index 73 ','Color Index 79 ',
     $     'Color Index 85 ','Color Index 91 ',
     $     'Color Index 97 ','Color Index 103',
     $     'Color Index 109','Color Index 115',
     $     'Color Index 121','Color Index 127'/

      integer NCGM
C
C Default is to display output to an X workstation
C
      NCGM=0
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. In this example the resource file supplies the
C plot title only.
C
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'lb02',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lb02f.ncgm',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else 
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'lb02Work',NhlFXWorkstationClass,
     $        0,rlist,ierr)
      endif
C
C Create a plot with 22 color indices (Every 5th one of the default
C workstation colormap.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetintegerarray(rlist,'lbFillColors',
     $     colors,22,ierr)
      call NhlFRLSetstringarray(rlist,'lbLabelStrings',
     $     line_labels,22,ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',1.,ierr)
      call NhlFCreate(pid,'LabelBar',NhlFLabelBarClass,
     $     wid,rlist,ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
