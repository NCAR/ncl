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
C                      Creates color bars with every fifth index of the
C                      255 different colors in the default colormap.
C
      external NhlFLabelBarClass
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr
      integer colors(51)
      character*16 line_labels(51)

      data line_labels / 'Color Index 1 ',
     $                   'Color Index 6',
     $                   'Color Index 11',
     $                   'Color Index 16',
     $                   'Color Index 21',
     $                   'Color Index 26',
     $                   'Color Index 31',
     $                   'Color Index 36',
     $                   'Color Index 41',
     $                   'Color Index 46',
     $                   'Color Index 51',
     $                   'Color Index 56',
     $                   'Color Index 61',
     $                   'Color Index 66',
     $                   'Color Index 71',
     $                   'Color Index 76',
     $                   'Color Index 81',
     $                   'Color Index 86',
     $                   'Color Index 91',
     $                   'Color Index 96',
     $                   'Color Index 101',
     $                   'Color Index 106',
     $                   'Color Index 111',
     $                   'Color Index 116',
     $                   'Color Index 121',
     $                   'Color Index 126',
     $                   'Color Index 131',
     $                   'Color Index 136',
     $                   'Color Index 141',
     $                   'Color Index 146',
     $                   'Color Index 151',
     $                   'Color Index 156',
     $                   'Color Index 161',
     $                   'Color Index 166',
     $                   'Color Index 171',
     $                   'Color Index 176',
     $                   'Color Index 181',
     $                   'Color Index 186',
     $                   'Color Index 191',
     $                   'Color Index 196',
     $                   'Color Index 201',
     $                   'Color Index 206',
     $                   'Color Index 211',
     $                   'Color Index 216',
     $                   'Color Index 221',
     $                   'Color Index 226',
     $                   'Color Index 231',
     $                   'Color Index 236',
     $                   'Color Index 241',
     $                   'Color Index 246',
     $                   'Color Index 251'/


      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
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

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lb02f.ncgm',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'lb02Work',
     +        NhlFCairoWindowWorkstationClass,
     $        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./lb02f.ps',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFPSWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./lb02f.pdf',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lb02f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lb02f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFCairoImageWorkstationClass,0,rlist,ierr) 
      endif
C
C Initialize data values
C
      do i=1,51,1
         ii = (i-1)*5
         colors(i) = ii
      end do

C
C Create a plot with 51 color indices (Every other one of the default
C workstation colormap.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetinteger(rlist,'lbBoxCount',51,ierr)
      call NhlFRLSetintegerarray(rlist,'lbFillColors',
     $     colors,51,ierr)
      call NhlFRLSetstringarray(rlist,'lbLabelStrings',
     $     line_labels,51,ierr)
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
