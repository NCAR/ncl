CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lg03f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates a Legend of 5 markers.
C
      external NhlFAppClass
      external NhlFLegendClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr

      character*11 labels(5)
      integer colors(5)
      integer item_ind(5)
      real lnthik(5)
  
C
C Initialize data values
C
      data labels / 'Line_Type_0', 'Line_Type_1', 'Line_Type_2', 
     1      'Line_Type_3', 'Line_Type_4' /
      
      data colors / 2, 4, 6, 8, 10 /

      data lnthik / 4.0, 4.0, 4.0, 4.0, 4.0 /

      data item_ind / 2, 3, 4, 5, 6 /
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
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'lg03',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lg03f.ncgm',ierr)
         call NhlFCreate(wid,'lg03Work',
     1       NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'lg03Work',
     +        NhlFCairoWindowWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./lg03f.ps',ierr)
         call NhlFCreate(wid,'lg03Work',
     1       NhlFPSWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./lg03f.pdf',ierr)
         call NhlFCreate(wid,'lg03Work',
     1       NhlFPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     1         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lg03f',ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lg03Work',
     1       NhlFCairoPSPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lg03f',ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lg03Work',
     1       NhlFCairoImageWorkstationClass,0,rlist,ierr) 
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
C Specify the line types for the legend.
C
      call NhlFRLSetinteger(rlist,'lgItemCount',5,ierr)
      call NhlFRLSetfloat(rlist,'lgLabelFontHeightF',.03,ierr)
      call NhlFRLSetstringarray(rlist,'lgLabelStrings',labels,
     1      5,ierr)
      call NhlFRLSetinteger(rlist,'lgMonoItemType',1,ierr)
      call NhlFRLSetinteger(rlist,'lgItemType',0,ierr)
C
C Set the dashed lines and the line characters to the same colors.
C
      call NhlFRLSetintegerarray(rlist,'lgLineColors',colors,
     1      5,ierr)
      call NhlFRLSetintegerarray(rlist,'lgLineLabelFontColors',
     1      colors,5,ierr)
      call NhlFRLSetintegerarray(rlist,'lgDashIndexes',item_ind,
     1      5,ierr)
      call NhlFRLSetstring(rlist,'lgMonoLineThickness','False',ierr)
      call NhlFRLSetfloatarray(rlist,'lgLineThicknesses',lnthik,
     1      5,ierr)
      call NhlFRLSetfloat(rlist,'lgLineLabelFontHeightF',.03,ierr)
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
