C
C      $Id: cn13f.f,v 1.8 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C  File:       cn13f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Nov 27 10:23:59 MST 1995
C
C  Description:  This example emulates LLU example "mpex10".  It shows
C                how to do inverse map tranformations and raster contour
C                plots.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass
      external NhlFMapPlotClass

      parameter(NCLS=300,NC=NCLS*NCLS,DTOR=.017453292519943,NCOLORS=66)

      real x(NC), y(NC),rlat(NC), rlon(NC)
      real icra(NCLS,NCLS), dval, oor, miss_val
      data miss_val/1.e12/
      integer count(2)
      integer appid, workid, dataid, cnid, mpid
      integer srlist, i, j, l, status, ierr, ierrx, ierry
C
C Declare variables for defining color map.
C
      integer length(2)
      real   cmap(3,NCOLORS)
      character*7  wks_type
C
C Default is to display to an X11 window.
C
      wks_type = "x11"

C     
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'SETRL')
C
C Create an application object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn13',NhlFAppClass,0,srlist,ierr)
C
C Modify the color map. Colors for contour fill areas varying from
C blue to red.
C
      cmap( 1,1) = 0.00
      cmap( 2,1) = 0.00
      cmap( 3,1) = 0.00
      cmap( 1,2) = 1.00
      cmap( 2,2) = 1.00
      cmap( 3,2) = 1.00
      do 10 i = 2,NCOLORS-1
         cmap(1,i+1) = real(i-2)/real(NCOLORS-3)
         cmap(2,i+1) = 0.
         cmap(3,i+1) = real((NCOLORS-2)-(i-1))/real(NCOLORS-3)
 10   continue
                                   
      length(1) = 3
      length(2) = NCOLORS

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn13f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFNcgmWorkstationClass,
     +        0,srlist,ierr) 
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn13f.ps',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFPSWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPDFFileName','./cn13f.pdf',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFPDFWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn13f',ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn13f',ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Create a MapPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'mpProjection','Orthographic',ierr)
      call NhlFRLSetFloat(srlist,'mpCenterLatF',40.,ierr)
      call NhlFRLSetFloat(srlist,'mpCenterLonF',-105.,ierr)
      call NhlFRLSetFloat(srlist,'mpCenterRotF',0.,ierr)
      call NhlFCreate(mpid,'MapPlot',nhlfmapplotclass,workid,srlist,
     +     ierr)
C
C Calculate nice range of x,y values, and then get their
C corresponding lon,lat values.
C
      l = 1
      do 20 i=1,NCLS
         do 15 j=1,NCLS
            x(l) = .05+.90*(real(i)+.5)/real(NCLS)
            y(l) = .05+.90*(real(j)+.5)/real(NCLS)
            l = l+1
 15      continue
 20   continue
      call NhlFNDCToData(mpid,x,y,NC,rlon,rlat,0.,0.,ierrx,ierry,
     +     status,oor,ierr)
C
C Now create a cell array.
C
      l = 1
      do 30 i=1,NCLS
         do 25 j=1,NCLS
            if (rlat(l) .eq. oor) then
               icra(i,j) = miss_val
            else
               dval=.25*(1.+cos(DTOR*10.*rlat(l)))+
     +              .25*(1.+sin(DTOR*10.*rlon(l)))*cos(DTOR*rlat(l))
               icra(i,j) = 2.+dval*real(NCOLORS-2)
               if( icra(i,j).ne.miss_val) then
                  icra(i,j) = min(real(NCOLORS-1),icra(i,j))
               endif
            endif
            l = l+1
 25      continue
 30   continue
C
C Create a ScalarField object.
C
      count(1) = NCLS
      count(2) = NCLS
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',icra,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',miss_val,ierr)
      call NhlFCreate(dataid,'DataItem',nhlfscalarfieldclass,appid,
     +     srlist,ierr)
C
C Create ContourPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',dataid,ierr)
      call NhlFCreate(cnid,'ContourPlot',nhlfcontourplotclass,workid,
     +     srlist,ierr)
C
C Draw MapPlot on ContourPlot.
C
      call NhlFDraw(cnid,ierr)
      call NhlFDraw(mpid,ierr)
      call NhlFFrame(workid,ierr)
C
C NhlDestroy destroys the given id and all of its children.
C
      call NhlFRLDestroy(srlist)
      call NhlFDestroy(workid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
