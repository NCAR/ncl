C
C      $Id: cn13f.f,v 1.1 1995-11-28 17:35:08 haley Exp $
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
      external nhlfappclass
      external nhlfxworkstationclass
      external nhlfncgmworkstationclass
      external nhlfpsworkstationclass
      external nhlfscalarfieldclass
      external nhlfcontourplotclass
      external nhlfmapplotclass
      external nhlfmapplotclass

      parameter(NCLS=300,NC=NCLS*NCLS,DTOR=.017453292519943,NCOLORS=66)

      real x(NC), y(NC),rlat(NC), rlon(NC)
      real dval, oor
      integer icra(NCLS,NCLS), count(2)
      integer appid, workid, dataid, cnid, mpid
      integer srlist, i, j, l, status, ierr, ierrx, ierry
C
C Declare variables for defining color map.
C
      integer length(2)
      real   cmap(3,NCOLORS)
C
C Default is to display to an X11 window.
C
      NCGM=0
      X11=1
      PS=0
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

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn13f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFNcgmWorkstationClass,
     +        0,srlist,ierr) 
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFXWorkstationClass,
     +        0,srlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn13f.ps',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn13Work',NhlFPSWorkstationClass,
     +        0,srlist,ierr)
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
               icra(i,j) = 0
            else
               dval=.25*(1.+cos(DTOR*10.*rlat(l)))+
     +              .25*(1.+sin(DTOR*10.*rlon(l)))*cos(DTOR*rlat(l))
               icra(i,j) = max(2,min(NCOLORS-1,
     +              2+int(dval*real(NCOLORS-2))))
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
      call NhlFRLSetMDIntegerArray(srlist,'sfDataArray',icra,2,count,
     +     ierr)
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
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
