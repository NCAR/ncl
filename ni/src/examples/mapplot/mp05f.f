C     
C     $Id: mp05f.f,v 1.1 1995-10-16 22:28:09 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      File:            mp05f.f
C
C      Author:          Mary Haley
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Oct 16 16:15:09 MDT 1995
C
C   Description:  Draws each of the ten map projections, with and 
C                 without fills.
C
      parameter(NMAPS=10)
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFMapPlotClass
      external NhlFtextItemClass

      integer appid,wid,mapid(NMAPS),txid
      integer rlist,i
      integer NCGM, X11, PS
      character*20 mapstr
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
C Create an application object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp05',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp05f.ncgm',ierr)
         call NhlFCreate(wid,'mp05Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (X11.eq.1) then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp05Work',NhlFXWorkstationClass,0,
     1     rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp05f.ps',ierr)
         call NhlFCreate(wid,'mp05Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      endif
C
C Create a TextItem object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.2,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.95,ierr)
      call NhlFRLSetInteger(rlist,'txFont',26,ierr)
      call NhlFRLSetString(rlist,'txString',
     +     "Maximal-area projections of all types",ierr)
      call NhlFCreate(txid,'TextItem',NhlFtextItemClass,wid,rlist,ierr)
C
C Draw the default MapPlot object
C
      do 10 i=1,NMAPS
         write(mapstr,5)i-1
 5       format('map',i1)
         call NhlFRLClear(rlist)
         call NhlFCreate(mapid(i),mapstr,NhlFMapPlotClass,wid,rlist,
     +        ierr)
         call NhlFDraw(mapid(i),ierr)
 10   continue
      call NhlFDraw(txid,ierr)
      call NhlFFrame(wid,ierr)
C
C Draw each projection individually and fill the countries.
C
      do 20 i=1,NMAPS
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'mpEllipticalBoundary','True',ierr)
         call NhlFRLSetString(rlist,'mpFillOn','True',ierr)
         call NhlFRLSetString(rlist,'mpLabelsOn','False',ierr)
         call NhlFRLSetString(rlist,'mpGridMaskMode','MaskNotOcean',
     +        ierr)
         call NhlFRLSetFloat(rlist,'mpGridLineThicknessF',1.1,ierr)
         call NhlFRLSetFloat(rlist,'vpXF',0.1,ierr)
         call NhlFRLSetFloat(rlist,'vpYF',0.9,ierr)
         call NhlFRLSetFloat(rlist,'vpWidthF',0.8,ierr)
         call NhlFRLSetFloat(rlist,'vpHeightF',0.8,ierr)
         call NhlFSetValues(mapid(i),rlist,ierr)
         call NhlFDraw(mapid(i),ierr)
         call NhlFFrame(wid,ierr)
 20   continue
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end
