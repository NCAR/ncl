C     
C     $Id: mp07f.f,v 1.1 2002-03-04 21:57:13 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  2002                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      File:            mp07f.f
C
C      Author:          Mary Haley
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar  4 14:08:54 MST 2002
C
C   Description: Shows how to draw high-resolution coastlines using the
C   RANGS (Regionally Accessible Nested Global Shorelines), developed
C   by Rainer Feistel from Wessel and Smith's GSHHS (Global
C   Self-consistent Hierarchical High-resolution Shoreline) database.
C   To access the RANGS/GSHHS database, you must first download it from
C   Rainer Feistel's web site at
C   http://www.io-warnemuende.de/homepages/rfeistel/index.html.  Right
C   before the section entitled "Some WWW Links", you should see a
C   little table with ten *.zip files to download:
C
C     rangs(0).zip         gshhs(0).zip
C     rangs(1).zip         gshhs(1).zip
C     rangs(2).zip         gshhs(2).zip
C     rangs(3).zip         gshhs(3).zip
C     rangs(4).zip         gshhs(4).zip
C
C   You must download all ten of these files, unzip them, and either
C   put them in the default directory
C   "$NCARG_ROOT/lib/ncarg/database/rangs", or put them somewhere else
C   and set the environment variable NCARG_RANGS to this directory. The
C   files take up about 140 megabytes, unzipped. Once you have the
C   files in the appropriate location, then set the mpDataBaseVersion
C   resource to "RANGS_GSHHS" to create maps using this database. You
C   should not use this database to plot maximal area plots, because 1)
C   you will get horizontal lines through your plot, and 2) it takes a
C   long time.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFMapPlotClass

      integer appid,wks,mapid
      integer rlist
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
C Create an application object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp07',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp07f.ncgm',ierr)
         call NhlFCreate(wks,'mp07Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (X11.eq.1) then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFCreate(wks,'mp07Work',NhlFXWorkstationClass,0,
     1     rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp07f.ps',ierr)
         call NhlFCreate(wks,'mp07Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      endif

C
C Create and draw a map with the default databse.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpWidthF',  0.80,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF', 0.80,ierr)
      call NhlFRLSetFloat(rlist,'vpXF',      0.15,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',      0.90,ierr)
      call NhlFRLSetString(rlist,'mpProjection', 
     1     'CylindricalEquidistant',ierr)
C
C "LowRes" is the default database, also sometimes known as "Ncarg4_0".
C
      call NhlFRLSetString(rlist,'mpDataBaseVersion', 
     1     'LowRes',ierr)
C 
C Zoom in on part of the map.
C
      call NhlFRLSetString(rlist,'mpLimitMode','LatLon',ierr)
      call NhlFRLSetFloat(rlist,'mpMinLatF',   30.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLatF',   60.,ierr)
      call NhlFRLSetFloat(rlist,'mpMinLonF',  -15.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLonF',   15. ,ierr)
C
C Turn on the labeling of lat/lon lines.
C
      call NhlFRLSetString(rlist,'pmTickMarkDisplayMode', 
     1     'Always',ierr)
      call NhlFCreate(mapid,'mp07',NhlFMapPlotClass,wks,rlist,ierr)
C
C Draw map and advance the frame.
C
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wks,ierr)
C
C Set the resource indicating you want to use the high-resolution
C RANGS/GSHHS database.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'mpDataBaseVersion','RANGS_GSHHS',
     1     ierr)
      call NhlFSetValues(mapid,rlist,ierr)
C
C Draw map and advance the frame.
C
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wks,ierr)

      call NhlFDestroy(wks,ierr)
      call NhlFClose
      stop
      end
