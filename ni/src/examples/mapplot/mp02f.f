C
C     $Id: mp02f.f,v 1.15 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                            Copyright (C)  1995                       C
C                 University Corporation for Atmospheric Research      C
C                            All Rights Reserved                       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:            mp02f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:08:49 MST 1995
C
C      Description:     Demonstrates individual control of MapPlot areas
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFMapPlotClass
      integer appid,wid,mapid
      integer rlist
C
C String arrays for specifying areas
C
      character*10 fill_specs(7)
      data fill_specs/'mexico','bolivia','brazil','nicaragua',
     1                     'cuba','haiti','canada'/

      character*11 outline_specs(6)
      data outline_specs/'argentina','paraguay','colombia',
     1                    'us-colorado','us-texas','us-kentucky'/

      character*11 mask_specs(7)
      data mask_specs/'us-colorado','us-texas','us-kentucky',
     1            'bolivia','paraguay','nicaragua','oceans'/
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
C working directory. The resource file sets most of the Contour
C resources that remain fixed throughout the life of the Contour
C object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp02',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp02f.ncgm',ierr)
         call NhlFCreate(wid,'mp02Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp02Work',
     +     NhlFCairoWindowWorkstationClass,0,
     1     rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp02f.ps',ierr)
         call NhlFCreate(wid,'mp02Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./mp02f.pdf',ierr)
         call NhlFCreate(wid,'mp02Work',NhlFPDFWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp02f',ierr)
         call NhlFCreate(wid,'mp02Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp02f',ierr)
         call NhlFCreate(wid,'mp02Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create a plot focusing on North and South America
C Outlines are on by default turn fill on.
C By default the geophysical boundary set is used both for outline and
C fill.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetstring(rlist,'tiMainString','mp02f - Frame 1',
     1    ierr)
      call NhlFRLSetstring(rlist,'mpFillOn','TRUE',ierr)
      call NhlFRLSetstring(rlist,'mpProjection','orthographic',ierr)
      call NhlFRLSetString(rlist,'mpPerimOn','true',ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLatF',10.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLonF',-90.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterRotF',45.0,ierr)
      call NhlFRLSetstring(rlist,'mpLimitMode','latlon',ierr)
      call NhlFRLSetfloat(rlist,'mpMinLatF',-60.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLatF',60.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMinLonF',-135.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLonF',-45.0,ierr)
C
C Highlight selected countries using their "political" color.
C
      call NhlFRLSetstringarray(rlist,'mpFillAreaSpecifiers',
     1      fill_specs,7,ierr)
      
      call NhlFCreate(mapid,'Map0',NhlFMapPlotClass,wid,rlist,ierr)
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Individually outline some other countries and some US states.
C
      call NhlFRLClear(rlist)  
      call NhlFRLSetstring(rlist,'tiMainString','mp02f - Frame 2',ierr)
      call NhlFRLSetstringarray(rlist,'mpOutlineSpecifiers',
     1    outline_specs,6,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Turn off the base geophysical set for outlines and fill, leaving only
C the specified areas.
C Also change the specification, 'canada' to 'canada*', 
C in order to draw all areas belonging to Canada.
C Note that another color, mpDefaultFillColor, is used for all areas
C within the map projection that are otherwise not drawn, including the
C oceans. If you look closely, you will see that the Canadian lakes 
C are not drawn in the color used in the previous frame for the ocean.
C The wild card specification, 'canada*', picks up all the lakes of
C Canada Lakes are drawn using mpInlandWaterFillColor, which is, by
C default, set to the same color as mpOceanFillColor.
C
      fill_specs(5) = 'canada*'
      call NhlFRLClear(rlist)  
      call NhlFRLSetstring(rlist,'tiMainString','mp02f - Frame 3',ierr)
      call NhlFRLSetstring(rlist,'mpFillBoundarySets','noBoundaries',
     *   ierr)
      call NhlFRLSetstring(rlist,'mpOutlineBoundarySets','noBoundaries',
     1     ierr)
      call NhlFRLSetstringarray(rlist,'mpFillAreaSpecifiers',
     1     fill_specs,7,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C You can also specify area groupings using certain predefined 
C string constants: set 'continents' on to demonstrate.
C Masking an area is different from not explicitly drawing it. In order
C to mask a region you must explicitly include it on in the Mask
C specification list. There is an order of precedence for fill and
C masking. Explicitly named areas take precedence over area groupings, 
C and small areas take precedence over enclosing larger areas.
C Otherwise masking takes precedence over filling.
C >>> Masking or filling individual US states causes processing time
C >>> and memory requirements to increase substantially. Hopefully the 
C >>> performance can be improved before the release.
C
      fill_specs(1) = 'continents'
      fill_specs(2) = 'us'
      call NhlFRLClear(rlist)  
      call NhlFRLSetstring(rlist,'tiMainString','mp02f - Frame 4',ierr)
      call NhlFRLSetstring(rlist,'mpFillBoundarySets','NoBoundaries',
     1    ierr)
      call NhlFRLSetstringarray(rlist,'mpFillAreaSpecifiers',
     1     fill_specs,7,ierr)
      call NhlFRLSetString(rlist,'mpAreaMaskingOn','True',ierr)
      call NhlFRLSetstringarray(rlist,'mpMaskAreaSpecifiers',
     1        mask_specs,7,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(mapid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop 
      end
