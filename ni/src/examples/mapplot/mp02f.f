C
C     $Id: mp02f.f,v 1.5 1995-03-01 18:36:13 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            mp02f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:08:49 MST 1995
C
C      Description:    	Demonstrates individual control of MapPlot areas
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfmapplotlayerclass
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
C
C Initialize the high level utility library
C
      call nhlfinitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. The resource file sets most of the Contour
C resources that remain fixed throughout the life of the Contour
C object.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'mp02',nhlfapplayerclass,0,rlist,ierr)
C
C Create an X workstation
C
      call nhlfcreate(wid,'mp02Work',nhlfxworkstationlayerclass,0,
     1     0,ierr)
C
C Create a plot focusing on North and South America
C Outlines are on by default turn fill on.
C By default the geophysical boundary set is used both for outline and
C fill.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'ovTitleDisplayMode','always',ierr)
      call nhlfrlsetstring(rlist,'tiMainString','mp02f - Frame 1',
     1    ierr)
      call nhlfrlsetstring(rlist,'mpFillOn','TRUE',ierr)
      call nhlfrlsetstring(rlist,'mpProjection','orthographic',ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLatF',10.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLonF',-90.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterRotF',45.0,ierr)
      call nhlfrlsetstring(rlist,'mpLimitMode','latlon',ierr)
      call nhlfrlsetfloat(rlist,'mpMinLatF',-60.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLatF',60.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMinLonF',-135.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLonF',-45.0,ierr)
C
C Highlight selected countries using their "political" color.
C
      call nhlfrlsetstringarray(rlist,'mpFillAreaSpecifiers',
     1      fill_specs,7,ierr)
      
      call nhlfcreate(mapid,'Map0',nhlfmapplotlayerclass,wid,rlist,ierr)
      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Individually outline some other countries and some US states.
C
      call nhlfrlclear(rlist)  
      call nhlfrlsetstring(rlist,'tiMainString','mp02f - Frame 2',ierr)
      call nhlfrlsetstringarray(rlist,'mpOutlineSpecifiers',
     1    outline_specs,6,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
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
      call nhlfrlclear(rlist)  
      call nhlfrlsetstring(rlist,'tiMainString','mp02f - Frame 3',ierr)
      call nhlfrlsetstring(rlist,'mpFillBoundarySets','noBoundaries',
     *   ierr)
      call nhlfrlsetstring(rlist,'mpOutlineBoundarySets','noBoundaries',
     1     ierr)
      call nhlfrlsetstringarray(rlist,'mpFillAreaSpecifiers',
     1     fill_specs,7,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
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
      call nhlfrlclear(rlist)  
      call nhlfrlsetstring(rlist,'tiMainString','mp02f - Frame 4',ierr)
      call nhlfrlsetstring(rlist,'mpFillBoundarySets','noBoundaries',
     1    ierr)
      call nhlfrlsetstringarray(rlist,'mpFillAreaSpecifiers',
     1     fill_specs,7,ierr)
      call nhlfrlsetstringarray(rlist,'mpMaskAreaSpecifiers',
     1        mask_specs,7,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call nhlfdestroy(mapid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose
      stop 
      end
