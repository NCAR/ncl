C
C      $Id: cn17f.f,v 1.5 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1997                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C  File:       cn17f.f
C
C  Author:     Mary Haley
C              (original LLU example by Dave Kennison)
C              National Center for Atmospheric Research
C              PO Box 3000, Boulder, Colorado
C
C  Date:       Wed Apr 23 11:09:03 MST 1997
C
C  Description:  This example is somewhat similar to the LLU example
C                "cpex10", which draws contours bands within a circle
C                on a satellite map projection. It also shows how to
C                use the NhlDataPolyline and NhlDataPolymarker routines
C                to draw lines and markers on a map projection.
C                
C                This example uses the AnnoManager class to label each
C                of the United States with a two-letter mnemonic. The
C                labeling is not done in the map projection, however,
C                so the text is not part of the map.  To show how you
C                *can* get your text to be part of the map, this example
C                has a second frame which mixes the LLUs and the HLUs
C                to achieve this affect (since this *is* doable in the
C                LLUs).
C
      external nhlfappclass
      external nhlfxworkstationclass
      external nhlfncgmworkstationclass
      external nhlfpsworkstationclass
      external nhlfpdfworkstationclass
      external nhlfcairopspdfworkstationclass
      external nhlfcairoimageworkstationclass
      external nhlfcairowindowworkstationclass
      external nhlfscalarfieldclass
      external nhlftextitemclass
      external nhlfcontourplotclass
      external nhlfmapplotclass
      external nhlfannomanagerclass

      parameter(NCLS=100,NCOLORS=23,NDIM=50,NCIRC=100)
C
C Declare the common block in which the angle at which the label of a
C point on the globe is to be written and the latitude and longitude
C of the point being labelled are transmitted to the routine PCMPXY,
C in the package PLOTCHAR (this is for the LLU version of this plot).
C
      common /pcmp04/ pang,plat,plon
      save   /pcmp04/
C
C Define variables for HLU objects and other stuff.
C
      real x, y, zdat(NCLS, NCLS), xlat, xlon
      real xlonrng, xlatrng, xlonstp, xlatstp, dist
      real miss_val
      data miss_val/1.e12/
      integer count(2)
      integer appid, workid, dataid, cnid, mpid, gsid, gkswid
      integer srlist, grlist, i, j, ierr
      real dfce, rtod
      data dfce/1.3/
      data rtod / 57.2957795130823 /
C
C cminlon, cmaxlon, cminlat, and cmaxlat are the four corners where we
C want the contour plot to lie. ctrlat and ctrlon is the center of
C the circular contour plot (and also where we want to put a marker at
C the location of Boulder, CO). clat and clon will hold the lat/lon 
C coordinates for the circle we want to contain the contour plot in.
C
      real cminlon, cmaxlon, cminlat, cmaxlat, ctrlat, ctrlon
      data cminlon,cmaxlon,cminlat,cmaxlat/-115.,-95.,32.,48./
      data ctrlat/40./
      data ctrlon/-105./
      real clat(NCIRC),clon(NCIRC)
C
C Define arrays to hold a list of two-character mnemonics
C for the states, and the latitude and longitude of a point where the
C mnemonic may be placed to label the state.
C
      integer num_am_ids
      data num_am_ids/NDIM/
      integer am_ids(NDIM), text_ids(NDIM)
      data am_ids/50*-1/

      character*2 smne(NDIM)

      real slat(NDIM),slon(NDIM)
C
C Define the state-labelling data.
C
      data smne( 1),slat( 1), slon( 1)
     +                       / 'AL' , 33.0 ,  -86.5 /
      data smne( 2),slat( 2), slon( 2)
     +                       / 'AK' , 65.0 , -152.0 /
        data smne( 3),slat( 3), slon( 3)
     +                       / 'AZ' , 34.7 , -111.5 /
        data smne( 4),slat( 4), slon( 4)
     +                       / 'AR' , 35.0 ,  -92.5 /
        data smne( 5),slat( 5), slon( 5)
     +                       / 'CA' , 37.5 , -120.5 /
        data smne( 6),slat( 6), slon( 6)
     +                       / 'CO' , 39.0 , -105.8 /
        data smne( 7),slat( 7), slon( 7)
     +                       / 'CT' , 41.6 ,  -72.6 /
        data smne( 8),slat( 8), slon( 8)
     +                       / 'DE' , 39.0 ,  -75.5 /
        data smne( 9),slat( 9), slon( 9)
     +                       / 'FL' , 28.5 ,  -82.0 /
        data smne(10),slat(10), slon(10)
     +                       / 'GA' , 32.5 ,  -83.0 /
        data smne(11),slat(11), slon(11)
     +                       / 'HI' , 20.0 , -157.0 /
        data smne(12),slat(12), slon(12)
     +                       / 'ID' , 43.5 , -114.0 /
        data smne(13),slat(13), slon(13)
     +                       / 'IL' , 40.2 ,  -89.2 /
        data smne(14),slat(14), slon(14)
     +                       / 'IN' , 40.0 ,  -86.0 /
        data smne(15),slat(15), slon(15)
     +                       / 'IA' , 42.0 ,  -93.2 /
        data smne(16),slat(16), slon(16)
     +                       / 'KS' , 38.5 ,  -98.2 /
        data smne(17),slat(17), slon(17)
     +                       / 'KY' , 37.4 ,  -84.5 /
        data smne(18),slat(18), slon(18)
     +                       / 'LA' , 31.2 ,  -92.5 /
        data smne(19),slat(19), slon(19)
     +                       / 'ME' , 45.5 ,  -69.0 /
        data smne(20),slat(20), slon(20)
     +                       / 'MD' , 39.2 ,  -76.5 /
        data smne(21),slat(21), slon(21)
     +                       / 'MA' , 42.3 ,  -72.0 /
        data smne(22),slat(22), slon(22)
     +                       / 'MI' , 44.0 ,  -85.0 /
        data smne(23),slat(23), slon(23)
     +                       / 'MN' , 46.0 ,  -94.5 /
        data smne(24),slat(24), slon(24)
     +                       / 'MS' , 32.5 ,  -89.5 /
        data smne(25),slat(25), slon(25)
     +                       / 'MO' , 38.5 ,  -92.5 /
        data smne(26),slat(26), slon(26)
     +                       / 'MT' , 47.0 , -109.5 /
        data smne(27),slat(27), slon(27)
     +                       / 'NE' , 41.5 ,  -99.5 /
        data smne(28),slat(28), slon(28)
     +                       / 'NV' , 39.8 , -117.0 /
        data smne(29),slat(29), slon(29)
     +                       / 'NH' , 43.2 ,  -71.6 /
        data smne(30),slat(30), slon(30)
     +                       / 'NJ' , 39.7 ,  -74.5 /
        data smne(31),slat(31), slon(31)
     +                       / 'NM' , 34.7 , -106.0 /
        data smne(32),slat(32), slon(32)
     +                       / 'NY' , 43.0 ,  -75.0 /
        data smne(33),slat(33), slon(33)
     +                       / 'NC' , 35.5 ,  -79.5 /
        data smne(34),slat(34), slon(34)
     +                       / 'ND' , 47.5 , -100.5 /
        data smne(35),slat(35), slon(35)
     +                       / 'OH' , 40.2 ,  -82.5 /
        data smne(36),slat(36), slon(36)
     +                       / 'OK' , 35.6 ,  -97.5 /
        data smne(37),slat(37), slon(37)
     +                       / 'OR' , 44.0 , -120.2 /
        data smne(38),slat(38), slon(38)
     +                       / 'PA' , 40.8 ,  -77.6 /
        data smne(39),slat(39), slon(39)
     +                       / 'RI' , 41.7 ,  -71.5 /
        data smne(40),slat(40), slon(40)
     +                       / 'SC' , 34.0 ,  -80.5 /
        data smne(41),slat(41), slon(41)
     +                       / 'SD' , 44.5 , -100.5 /
        data smne(42),slat(42), slon(42)
     +                       / 'TN' , 36.0 ,  -86.5 /
        data smne(43),slat(43), slon(43)
     +                       / 'TX' , 32.0 , -100.0 /
        data smne(44),slat(44), slon(44)
     +                       / 'UT' , 39.5 , -111.5 /
        data smne(45),slat(45), slon(45)
     +                       / 'VT' , 44.2 ,  -72.5 /
        data smne(46),slat(46), slon(46)
     +                       / 'VA' , 37.6 ,  -78.6 /
        data smne(47),slat(47), slon(47)
     +                       / 'WA' , 47.5 , -120.5 /
        data smne(48),slat(48), slon(48)
     +                       / 'WV' , 38.5 ,  -80.8 /
        data smne(49),slat(49), slon(49)
     +                       / 'WI' , 44.5 ,  -89.5 /
        data smne(50),slat(50), slon(50)
     +                       / 'WY' , 43.0 , -107.5 /
c
c Declare variables for defining color map.
c
      integer length(2)
      real   cmap(3,NCOLORS)
      character*7  wks_type
c
c Default is to display to an x11 window.
c
      wks_type = "x11"
C     
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')
C
C Create an application object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn17',NhlFAppClass,0,srlist,ierr)
C
C Modify the color map. Colors for contour fill areas varying from
C blue to red.
C
      cmap(1,1) = 0.00
      cmap(2,1) = 0.00
      cmap(3,1) = 0.00
      cmap(1,2) = 1.00
      cmap(2,2) = 1.00
      cmap(3,2) = 1.00
      cmap(1,3) = .6 
      cmap(2,3) = .6 
      cmap(3,3) = .6 
      cmap(1,4) = 0. 
      cmap(2,4) = 0. 
      cmap(3,4) = 0. 
      cmap(1,5) = 1.
      cmap(2,5) = 1.
      cmap(3,5) = 1.
      cmap(1,6) = .4
      cmap(2,6) = .4
      cmap(3,6) = .4
      cmap(1,7) = 1.
      cmap(2,7) = 1.
      cmap(3,7) = 0.
      do i=8,NCOLORS
         cmap(1,i) = real(NCOLORS-i)/15.
         cmap(2,i) = 0.
         cmap(3,i) = real(i-8)/15.
      end do
      length(1) = 3
      length(2) = NCOLORS

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn17f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',NhlFNcgmWorkstationClass,
     +        0,srlist,ierr) 
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn17f.ps',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',NhlFPSWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPDFFileName','./cn17f.pdf',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',NhlFPDFWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn17f',ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',
     +        NhlFcairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn17f',ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(workid,'cn17Work',
     +        NhlFcairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Create a "great" circle in lat/lon coordinates. We don't want to draw
C any contour lines outside of this circle.
C
      xlonrng = cmaxlon - cminlon
      xlatrng = cmaxlat - cminlat
      do i=1,NCIRC
         clon(i) = ctrlon + 7.*cos(((i-1)*6.28)/real(NCIRC-1))
         clat(i) = ctrlat + 7.*sin(((i-1)*6.28)/real(NCIRC-1))
      end do
C
C Generate some dummy data to contour later.
C
      xlonstp = xlonrng/(NCLS-1)
      xlatstp = xlatrng/(NCLS-1)
      do i=1,NCLS
         xlon = cminlon + (i-1) * xlonstp
         x=real(i-1)/real(NCLS-1)
         do j=1,NCLS
            xlat = cminlat + (j-1) * xlatstp
            dist = sqrt((ctrlat - xlat)**2 + (ctrlon - xlon)**2)
C
C If xlat/xlon falls outside of circle, then we don't
C want to contour this location.
C
            if(dist.le.7.0) then
               y=real(j-1)/real(NCLS-1)
               zdat(i,j) = x**2 + y**2 + x*y + sin(9.*x)*cos(9.*y)
            else
               zdat(i,j)=miss_val
            end if
         end do
      end do
C
C AnnoManager objects allow the PlotManager to manipulate any View
C class object as an annotation a uniform fashion. They allow
C the user to set the View object's size and location relative to
C the viewport of a Plot. They may be located relative to one
C of the viewport sides, or, as in this example, aligned with the 
C plot's data space (amTrackData is set True in the resource file).
C
C Create a TextItem for each place name to be included on the map.
C Collect the object ids into an array.
C
      do i = 1,NDIM
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'txString',smne(i),ierr)
         call NhlFCreate(text_ids(i),smne(i),NhlFtextitemClass,workid,
     +        srlist,ierr)
      end do
C
C Create a MapPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'mpSatelliteAngle1F',
     +     7.*rtod*asin(1./dfce)/8.,ierr)
      call NhlFRLSetIntegerArray(srlist,'pmAnnoViews',text_ids,NDIM,
     +     ierr)
      call NhlFCreate(mpid,'MapPlot',nhlfmapplotclass,workid,srlist,
     +     ierr)
C
C Retrieve the ids of the AnnoManager objects created by the PlotManager
C and then set their location in data coordinate space. The AnnoManager
C objects are arranged in the same order as the TextItems in the
C pmAnnoViews resource.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetintegerarray(grlist,'pmAnnoManagers',am_ids,
     +                           num_am_ids,ierr)
      call NhlFGetValues(mpid,grlist,ierr)

      do i=1,num_am_ids
         call NhlFRLClear(srlist)
         call NhlFRLSetfloat(srlist,'amDataXF',slon(i),ierr)
         call NhlFRLSetfloat(srlist,'amDataYF',slat(i),ierr)
         call NhlFSetValues(am_ids(i),srlist,ierr)
      end do
C
C Create a ScalarField object.
C
      count(1) = NCLS
      count(2) = NCLS
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',zdat,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',miss_val,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',cminlon,ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',  cmaxlon,ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',cminlat,ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',  cmaxlat,ierr)
      call NhlFCreate(dataid,'DataItem',nhlfscalarfieldclass,appid,
     +     srlist,ierr)
C
C Create ContourPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +'Satellite view of contour bands in a limited area (using HLUS)',
     +ierr)
      call NhlFCreate(cnid,'ContourPlot',nhlfcontourplotclass,workid,
     +     srlist,ierr)
C
C Overlay ContourPlot on MapPlot and draw.
C
      call NhlFAddOverlay(mpid,cnid,-1,ierr)
      call NhlFDraw(mpid,ierr)
C
C Retrieve the GraphicStyle object that was created for us when
C we created the Workstation object.  We can then draw polylines
C and polymarkers on our MapPlot on behalf of the GraphicStyle object.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkDefGraphicStyleId',gsid,ierr)
      call NhlFGetValues(workid,grlist,ierr)
C
C Draw circle around our contours.
C
      call NhlFDataPolyline(mpid,gsid,clon,clat,NCIRC,ierr)
C
C Draw a polymarker at the position of Boulder, Colorado (where
C NCAR is located).
C
      call NhlFDataPolymarker(mpid,gsid,ctrlon,ctrlat,1,ierr)
C
C Advance the frame.
C
      call NhlFFrame(workid,ierr)
C
C Now, let's show how we can mix LLUs and HLUs so we can use Plotchar
C to draw the text in the map projection (which we can't do in the
C HLUs yet). First we need to get the GKS workstation id so we can
C activate this workstation at the LLU level.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetinteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(workid,grlist,ierr)
C     
C Turn off our previous annotations since we are now annotating
C with LLU calls.
C  
      do i=1,NDIM
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'amOn','false',ierr)
         call NhlFSetValues(am_ids(i),srlist,ierr)
      end do
C
C Draw the map, circle, and polymarker.
C     
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'tiMainString',
     +'Satellite view of contour bands in a limited area (using HLUs/LLU
     +S)',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(mpid,ierr)
      call NhlFDataPolyline(mpid,gsid,clon,clat,NCIRC,ierr)
      call NhlFDataPolymarker(mpid,gsid,ctrlon,ctrlat,1,ierr)
C
C Here's where the LLU calls come in. We are using Plotchar to
C draw the text in our map projection.  First we need to activate
C the workstation with the GKS id we retrieved earlier.
C
      call gacwk(gkswid)
      call pcseti ('MAP',4)
      call pcsetr ('ORV',1.E12)
      pang=45.
C
      do i=1,NDIM
         plat=slat(i)
         plon=slon(i)
         call plchhq (0.,0.,smne(i),.5,0.,0.)
      end do
      call gdawk(gkswid)
C
C Advance the frame.
C
      call NhlFFrame(workid,ierr)
C
C NhlDestroy destroys the given id and all of its children.
C
      call NhlFDestroy(workid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
