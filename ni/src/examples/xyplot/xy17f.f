C
C      $Id: xy17f.f,v 1.9 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                Copyright (C)  1996                                  C
C        University Corporation for Atmospheric Research              C
C                All Rights Reserved                                  C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy17f.f
C
C  Author:     Bob Lackman
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       24 Jan 1996
C
C  Description:    Reads an ASCII file with 4 variables:
C                  lon, u, v, and t.  u, v, and t are plotted
C                  with 3 stacked y axes.
C

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass

      parameter (ncurve=3, npts=129) 

      CHARACTER*7  wks_type

      integer i, rlist, wks, appid, field1, field2, field3
      integer xy1, xy2, xy3, grlist
      integer num_dspec
      data num_dspec /1/

C
C  Create variables to contain data.
C

      real a,b,c,d
      real y (ncurve,npts), lon (npts), u (npts), v (npts), t (npts)
      real y1val (5), y2val (6), y3val(5)
      data y1val /-90.0 , -80.0, -70.0, -60.0, -50.0/
      data y2val /10.0, 20.0, 30.0, 40.0, 50.0, 60.0/
      data y3val /-20.0, -10.0, 0.0, 10.0, 20.0/
 
      character*5 y1lab(5), y2lab(6), y3lab(5)
      data y1lab /'-90.', '-80.', '-70.', '-60.', '-50.'/
      data y2lab /'10.', '20.', '30.', '40.', '50.', '60.'/
      data y3lab /'-20.', '-10.', '0.', '10.', '20.'/


C
C Define the workstation type
C
      wks_type = "x11"
C
C  Read ASCII file xy.asc
C
      open(unit=10,file='xy.asc',status='old',form='formatted',err=105)

C
C   xy.asc has 4 vars of length 129 longitudes, lon, u, v, t
C
C     The data is taken at 43N latitude.  Longitude is an index
C     1-129 standing for 0 deg - 360 deg in steps of 360/128?
C     u and v are in m/s, and t is in deg K.
C

      do 100 i=1,npts,1
         read (10,101,END=103,ERR=103) a,b,c,d
         lon(i)=a
         u(i)=b
         v(i)=c
         t(i)=d
  100 continue
  101 FORMAT (4X,F6.2,5X,F5.2,4X,F6.2,4X,F6.2)

  103 close (10)

      do 104 i=1,npts,1
         lon (i) = (lon(i)- 1.0) * 360.0/128.0
         t (i) =  (t(i) - 273.15) * 9 / 5 + 32.0
         y (1,i) = u(i)
         y (2,i) = v(i)
         y (3,i) = (t(i) - 273.15) * 9.0 / 5.0 + 32.0
 104  continue

      call NhlFInitialize
      call NhlFRLCreate (rlist,'setrl')

C
C  Create Application object.  The Application object name is used to
C  determine the name of the resource file, which is "xy17.res" in this
C  case. 
C
 
      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'appDefaultParent', 'True', ierr)
      call NhlFRLSetString (rlist, 'appUsrDir', './', ierr)
      call NhlFCreate (appid, 'xy17', NhlFAppClass, 0, rlist, ierr)

C
C  If NCGM=1, then open NCGM workstation. 
C

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkMetaName', 
     +                         'xy17f.ncgm', ierr)
         call NhlFCreate (wks, 'xy17Work', NhlFNcgmWorkstationClass, 0,
     +                    rlist, ierr)

      endif

C
C  Create an X workstation. 
C

      if (wks_type.eq."x11".or.wks_type.eq."X11") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPause', 'True', ierr)
         call NhlFCreate (wks, 'xy17Work', 
     +                    NhlFCairoWindowWorkstationClass,
     +                    0, rlist, ierr)
      endif

C
C  Open PS workstation. 
C
      if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPSFileName', 
     +                         'xy17f.ps', ierr)
         call NhlFCreate (wks, 'xy17Work', NhlFPSWorkstationClass, 
     +                    0, rlist, ierr)
      endif
C
C  Open PDF workstation. 
C
      if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPDFFileName', 
     +                         'xy17f.pdf', ierr)
         call NhlFCreate (wks, 'xy17Work', NhlFPDFWorkstationClass, 
     +                    0, rlist, ierr)
      endif
C
C  Open cairo PS/PDF workstation. 
C
      if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +    wks_type.eq."ps".or.wks_type.eq."PS") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkFileName', 
     +                         'xy17f', ierr)
         call NhlFRLSetString (rlist, 'wkFormat',wks_type,ierr) 
         call NhlFCreate (wks,'xy17Work',
     +                    NhlFCairoPSPDFWorkstationClass, 
     +                    0, rlist, ierr)
      endif
C
C  Open cairo PNG workstation. 
C
      if (wks_type.eq."png".or.wks_type.eq."PNG") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkFileName', 
     +                         'xy17f', ierr)
         call NhlFRLSetString (rlist, 'wkFormat',wks_type,ierr) 
         call NhlFCreate (wks,'xy17Work',
     +                    NhlFCairoImageWorkstationClass, 
     +                    0, rlist, ierr)
      endif

C
C  Create a coordarrays data object and configure its extents missing
C  values and at the same time convert it from Degrees K to Degrees F  
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloatArray (rlist, 'caXArray', lon, npts, ierr)
      call NhlFRLSetFloatArray (rlist, 'caYArray', t, npts, ierr)
      call NhlFCreate (field1, 'field1', NhlFcoordArraysClass, appid,
     +                 rlist, ierr)

C
C  Create a coordarrays data object and configure its extents missing
C  values and at the same time convert it from Degrees K to Degrees F
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloatArray (rlist, 'caXArray', lon, npts, ierr)
      call NhlFRLSetFloatArray (rlist, 'caYArray', u, npts, ierr)
      call NhlFCreate (field2, 'field2', NhlFcoordArraysClass, appid, 
     +                 rlist, ierr)

C
C  Create a coordarrays data object and configure its extents missing
C  values and at the same time convert it from Degrees K to Degrees F
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloatArray (rlist, 'caXArray', lon, npts, ierr)
      call NhlFRLSetFloatArray (rlist, 'caYArray', v, npts, ierr)
      call NhlFCreate (field3, 'field3', NhlFcoordArraysClass, appid,
     +                 rlist, ierr)

C
C  Create XyPlot object for curve 1 and assign data to it
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat   (rlist, 'vpXF', 0.20, ierr)
      call NhlFRLSetFloat   (rlist, 'vpYF', 0.80, ierr)
      call NhlFRLSetFloat   (rlist, 'vpWidthF', 0.6, ierr)
      call NhlFRLSetFloat   (rlist, 'vpHeightF', 0.2, ierr)
      call NhlFRLSetInteger (rlist, 'xyCoordData', field1, ierr)
      call NhlFRLSetString  (rlist, 'trYReverse', 'False', ierr)
      call NhlFRLSetFloat   (rlist, 'trYMaxF',  -50.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trYMinF',  -90.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trXMaxF',  360.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trXMinF',   0.0, ierr)
      call NhlFRLSetString  (rlist, 'tmYROn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYUseLeft', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmXMajorGrid', 'True', ierr)
      call NhlFRLSetString  (rlist, 'tmXBLabelsOn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYLLabelsOn', 'True', ierr)
      call NhlFRLSetFloat   (rlist, 'tmYLMajorLengthF', 0.01, ierr)
      call NhlFRLSetFloat   (rlist, 'tmYLMajorOutwardLengthF', 
     +                       0.0,ierr)
      call NhlFRLSetString  (rlist, 'tmYLMode', 'Explicit', ierr)
      call NhlFRLSetFloatArray  (rlist, 'tmYLValues', y1val, 5, ierr)
      call NhlFRLSetStringArray (rlist, 'tmYLLabels', y1lab, 5, ierr)
      call NhlFRLSetString  (rlist, 'tmYLLabelsOn', 'True', ierr)
      call NhlFRLSetString (rlist, 'tmYLLabelFontColor','red', ierr)
      call NhlFRLSetString  (rlist, 'tiMainString', 
     +                       'Temperature, U, V Stacked Plots', ierr)
      call NhlFRLSetString  (rlist, 'tiYAxisString', 
     +                       'Temp (Deg C)', ierr)
      call NhlFRLSetFloat   (rlist, 'tiXAxisFontHeightF', 0.02, ierr)
      call NhlFRLSetFloat   (rlist, 'tiYAxisFontHeightF', 0.02, ierr)
      call NhlFRLSetString  (rlist, 'tiXAxisFont', 
     +                       'helvetica-bold', ierr)
      call NhlFRLSetString  (rlist, 'tiYAxisFont', 
     +                       'helvetica-bold', ierr)
      call NhlFRLSetString (rlist, 'tiYAxisFontColor','red', ierr)
      call NhlFRLSetString  (rlist, 'tmYRMinorOn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYLMinorOn', 'False', ierr)
      call NhlFCreate(xy1, 'xy1', NhlFxyPlotClass, wks, rlist, ierr)

C
C  Create XyPlot object for curve 2 and assign data to it
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat   (rlist, 'vpXF', 0.20, ierr)
      call NhlFRLSetFloat   (rlist, 'vpYF', 0.60, ierr)
      call NhlFRLSetFloat   (rlist, 'vpWidthF', 0.6, ierr)
      call NhlFRLSetFloat   (rlist, 'vpHeightF', 0.2, ierr)
      call NhlFRLSetInteger (rlist, 'xyCoordData', field2, ierr)
      call NhlFRLSetString  (rlist, 'trYReverse', 'False', ierr)
      call NhlFRLSetFloat   (rlist, 'trYMaxF',   60.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trYMinF',   10.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trXMaxF',  360.0, ierr)
      call NhlFRLSetFloat   (rlist, 'trXMinF',   0.0, ierr)
      call NhlFRLSetString  (rlist, 'tmYROn', 'True', ierr)
      call NhlFRLSetString  (rlist, 'tmYLOn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYUseLeft', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmXMajorGrid', 'True', ierr)
      call NhlFRLSetString  (rlist, 'tmYLLabelsOn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYRLabelsOn', 'True', ierr)
      call NhlFRLSetString  (rlist, 'tmYRMode', 'Explicit', ierr)
      call NhlFRLSetFloatArray  (rlist, 'tmYRValues', y2val, 6, ierr)
      call NhlFRLSetStringArray (rlist, 'tmYRLabels', y2lab, 6, ierr)
      call NhlFRLSetString  (rlist, 'tmXBLabelsOn', 'False', ierr)
      call NhlFRLSetString (rlist, 'tmYRLabelFontColor','green', ierr)
      call NhlFRLSetString  (rlist, 'tiYAxisString', 'U (m/s)', ierr)
      call NhlFRLSetFloat   (rlist, 'tiXAxisFontHeightF', 0.02, ierr)
      call NhlFRLSetFloat   (rlist, 'tiYAxisFontHeightF', 0.02, ierr)
      call NhlFRLSetString  (rlist, 'tiXAxisFont',
     +                       'helvetica-bold', ierr)
      call NhlFRLSetString  (rlist, 'tiYAxisFont',
     +                       'helvetica-bold', ierr)
      call NhlFRLSetString (rlist, 'tiYAxisFontColor','green', ierr)
      call NhlFRLSetString  (rlist, 'tmYRMinorOn', 'False', ierr)
      call NhlFRLSetString  (rlist, 'tmYLMinorOn', 'False', ierr)
      call NhlFCreate(xy2, 'xy2', NhlFxyPlotClass, wks, rlist, ierr)

C
C  Create XyPlot object for curve 3 and assign data to it
C
C  Increase the veiwport so the right scale will be about .15 NDC
C  right of the other grids.  Plot only the right vertical axis.
C  .5NDC = 360 deg lon, thus .65NDC = 360+108 deg lon.
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat   (rlist,'vpXF', 0.20, ierr)
      call NhlFRLSetFloat   (rlist,'vpYF', 0.40, ierr)
      call NhlFRLSetFloat   (rlist,'vpWidthF', 0.6, ierr)
      call NhlFRLSetFloat   (rlist,'vpHeightF', 0.2, ierr)
      call NhlFRLSetInteger (rlist,'xyCoordData', field3, ierr)
      call NhlFRLSetString  (rlist,'trYReverse', 'False', ierr)
      call NhlFRLSetFloat   (rlist,'trYMaxF',   20.0, ierr)
      call NhlFRLSetFloat   (rlist,'trYMinF',  -20.0, ierr)
      call NhlFRLSetFloat   (rlist,'trXMaxF',  360.0, ierr)
      call NhlFRLSetFloat   (rlist,'trXMinF',   0.0, ierr)
      call NhlFRLSetString  (rlist,'tmYROn', 'False', ierr)
      call NhlFRLSetString  (rlist,'tmYUseLeft', 'False', ierr)
      call NhlFRLSetString  (rlist,'tmYLLabelsOn', 'True', ierr)
      call NhlFRLSetString  (rlist,'tmXBLabelsOn', 'True', ierr)
      call NhlFRLSetString  (rlist,'tmXMajorGrid', 'True', ierr)
      call NhlFRLSetFloat   (rlist,'tmYLMajorLengthF',0.01,ierr)
      call NhlFRLSetFloat   (rlist,'tmYLMajorOutwardLengthF', 
     +                       0.0, ierr)
      call NhlFRLSetString  (rlist,'tmYLMode', 'Explicit', ierr)
      call NhlFRLSetString  (rlist,'tmYLLabelsOn', 'True', ierr)
      call NhlFRLSetString (rlist,'tmYLLabelFontColor','blue', ierr)
      call NhlFRLSetString  (rlist,'tiYAxisString','V (m/s)',ierr)
      call NhlFRLSetString  (rlist,'tiXAxisString', 
     +                      'Longitude (Degs)', ierr)
      call NhlFRLSetFloat   (rlist,'tiXAxisFontHeightF',0.02,ierr)
      call NhlFRLSetFloat   (rlist,'tiYAxisFontHeightF',0.02,ierr)
      call NhlFRLSetString  (rlist,'tiXAxisFont','helvetica-bold',
     +                       ierr)
      call NhlFRLSetString  (rlist,'tiYAxisFont','helvetica-bold',
     +                       ierr)
      call NhlFRLSetString (rlist,'tiYAxisFontColor','blue', ierr)
      call NhlFRLSetString  (rlist,'tmYRMinorOn', 'False', ierr)
      call NhlFRLSetString  (rlist,'tmYLMinorOn', 'False', ierr)
      call NhlFRLSetFloatArray  (rlist,'tmYLValues',y3val,5,ierr)
      call NhlFRLSetStringArray (rlist,'tmYLLabels',y3lab,5,ierr)
      call NhlFCreate(xy3,'xy3',NhlFxyPlotClass,wks,rlist,ierr)



      call NhlFRLCreate (grlist,'getrl')
      call NhlFRLClear (grlist)
      call NhlFRLGetIntegerArray(grlist, 'xyCoordDataSpec', dspec,
     +                           num_dspec, ierr)
      call NhlFGetValues(xy1, grlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'xyMonoLineColor', 'True', ierr)
      call NhlFRLSetString (rlist, 'xyLineColor','red', ierr)
      call NhlFSetValues (dspec, rlist, ierr)

      call NhlFRLClear (grlist)
      call NhlFRLGetIntegerArray(grlist, 'xyCoordDataSpec', dspec,
     +                           num_dspec, ierr)
      call NhlFGetValues(xy2, grlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'xyMonoLineColor', 'True', ierr)
      call NhlFRLSetString (rlist, 'xyLineColor','green', ierr)
      call NhlFSetValues (dspec, rlist, ierr)

      call NhlFRLClear (grlist)
      call NhlFRLGetIntegerArray (grlist, 'xyCoordDataSpec', dspec,
     +                            num_dspec, ierr)
      call NhlFGetValues(xy3, grlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'xyMonoLineColor', 'True', ierr)
      call NhlFRLSetString (rlist, 'xyLineColor', 'blue', ierr)
      call NhlFSetValues (dspec, rlist, ierr)

      call NhlFDraw(xy1, ierr)
      call NhlFDraw(xy2, ierr)
      call NhlFDraw(xy3, ierr)
      call NhlFFrame(wks, ierr)

      call NhlFDestroy (wks, ierr)
      call NhlFClose

      stop
 105  write  (6,*) 'error in opening file xy.asc'
      end
