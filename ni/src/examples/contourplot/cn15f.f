C
C      $Id: cn15f.f,v 1.12 2010-03-16 21:00:45 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1997                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn15f.f
C
C   Author:     Bob Lackman
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C
C   Date:       Wed Jan 24, 1996
C
C   Description:    Combines a contour plot and an xy plot on a
C                   single frame.  Output goes to an X11 window,
C                   an NCGM, *and* a PostScript file.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFScalarFieldClass
      external NhlFTextItemClass
      external NhlFCoordArraysClass
      external NhlFContourPlotClass
      external NhlFMapPlotClass
      external NhlFXyPlotClass
      parameter(NLAT=91, NLON=181, NCOLORS=16)
C
C Declare variables for the HLU routine calls.
C
      integer appid, jan, cn, ice, mp, tx, srlist, zonal, xy_plot
C
C Declare variables for color map
C
      integer length(2)
      real cmap(3,NCOLORS)
C
C Declare variables for contour plot fills.
C
      integer linecolors(15),fillcolors(16)
      data linecolors/12,13,14,7,2,0,8,11,6,9,4,3,5,7,10/
      data fillcolors/0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/
C
C Declare variables for Tick Mark values.
C
      real xbvalues1(7),xbvalues2(11),ylvalues(7)
      character*4 xblabels1(7)
      character*2 xblabels2(11)
      character*3 yllabels(7)
      data xbvalues1/0,60,120,180,240,300,360/
      data xblabels1/'0','60E','120E','180','120W','60W','0'/
      data xbvalues2/0,3,6,9,12,15,18,21,24,27,30/
      data ylvalues/-90, -60,-30,0,30,60,90/
      data xblabels2/'0','3','6','9','12','15','18','21','24','27','30'/
      data yllabels/'90S','60S','30S','EQ','30N','60N','90N'/
C
C Declare variables to hold contour and xy plot data.
C
      real sstjan(NLON,NLAT), lat(NLAT), zoneave(NLAT)
      integer ocean1
      data zoneave/NLAT*1.e36/
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, stid
      integer start(3), count(3), lonlen, latlen
      character*256 filename
      character*50 recname
C
C Output to several workstations.
C
      integer ncgm1, x1, ps1, pdf1, ps2, pdf2, png1
C
C Generate a color map.
C
      cmap(1,1) = 1.00  ! white
      cmap(2,1) = 1.00
      cmap(3,1) = 1.00
      cmap(1,2) = 0.00  ! black
      cmap(2,2) = 0.00
      cmap(3,2) = 0.00
      cmap(1,3) = 0.00  ! blue
      cmap(2,3) = 0.00
      cmap(3,3) = 1.00
      cmap(1,4) = 0.20  ! sky blue
      cmap(2,4) = 0.56
      cmap(3,4) = 0.80
      cmap(1,5) = 0.00  ! cyan
      cmap(2,5) = 1.00
      cmap(3,5) = 1.00
      cmap(1,6) = 0.50  ! blue magenta
      cmap(2,6) = 0.00
      cmap(3,6) = 1.00
      cmap(1,7) = 0.00  ! green
      cmap(2,7) = 1.00
      cmap(3,7) = 0.00
      cmap(1,8) = 0.14  ! forest green
      cmap(2,8) = 0.56
      cmap(3,8) = 0.14
      cmap(1,9) = 1.00  ! yellow
      cmap(2,9) = 1.00
      cmap(3,9) = 0.00
      cmap(1,10) = 1.00  ! orange
      cmap(2,10) = 0.50
      cmap(3,10) = 0.00
      cmap(1,11) = 1.00 ! magenta
      cmap(2,11) = 0.00
      cmap(3,11) = 1.00
      cmap(1,12) = 1.00 ! red
      cmap(2,12) = 0.00
      cmap(3,12) = 0.00
      cmap(1,13) = 0.65 ! brown
      cmap(2,13) = 0.16
      cmap(3,13) = 0.16
      cmap(1,14) = 0.86 ! tan
      cmap(2,14) = 0.58
      cmap(3,14) = 0.44
      cmap(1,15) = 0.66 ! light gray
      cmap(2,15) = 0.66
      cmap(3,15) = 0.66
      cmap(1,16) = 0.40 ! dark gray
      cmap(2,16) = 0.40
      cmap(3,16) = 0.40
C
C Initialize the HLU library and set up resource template.
C A resource file is not used in this example, but if you did
C want one, it would be called "cn15.res".
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn15',NhlFAppClass,0,srlist,ierr)

C
C Create an NCGM workstation.
C
      length(1) = 3
      length(2) = NCOLORS

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkMetaName','./cn15f.ncgm',ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(ncgm1,'cn15Work',
     +     NhlFNcgmWorkstationClass,0,srlist,ierr)
C
C Create an XWorkstation object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPause','True',ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(x1,'cn15Work',
     +     NhlFCairoWindowWorkstationClass,
     +     0,srlist,ierr)
C
C Create an older-style PostScript workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPSFileName','./cn15f.ps',ierr)
      call NhlFRLSetString(srlist,'wkVisualType','color',ierr)
C
C Since the plots are beside each other, use landscape mode and the
C PostScript resources for positioning the plot on the paper.
C
      call NhlFRLSetString(srlist,'wkOrientation','landscape',ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerX',0,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerY',60,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperX',600,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperY',700,ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(ps1,'cn15Work',
     +     NhlFPSWorkstationClass,0,srlist,ierr)
C
C Create an older-style PDF workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPDFFileName','./cn15f.pdf',ierr)
      call NhlFRLSetString(srlist,'wkVisualType','color',ierr)
C
C Since the plots are beside each other, use landscape mode and the
C PDF resources for positioning the plot on the paper.
C
      call NhlFRLSetString(srlist,'wkOrientation','landscape',ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerX',0,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerY',60,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperX',600,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperY',700,ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(pdf1,'cn15Work',
     +     NhlFPDFWorkstationClass,0,srlist,ierr)
C
C Create a cairo PostScript workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkFileName','./cn15f.cairo',ierr)
      call NhlFRLSetString(srlist,'wkFormat','ps',ierr)
C
C Since the plots are beside each other, use landscape mode and the
C PostScript resources for positioning the plot on the paper.
C
      call NhlFRLSetString(srlist,'wkOrientation','landscape',ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerX',0,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerY',60,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperX',600,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperY',700,ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(ps2,'cn15Work',
     +     NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
C
C Create a cairo PDF workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkFileName','./cn15f.cairo',ierr)
      call NhlFRLSetString(srlist,'wkFormat','pdf',ierr)
C
C Since the plots are beside each other, use landscape mode and the
C PostScript resources for positioning the plot on the paper.
C
      call NhlFRLSetString(srlist,'wkOrientation','landscape',ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerX',0,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceLowerY',60,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperX',600,ierr)
      call NhlFRLSetInteger(srlist,'wkDeviceUpperY',700,ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(pdf2,'cn15Work',
     +     NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
C
C Create a cairo PNG workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkFileName','./cn15f.cairo',ierr)
      call NhlFRLSetString(srlist,'wkFormat','png',ierr)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFCreate(png1,'cn15Work',
     +     NhlFCairoImageWorkstationClass,0,srlist,ierr)
C
C Open and read NetCDF file.
C
      call gngpat(filename,'data',ierr)
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+21)='/cdf/sstdata_netcdf.nc'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Get the lat/lon dimensions.
C
      lat_id = ncdid(ncid,'latitude',ierr)
      lon_id = ncdid(ncid,'longitude',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)

      start(1) = 1
      lat_id = ncvid(ncid,'lat',ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)
C
C Read in sstdata netCDF file.
C
      stid = ncvid(ncid,'sst',ierr)
      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      call ncvgt(ncid,stid,start,count,sstjan,ierr)
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C Create an sst data object.
C
      count(1) = NLON
      count(2) = NLAT
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',sstjan,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',0.,ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',360.,ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(jan,'sf',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Create a ContourPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',jan,ierr)
      call NhlFRLSetFloat(srlist,'vpXF',.06,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.65,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.60,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.30,ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLowLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnMonoLineColor','False',ierr)
      call NhlFRLSetIntegerArray(srlist,'cnLineColors',linecolors,15,
     +     ierr)
      call NhlFRLSetString(srlist,'cnLineDrawOrder','predraw',ierr)
      call NhlFRLSetString(srlist,'cnFillDrawOrder','predraw',ierr)
      call NhlFRLSetString(srlist,'cnLabelDrawOrder','predraw',ierr)
      call NhlFRLSetInteger(srlist,'cnLineLabelInterval',2,ierr)
      call NhlFRLSetString(srlist,'cnLineLabelPlacementMode',
     +     'computed',ierr)
      call NhlFRLSetString(srlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmXBValues',xbvalues1,7,ierr)
      call NhlFRLSetStringArray(srlist,'tmXBLabels',xblabels1,7,ierr)
      call NhlFRLSetString(srlist,'tmYLMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmYLValues',ylvalues,7,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',yllabels,7,ierr)
      call NhlFRLSetString(srlist,'tmXTLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRLabelsOn','True',ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',.010,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',.010,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXTMajorLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmXTMajorOutwardLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',.006,ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXTMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
      call NhlFCreate(cn,'cn',NhlFcontourPlotClass,x1,
     +     srlist,ierr)
C
C Create another ContourPlot object.
C
      cellsize = .8/360.
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',jan,ierr)
      call NhlFRLSetFloat(srlist,'vpXF',.06,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.65,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.60,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.30,ierr)
      call NhlFRLSetString(srlist,'tmXBOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','False',ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',0.,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetString(srlist,'cnFillMode','RasterFill',ierr)
      call NhlFRLSetFloat(srlist,'cnRasterCellSizeF',cellsize,ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',-2.0,ierr)
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',fillcolors,16,
     +     ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLinesOn','False',ierr)
      call NhlFRLSetString(srlist,'cnMonoFillColor','False',ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFCreate(ice,'ice',NhlFcontourPlotClass,x1,
     +     srlist,ierr)
C
C Create a MapPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.06,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.65,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.60,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.30,ierr)
      call NhlFRLSetString(srlist,'mpFillOn','True',ierr)
      call NhlFRLSetString(srlist,'mpLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'mpDefaultFillColor','DarkSalmon',
     +     ierr)
      call NhlFRLSetString(srlist,'mpLandFillColor','DarkSalmon',ierr)
      call NhlFRLSetString(srlist,'mpOutlineOn','False',ierr)
      call NhlFRLSetString(srlist,'mpAreaMaskingOn','True',ierr)
      call NhlFRLSetString(srlist,'mpMaskAreaSpecifiers','Oceans',ierr)
      call NhlFRLSetInteger(srlist,'mpInlandWaterFillColor',2,ierr)
      call NhlFRLSetString(srlist,'mpGridAndLimbOn','False',ierr)
      call NhlFRLSetString(srlist,'mpLimitMode','latlon',ierr)
      call NhlFRLSetFloat(srlist,'mpMinLonF',0.,ierr)
      call NhlFRLSetFloat(srlist,'mpMaxLonF',360.,ierr)
      call NhlFRLSetFloat(srlist,'mpCenterLonF',180.,ierr)
      call NhlFCreate(mp,'mp',nhlfmapplotclass,x1,srlist,
     +     ierr)
C
C Create a TextItem object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'txPosXF',0.5,ierr)
      call NhlFRLSetFloat(srlist,'txPosYF',0.8,ierr)
      call NhlFRLSetString(srlist,'txJust','CENTERCENTER',ierr)
      call NhlFRLSetString(srlist,'txString',
     +     'January Climatological Surface Temperature',ierr)
      call NhlFRLSetFloat(srlist,'txFontHeightF',.030,ierr)
      call NhlFRLSetInteger(srlist,'txFont',25,ierr)
      call NhlFCreate(tx,'tx',nhlftextitemclass,x1,srlist,ierr)
C
C  Read the ocean(1)/land(2) mask ascii dataset created by Areas/Ezmap.
C
      call gngpat(filename,'data',ierr)
      do 20 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+20)='/asc/oceanland30e.asc'
            goto 25
         endif
 20   continue
 25   open(unit=10,file=filename,status='old',form='formatted')
      do nlt=1,NLAT
         ksst   = 0      ! knt the number of grid pts at ocean locations
         sstzon = 0.0    ! save and plt later as [lat vs zoneave]

         do nln=1,NLON    ! loop over all lon at a particular lat
            read(10,*)ocean1
            if (ocean1.eq.1) then
               ksst = ksst+1
               sstzon = sstzon + sstjan(nln,nlt)
            end if
         end do

         if (ksst.ne.0) then
            zoneave(nlt) = sstzon/ksst
         end if

      end do
C
C Create a coordarrays data object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caYArray',lat,NLAT,ierr)
      call NhlFRLSetFloatArray(srlist,'caXArray',zoneave,NLAT,ierr)
      call NhlFRLSetFloat(srlist,'caXMissingV',1.e36,ierr)
      call NhlFCreate(zonal,'zonal',nhlfcoordarraysclass,appid,srlist,
     +     ierr)
C
C Create XyPlot object and assign data to it.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF', .73,ierr)
      call NhlFRLSetFloat(srlist,'vpYF', .65,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF', .25,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.30,ierr)
      call NhlFRLSetInteger(srlist,'xyCoordData', zonal,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',90.,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',-90.,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',30.,ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',0.,ierr)
      call NhlFRLSetString(srlist,'tmXTLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','False',ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF', .010,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXTMajorLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmXTMajorOutwardLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorOutwardLengthF',0.,ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmXBValues',xbvalues2,11,ierr)
      call NhlFRLSetStringArray(srlist,'tmXBLabels',xblabels2,11,ierr)
      call NhlFCreate(xy_plot,'xy_plot',nhlfxyplotclass,x1,srlist,
     +     ierr)
C
C Draw all objects to X11 window.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(x1,ierr)
C
C Reassign the workstation to save an ncgm.
C
      call NhlFChangeWorkstation (ice,ncgm1,ierr)
      call NhlFChangeWorkstation (cn,ncgm1,ierr)
      call NhlFChangeWorkstation (mp,ncgm1,ierr)
      call NhlFChangeWorkstation (xy_plot,ncgm1,ierr)
      call NhlFChangeWorkstation (tx,ncgm1,ierr)
C
C Draw all objects to the NCGM.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(ncgm1,ierr)
C
C Reassign the workstation to save PS.
C
      call NhlFChangeWorkstation (ice,ps1,ierr)
      call NhlFChangeWorkstation (cn,ps1,ierr)
      call NhlFChangeWorkstation (mp,ps1,ierr)
      call NhlFChangeWorkstation (xy_plot,ps1,ierr)
      call NhlFChangeWorkstation (tx,ps1,ierr)
C
C Draw all objects to PostScript.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(ps1,ierr)

C
C Reassign the workstation to save PDF.
C
      call NhlFChangeWorkstation (ice,pdf1,ierr)
      call NhlFChangeWorkstation (cn,pdf1,ierr)
      call NhlFChangeWorkstation (mp,pdf1,ierr)
      call NhlFChangeWorkstation (xy_plot,pdf1,ierr)
      call NhlFChangeWorkstation (tx,pdf1,ierr)
C
C Draw all objects to PDF.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(pdf1,ierr)

C
C Reassign the workstation to save cairo PS.
C
      call NhlFChangeWorkstation (ice,ps2,ierr)
      call NhlFChangeWorkstation (cn,ps2,ierr)
      call NhlFChangeWorkstation (mp,ps2,ierr)
      call NhlFChangeWorkstation (xy_plot,ps2,ierr)
      call NhlFChangeWorkstation (tx,ps2,ierr)
C
C Draw all objects to cairo PostScript.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(ps2,ierr)

C
C Reassign the workstation to save cairo PDF.
C
      call NhlFChangeWorkstation (ice,pdf2,ierr)
      call NhlFChangeWorkstation (cn,pdf2,ierr)
      call NhlFChangeWorkstation (mp,pdf2,ierr)
      call NhlFChangeWorkstation (xy_plot,pdf2,ierr)
      call NhlFChangeWorkstation (tx,pdf2,ierr)
C
C Draw all objects to cairo PDF.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(pdf2,ierr)

C
C Reassign the workstation to save cairo PNG.
C
      call NhlFChangeWorkstation (ice,png1,ierr)
      call NhlFChangeWorkstation (cn,png1,ierr)
      call NhlFChangeWorkstation (mp,png1,ierr)
      call NhlFChangeWorkstation (xy_plot,png1,ierr)
      call NhlFChangeWorkstation (tx,png1,ierr)
C
C Draw all objects to cairo PNG.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFDraw(xy_plot,ierr)
      call NhlFDraw(tx,ierr)
      call NhlFFrame(png1,ierr)

C
C Cleanup
C
      call NhlFDestroy(ncgm1,ierr)
      call NhlFDestroy(x1,ierr)
      call NhlFDestroy(ps1,ierr)
      call NhlFDestroy(ps2,ierr)
      call NhlFDestroy(pdf1,ierr)
      call NhlFDestroy(pdf2,ierr)
      call NhlFDestroy(png1,ierr)
      call NhlFDestroy(appid,ierr)
      stop
      end

