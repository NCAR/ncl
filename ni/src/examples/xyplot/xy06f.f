C
C      $Id: xy06f.f,v 1.14 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy06f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri Mar 10 15:08:06 MST 1995
C
C Description:    This example shows how to do several things:
C
C                 1) How to create an XyPlot object with multiple
C                    lines using the CoordArrays and Data objects.
C                 2) How to change the data to create a different
C                    plot, without having to create a new data object
C                    (using the NhlFSetValues call).
C                 3) How to overlay a Legend object and to tweak
C                    several kinds of resources (see "xy06.res").
C                 4) How to use netCDF interface routines to open
C                    and access a netCDF file.
C                 5) How to use a kludgy method for using the resource
C                    file to specify which stations you want to observe.
C                
C                 This example requires that you have the netCDF
C                 library built on your system!  It's available via
C                 anonymous ftp to unidata.ucar.edu.
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
C
C Define the maximum number of weather stations and hours in a day.
C The actual stations you want to get data values for should be
C defined in the "xy06.res" resource file using the
C "xyExplicitLegendLabels" resource.
C
C You cannot request more than NSTATIONS station ids in the resource
C file.
C
      parameter(NSTATIONS=8,NHOURS=24,NCOLORS=10)
C
C Declare array that will hold station ids.
C
      character*20 station_abrev(NSTATIONS)
C
C Declare variables for the HLU routine calls.
C
      integer appid,xworkid,plotid,dataid
      integer grlist, rlist, i, j
      integer dspec(1), num_dspec
      data num_dspec/1/
      integer length(2), flen
      data length/NHOURS,NSTATIONS/
      real special_value
      data special_value/-9999./
C
C Declare variables for getting information from netCDF file.
C
      character*100 error_msg
      character*50 dir, recname, idname
      character*256 filename
      character*12 station_name
      integer ncid, stid, stdmid, tempid, presid, windid, recid
      integer ndims, nvars, ngatts, nhour
      integer num_values, have_value(NSTATIONS)
      integer stid_len, rec_len, numids
      data numids/NSTATIONS/
      integer ststart(2), stcount(2), idx(1)
C
C Declare 2-d arrays to hold data values.
C 
      real temp(NHOURS,NSTATIONS)
      real pressure(NHOURS,NSTATIONS)
      real wind_speed(NHOURS,NSTATIONS)
C
C Modify the color map.  Color indices '1' and '2' are the background
C and foreground colors respectively.
C
      real cmap(3,NCOLORS)
      data cmap/0.00,0.00,0.00,
     +          1.00,1.00,1.00,
     +          0.00,0.00,1.00,
     +          0.00,1.00,0.00,
     +          0.00,1.00,0.75,
     +          0.50,0.50,0.63,
     +          1.00,0.00,0.00,
     +          0.75,0.38,0.25,
     +          0.75,0.00,0.75,
     +          1.00,1.00,0.00/

      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy06.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy06',NhlFAppClass,0,rlist,ierr)

      length(1) = 3
      length(2) = NCOLORS

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy06f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy06f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy06f.pdf',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFPDfWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy06f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFcairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy06f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(xworkid,'xy06Work',
     +        NhlFcairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C We need to initialize a non-constant dummy array for our Data
C object, otherwise we'll get error messages.
C
      do 10 j = 1,NHOURS
         temp(j,1) = real(j)
 10   continue
C
C Define a dummy Data object.  We do this so a DataSpec object is
C created automatically and then we can use a NhlFGetValues call to
C get the names of the stations we want data values for.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',temp,NHOURS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,0,
     +     rlist,ierr)
C
C The id for this dummy Data object will now become the resource
C value for "xyCoordData".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot1',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C
C Get the DataSpec object id.
C
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(grlist)
      call NhlFRLGetintegerarray(grlist,'xyCoordDataSpec',dspec,
     +     num_dspec,ierr)
      call NhlFGetValues(plotid,grlist,ierr)
C
C Get station id names that have been set in resource file.  This
C is a round-about way of doing things, but it makes it convenient to
C be able to specify the stations we want.  We used the
C 'xyExplicitLegendLabels' resource, because we also want to use the 
C station names to label the lines in the legend.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetstringarray(grlist,'xyExplicitLegendLabels',
     +     station_abrev,numids,ierr)
      call NhlFGetValues(dspec(1),grlist,ierr)
C
C Make sure we didn't request too many stations or none at all.
C
      if( numids .gt. NSTATIONS ) then
         write(error_msg,25)numids
 25      format('Number of requested stations ', i2,' is illegal.')
         call NhlFPError('FATAL','EUNKNOWN',error_msg)
         write(error_msg,26)NSTATIONS
 26      format('Can only request 1-',i2,' stations.')
         call NhlFPError('FATAL','EUNKNOWN',error_msg)
         stop
      endif
C
C Loop through all NHOURS hours.
C
      do 40 nhour=1,NHOURS
C
C Initialize the data arrays to our special value that we have set
C above.
C
         num_values = 0
         do 30 i = 1,numids
            have_value(i) = 0
            temp(nhour,i) = special_value
            pressure(nhour,i) = special_value
            wind_speed(nhour,i) = special_value
 30      continue
C
C Open the netCDF file for a particular hour.
C
         call gngpat(filename,"data",ierr)
         write(dir,31)nhour-1
 31      format('/cdf/950318',i2.2,'_sao.cdf' )
         flen = 20
         do 32 i=1,256
            if( filename(i:i).eq.char(0) ) then
               filename(i:i+flen)=dir
               goto 34
            endif
 32      continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 34      ncid = ncopn(filename,0,ierr)
C
C Get the station id dimension.
C
         stdmid = ncdid(ncid,'id_len',ierr)
         call ncdinq(ncid, stdmid, idname, stid_len, ierr )
C
C Get the record length and dimension name.
C
         call ncinq(ncid, ndims, nvars, ngatts, recid, ierr)
         call ncdinq(ncid, recid, recname, rec_len, ierr)
C
C Get the id of the station ids, temperature, pressure, and 
C wind speed variables.
C
         stid = ncvid(ncid,'id',ierr)
         tempid = ncvid(ncid,'T',ierr)
         presid = ncvid(ncid,'PSL',ierr)
         windid = ncvid(ncid,'SPD',ierr)
C
C Get data values for the stations we have selected.
C
         ststart(1) = 1
         stcount(2) = 1
         stcount(1) = stid_len
         do 37 i = 1, rec_len
            ststart(2) = i
            call ncvgtc(ncid,stid,ststart,stcount,station_name,
     +           12,ierr)
            do 35 j = 1, numids
C
C Check if this is one of the stations we've requested and that we
C don't already have a data value for this station.
C
               if( station_abrev(j).eq.station_name(1:3) .and.
     +              have_value(j).eq.0 .and.
     +              ichar(station_name(4:4)).eq.0) then
                  idx(1) = i
C
C Get the temperature value.
C
                  call ncvgt1(ncid,tempid,idx,temp(nhour,j),ierr)
C
C Get the pressure value.
C
                  call ncvgt1(ncid,presid,idx,pressure(nhour,j),ierr)
C
C Get the wind speed.
C
                  call ncvgt1(ncid,windid,idx,wind_speed(nhour,j),ierr)
C
C Keep track of how many values we've received.
C 
                  have_value(j) = 1
                  num_values = num_values + 1
                  goto 36
               endif
 35         continue
C
C Check if we have received data values for each station.  If
C so, then we don't need to loop through the rest of the stations.
C
 36         if( num_values .eq. numids ) goto 40
 37      continue
C
C Let the user know if there are some stations missing.  Special
C values will be used in this case.
C
         do j = 1,numids
            if(have_value(j).eq.0  ) then
               write(error_msg,38)station_abrev(j),nhour
 38            format('Station ',a3,' not in netCDF file for hour ',i2)
               call NhlFPError('WARNING',1000,error_msg)
               write(error_msg,39)special_value
 39            format('Will substitute missing value = ', f7.1)
               call NhlFPError('WARNING',1000,error_msg)
            endif
         end do
C
C Close the netCDF file.
C
         call ncclos(ncid,ierr)
 40   continue
C
C Define the Data object.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The data
C id from this object will become the value for the XyPlot resource
C "xyCoordData".
C
      length(1) = NHOURS
      length(2) = numids
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'caYArray',temp,2,length,ierr)
      call NhlFRLSetFloat(rlist,'caYMissingV',special_value,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,0,
     +     rlist,ierr)
C
C The id for this Data object is now the resource value for
C xyCoordData.  Tweak some XyPlot resources in the resource file
C ("xy06.res").
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot1',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C     
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Change the data in our Data object.  Notice we use NhlFSetValues
C instead of NhlFCreate, so our data object will have the same
C name as when we originally created it, "xyData".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'caYArray',pressure,2,length,
     +     ierr)
      call NhlFSetValues(dataid,rlist,ierr)
C
C Create another XyPlot object with this new Data object.  We have to
C create another object instead of just changing the current one,
C because we want to change some resource values to title our axes
C differently.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot2',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Change the data in our Data object.  Notice we use NhlFSetValues
C instead of NhlFCreate, so our data object will have the same
C name as when we originally created it, "xyData".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'caYArray',wind_speed,2,length,
     +     ierr)
      call NhlFSetValues(dataid,rlist,ierr)
C
C Create another XyPlot object with this new Data object.  We have to
C create another object instead of just changing the current one,
C because we want to change some resource values to title our axes
C differently.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot3',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlFDestroy destroys the given id and all of its children so
C destroying "appid will destroy "xworkid" which will also destroy
C "plotid".
C
      call NhlFRLDestroy(rlist)
      call NhlFRLDestroy(grlist)
      call NhlFDestroy(xworkid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

