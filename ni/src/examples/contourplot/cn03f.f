C
C     $Id: cn03f.f,v 1.9 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn03f.f
C
C      Author:          Dave Brown (Converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:24 MST 1995
C
C      Description: Demonstrates basic features of the ContourPlot 
C                   object. The first frame emulates the contour plot 
C                   drawn in cn01c using low-level NCARG calls. 
C
C External functions
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external nhlfscalarfieldclass
      external nhlfcontourplotclass
      external nhlfloglinplotclass
C
C Data array
C     
      parameter(M=73, N=10)
      real t(M,N)
C
C The coordinates of the irregular Y dimension
C
      real level(10)
      data level/1000.,850.,700.,500.,400.,300.,250.,200.,150.,100./
C
C Explicit labels and label locations for the X Axis tickmarks
C
      character*10 labels(7)
      data labels/'90~S~o~N~S', '60~S~o~N~S', '30~S~o~N~S', 'EQ', 
     1      '30~S~o~N~N', '60~S~o~N~N', '90~S~o~N~N'/

      real labellocs(7)
      data labellocs /-90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0/
C
C The data dimensions
C
      integer appid,wid,cnid,dataid,llid
      integer rlist, grlist
      integer len_dims(2)
      real xvp,yvp,heightvp,widthvp
      character*7  wks_type
C
C Data file name
C
      character*12 filenm
      filenm = 'cn03f.asc'
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
C
C Read data
C     
      open(unit=10,file=filenm,status='old',form='formatted',err=104)
      do 10 j=1,N
         read (10,1001)(t(i,j),i=1,M)
 10   continue
 1001 format(1x,15F13.8)
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for the resource file the
C directory it executes from. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn03',NhlFAppClass,0,rlist,ierr)
      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./cn03f.ncgm',ierr)
         call NhlFCreate(wid,'cn03Work',NhlFNcgmWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn03Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./cn03f.ps',ierr)
         call NhlFCreate(wid,'cn03Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./cn03f.pdf',
     1        ierr)
         call NhlFCreate(wid,'cn03Work',NhlFPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn03f',
     1        ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn03Work',NhlFCairoPSPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn03f',
     1        ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn03Work',NhlFCairoImageWorkstationClass,
     1        0,rlist,ierr)
      endif
C
C Create a scalar field data object with a linear X dimension
C representing latitude and an irregular Y dimension representing
C geopotential height. 
C Define the start and end points of the data, based on the dataset.
C
      call NhlFRLClear(rlist)
      len_dims(1) = M
      len_dims(2) = N
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',t,2,len_dims,ierr)
      call NhlFRLSetfloatarray(rlist,'sfYArray',level,10,ierr)
      call NhlFRLSetfloat(rlist,'sfXCStartV',-90.0,ierr)
      call NhlFRLSetfloat(rlist,'sfXCEndV',90.0,ierr)
      call NhlFRLSetfloat(rlist,'sfYCStartV',1000.0,ierr)
      call NhlFRLSetfloat(rlist,'sfYCEndV',100.0,ierr)
      call NhlFCreate(dataid,'mydata',nhlfscalarfieldclass,0,rlist,
     1      ierr)
C
C Create a ContourPlot object. Since ContourPlot contains a TickMark 
C object by default, the non-default TickMark resources can be set 
C in the ContourPlot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString',
     1      'Profile @ 105~S~o~N~W - Frame 1',ierr)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.125,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',0.85,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',0.6,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',0.6,ierr)
      call NhlFRLSetfloat(rlist,'cnLevelSpacingF',5.0,ierr)
      call NhlFRLSetstring(rlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetstring(rlist,'tmXBMinorOn','FALSE',ierr)
      call NhlFRLSetfloatarray(rlist,'tmXBValues',labellocs,7,ierr)
      call NhlFRLSetstringarray(rlist,'tmXBLabels',labels,7,ierr)
      call NhlFCreate(cnid,'ContourPlot1',nhlfcontourplotclass,
     1      wid,rlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Color and add dash patterns to the lines, then display a legend
C listing the line types. The position of the Legend is controlled by
C resources set in the resource file. Thicken lines.
C Note that the Legend and LabelBar are provided to the ContourPlot 
C object by its PlotManager (created by default when the ContourPlot 
C object is initialized). Therefore the resources to control them have
C  the prefix 'pm' rather than 'cn'. 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString',
     1      'Profile @ 105~S~o~N~W - Frame 2',ierr)
      call NhlFRLSetstring(rlist,'cnMonoLineColor','FALSE',ierr)
      call NhlFRLSetstring(rlist,'cnMonoLineDashPattern','FALSE',ierr)
      call NhlFRLSetstring(rlist,'pmLegendDisplayMode','always',ierr)
      call NhlFSetValues(cnid,rlist,ierr)

      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Turn lines off, and use solid color fill instead.
C Remove the Legend and display a LabelBar.
C Turn off line and high/low labels.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString',
     1      'Profile @ 105~S~o~N~W - Frame 3',ierr)
      call NhlFRLSetstring(rlist,'cnLinesOn','FALSE',ierr)
      call NhlFRLSetstring(rlist,'cnFillOn','TRUE',ierr)
      call NhlFRLSetstring(rlist,'pmLegendDisplayMode','never',ierr)
      call NhlFRLSetstring(rlist,'pmLabelBarDisplayMode','always',ierr)
      call NhlFRLSetstring(rlist,'cnLineLabelsOn','FALSE',ierr)
      call NhlFRLSetstring(rlist,'cnHighLabelsOn','FALSE',ierr)
      call NhlFRLSetstring(rlist,'cnLowLabelsOn','FALSE',ierr)
      call NhlFSetValues(cnid,rlist,ierr)

      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Now show the plot with the Y-Axis linearized, by overlaying the
C plot on a LogLinPlot object. Retrieve the current view coordinates
C of the ContourPlot object and pass them on to the LogLinPlot object.
C Note the LogLinPlot needs to be told the data boundaries. 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString',
     1      'Profile @ 105~S~o~N~W - Frame 4',ierr)
      call NhlFSetValues(cnid,rlist,ierr)

      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(grlist)
      call NhlFRLGetfloat(grlist,'vpXF',xvp,ierr)
      call NhlFRLGetfloat(grlist,'vpYF',yvp,ierr)
      call NhlFRLGetfloat(grlist,'vpWidthF',widthvp,ierr)
      call NhlFRLGetfloat(grlist,'vpHeightF',heightvp,ierr)
      call NhlFGetValues(cnid,grlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',xvp,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',yvp,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',widthvp,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',heightvp,ierr)
      call NhlFRLSetfloat(rlist,'trXMinF',-90.0,ierr)
      call NhlFRLSetfloat(rlist,'trXMaxF',90.0,ierr)
      call NhlFRLSetfloat(rlist,'trYMaxF',1000.0,ierr)
      call NhlFRLSetfloat(rlist,'trYMinF',100.0,ierr)
      call NhlFRLSetstring(rlist,'trYReverse','True',ierr)
      call NhlFCreate(llid,'LogLin1',nhlfloglinplotclass,wid,
     1      rlist,ierr)


C
C The LogLinPlot becomes the Base Plot, since it controls the 
C coordinate system that we are mapping to. Overlay the ContourPlot 
C object on the Base Plot, then plot the LogLinPlot object. 
C Note that you cannot draw the ContourPlot object directly, once 
C it becomes an Overlay Plot.
C
      call NhlFAddOverlay(llid,cnid,-1,ierr)
      call NhlFDraw(llid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFDestroy(llid,ierr)
      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(cnid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
 104  write (6,*) 'Error in opening file: ',filenm
      end
