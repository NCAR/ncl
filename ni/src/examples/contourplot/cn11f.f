C
C     $Id: cn11f.f,v 1.4 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn11f.f
C
C      Author:          Ethan Alpert 
C                                (converted to Fortran by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:22 MST 1995
C
C      Description:     Demonstrates how to mix NCARG3.2 calls and
C                       HLU calls.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFTickmarkclass
      external NhlFTitleClass
C  
C  define label strings for explicit mode tick mark placement
C  
      parameter(M=73, N=10)
      character*14 labels(7)
      data labels / '90:S:o:N:S', '60:S:o:N:S', '30:S:o:N:S', 
     1      'EQ        ', '30:S:o:N:N', '60:S:o:N:N', 
     2      '90:S:o:N:N' /
C  
C  specify data locations for above labels
C  
      real labellocs(7)
      data labellocs / -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 /
      
      integer appid, wid, pid, pid1, gkswid, ierr
      integer i,j,rlist,grlist
      integer iwrk(1000)
      
      real top,bottom,left,right
      
      real rwrk(5000)
      
      real level(N)
      real t(M,N)
      data level / 1000.0, 850.0, 700.0, 500.0, 400.0, 300.0, 
     1      250.0, 200.0, 150.0, 100.0 /
      character*7  wks_type
C
C data file name
C
      character*12 filenm
      filenm = 'cn11f.asc'
C
C Define the workstation type
C
      wks_type = "x11"
C
C read data
C     
      open(unit=10,file=filenm,status='old',form='formatted',err=104)
      do 10 j=1,N
         read (10,1001)(t(i,j),i=1,M)
 10   continue
 1001 format(1x,15F13.8)
C  
C  initialize the high level utility library
C  
      call NhlFInitialize
C  
C  Create an application context. set the app dir to the current 
C  directory so the application looks for a resource file in the 
C  working directory. In this example the resource file supplies 
C  the plot title only.
C  
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn11',NhlFAppClass,
     1      0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./cn11f.ncgm',ierr)
         call NhlFCreate(wid,'cn11Work',
     1        NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn11Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./cn11f.ps',ierr)
         call NhlFCreate(wid,'cn11Work',
     1        NhlFPSWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./cn11f.pdf',ierr)
         call NhlFCreate(wid,'cn11Work',
     1        NhlFPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn11f',ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn11Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn11f',ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn11Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr) 
      endif
C  
C  retrieve gks workstation id from the workstation object
C  
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(rlist)
      call NhlFRLGetinteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C     
C  The following are calls to the low level ncar graphics library to
C  draw labeled contour lines
C  
      call gacwk(gkswid)
      call cpseti('set',0)
      call cpsetr('xc1',-90.0)
      call cpsetr('xcm',90.0)
      call cpsetr('yc1',0.0)
      call cpsetr('ycn',9.0)
      call cpsetr('cis',5.0)
      call cpsetr('dps',.02)
      call cpseti('lis',2)
C  
C  Note that y axis user coordinates are set to grid coordinates 
C  rather than data coordinates. this is done because the input 
C  data is irregularly spaced in the y direction. Basicly this 
C  'tricks' Conpack into thinking the grid points are evenly spaced.
C  
      call set(0.2,0.8,0.2,0.8,-90.0,90.0,0.0,9.0,1)
      call cprect(t,73,73,10,rwrk,5000,iwrk,1000)
      call cpcldr(t,rwrk,iwrk)
      call gdawk(gkswid)
C  
C  End of ncar graphics section. Note deactivation of gks workstation
C  
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',.2,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',.8,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',.6,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',.6,ierr)
      call NhlFRLSetfloat(rlist,'tmYLDataTopF',100.0,ierr)
      call NhlFRLSetfloat(rlist,'tmYLDataBottomF',1000.0,ierr)
      call NhlFRLSetfloat(rlist,'tmXBDataRightF',90.0,ierr)
      call NhlFRLSetfloat(rlist,'tmXBDataLeftF',-90.0,ierr)
      call NhlFRLSetstring(rlist,'tmYLStyle','irregular',ierr)
      call NhlFRLSetstring(rlist,'tmXBMode','explicit',ierr)
      call NhlFRLSetstring(rlist,'tmXBMinorOn','false',ierr)
      call NhlFRLSetfloatarray(rlist,'tmXBValues',
     1      labellocs,7,ierr)
C  
C  Array 'level' contains original grid point data locations in 
C  y direction. Providing the grid points to the tickmark object as 
C  the control points for the irregular style transformation, means 
C  that these points will be evenly spaced along the y axis. Since 
C  this is how conpack thinks the points are spaced, the tick marks 
C  will correctly correspond with the data coordinates. See the hlu 
C  user's guide prototype document for a complete discussion of 
C  irregular style transformations.
C  
      call NhlFRLSetstringarray(rlist,'tmXBLabels',
     1      labels,7,ierr)
      call NhlFRLSetfloatarray(rlist,'tmYLIrregularPoints',
     1      level,10,ierr)
      call NhlFCreate(pid,'TickMarksForContour',
     1      nhlftickmarkclass,wid,rlist,ierr)
C  
C  Retrieves bounding box information from tick mark object so 
C  title object can be correctly configured.
C  
      call NhlFGetBB(pid,top,bottom,left,right,ierr)
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',left,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',top,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',right-left,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',top-bottom,ierr)
C  
C  These offsets are computed in order to center the labels around 
C  the plot viewport rather than the bounding box of the TickMark 
C  object.
C
      call NhlFRLSetfloat(rlist,'tiMainOffsetXF',
     1      (.2 - left)/2.0,ierr)
      call NhlFRLSetfloat(rlist,'tiXAxisOffsetXF',
     1      (.2 - left)/2.0,ierr)
      call NhlFRLSetfloat(rlist,'tiYAxisOffsetYF',
     1      (.2 - bottom)/2.0,ierr)
      
      call NhlFCreate(pid1,'TitlesForContour',NhlFTitleClass,
     1      wid,rlist,ierr)
      
      call NhlFDraw(pid,ierr)
      call NhlFDraw(pid1,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(pid1,ierr)
      call NhlFClose
      
      stop
 104  write (6,*) 'error in opening file: ',filenm
      
      end
