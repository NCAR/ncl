C
C     $Id: cn01f.f,v 1.1 1995-03-31 21:54:46 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            cn01f.f
C
C      Author:          Ethan Alpert (converted to Fortran by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:22 MST 1995
C
C      Description:     Demonstrates how to mix NCARG3.2 calls and
C                       HLU calls.
C
      external NhlFAppLayerClass
      external NhlFNcgmWorkstationLayerClass
      external NhlFXWorkstationLayerClass
      external nhlftickmarklayerclass
      external NhlFTitleLayerClass
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
      integer NCGM
C
C data file name
C
      character*12 filenm
      filenm = 'cn01f.asc'
C
C Default is to display output to an X workstation
C
      NCGM=0
C
C read data
C     
      open(unit=10,file=filenm,status='old',form='formatted',err=104)
      do 10 j=1,N
         read (10,*)(t(i,j),i=1,M)
 10   continue
C  
C  initialize the high level utility library
C  
      call NhlFInitialize
C  
C  create an application context. set the app dir to the current directory
C  so the application looks for a resource file in the working directory.
C  in this example the resource file supplies the plot title only.
C  
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn01',NhlFAppLayerClass,
     1      0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./cn01f.ncgm',ierr)
         call NhlFCreate(wid,'cn01Work',
     1        NhlFNcgmWorkstationLayerClass,0,rlist,ierr) 
      else 
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn01Work',NhlFXWorkstationLayerClass,
     1        0,rlist,ierr) 
      endif
C  
C  retrieve gks workstation id from the workstation object
C  
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(rlist)
      call NhlFRLGetinteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C     
C  the following are calls to the low level ncar graphics library to
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
C  note that y axis user coordinates are set to grid coordinates rather than
C  data coordinates. this is done because the input data is irregularly spaced
C  in the y direction. basicly this 'tricks' conpack into thinking the grid
C  points are evenly spaced.
C  
      call set(0.2,0.8,0.2,0.8,-90.0,90.0,0.0,9.0,1)
      call cprect(t,73,73,10,rwrk,5000,iwrk,1000)
      call cpcldr(t,rwrk,iwrk)
      call gdawk(gkswid)
C  
C  end of ncar graphics section. note deactivation of gks workstation
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
C  array 'level' contains original grid point data locations in y direction.
C  providing the grid points to the tickmark object as the control points
C  for the irregular style transformation, means that these points will be
C  evenly spaced along the y axis. since this is how conpack thinks the
C  points are spaced, the tick marks will correctly correspond with the 
C  data coordinates. see the hlu user's guide prototype document for a complete
C  discussion of irregular style transformations.
C  
      call NhlFRLSetstringarray(rlist,'tmXBLabels',
     1      labels,7,ierr)
      call NhlFRLSetfloatarray(rlist,'tmYLIrregularPoints',
     1      level,10,ierr)
      call NhlFCreate(pid,'TickMarksForContour',
     1      nhlftickmarklayerclass,wid,rlist,ierr)
C  
C  retrieves bounding box information from tick mark object so title object
C  can be correctly configured.
C  
      call NhlFGetBB(pid,top,bottom,left,right) 
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',left,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',top,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',right-left,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',top-bottom,ierr)
C  
C  these offsets are computed in order to center the labels around the plot
C  viewport rather than the bounding box of the tickmark object.
C  
      call NhlFRLSetfloat(rlist,'tiMainOffsetXF',
     1      (.2 - left)/2.0,ierr)
      call NhlFRLSetfloat(rlist,'tiXAxisOffsetXF',
     1      (.2 - left)/2.0,ierr)
      call NhlFRLSetfloat(rlist,'tiYAxisOffsetYF',
     1      (.2 - bottom)/2.0,ierr)
      
      call NhlFCreate(pid1,'TitlesForContour',NhlFTitleLayerClass,
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
