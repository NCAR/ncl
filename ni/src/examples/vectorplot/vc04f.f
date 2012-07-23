C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    
C                Copyright (C)  1993                                 
C        University Corporation for Atmospheric Research             
C                All Rights Reserved                                  
C                                                                   
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:       vc04f.f
C 
C   Author:     David Brown (converted to FORTRAN by Lynn Hermanson)
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C 
C   Date:       June 17, 1996
C 
C 
C  Description: Manipulates the FillArrow resources to demonstrate some
C                of the possible stylistic variations on the appearance
C                of the filled vector arrows.
C                The data is extracted from an NMC forecast dataset for
C                11/10/1994.
C
       external NhlFAppClass
       external NhlFNcgmWorkstationClass
       external NhlFPSWorkstationClass
       external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
       external NhlFCairoWindowWorkstationClass
       external NhlFVectorPlotClass
       external NhlFVectorFieldClass

       parameter(LL=37,MM=37,NN=2)
       character*7  wks_type
       integer appid,wid,vcid,vfid
       integer rlist,grlist
       integer len_dims(3)
       integer flen,ierr
       real x(LL,MM,NN)
       integer i,j,k
       real reflen, ref, tiheight, tiht
C
C Generate vector data array
C
      character*256  filename
      call gngpat(filename,'data',ierr)
      flen = 15

      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/asc/uvdata0.asc'
            goto 15
         endif
 10   continue

 15   open(UNIT=10,FILE=filename,STATUS='OLD')
      read(10,*)(((x(i,j,k),i=1,LL),j=1,MM),k=1,NN)
C
C Initialize the HLU library and set up resource template.
C  
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLCreate(grlist, 'getrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'vc04',NhlFAppClass,0,rlist,ierr)
C
C Default is to create an X11 window.
C
      wks_type = "x11"

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./vc04f.ncgm',ierr)
         call NhlFCreate(wid,'vc04Work',

     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'vc04Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./vc04f.ps',ierr)
         call NhlFCreate(wid,'vc04Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./vc04f.pdf',ierr)
         call NhlFCreate(wid,'vc04Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./vc04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'vc04Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./vc04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'vc04Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C  Create a VectorField data object using the data set defined above.
C  By default the array bounds will define the data boundaries 
C  (zero-based,
C  as in C language conventions)
C
      len_dims(1) = LL
      len_dims(2) = MM
      len_dims(3) = NN
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfDataArray',x,3,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV', -180.0,ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', 0.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV', 0.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV', 90.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartSubsetV', 20.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndSubsetV', 80.0,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFvectorFieldClass,appid,
     +rlist,ierr)
C
C  Create a VectorPlot object, supplying the VectorField object as data
C  Setting vcMonoFillArrowFillColor False causes VectorPlot to color
C   the vector arrows individually based, by default, on the vector
C   magnitude.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vcRefMagnitudeF', 20.0,ierr)
      call NhlFRLSetString(rlist,'vcFillArrowsOn', 'True',ierr)
      call NhlFRLSetFloat(rlist,'vcMinFracLengthF', 0.25,ierr)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfid,ierr)
      call NhlFRLSetString(rlist,'vcMonoFillArrowFillColor', 'False',
     +ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +ierr)

      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vcRefLengthF',reflen,ierr)
      call NhlFRLGetFloat(grlist,'tiMainFontHeightF',tiheight,ierr)
      call NhlFGetValues(vcid,grlist,ierr)

      ref= 1.75 * reflen
      tiht = 0.9 * tiheight
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vcRefLengthF',ref,ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     +'How to Rotate a Vector Plot 90:F34:0:F:',ierr) 
      call NhlFRLSetFloat(rlist,'tiMainFontHeightF',tiht,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     + '1::Exchange the Dimensions',ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'vfExchangeDimensions', 'True',ierr)
      call NhlFSetValues(vfid,rlist,ierr)
 
      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','2:: Exchange the U and 
     +V Data',ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFRLClear(rlist) 
      call NhlFRLSetString(rlist,'vfExchangeUVData', 'True',ierr)
      call NhlFSetValues(vfid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','3a:: Reverse the Y-Axis
     + for Clockwise Rotation',ierr)
      call NhlFRLSetString(rlist,'trYReverse', 'True',ierr)

      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +'3b:: Or the X-Axis for Counter Clockwise Rotation',ierr)
      call NhlFRLSetString(rlist,'trYReverse', 'False',ierr)
      call NhlFRLSetString(rlist,'trXReverse', 'True',ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
