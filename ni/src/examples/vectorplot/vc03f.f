C
C  $Id: vc03f.f,v 1.3 1996-09-18 19:27:04 haley Exp $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      
C                 Copyright (C)  1996                                  
C         University Corporation for Atmospheric Research              
C                 All Rights Reserved                                  
C                                                                      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 
C    File:       vc03f.f
C  
C    Author:     David Brown (converted to Fortran by Lynn Hermanson)
C                National Center for Atmospheric Research
C                PO 3000, Boulder, Colorado
C  
C    Date:       June 6, 1996
C  
C  
C   Description: Manipulates the FillArrow resources to demonstrate
C                some of the possible stylistic variations on 
C                 the appearance of the filled vector arrows.
C                 The data is extracted from an NMC forecast 
C                 dataset for 11/10/1994.
C
       external NhlFAppClass
       external NhlFNcgmWorkstationClass
       external NhlFPSWorkstationClass
       external NhlFXWorkstationClass
       external NhlFVectorPlotClass
       external NhlFVectorFieldClass

       parameter(LL=37,MM=37,NN=2)

       integer NCGM, X11, PS
       integer appid,wid,vcid,vfid
       integer rlist,grlist
       integer len_dims(3)
       integer flen,ierr
       real reflen, ref
       real x(LL,MM,NN)
       integer i,j,k
C
C  Generate vector data array
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

      flen = flen

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
      call NhlFCreate(appid,'vc03',NhlFAppClass,0,rlist,ierr)
C
C Default is to create an X11 window.
C
      NCGM=0
      X11=1
      PS=0

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./vc03f.ncgm',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'vc03Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./vc03f.ps',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      endif
C
C Create a VectorField data object using the data set defined above.
C By default the array bounds will define the data boundaries 
C (zero-based, as in C language conventions)
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
     +     rlist,ierr)
C
C  Create a VectorPlot object, supplying the VectorField object as
C  data.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +                    'Filled Arrow VectorPlot',ierr)
      call NhlFRLSetFloat(rlist,'vcRefMagnitudeF', 20.0,ierr)
      call NhlFRLSetString(rlist,'vcFillArrowsOn', 'True',ierr)
      call NhlFRLSetFloat(rlist,'vcMinFracLengthF', 0.2,ierr)

      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfid,ierr)

      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +     ierr)


      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vcRefLengthF',reflen,ierr)
      call NhlFGetValues(vcid,grlist,ierr)

      ref= 1.5 * reflen  
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vcRefLengthF',ref,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'Variation #1:: Constant Width',ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowWidthF', 0.15,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowMinFracWidthF', 1.0,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)


      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString', 'Variation #2',ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowMinFracWidthF',0.25,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadMinFracXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowWidthF',0.2,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadXF',0.8,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadInteriorXF',0.7,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadYF',0.2,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)


      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Variation #3',ierr)
      ref = 1.2 * reflen   
      call NhlFRLSetFloat(rlist,'vcRefLengthF',ref,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowWidthF',0.3,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadXF',0.4,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadInteriorXF',0.35,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadYF',0.3,ierr)

      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Variation #4',ierr)
      ref = 1.2 * reflen   
      call NhlFRLSetFloat(rlist,'vcRefLengthF',ref,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowWidthF',0.2,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadXF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadInteriorXF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadYF',0.2,ierr)

      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Variation #5',ierr)
      ref = 0.8 * reflen   
      call NhlFRLSetFloat(rlist,'vcRefLengthF',ref,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowWidthF',0.2,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadXF',1.5,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadInteriorXF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vcFillArrowHeadYF',0.5,ierr)

      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C  Destroy the objects created, close the HLU library and exit.
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end

