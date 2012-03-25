C
C  $Id: vc03f.f,v 1.6 2010-03-15 22:49:25 haley Exp $
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
      wks_type = "x11"

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./vc03f.ncgm',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./vc03f.ps',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./vc03f.pdf',ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./vc03f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./vc03f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'vc03Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
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

