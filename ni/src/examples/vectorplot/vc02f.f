C
C      $Id: vc02f.f,v 1.5 2010-03-15 22:49:25 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C   File:       vc02f.f
C 
C   Author:     David Brown
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C 
C   Date:       Fri May 10 8:30:55 MDT 1996
C 
C    Description:   Given a simple mathematically generated data set,
C                   demonstrates line-drawn vector arrows and the use
C                   of some basic VectorPlot resources
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
      parameter(M=30,N=25)
      parameter(PI=3.14159)

      character*7  wks_type
      integer appid,wid,vcid,vfid
      integer rlist,grlist
      integer len_dims(2)
      real reflen
      real U(M,N),V(M,N)
      real igrid, jgrid
      integer i,j

      wks_type = "x11"
C
C Generate vector data arrays
C
      igrid = 2.0 * PI / real(M)
      jgrid = 2.0 * PI / real(N)
      do 20 j = 1,N
         do 10 i=1,M
            U(i,j) = 10.0 * cos(jgrid * real(j-1))
            V(i,j) = 10.0 * cos(igrid * real(i-1))
 10      continue
 20   continue
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'vc02',NhlFappClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./vc02f.ncgm',ierr)
         call NhlFCreate(wid,'vc02Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'vc02Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./vc02f.ps',ierr)
         call NhlFCreate(wid,'vc02Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./vc02f.pdf',ierr)
         call NhlFCreate(wid,'vc02Work',NhlFPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./vc02f',ierr)
         call NhlFCreate(wid,'vc02Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./vc02f',ierr)
         call NhlFCreate(wid,'vc02Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create a VectorField data object using the data set defined above.
C By default the array bounds will define the data boundaries
C (zero-based, as in C language conventions)
C
      len_dims(1) = M
      len_dims(2) = N
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFVectorFieldClass,appid,
     1     rlist,ierr)
C
C Create a VectorPlot object, supplying the VectorField object as data
C Setting vcMonoLineArrowColor False causes VectorPlot to color the
C vector arrows individually, based, by default, on each vector's
C magnitude.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Line-Drawn Vectors (colored by magnitude)',ierr)

      call NhlFRLSetString(rlist,'vcMonoLineArrowColor','false',ierr)

      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfid,ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFVectorPlotClass,wid,
     1     rlist,ierr)
      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C All the vector arrows are scaled in length based on their magnitude
C relative to a reference magnitude (the maximum magnitude by default)
C Setting the arrow length of the reference magnitude, therefore,
C adjusts the length of all the vector arrows in the plot. In
C addition, you can specify a length for the smallest vector in the
C plot, as a fraction of the length of the reference vector. In this
C case, the remaining arrows are scaled based on both these lengths.
C Since the initial reference length is established dynamically, the 
C simplest way to adjust the overall arrow length is to 
C retrieve the initially set value of 'vcRefLengthF' and then multiply 
C by the desired factor.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vcRefLengthF',reflen,ierr)
      call NhlFGetValues(vcid,grlist,ierr)

      reflen = reflen * 1.5
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Adjusting the Reference and Minimum Length',ierr)
      call NhlFRLSetFloat(rlist,'vcRefLengthF',reflen,ierr)
      call NhlFRLSetFloat(rlist,'vcMinFracLengthF',0.3,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C Note that setting the reference magnitude also affects the length
C of the arrows. In this case it is an inverse relationship.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Adjusting the Reference Magnitude',ierr)
      call NhlFRLSetFloat(rlist,'vcRefMagnitudeF',20.0,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C There are two vector annotations known as the reference vector 
C annotation and the minimum vector annotation. Their resources are
C prefixed respectively by "vcRefAnno" and "vcMinAnno". Only the
C reference vector annotation is displayed by default. Here, the
C minimum vector annotation is turned on, and some resources of each
C annotation are modified.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Modifying the Vector Annotations',ierr)
      call NhlFRLSetFloat(rlist,'vcRefAnnoFontHeightF',0.015,ierr)
      call NhlFRLSetString(rlist,'vcRefAnnoString1',
     1     '$VMG$ meters/sec',ierr)
      call NhlFRLSetString(rlist,'vcRefAnnoString2On','False',ierr)
      call NhlFRLSetString(rlist,'vcMinAnnoOn','True',ierr)
      call NhlFRLSetString(rlist,'vcMinAnnoString1',
     1     '$VMG$ meters/sec',ierr)
      call NhlFRLSetString(rlist,'vcMinAnnoString2On','False',ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C Line-drawn arrowheads are sized proportionally to the arrow length 
C unless the resulting size would be outside the limits defined by 
C the arrowhead minimum and maximum size resources. Setting the 
C minimum and maximum sizes to the same value causes all the
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Uniformly-sized Arrow Heads',ierr)
      call NhlFRLSetFloat(rlist,'vcLineArrowHeadMinSizeF',0.01,ierr)
      call NhlFRLSetFloat(rlist,'vcLineArrowHeadMaxSizeF',0.01,ierr)
      call NhlFSetValues(vcid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end

