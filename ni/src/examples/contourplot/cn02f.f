C
C     $Id: cn02f.f,v 1.9 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn02f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:23 MST 1995
C
C      Description:     Demonstrates basic features of ContourPlot
C
C
C Extern declarations for Types of objects that will be used
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

      parameter(M=25,N=25)
      parameter(PI=3.14159)

      real  t(625)
      integer len_dims(2)
      integer appid,wid,dataid,cnid
      integer srlist,grlist
      real  x,y
      integer i,j
      real fscales(1000)
      integer colors(256)
      integer count, itmp
      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
C
C Create a simple bull's eye pattern test data set
C     
      ii = 1
      do 20 i = -N/2,N/2
         do 10 j = -M/2,M/2
            x = 8.0 * i
            y = 8.0 * j
            t(ii) = 100.0 - sqrt(x*x + y*y)
            ii = ii + 1
 10      continue
 20   continue
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current dir
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(srlist,'SETRL')
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn02',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn02f.ncgm',ierr)
         call NhlFCreate(wid,'cn02Work',NhlFNcgmWorkstationClass,
     1     0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn02Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,srlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn02f.ps',ierr)
         call NhlFCreate(wid,'cn02Work',NhlFPSWorkstationClass,
     1     0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPDFFileName','./cn02f.pdf',
     1        ierr)
         call NhlFCreate(wid,'cn02Work',NhlFPDFWorkstationClass,
     1     0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn02f',
     1        ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn02Work',NhlFCairoPSPDFWorkstationClass,
     1     0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkFileName','./cn02f',
     1        ierr)
         call NhlFRLSetstring(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn02Work',NhlFCairoImageWorkstationClass,
     1     0,srlist,ierr)
      endif
C
C Create a ScalarField data object using the data set defined above.
C By default the array bounds will define the data boundaries
C (zero-based, as in C language conventions)
C
      call NhlFRLClear(srlist)
      len_dims(1) = N
      len_dims(2) = M
      call NhlFRLSetmdfloatarray(srlist,'sfDataArray',t,2,len_dims,ierr)
      call NhlFCreate(dataid,'bullseye',nhlfscalarfieldclass,
     1      appid, srlist,ierr)
C
C Create a ContourPlot object, supplying the ScalarField object as data
C
      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'cnScalarFieldData',dataid,ierr)
      call NhlFCreate(cnid,'ContourPlot1',nhlfcontourplotclass,
     1      wid,srlist,ierr)
C
C In the ContourPlot object, many resources that apply to the lines 
C representing the contour levels and the fill areas between the levels
C have both a scalar and an array form. You control which applies by 
C setting an associated boolean flag, identified by the prefix "Mono".
C As an illustration, set NhlNcnMonoLineDashPattern and 
C NhlNcnMonoLineColor false to use a different line dash pattern and 
C line color at each level.
C At the same time set the line thickness of all lines to twice the
C default thickness.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'cnMonoLineDashPattern','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineColor','FALSE',ierr)
      call NhlFRLSetfloat(srlist,'cnLineThicknessF',2.0,ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Change back to a single solid line color and use pattern fill
C
      call NhlFRLClear(srlist)
      call NhlFRLSetfloat(srlist,'cnLineThicknessF',1.0,ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineDashPattern','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineColor','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnFillOn','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillColor','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillPattern','FALSE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Get the fill scale array to illustrate how you would modify the
C values of an array resource. By default all elements of this array
C are set to 1.0, resulting in each fill pattern appearing at its
C 'standard' size. Note that the array must be declared with
C at least as many elements as are retrieved. Since ContourPlot permits
C at most 255 levels, and none of the associated attribute arrays
C contain more than the number of levels + 1, you should be always
C be safe declaring any of these arrays with 256 elements. By default,
C no more than 16 levels will be used.
C Modify the array to range from sparse (2.5) at the low data values 
C to dense (0.5) at high values, and set the new values. 
C
      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(grlist)
      count = 1000
      call NhlFRLGetfloatarray(grlist,'cnFillScales',fscales,count,ierr)
      call NhlFGetValues(cnid,grlist,ierr)

      do 30 i = 0, count-1
         fscales(i+1) = 2.5 - 2.0 * i / real(count - 1)
 30   continue
      call NhlFRLClear(srlist)
      call NhlFRLSetfloatarray(srlist,'cnFillScales',fscales,count,ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillScale','FALSE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Use solid multi-colored fill instead of single-colored pattern fill.
C Using the scalar form of the line color resource, change the contour
C lines to use the background color.
C Change the contour lines to use the background color.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'cnLineColor',NhlBACKGROUND,ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillColor','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillPattern','TRUE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Invert the fill colors.
C First get the current array contents, reverse their order, 
C then re-set the resource using the modified array.
C Turn lines off altogether and also turn off the line and high/low
C labels.
C
      call NhlFRLClear(grlist)
      count = 1000
      call NhlFRLGetintegerarray(grlist,'cnFillColors',colors,count,
     1      ierr)
      call NhlFGetValues(cnid,grlist,ierr)

      do 40 i = 0,(count/2)-1
         itmp = colors(i+1)
         colors(i+1) = colors(count-i)
         colors(count-i) = itmp
 40   continue

      call NhlFRLClear(srlist)
      call NhlFRLSetintegerarray(srlist,'cnFillColors',colors,count,
     1      ierr)
      call NhlFRLSetstring(srlist,'cnLinesOn','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnLineLabelsOn','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnHighLabelsOn','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnLowLabelsOn','FALSE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(cnid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)

      call NhlFClose
      stop
      end
