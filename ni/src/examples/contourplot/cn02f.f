C
C     $Id: cn02f.f,v 1.4 1995-06-22 21:07:39 haley Exp $
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
      external NhlFXWorkstationClass
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
      integer NCGM, X11, PS
C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0
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

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn02f.ncgm',ierr)
         call NhlFCreate(wid,'cn02Work',NhlFNcgmWorkstationClass,
     1     0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn02Work',NhlFXWorkstationClass,
     1        0,srlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn02f.ps',ierr)
         call NhlFCreate(wid,'cn02Work',NhlFPSWorkstationClass,
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
C Draw the plot.  Notice that it illustrates the basic default behavior
C of the ContourPlot object. The contours appear as solid lines with 
C unboxed labels in a linear coordinate system with the origin at the 
C lower left. Tickmarks with labels show the data coordinate range, and
C an informational label at the lower right gives the minimum and 
C maximum data values and the contour interval spacing. The title is 
C NOT a default item: it appears because it is defined in the 
C resource file.
C
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C In the ContourPlot object, you can treat most of resources controlling
C contour level attributes either individually or collectively depending
C on the setting of an associated 'mono' flag resource. If the 'mono'
C flag is true, the resource is treated as a scalar: its first element
C sets the value of the attribute for all levels. Setting the 'mono'
C flag false has the effect of applying the resource as an array: each
C element has different default value.  As an illustration, set
C NhlNcnMonoLineDashPattern and NhlNcnMonoLineColor false to use a
C different line dash pattern and line color at each level.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetfloat(srlist,'cnLineThicknesses',2.0,ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineDashPattern','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineColor','FALSE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Change back to a single solid line color and use pattern fill
C
      call NhlFRLClear(srlist)
      call NhlFRLSetfloat(srlist,'cnLineThicknesses',1.0,ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineDashPattern','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoLineColor','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillScale','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnFillOn','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillColor','TRUE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillPattern','FALSE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Get the fill scale array. Note that the preceding SetValues turned off
C the 'mono' fill scale flag. There was no effect at that stage because
C the default fill scale array values are all set identically to 1.0. If
C you try to get the array while the 'mono' flag is set True, you will
C only get a single element. The user is responsible for freeing the
C memory allocated for this array.
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
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Use solid multi-colored fill instead of single-colored pattern fill.
C Change the contour lines to use the background color (note that
C although the NhlNcnLineColors resource is an array, here it is set
C as a scalar).
C
      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'cnLineColors',NhlBACKGROUND,ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillColor','FALSE',ierr)
      call NhlFRLSetstring(srlist,'cnMonoFillPattern','TRUE',ierr)
      call NhlFSetValues(cnid,srlist,ierr)
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Invert the fill colors.
C First get the current array contents, reverse their order, 
C then re-set the resource using the modified array. Note that the user 
C is responsible for freeing the memory allocated for the array. 
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
