C
C     $Id: cn04f.f,v 1.10 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn04f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:24 MST 1995
C
C      Description:      Emulates the output of the Conpack example
C                       'cpex02.f' using the HLU library.
C
      external NhlFTextItemClass
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external nhlfscalarfieldclass
      external nhlfcontourplotclass

      integer appid,wid,dataid,cnid,txid
      integer rlist,grlist
      integer gkswid

      parameter(M = 33, N = 33)
      real z(M,N)
      integer mlow, mhigh
      data mlow,mhigh/20,20/
      real dlow, dhigh
      data dlow,dhigh/.000025,.000075/
      integer len_dims(2)

      integer i
      integer lvlflags(25), pats(25)
      real levels(25), thicknesses(25)
      integer lvlflag_count, pat_count, level_count,thick_count
      data lvlflag_count,pat_count,level_count,thick_count/25,25,25,25/

      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
C
C This program emulates the output of cpex02 with a few differences:
C 1. Because the information label is implemented as an HLU Annotation
C    object, Conpack is unaware of its existence, much less its 
C    location. Therefore it is not possible to have Conpack remove 
C    the high/low labels that occupy the same space as the info label.
C 2. Line labels do not appear in the same positions.
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory.  The resource file sets most of the ContourPlot
C resources that remain fixed throughout the life of the ContourPlot 
C object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn04',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./cn04f.ncgm',ierr)
         call NhlFCreate(wid,'cn04Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr) 
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn04Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./cn04f.ps',ierr)
         call NhlFCreate(wid,'cn04Work',NhlFPSWorkstationClass,
     1     0,rlist,ierr) 
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./cn04f.pdf',
     1        ierr)
         call NhlFCreate(wid,'cn04Work',NhlFPDFWorkstationClass,
     1     0,rlist,ierr) 
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn04f',
     1        ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn04Work',NhlFCairoPSPDFWorkstationClass,
     1     0,rlist,ierr) 
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./cn04f',
     1        ierr)
         call NhlFRLSetstring(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn04Work',NhlFCairoImageWorkstationClass,
     1     0,rlist,ierr) 
      endif
C
C Call the routine 'GENDAT' to create the first array of contour
C data. Create a ScalarField data object and hand it the data created by
C 'GENDAT'.
C
      call NhlFRLClear(rlist)
      len_dims(1) = N
      len_dims(2) = M
      call gendat (z,M,M,N,mlow,mhigh,dlow,dhigh)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',z,2,len_dims,ierr)
      call NhlFCreate(dataid,'Gendat',nhlfscalarfieldclass,appid,
     1                rlist,ierr)
C
C Create a ContourPlot object, supplying the ScalarField object as data,
C and setting the size of the viewport.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetstring(rlist,'tiMainString','EXAMPLE 2-1',ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',0.4625,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',0.4625,ierr)
      call NhlFCreate(cnid,'ContourPlot1',nhlfcontourplotclass,
     1   wid,rlist,ierr)
C
C In order to set the contour array resources of interest, you must 
C fill in the correct value for each element. By calling GetValues 
C for the arrays the ContourPlot fills in the current values for you. 
C Then all that is necessary is to modify the values that need 
C changing. Note that you must declare the arrays to have a least
C as many elements as you expect to get. Note that a GetValues resource
C list is different than a SetValues list.
C
      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(grlist)
      call NhlFRLGetintegerarray(grlist,'cnLevelFlags',lvlflags,
     1  lvlflag_count,ierr)
      call NhlFRLGetintegerarray(grlist,'cnFillPatterns',pats,pat_count,
     1  ierr)
      call NhlFRLGetfloatarray(grlist,'cnLevels',levels,level_count,
     1  ierr)
      call NhlFRLGetfloatarray(grlist,'cnLineThicknesses',thicknesses,
     1   thick_count,ierr)
      call NhlFGetValues(cnid,grlist,ierr)
C 
C Depending on the level flag for each contour line, widen the line if
C there is a label on the line. Also set the fill style to pattern #6
C if the level is between certain values. Note that there is always one
C more element in the fill resource arrays than there are ContourPlot
C line levels: the first element of these arrays specifies the 
C attributes of areas less than the minimum contour level and the last 
C element specifies attributes of areas greater than the maximum contour
C level.
C
      do 10 i = 1,level_count
         if (lvlflags(i) .eq. 3) then
            thicknesses(i) = 2.0
         endif
         if (levels(i) .ge. 0.000045 .and. levels(i) .lt. 0.000055) then
            pats(i) = 6
         else
            pats(i) = -1
         endif
 10   continue
      pats(pat_count) = -1
C
C Now that the arrays are correctly filled in set the arrays that have
C been modified. Also set the position of the first ContourPlot plot and
C the label scaling mode.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetintegerarray(rlist,'cnFillPatterns',pats,
     1   pat_count,ierr)
      call NhlFRLSetfloatarray(rlist,'cnLineThicknesses',thicknesses,
     1    thick_count,ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.0250,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',0.9750,ierr)
      call NhlFRLSetstring(rlist,'cnLabelScalingMode','ConfineToRange',
     1     ierr)
      call NhlFRLSetfloat(rlist,'cnLabelScaleValueF',10.0,ierr)
      call NhlFSetValues(cnid,rlist,ierr)

      call NhlFDraw(cnid,ierr)
C
C Plot 2 - Set the Scalar Field object with a newly generated data set
C Set the ContourPlot object with a new title, position, and a new label
C scaling mode.
C
      call NhlFRLClear(rlist)
      call gendat (z,M,M,N,mlow,mhigh,dlow,dhigh)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',Z,2,len_dims,ierr)
      call NhlFSetValues(dataid,rlist,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','EXAMPLE 2-2',ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.5125,ierr)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetstring(rlist,'cnLabelScalingMode',
     1        'MaxSigDigitsLeft',ierr)
      call NhlFSetValues(cnid,rlist,ierr)
      call NhlFDraw(cnid,ierr)
C
C Plot 3 - Set the Scalar Field object with a newly generated data set
C Set the ContourPlot object with a new title, position, and a new label
C scaling mode.
C
      call NhlFRLClear(rlist)
      call gendat (z,M,M,N,mlow,mhigh,dlow,dhigh)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',Z,2,len_dims,ierr)
      call NhlFSetValues(dataid,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','EXAMPLE 2-3',ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.0250,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',0.4875,ierr)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetstring(rlist,'cnLabelScalingMode','TrimZeros',ierr)
      call NhlFSetValues(cnid,rlist,ierr)
      call NhlFDraw(cnid,ierr)
C
C Plot 4 - Set the Scalar Field object with a newly generated data set
C Set the ContourPlot object with a new title, position, and a new label
C scaling mode.
C
      call NhlFRLClear(rlist)
      call gendat (z,M,M,N,mlow,mhigh,dlow,dhigh)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',z,2,len_dims,ierr)
      call NhlFSetValues(dataid,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','EXAMPLE 2-4',ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.5125,ierr)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetstring(rlist,'cnLabelScalingMode',
     1       'IntegerLineLabels',ierr)
      call NhlFSetValues(cnid,rlist,ierr)
      call NhlFDraw(cnid,ierr)
C
C Activate the GKS workstation and use the low-level routine that cpex02
C uses to draw the line around the edge of the plotter frame.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetinteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(wid,grlist,ierr)
      call gacwk(gkswid)
      call bndary
      call gdawk(gkswid)
C
C Label the plot as an emulation
C
      call NhlFRLClear(rlist)
      call NhlFCreate(txid,'TextItem1',NhlFTextItemClass,wid,
     1         rlist,ierr)
      call NhlFDraw(txid,ierr)
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

      subroutine gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
C
C This is a routine to generate test data for two-dimensional graphics
C routines.  Given an array 'DATA', dimensioned 'IDIM x 1', it fills
C the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
C of data having approximately 'MLOW' lows and 'MHGH' highs, a minimum
C value of exactly 'DLOW' and a maximum value of exactly 'DHGH'.
C
C 'MLOW' and 'MHGH' are each forced to be greater than or equal to 1
C and less than or equal to 25.
C
C The function used is a sum of exponentials.
C
        dimension data(idim,1),ccnt(3,50)

        fovm=9./float(m)
        fovn=9./float(n)

        nlow=max0(1,min0(25,mlow))
        nhgh=max0(1,min0(25,mhgh))
        ncnt=nlow+nhgh

        do 101 k=1,ncnt
          ccnt(1,k)=1.+(float(m)-1.)*fran()
          ccnt(2,k)=1.+(float(n)-1.)*fran()
          if (k.le.nlow) then
            ccnt(3,k)=-1.
          else
            ccnt(3,k)=+1.
          end if
  101   continue

        dmin=+1.e36
        dmax=-1.e36
        do 104 j=1,n
          do 103 i=1,m
            data(i,j)=.5*(dlow+dhgh)
            do 102 k=1,ncnt
              temp=-((fovm*(float(i)-ccnt(1,k)))**2+
     +               (fovn*(float(j)-ccnt(2,k)))**2)
              if (temp.ge.-20.) data(i,j)=data(i,j)+
     +            .5*(dhgh-dlow)*ccnt(3,k)*exp(temp)
  102       continue
            dmin=amin1(dmin,data(i,j))
            dmax=amax1(dmax,data(i,j))
  103     continue
  104   continue

        do 106 j=1,n
          do 105 i=1,m
            data(i,j)=(data(i,j)-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow
  105     continue
  106   continue

        return

      end

      function fran ()
        dimension rseq (100)
        save iseq
        data rseq / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +              .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +              .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +              .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +              .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +              .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +              .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +              .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +              .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +              .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
        data iseq / 0 /
        iseq=mod(iseq,100)+1
        fran=rseq(iseq)
        return
      end

      subroutine bndary
C
C Draw a line showing where the edge of the plotter frame is.
C
        call plotif (0.,0.,0)
        call plotif (1.,0.,1)
        call plotif (1.,1.,1)
        call plotif (0.,1.,1)
        call plotif (0.,0.,1)
        call plotif (0.,0.,2)
C
C Done.
C
        return
C
      end
