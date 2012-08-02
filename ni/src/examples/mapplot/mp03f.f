C
C     $Id: mp03f.f,v 1.10 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1993                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:       mp03f.f
C
C   Author:     David Brown (converted to Fortran by Mary)
C           National Center for Atmospheric Research
C           PO 3000, Boulder, Colorado
C
C   Date:       Tue Jan 24 10:08:50 MST 1995
C
C   Description:    Demonstrates MapPlot masking; loosely emulates the
C           LLU example 'colcon'
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFMapPlotClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass

      integer appid,wid,mapid,dataid,cnid
      integer rlist

      real z(50,50)
      integer M,N
      data M,N/50,50/
      integer mlow, mhigh
      data mlow,mhigh/13,18/
      real dlow, dhigh
      data dlow,dhigh/13.,18./
      integer len_dims(2)

      character*6 mask_specs(1)
      data mask_specs/'oceans'/
      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"

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
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp03',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp03f.ncgm',ierr)
         call NhlFCreate(wid,'mp03Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp03Work',
     +        NhlFCairoWindowWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp03f.ps',ierr)
         call NhlFCreate(wid,'mp03Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./mp03f.pdf',ierr)
         call NhlFCreate(wid,'mp03Work',NhlFPDFWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp03f',ierr)
         call NhlFCreate(wid,'mp03Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp03f',ierr)
         call NhlFCreate(wid,'mp03Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Call the routine 'gendat' to create the first array of contour
C data. Create a ScalarField data object and hand it the data created
C by 'gendat'. Define the extent of the data coordinates as the whole
C globe. 

      call NhlFRLClear(rlist)
      len_dims(1) = N
      len_dims(2) = M
      call gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',z,2,len_dims,ierr)
      call NhlFRLSetinteger(rlist,'sfXCStartV',-180,ierr)
      call NhlFRLSetinteger(rlist,'sfXCEndV',180,ierr)
      call NhlFRLSetinteger(rlist,'sfYCStartV',-90,ierr)
      call NhlFRLSetinteger(rlist,'sfYCEndV',90,ierr)
      call NhlFCreate(dataid,'Gendat',NhlFScalarFieldClass,appid,
     1     rlist,ierr)

C
C Create a ContourPlot object, supplying the ScalarField object as data,
C and setting the size of the viewport.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetstring(rlist,'cnLabelDrawOrder','postdraw',ierr)
      call NhlFCreate(cnid,'Contour1',NhlFContourPlotClass,
     1     wid,rlist,ierr)
C
C Create a MapPlot object, setting the fill to draw over the main draw,
C and masking out the oceans.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpYF',0.775,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',0.45,ierr)
      call NhlFRLSetstring(rlist,'mpFillOn','true',ierr)
      call NhlFRLSetstring(rlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetstring(rlist,'tiMainString','mp03f',ierr)
      call NhlFRLSetstring(rlist,'mpFillDrawOrder','postdraw',ierr)
      call NhlFRLSetstring(rlist,'mpAreaMaskingOn','true',ierr)
      call NhlFRLSetstringarray(rlist,'mpMaskAreaSpecifiers',mask_specs,
     1     1,ierr)
      call NhlFCreate(mapid,'Map1',NhlFMapPlotClass,wid,rlist,ierr)
C
C Overlay the ContourPlot on the MapPlot
C
      call NhlFAddOverlay(mapid,cnid,-1,ierr)
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C

      call NhlFDestroy(mapid,ierr)
      call NhlFDestroy(wid,ierr)
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
