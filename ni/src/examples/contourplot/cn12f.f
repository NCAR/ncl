C
C     $Id: cn12f.f,v 1.1 1995-10-16 17:21:01 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn12f.f
C
C      Author:          Mary Haley
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Oct 16 11:02:58 MDT 1995
C
C   Description:  This example emulates LLU example "cpex08".  It
C                 draws a filled map with filled contours appearing in
C                 Africa. In order to mask Africa from the map fill, we
C                 use the mpMaskAreaSpecifiers resource and mask all of
C                 the countries in Africa.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external nhlfscalarfieldclass
      external nhlfcontourplotclass
      external nhlfmapplotclass

      integer appid,wid,dataid,conid,mapid
      integer rlist

      parameter(M = 40, N = 40)
      real z(M,N)
      integer len_dims(2)

      integer NCGM, X11, PS
C
C Areas we want to fill.
C
      character*5 fillspcs(2)
      data fillspcs/'water','land'/
C
C Areas we want to mask (countries of Africa).
C
      character*30 maskspcs(50)
      data maskspcs/'algeria','angola','angola-exclave-called-cabinda',
     +     'benin','botswana','burundi','cameroon',
     +     'central-african-republic','chad','congo','djibouti','egypt',
     +     'equatorial-guinea','ethiopia','gabon','gambia','ghana',
     +     'guinea','guinea-bissau','ivory-coast','kenya','lesotho',
     +     'liberia','libya','madagascar','malawi','mali','mauritania',
     +     'mauritius','morocco','mozambique','namibia','niger',
     +     'nigeria','rwanda','senegal','sierra-leone','somalia',
     +     'south-africa','sudan','swaziland','tanzania','togo',
     +     'tunisia','uganda','upper-volta','western-sahara','zaire',
     +     'zambia','zimbabwe'/
C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0
      call NhlFInitialize
C
C Create an application object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn12',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./cn12f.ncgm',ierr)
         call NhlFCreate(wid,'cn12Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr) 
      else  if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn12Work',NhlFXWorkstationClass,
     1        0,rlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./cn12f.ps',ierr)
         call NhlFCreate(wid,'cn12Work',NhlFPSWorkstationClass,
     1     0,rlist,ierr) 
      endif
C
C Call the routine 'gendat' to create an array of contour data.
C Create a ScalarField data object and hand it the data created by
C 'gendat'.
C
      call NhlFRLClear(rlist)
      len_dims(1) = N
      len_dims(2) = M
      call gendat(z,M,M,N,15,15,-10.,110.)
      call NhlFRLSetmdfloatarray(rlist,'sfDataArray',z,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'sfXCStartV',-18.,ierr)
      call NhlFRLSetFloat(rlist,'sfXCEndV',52.,ierr)
      call NhlFRLSetFloat(rlist,'sfYCStartV',-35.,ierr)
      call NhlFRLSetFloat(rlist,'sfYCEndV',38.,ierr)
      call NhlFCreate(dataid,'DataPlot',nhlfscalarfieldclass,appid,
     1                rlist,ierr)
C
C Create a ContourPlot object using the above data field, and make sure
C the LabelBar is displayed.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'cnScalarFieldData',dataid,ierr)
      call NhlFRLSetString(rlist,'pmLabelBarDisplayMode','always',ierr)
      call NhlFCreate(conid,'con1',nhlfcontourplotclass,wid,rlist,ierr)
C
C Create a map object, specifying the areas we want filled and masked.
C     
      call NhlFRLClear(rlist)
      call NhlFRLSetStringArray(rlist,'mpMaskAreaSpecifiers',maskspcs,
     +     50,ierr)
      call NhlFRLSetStringArray(rlist,'mpFillAreaSpecifiers',fillspcs,2,
     +     ierr)
      call NhlFRLSetFloat(rlist,'vpXF',0.1,ierr)
      call NhlFCreate(mapid,'map',nhlfmapplotclass,wid,rlist,ierr)

      call NhlFAddOverlay(mapid,conid,-1,ierr)
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the workstation object and exit.
C
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
