C
C     $Id: mp03f.f,v 1.1 1995-01-24 23:27:42 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            mp03f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:08:50 MST 1995
C
C      Description:    	Demonstrates MapPlot masking; loosely emulates
C                       the LLU example 'colcon'
C
      external nhlfapplayerclass
      external nhlfncgmworkstationlayerclass
      external nhlfcontourlayerclass
      external nhlfscalarfieldlayerclass
      external nhlfmapplotlayerclass

      parameter(M = 50, N = 50)
      integer appid,wid,mapid,dataid,cnid
      integer rlist

      real z(M,N)
      integer mlow, mhigh
      data mlow,mhigh/13,13/
      real dlow,dhigh
      data dlow,dhigh/13.,18./
      integer len_dims(2)

      character*6 mask_specs(1) 
      data mask_specs/'oceans'/
C
C Initialize the high level utility library
C
      call nhlfinitialize
C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C The resource file sets most of the Contour resources that remain fixed
C throughout the life of the Contour object.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'mp03',nhlfapplayerclass,0,rlist,ierr)
C
C Create a meta file workstation
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'wkMetaName','./mp03f.ncgm',ierr)
      call nhlfcreate(wid,'mp03Work',nhlfncgmworkstationlayerclass,0,
     1    rlist,ierr)
C
C Call the Fortran routine 'GENDAT' to create the first array of contour
C data. Create a ScalarField data object and hand it the data created by
C 'GENDAT'. Define the extent of the data coordinates as the whole globe 
C
      call nhlfrlclear(rlist)
      len_dims(1) = N
      len_dims(2) = M
      call gendat(z,M,M,N,mlow,mhigh,dlow,dhigh)
      call nhlfrlsetmdfloatarray(rlist,'sfDataArray',z,2,len_dims,ierr)
      call nhlfrlsetinteger(rlist,'sfXCStartV',-180,ierr)
      call nhlfrlsetinteger(rlist,'sfXCEndV',180,ierr)
      call nhlfrlsetinteger(rlist,'sfYCStartV',-90,ierr)
      call nhlfrlsetinteger(rlist,'sfYCEndV',90,ierr)
      call nhlfcreate(dataid,'Gendat',nhlfscalarfieldlayerclass,appid,
     1        rlist,ierr)
C
C Create a Contour object, supplying the ScalarField object as data,
C and setting the size of the viewport.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'cnScalarFieldData',dataid,ierr)
      call nhlfrlsetstring(rlist,'cnLabelDrawOrder','postdraw',ierr)
      call nhlfcreate(cnid,'Contour1',nhlfcontourlayerclass,wid,rlist,
     1      ierr)
C
C Create a MapPlot object, setting the fill to draw over the main draw,
C and masking out the oceans.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'vpYF',0.775,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',0.45,ierr)
      call nhlfrlsetstring(rlist,'mpFillOn','true',ierr)
      call nhlfrlsetstring(rlist,'ovTitleDisplayMode','always',ierr)
      call nhlfrlsetstring(rlist,'tiMainString','mp03f',ierr)
      call nhlfrlsetstring(rlist,'mpFillDrawOrder','postdraw',ierr)
      call nhlfrlsetstringarray(rlist,'mpMaskAreaSpecifiers',mask_specs,
     1        1,ierr)
      call nhlfcreate(mapid,'Map1',nhlfmapplotlayerclass,wid,rlist,ierr)
C
C Overlay the Contour object on the MapPlot object
C
      call nhlfaddtooverlay(mapid,cnid,-1,ierr)
    
      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call nhlfdestroy(mapid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfclose
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
