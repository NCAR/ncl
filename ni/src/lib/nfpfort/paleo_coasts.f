c nclfortstart
      subroutine paleooutline(mask,zdat,lat,lon,nlat,nlon,jm,im,
     +                        iwrk,liwk,name,mskval)
      integer nlat,nlon
      integer im,jm,liwk
      real lat1,latn,lon1,lonn
      double precision mask(nlon,nlat), lat(nlat), lon(nlon)
      real zdat(im,jm), mskval
      character*(*) name
      integer iwrk(liwk)
c nclend
      integer i,j
C
C The subroutine SVBLED requires that the input mask array be twice as
C large(+1) as the orginal. This is an artificat of the original 
C program written for Pat Behling. 
C
      do i=1,im
         do j=1,jm
            zdat(i,j)=1.-mskval
         enddo
      enddo
      do i=1,nlon
         do j=1,nlat
            ZDAT(2*I,2*J)=REAL(mask(I,J))
         enddo
      enddo
C
C The last longitude point is not actually 360, but 360-xx. So 
C index 2*nlon+2 represents lon=360, and index 2*nlon+2
C represents lon=360-xx. Index 2 represents lon=0.
C
C For latitude, index 2 represents lat(1), and index 2*nlat represents
C lat(nlat). (Because index 0 should represent lat=-90, and index 
C 2*nlat+1 represents lat=90.)
C
C      lon1 = real(lon(1))
C      lonn = real(lon(nlon))
C
      ibot = 2
      lat1 = real(lat(1))
      itop = 2 * nlat
      latn = real(lat(nlat))

      ilft = 2
      lon1 = real(lon(1))
      irgt = 2 * nlon
      lonn = real(lon(nlon))
      lonn = 360.
C     
      CALL SVBLED (ZDAT,im,jm,IWRK,liwk,mskval,ilft,lon1,irgt,lonn,
     +     ibot,lat1,itop,latn,'Land','Water',name)

c ZDAT = data (idim,jdim)
c IWRK = scratch (LIWK) 
c SVAL = "special value".  The "special value" area of the
C data array is considered to be the union of all grid boxes having a
C special value at one or more of their corners.  

c XLFT = longitude of first subscript of ZDAT
c XRGT = longitude of last  subscript of ZDAT 
c YBOT = latitude  of first subscript of ZDAT
c YTOP = latitude  of last  subscript of ZDAT

c NAML and NAMR are the
C names of the areas to the left and right, respectively, of the
C boundary lines.  this program automatically puts the special value
c to the left, so if we are creating a landmask, then NAML='Land'
c and NAMR='Water'

c FLNM is the base name of the two files to which the EZMAP data are to be 
c written.

      return
      end

      SUBROUTINE SVBLED (ZDAT,IDIM,JDIM,IWRK,LIWK,SVAL,ILFT,XLFT,
     +                   IRGT,XRGT,JBOT,YBOT,JTOP,YTOP,NAML,NAMR,FLNM)
C
C This routine is adapted from the CONPACK routine CPTRES, which traces
C the edge of the special-value area in a contour field.  (The mnemonic
C SVBLED stands for "Special Value Boundary Line to Ezmap Dataset".)
C The boundary of the special-value area is generated in such a way
C that special values are to the left and non-special values are to the
C right.  SVBLED has the following arguments: ZDAT is an array of data,
C dimensioned IDIM by JDIM.  IWRK is an integer scratch array of length
C LIWK.  SVAL is the "special value".  The "special value" area of the
C data array is considered to be the union of all grid boxes having a
C special value at one or more of their corners.  XLFT is the longitude
C associated with a first subscript of ILFT and XRGT the longitude
C associated with a first subscript of IRGT.  YBOT is the latitude
C associated with a second subscript of JBOT and YTOP the latitude
C associated with a second subscript of JTOP.  NAML and NAMR are the
C names of the areas to the left and right, respectively, of the
C boundary lines.  FLNM is the base name of the two files to which the
C EZMAP data are to be written.
C
C Declare argument dimensions and sizes.
C
        DIMENSION ZDAT(IDIM,JDIM),IWRK(LIWK)
C
        CHARACTER*(*) NAML,NAMR,FLNM
C
C Declare local variables used in the tracing algorithm.
C
        DIMENSION INCX(8),JNCY(8)
C
C Declare local file-name variables.
C
        CHARACTER*128 FNM1,FNM2
C
C Declare arrays from which the data are to be written to the line
C data file.
C
        DIMENSION INTS(4),FLTS(204)
C
C Declare an array to use as an output buffer to the name data file.
C
        CHARACTER*1 CHRS(1024)
C
C Define vectors pointing in eight different directions for use in the
C tracing algorithm.
C
        DATA INCX / -1 , -1 ,  0 ,  1 ,  1 ,  1 ,  0 , -1 /
        DATA JNCY /  0 ,  1 ,  1 ,  1 ,  0 , -1 , -1 , -1 /
C
C Form the names of the two files to be written - the line data file
C and the name data file.
C
        LFNM=IOLNBC(FLNM)
C
        FNM1=FLNM(1:LFNM)//'.lines'//CHAR(0)
        FNM2=FLNM(1:LFNM)//'.names'//CHAR(0)
C
C Open the file to which the line data will be written.
C
        CALL NGOFWO (FNM1(1:LFNM+7),IFDE,ISTA)
C
        IF (ISTA.NE.0) THEN
          PRINT * , 'SVBLED - FAILED TO OPEN LINE DATA FILE'
          STOP
        END IF
C
        INTS(1)=0
        INTS(2)=1
        INTS(3)=1
        INTS(4)=2
C
        NHSS=0
C
        DO 107 I=1,IDIM-1
          DO 106 J=1,JDIM-1
            IF (J.EQ.1) THEN
              IF (ZDAT(I  ,1  ).EQ.SVAL.OR.
     +            ZDAT(I+1,1  ).EQ.SVAL.OR.
     +            ZDAT(I  ,2  ).EQ.SVAL.OR.
     +            ZDAT(I+1,2  ).EQ.SVAL) GO TO 101
            ELSE
              IF (ZDAT(I  ,J-1).NE.SVAL.AND.
     +            ZDAT(I+1,J-1).NE.SVAL.AND.
     +            ZDAT(I  ,J  ).NE.SVAL.AND.
     +            ZDAT(I+1,J  ).NE.SVAL.AND.
     +           (ZDAT(I  ,J+1).EQ.SVAL.OR.
     +            ZDAT(I+1,J+1).EQ.SVAL)) GO TO 101
            END IF
            GO TO 106
  101       IPXY=JDIM*I+J
            DO 102 K=1,NHSS
              IF (IPXY.EQ.IWRK(K)) GO TO 106
  102       CONTINUE
            IF (NHSS.GE.LIWK) THEN
              PRINT * , 'SVBLED - INTEGER WORKSPACE OVERFLOW'
              STOP
            END IF
            NHSS=NHSS+1
            IWRK(NHSS)=IPXY
            INTS(1)=0
            XCES=XLFT+(REAL(I-ILFT)/REAL(IRGT-ILFT))*(XRGT-XLFT)
            YCES=YBOT+(REAL(J-JBOT)/REAL(JTOP-JBOT))*(YTOP-YBOT)
            INSX=I
            JNSY=J
            INOX=I
            JNOY=J
            INDX=I+1
            JNDY=J
            IDIR=5
  103       XCSS=XCES
            YCSS=YCES
            XCES=XLFT+(REAL(INDX-ILFT)/REAL(IRGT-ILFT))*(XRGT-XLFT)
            YCES=YBOT+(REAL(JNDY-JBOT)/REAL(JTOP-JBOT))*(YTOP-YBOT)
            IDRW=1
            IF (INDX.EQ.INOX) THEN
              IF ((REAL(IDIM-1)/REAL(IRGT-ILFT))*(XRGT-XLFT).GT.359.99)
     +                                                              THEN
                IF (INDX.EQ.1) THEN
                  IF (ZDAT(IDIM  ,JNDY).EQ.SVAL.OR.
     +                ZDAT(IDIM-1,JNDY).EQ.SVAL.OR.
     +                ZDAT(IDIM  ,JNOY).EQ.SVAL.OR.
     +                ZDAT(IDIM-1,JNOY).EQ.SVAL) IDRW=0
                ELSE IF (INDX.EQ.IDIM) THEN
                  IF (ZDAT(1,JNDY).EQ.SVAL.OR.
     +                ZDAT(2,JNDY).EQ.SVAL.OR.
     +                ZDAT(1,JNOY).EQ.SVAL.OR.
     +                ZDAT(2,JNOY).EQ.SVAL) IDRW=0
                END IF
              END IF
            ELSE IF (JNDY.EQ.JNOY.AND.ABS(ABS(YCES)-90.).LT..01) THEN
              IDRW=0
            END IF
            IF (IDRW.EQ.0) THEN
              IF (INTS(1).NE.0) THEN
                CALL WTEMLR (IFDE,INTS,FLTS)
                INTS(1)=0
              END IF
            ELSE
              IF (INTS(1).EQ.0) THEN
                INTS(1)=2
                FLTS(5)=YCSS
                FLTS(6)=XCSS
              END IF
              IF (INTS(1).EQ.200) CALL WTEMLR (IFDE,INTS,FLTS)
              INTS(1)=INTS(1)+2
              FLTS(3+INTS(1))=YCES
              FLTS(4+INTS(1))=XCES
            END IF
            IF (INDX.EQ.INSX.AND.JNDY.EQ.JNSY) THEN
              IF (INTS(1).NE.0) THEN
                CALL WTEMLR (IFDE,INTS,FLTS)
                INTS(1)=0
              END IF
              GO TO 106
            END IF
            INOX=INDX
            JNOY=JNDY
            IDIR=MOD(IDIR+1,8)+1
            DO 104 K=1,3
              IF (IDIR.EQ.5) THEN
                IF (INOX.NE.IDIM.AND.JNOY.NE.JDIM) THEN
                  IF (JNOY.EQ.1) THEN
                    IF (ZDAT(INOX  ,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX  ,JNOY+1).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY+1).EQ.SVAL) GO TO 105
                  ELSE
                    IF (ZDAT(INOX  ,JNOY-1).NE.SVAL.AND.
     +                  ZDAT(INOX+1,JNOY-1).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX+1,JNOY  ).NE.SVAL.AND.
     +                 (ZDAT(INOX  ,JNOY+1).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY+1).EQ.SVAL)) GO TO 105
                  END IF
                END IF
              ELSE IF (IDIR.EQ.3) THEN
                IF (INOX.NE.1.AND.JNOY.NE.JDIM) THEN
                  IF (INOX.EQ.IDIM) THEN
                    IF (ZDAT(INOX  ,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX  ,JNOY+1).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY+1).EQ.SVAL) GO TO 105
                  ELSE
                    IF (ZDAT(INOX+1,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX+1,JNOY+1).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY+1).NE.SVAL.AND.
     +                 (ZDAT(INOX-1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY+1).EQ.SVAL)) GO TO 105
                  END IF
                END IF
              ELSE IF (IDIR.EQ.1) THEN
                IF (INOX.NE.1.AND.JNOY.NE.1) THEN
                  IF (JNOY.EQ.JDIM) THEN
                    IF (ZDAT(INOX  ,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX  ,JNOY-1).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY-1).EQ.SVAL) GO TO 105
                  ELSE
                    IF (ZDAT(INOX  ,JNOY+1).NE.SVAL.AND.
     +                  ZDAT(INOX-1,JNOY+1).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX-1,JNOY  ).NE.SVAL.AND.
     +                 (ZDAT(INOX  ,JNOY-1).EQ.SVAL.OR.
     +                  ZDAT(INOX-1,JNOY-1).EQ.SVAL)) GO TO 105
                  END IF
                END IF
              ELSE
                IF (INOX.NE.IDIM.AND.JNOY.NE.1) THEN
                  IF (INOX.EQ.1) THEN
                    IF (ZDAT(INOX  ,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX  ,JNOY-1).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY-1).EQ.SVAL) GO TO 105
                  ELSE
                    IF (ZDAT(INOX-1,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX-1,JNOY-1).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY  ).NE.SVAL.AND.
     +                  ZDAT(INOX  ,JNOY-1).NE.SVAL.AND.
     +                 (ZDAT(INOX+1,JNOY  ).EQ.SVAL.OR.
     +                  ZDAT(INOX+1,JNOY-1).EQ.SVAL)) GO TO 105
                  END IF
                END IF
              END IF
              IDIR=MOD(IDIR+5,8)+1
  104       CONTINUE
            PRINT * , 'SVBLED - EDGE-FOLLOWING ALGORITHM FAILURE'
            STOP
  105       INDX=INOX+INCX(IDIR)
            JNDY=JNOY+JNCY(IDIR)
            IF (IDIR.EQ.5) THEN
              IF (NHSS.GE.LIWK) THEN
                PRINT * , 'SVBLED - INTEGER WORKSPACE OVERFLOW'
                STOP
              END IF
              NHSS=NHSS+1
              IWRK(NHSS)=JDIM*INOX+JNOY
            END IF
            GO TO 103
  106     CONTINUE
  107   CONTINUE
C
C Write a final logical record to the line data file.
C
        INTS(1)=0
        INTS(2)=0
        INTS(3)=0
        INTS(4)=0
C
        CALL WTEMLR (IFDE,INTS,FLTS)
C
C Close the line data file.
C
        CALL NGCLFI (IFDE)
C
C Open the name data file, write the required items to it, and close it.
C
        CALL NGOFWO (FNM2(1:LFNM+7),IFDE,ISTA)
C
        IF (ISTA.NE.0) THEN
          PRINT * , 'SVBLED - FAILED TO OPEN NAME DATA FILE'
          STOP
        END IF
C
        NCHR=0
C
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   1)  !  index of 1st area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   1)  !  type of 1st area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   2)  !  color of 1st area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   0)  !  parent of 1st area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        IBEG=IOFNBC(NAML)
        IEND=IOLNBC(NAML)
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,IEND-IBEG+1)  !  name length
        CALL WRCHAR (IFDE,CHRS,1024,NCHR,        ' ')
        DO 108 I=IBEG,IEND
          CALL WRCHAR (IFDE,CHRS,1024,NCHR,NAML(I:I))  !  name of area
  108   CONTINUE
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   2)  !  index of 2nd area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   1)  !  type of 2nd area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   1)  !  color of 2nd area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,   0)  !  parent of 2nd area
        CALL WRCHAR (IFDE,CHRS,1024,NCHR, ' ')
        IBEG=IOFNBC(NAMR)
        IEND=IOLNBC(NAMR)
        CALL WRNUMB (IFDE,CHRS,1024,NCHR,IEND-IBEG+1)  !  name length
        CALL WRCHAR (IFDE,CHRS,1024,NCHR,        ' ')
        DO 109 I=IBEG,IEND
          CALL WRCHAR (IFDE,CHRS,1024,NCHR,NAMR(I:I))  !  name of area
  109   CONTINUE
C
        IF (NCHR.GT.0) THEN
          CALL NGWRCH (IFDE,CHRS,NCHR,ISTA)
          IF (ISTA.LE.0) THEN
            PRINT * , 'SVBLED - ERROR WRITING CHARS TO NAME DATA FILE'
            STOP
          END IF
        END IF
C
        CALL NGCLFI (IFDE)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE WTEMLR (IFDE,INTS,FLTS)
C
C Write a logical record to the EZMAP line data file.
C
        DIMENSION INTS(4),FLTS(204)
C
        CALL NGWRIN (IFDE,INTS,4,ISTA)
C
        IF (ISTA.NE.4) THEN
          PRINT * , 'WTEMLR - ERROR WRITING INTEGERS TO LINE DATA FILE'
          STOP
        END IF
C
        IF (INTS(1).NE.0) THEN
C
          FLTS(1)=FLTS(5)
          FLTS(2)=FLTS(5)
          FLTS(3)=FLTS(6)
          FLTS(4)=FLTS(6)
C
          DO 101 I=7,3+INTS(1),2
            FLTS(1)=MAX(FLTS(1),FLTS(I  ))
            FLTS(2)=MIN(FLTS(2),FLTS(I  ))
            FLTS(3)=MAX(FLTS(3),FLTS(I+1))
            FLTS(4)=MIN(FLTS(4),FLTS(I+1))
  101     CONTINUE
C
          CALL NGWRFL (IFDE,FLTS,4+INTS(1),ISTA)
C
          IF (ISTA.NE.4+INTS(1)) THEN
            PRINT * , 'WTEMLR - ERROR WRITING REALS TO LINE DATA FILE'
            STOP
          END IF
C
          FLTS(5)=FLTS(3+INTS(1))
          FLTS(6)=FLTS(4+INTS(1))
C
          INTS(1)=2
C
        END IF
C
        RETURN
C
      END



      SUBROUTINE WRNUMB (IFDE,CHRS,MCHR,NCHR,INUM)
C
        CHARACTER*1 CHRS(MCHR)
C
C Translate the integer INUM to a character form and put it in the
C buffer CHRS.
C
        CHARACTER*1 CTMP(20)
C
C Copy the integer to a local variable.
C
        ITMP=INUM
C
C Generate digits from right to left in the temporary buffer CTMP.
C
        NDGT=0
C
  101   NDGT=NDGT+1
C
        IF (NDGT.GT.20) THEN
          PRINT * , 'WRNUMB - INTEGER TOO BIG - STOP'
          STOP
        END IF
C
        CTMP(NDGT)=CHAR(ICHAR('0')+MOD(ITMP,10))
C
        ITMP=ITMP/10
        IF (ITMP.NE.0) GO TO 101
C
C Send the digits to the output buffer in correct order.
C
        DO 102 IDGT=NDGT,1,-1
          CALL WRCHAR (IFDE,CHRS,MCHR,NCHR,CTMP(IDGT))
  102   CONTINUE
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE WRCHAR (IFDE,CHRS,MCHR,NCHR,ICHR)
C
        CHARACTER*1 CHRS(MCHR),ICHR
C
C Put the character ICHR in the buffer CHRS.
C
        NCHR=NCHR+1
        CHRS(NCHR)=ICHR
C
C If the buffer is now full, dump it to the file specified by IFDE.
C
        IF (NCHR.GE.MCHR) THEN
          CALL NGWRCH (IFDE,CHRS,NCHR,ISTA)
          IF (ISTA.LE.0) THEN
            PRINT * , 'WRCHAR - BAD STATUS FROM NGWRCH = ',ISTA
            STOP
          END IF
          NCHR=0
        END IF
C
C Done.
C
        RETURN
C
      END



      INTEGER FUNCTION IOFNBC (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of IOFNBC(CHRS) is the index of the first non-blank in the
C character string CHRS.
C
        DO 101 I=1,LEN(CHRS)
          IF (CHRS(I:I).NE.' ') THEN
            IOFNBC=I
            RETURN
          END IF
  101   CONTINUE
C
        IOFNBC=1
C
        RETURN
C
      END



      INTEGER FUNCTION IOLNBC (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of IOLNBC(CHRS) is the index of the last non-blank in the
C character string CHRS.
C
        DO 101 I=LEN(CHRS),1,-1
          IF (CHRS(I:I).NE.' ') THEN
            IOLNBC=I
            RETURN
          END IF
  101   CONTINUE
C
        IOLNBC=1
C
        RETURN
C
      END
