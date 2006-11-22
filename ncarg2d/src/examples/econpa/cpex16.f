
      PROGRAM CPEX16
C
C This program reads user data defining a global field of temperatures
C on what is called a POP grid and draws any or all of the following
C five plots: 1) the POP grid; 2) temperature contours; 3) color-filled
C temperature contour bands, drawn using AREAS; 4) the same contour
C bands, drawn using a cell array; and 5) the POP grid cells, filled
C in the colors chosen by CONPACK for the contour bands.  The values
C of the flags that determine which of these plots are drawn are set
C in DATA statements below.
C
C Of principal interest is the version of CPMPXY included, which solves
C the difficult problem of doing the inverse transformation (to find,
C given a position on the map, that point on the POP grid which maps
C into that position).
C
C Include definitions that "NetCDF" needs (now commented out, because
C the data are being read from an ASCII file, instead).
C
C       include 'netcdf.inc'
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C The parameters MCRA and NCRA declare the dimensions of the cell array,
C ICRA, to be used.  Using larger values of MCRA and NCRA gives a better
C picture, but also increases execution time and metafile size.  Using
C smaller values gives smaller execution times and smaller metafiles.
C The user must determine whether the picture given by a particular
C pair of values is acceptable or not.
C
C       PARAMETER (MCRA=  25,NCRA=  25)
C       PARAMETER (MCRA=  50,NCRA=  50)
C       PARAMETER (MCRA= 100,NCRA= 100)
C       PARAMETER (MCRA= 200,NCRA= 200)
C       PARAMETER (MCRA= 400,NCRA= 400)
C       PARAMETER (MCRA= 512,NCRA= 512)
        PARAMETER (MCRA= 800,NCRA= 800)
C       PARAMETER (MCRA=1000,NCRA=1000)
C       PARAMETER (MCRA=1024,NCRA=1024)
C       PARAMETER (MCRA=2048,NCRA=2048)
C
C Declare the lengths of workspace arrays to be passed to CONPACK, the
C area-map array to be used, and workspace arrays for ARSCAM.
C
        PARAMETER (LRWK=2000,LIWK=1000,LAMA=400000,NCRD=5000)
C
C Dimension arrays to receive the data we want to look at.  TLAT and
C TLON specify the positions of the centers of the POP grid boxes; the
C values in these arrays, as read from the source file, are in error
C near the North Pole, so this program recomputes some of the values.
C ULAT and ULON specify the positions of the corners of the POP grid
C boxes.  TEMP specifies temperatures at the centers of the POP grid
C boxes.
C
        DIMENSION TLAT(100,116),TLON(100,116)
        DIMENSION ULAT(100,116),ULON(100,116)
        DIMENSION TEMP(100,116)
C
C Declare arrays to hold information derived from ULAT and ULON, for
C use in calling IPIQSP or IPIQDP.  For each I and J, (UQSP(K,I,J),
C K=1,4) is a vector containing the single-precision cosine and sine
C of ULAT(I,J) and the single-precision cosine and sine of ULON(I,J),
C in that order.  UQDP is similar, but holds double-precision values.
C (The routines that do spherical geometry are given cosines and sines
C of latitudes and longitudes, instead of the latitudes and longitudes
C themselves, because this makes them run a lot faster.)
C
        DIMENSION        UQSP(4,100,116)
        DOUBLE PRECISION UQDP(4,100,116)
C
C Declare arrays that will define the positions of the center points
C of the grid on the globe and the data values associated with them.
C These arrays have one point of overlap in the first dimension.
C
        DIMENSION XLAT(101,116),XLON(101,116),ZDAT(101,116)
C
C Declare stuff that has to be passed to the mapping routine CPMPXY,
C including the arrays XQSP and XQDP, which are similar to UQDP and
C hold the cosines and sines of the latitudes and longitudes of the
C center points of the grid; a flag, ISOD, that says whether to use
C single- or double-precision arithmetic; and indices IBEG, IEND, JBEG,
C and JEND, that specify the grid box in which CPMPXY determined that
C the last point to be inverse-transformed lay.
C
        COMMON /TESTCM/ XQSP(4,101,116),XQDP(4,101,116),ISOD,
     +                  IBEG,IEND,JBEG,JEND
          DOUBLE PRECISION XQDP
        SAVE   /TESTCM/
C
C The arrays RWRK and IWRK are workspace arrays for CONPACK.
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C The array ICRA is the cell array.
C
        DIMENSION ICRA(MCRA,NCRA)
C
C The array IAMA is an area-map array.
C
        DIMENSION IAMA(LAMA)
C
C Declare arrays for ARSCAM to use in calling COLRAM - two for X and
C Y coordinate information and two for area-identifier information.
C
        DIMENSION XCRD(NCRD),YCRD(NCRD)
        DIMENSION IAIA(2),IGIA(2)
C
C Declare the routine that will be called by ARSCAM to color areas.
C
        EXTERNAL COLRAM
C
C Define a character temporary for index labels.
C
        CHARACTER*6 CTMP
C
C Declare double-precision multiplicative constants to convert from
C degrees to radians and vice-versa.
C
        DOUBLE PRECISION DRDP,RDDP
C
C Declare an array to use in timing calls.
C
C       DIMENSION TIME(2)
C
C Declare arrays into which to retrieve contour-level information and
C associated area identifiers.
C
        DIMENSION CLEV(256),KAIA(256),KAIB(256)
C
C Define flags that say whether or not to do timing calls and prints,
C whether or not to plot the POP grid, whether or not to label each grid
C cell center point on the POP grid, whether or not to do the plot of
C temperature contours, whether or not to do the fill area plot, whether
C or not to do the cell array plot, and whether or not to do a plot by
C filling POP grid cells directly.
C
C       DATA ITIM,IPOP,ILBL,ICON,IFIL,ICEL,IPGC /
C    +          1,   1,   0,   1,   1,   1,   1 /
        DATA      IPOP,ILBL,ICON,IFIL,ICEL,IPGC /
     +               1,   0,   1,   1,   1,   1 /
C
C Define single-precision and double-precision multiplicative constants
C to convert from degrees to radians and vice-versa.
C
        DATA DRSP / .017453292519943 /
        DATA RDSP / 57.2957795130823 /
C
        DATA DRDP / .017453292519943D0 /
        DATA RDDP / 57.2957795130823D0 /
C
C Print elapsed time at start.
C
C       IF (ITIM.NE.0) THEN
C         PRINT * , 'INITIALLY:'
C         IF (DTIME(TIME).GE.0.) THEN
C           PRINT * , '  USER TIME:  ',TIME(1)
C           PRINT * , '  SYSTEM TIME:',TIME(2)
C         ELSE
C           PRINT * , '  ERROR IN DTIME'
C         END IF
C       END IF
C
C R E A D   T H E   D A T A
C - - - -   - - -   - - - -
C
C The data for this example were originally read from a user's "NetCDF"
C file.  The code used for the purpose follows, but has been commented
C out, as the data are now read from an ASCII file.  This gets around
C certain procedural problems in running the example from "ncargex".
C
C Open the "NetCDF" file.
C
C       ISTA=NF_OPEN('cpex16.dat.nc',0,NCID)
C
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_OPEN: ',ISTA
C         STOP
C       END IF
C
C Read the array of latitudes of cell centers.
C
C       ISTA=NF_INQ_VARID(NCID,'TLAT',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,TLAT)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the array of longitudes of cell centers.
C
C       ISTA=NF_INQ_VARID(NCID,'TLONG',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,TLON)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the array of latitudes of cell corner points.
C
C       ISTA=NF_INQ_VARID(NCID,'ULAT',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,ULAT)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the array of longitudes of cell corner points.
C
C       ISTA=NF_INQ_VARID(NCID,'ULONG',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,ULON)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the array of temperature values.
C
C       ISTA=NF_INQ_VARID(NCID,'TEMP',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,TEMP)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Close the "NetCDF" file.
C
C       ISTA=NF_CLOSE(NCID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_CLOSE: ',ISTA
C         STOP
C       END IF
C
C Write the data to unit 11.  (These statements are commented out; they
C were used to create a "fort.11" which was then renamed "cpex16.dat".)
C
C       WRITE (11,'(5E16.8)') TLAT
C       WRITE (11,'(5E16.8)') TLON
C       WRITE (11,'(5E16.8)') ULAT
C       WRITE (11,'(5E16.8)') ULON
C       WRITE (11,'(5E16.8)') TEMP
C
C Read the required data from the ASCII file "cpex16.dat".
C
        OPEN (11,FILE='cpex16.dat',STATUS='OLD',FORM='FORMATTED')
C
        READ (11,'(5E16.8)') TLAT
        READ (11,'(5E16.8)') TLON
        READ (11,'(5E16.8)') ULAT
        READ (11,'(5E16.8)') ULON
        READ (11,'(5E16.8)') TEMP
C
        CLOSE (11)
C
C Set ISWP = 0 to leave the data alone, or = 1 to flip it end-for-end in
C the second subscript.
C
        ISWP=0
C
        IF (ISWP.NE.0) THEN
          DO 902 I=1,100
            DO 901 J=1,58
              WORK=TLAT(I,J)
              TLAT(I,J)=TLAT(I,117-J)
              TLAT(I,117-J)=WORK
              WORK=TLON(I,J)
              TLON(I,J)=TLON(I,117-J)
              TLON(I,117-J)=WORK
              WORK=ULAT(I,J)
              ULAT(I,J)=ULAT(I,117-J)
              ULAT(I,117-J)=WORK
              WORK=ULON(I,J)
              ULON(I,J)=ULON(I,117-J)
              ULON(I,117-J)=WORK
              WORK=TEMP(I,J)
              TEMP(I,J)=TEMP(I,117-J)
              TEMP(I,117-J)=WORK
  901       CONTINUE
  902     CONTINUE
        END IF
C
C Print elapsed time.
C
C       IF (ITIM.NE.0) THEN
C         PRINT * , 'AFTER READING INPUT DATA:'
C         IF (DTIME(TIME).GE.0.) THEN
C           PRINT * , '  USER TIME:  ',TIME(1)
C           PRINT * , '  SYSTEM TIME:',TIME(2)
C         ELSE
C           PRINT * , '  ERROR IN DTIME'
C         END IF
C       END IF
C
C C O M P U T E   O T H E R   Q U A N T I T I E S   N E E D E D
C - - - - - - -   - - - - -   - - - - - - - - - -   - - - - - -
C
C Set the flag ISOD, which says whether to use single-precision or
C double-precision arithmetic, by calling a subroutine to generate and
C return the three real numbers 1, 1+1E-10, and 1+2E-10.  If all three
C numbers appear to be different, single-precision arithmetic suffices
C and we set ISOD = 0; otherwise, we need double-precision arithmetic
C and we set ISOD = 1.  (The subroutine call is necessary to fool some
C compilers into storing TST1, TST2, and TST3; otherwise, we might use
C single precision on machines on which double precision should be.)
C
        CALL GETTRN (TST1,TST2,TST3)
C
        IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
          ISOD=0
        ELSE
          ISOD=1
        END IF
C
C Define the contents of one of the arrays UQSP (if ISOD=0) or UQDP (if
C ISOD=1).
C
        IF (ISOD.EQ.0) THEN
          DO 102 I=1,100
            DO 101 J=1,116
              UQSP(1,I,J)=COS(DRSP*ULAT(I,J))
              UQSP(2,I,J)=SIN(DRSP*ULAT(I,J))
              UQSP(3,I,J)=COS(DRSP*ULON(I,J))
              UQSP(4,I,J)=SIN(DRSP*ULON(I,J))
  101       CONTINUE
  102     CONTINUE
        ELSE
          DO 104 I=1,100
            DO 103 J=1,116
              UQDP(1,I,J)=COS(DRDP*DBLE(ULAT(I,J)))
              UQDP(2,I,J)=SIN(DRDP*DBLE(ULAT(I,J)))
              UQDP(3,I,J)=COS(DRDP*DBLE(ULON(I,J)))
              UQDP(4,I,J)=SIN(DRDP*DBLE(ULON(I,J)))
  103       CONTINUE
  104     CONTINUE
        END IF
C
C Define the contents of the arrays XLAT, XLON, XQSP or XQDP, and ZDAT,
C using one point of overlap in the "I" subscript.  Note that we use
C center-point positions of POP grid cells as corner-point positions
C of the grid to be contoured by CONPACK.  Note also that, instead of
C using all the center-point positions from the NetCDF file, we compute
C those for J .GT. 100 ourselves, thus fixing a problem caused by the
C fact that SCRIP was used to compute them in a manner which failed
C badly near the North Pole.
C
        DO 106 I=1,101
          III=MOD(I+99,100)+1
          IM1=MOD(I+98,100)+1
          DO 105 J=1,116
            IF (J.LE.100) THEN
              XLAT(I,J)=TLAT(III,J)
              XLON(I,J)=TLON(III,J)
              IF (ISOD.EQ.0) THEN
                XQSP(1,I,J)=COS(DRSP*XLAT(I,J))
                XQSP(2,I,J)=SIN(DRSP*XLAT(I,J))
                XQSP(3,I,J)=COS(DRSP*XLON(I,J))
                XQSP(4,I,J)=SIN(DRSP*XLON(I,J))
              ELSE
                XQDP(1,I,J)=COS(DRDP*DBLE(XLAT(I,J)))
                XQDP(2,I,J)=SIN(DRDP*DBLE(XLAT(I,J)))
                XQDP(3,I,J)=COS(DRDP*DBLE(XLON(I,J)))
                XQDP(4,I,J)=SIN(DRDP*DBLE(XLON(I,J)))
              END IF
            ELSE
              IF (ISOD.EQ.0) THEN
                CALL IPIQSP (UQSP(1,IM1,J-1),UQSP(1,III,J-1),
     +                       UQSP(1,IM1,J  ),UQSP(1,III,J  ),
     +                                     .5,.5,XQSP(1,I,J))
                XLAT(I,J)=RDSP*ASIN(XQSP(2,I,J))
                IF (XQSP(3,I,J).NE.0..OR.XQSP(4,I,J).NE.0.) THEN
                  XLON(I,J)=RDSP*ATAN2(XQSP(4,I,J),XQSP(3,I,J))
                ELSE
                  XLON(I,J)=0.
                END IF
              ELSE
                CALL IPIQDP (UQDP(1,IM1,J-1),UQDP(1,III,J-1),
     +                       UQDP(1,IM1,J  ),UQDP(1,III,J  ),
     +                                 .5D0,.5D0,XQDP(1,I,J))
                XLAT(I,J)=REAL(RDDP*ASIN(XQDP(2,I,J)))
                IF (XQDP(3,I,J).NE.0.D0.OR.XQDP(4,I,J).NE.0.D0) THEN
                  XLON(I,J)=REAL(RDDP*ATAN2(XQDP(4,I,J),XQDP(3,I,J)))
                ELSE
                  XLON(I,J)=0.
                END IF
              END IF
            END IF
            ZDAT(I,J)=TEMP(III,J)
  105     CONTINUE
  106   CONTINUE
C
C Initialize the values of the indices saying in which box of the grid
C a desired point was found by the last call to CPMPXY with IMAP = -3.
C
        IBEG=0
        IEND=0
        JBEG=0
        JEND=0
C
C Print elapsed time.
C
C       IF (ITIM.NE.0) THEN
C         PRINT * , 'AFTER SET-UP COMPUTATIONS:'
C         IF (DTIME(TIME).GE.0.) THEN
C           PRINT * , '  USER TIME:  ',TIME(1)
C           PRINT * , '  SYSTEM TIME:',TIME(2)
C         ELSE
C           PRINT * , '  ERROR IN DTIME'
C         END IF
C       END IF
C
C I N I T I A L I Z E   G K S
C - - - - - - - - - -   - - -
C
C Open GKS.
C
        CALL GOPKS (IERRF,0)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn GKS clipping off.
C
        CALL GSCLIP (0)
C
C Define some basic colors to use.
C
        CALL GSCR   (IWKID,2,.7,.7,.7)
        CALL GSCR   (IWKID,3,0.,1.,0.)
        CALL GSCR   (IWKID,4,1.,0.,0.)
        CALL GSCR   (IWKID,5,1.,1.,0.)
        CALL GSCR   (IWKID,6,0.,.7,.7)
        CALL GSCR   (IWKID,7,0.,0.,1.)
        CALL GSCR   (IWKID,8,.8,.5,.5)
        CALL GSCR   (IWKID,9,.5,.8,.5)
C
C I N I T I A L I Z E   V A R I O U S   U T I L I T I E S
C - - - - - - - - - -   - - - - - - -   - - - - - - - - -
C
C Tell PLOTCHAR to use font number 25 (a filled font), to outline each
C character, and what colors to use.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETI ('CC - CHARACTER COLOR',8)
        CALL PCSETI ('OC - OUTLINE COLOR',1)
C
C Tell PLOTCHAR to tell the Bezier package to reproduce the curves
C outlining the characters with a little less fidelity.  This cuts
C down on the size of the metafile.
C
        CALL PCSETR ('FB - FIDELITY OF BEZIER CURVES',.00015)
C
C Tell EZMAP what projection to use (choices given include cylindrical
C equidistant; orthographic, including the North Pole; orthographic,
C opposite side of globe from the second choice; orthographic, from
C above the North Pole; and polar stereographic).
C
C       CALL MAPROJ ('CE',  0.,  0.,0.)
        CALL MAPROJ ('OR',+40.,-20.,0.)
C       CALL MAPROJ ('OR',-40.,160.,0.)
C       CALL MAPROJ ('OR', 90.,  0.,0.)
C       CALL MAPROJ ('ST', 90.,  0.,0.)
C
C Tell EZMAP what part of the projection to draw (choices given specify
C a maximal useful view; a 30-degree field of view; and a 60-degree
C field of view).
C
        CALL MAPSET ('MA', 0., 0., 0., 0.)
C       CALL MAPSET ('AN',15.,15.,15.,15.)
C       CALL MAPSET ('AN',60.,60.,60.,60.)
C
C Initialize EZMAP (to make it call SET).
C
        CALL MAPINT
C
C Print elapsed time.
C
C       IF (ITIM.NE.0) THEN
C         PRINT * , 'AFTER INITIALIZING GKS AND VARIOUS UTILITIES:'
C         IF (DTIME(TIME).GE.0.) THEN
C           PRINT * , '  USER TIME:  ',TIME(1)
C           PRINT * , '  SYSTEM TIME:',TIME(2)
C         ELSE
C           PRINT * , '  ERROR IN DTIME'
C         END IF
C       END IF
C
C P L O T   T H E   P O P   G R I D
C - - - -   - - -   - - -   - - - -
C
C Plot the POP grid, but only if a flag is set.
C
        IF (IPOP.NE.0) THEN
C
C Draw a basic map.
C
          CALL MAPDRW
C
C Draw the corner-point grid in gray.
C
          CALL SFLUSH
          CALL GSPLCI (2)
C
          DO 108 I=1,100
            DO 107 J=1,116
              IF (J.EQ.1) THEN
                IPEN=0
              ELSE IF (J.NE.116) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (ULAT(I,J),ULON(I,J),IPEN)
  107       CONTINUE
            CALL MAPIQ
  108     CONTINUE
C
          DO 110 J=1,116
            DO 109 I=1,100
              K=MOD(I-1,100)+1
              IF (I.EQ.1) THEN
                IPEN=0
              ELSE IF (I.NE.100) THEN
                IPEN=1
              ELSE
                IPEN=2 
              END IF
              CALL MAPIT (ULAT(K,J),ULON(K,J),IPEN)
  109       CONTINUE
            CALL MAPIQ
  110     CONTINUE
C
C Draw the original center-point grid in a reddish-gray.
C
          CALL SFLUSH
          CALL GSPLCI (8)
C
          DO 112 I=1,100
            DO 111 J=1,116
              IF (J.EQ.1) THEN
                IPEN=0
              ELSE IF (J.NE.116) THEN
                IPEN=1
              ELSE
                IPEN=2 
              END IF
              CALL MAPIT (TLAT(I,J),TLON(I,J),IPEN)
  111       CONTINUE
            CALL MAPIQ
  112     CONTINUE
C
          DO 114 J=1,116
            DO 113 I=1,101
              K=MOD(I-1,100)+1
              IF (I.EQ.1) THEN
                IPEN=0
              ELSE IF (I.NE.101) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (TLAT(K,J),TLON(K,J),IPEN)
  113       CONTINUE
            CALL MAPIQ
  114     CONTINUE
C
C Mark the original center points in red.
C
          CALL SFLUSH
          CALL GSPLCI (4)
C
          DO 116 I=1,100
            DO 115 J=1,116
              CALL MAPTRA (TLAT(I,J),TLON(I,J),X,Y)
              IF (X.NE.1.E12) CALL POINT (X,Y)
  115       CONTINUE
  116     CONTINUE
C
C Draw the recomputed center-point grid in a greenish-gray.
C
          CALL SFLUSH
          CALL GSPLCI (9)
C
          DO 118 I=1,101
            DO 117 J=1,116
              IF (J.EQ.1) THEN
                IPEN=0
              ELSE IF (J.NE.116) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (XLAT(I,J),XLON(I,J),IPEN)
  117       CONTINUE
            CALL MAPIQ
  118     CONTINUE
C
          DO 120 J=1,116
            DO 119 I=1,101
              IF (I.EQ.1) THEN
                IPEN=0
              ELSE IF (I.NE.101) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (XLAT(I,J),XLON(I,J),IPEN)
  119       CONTINUE
            CALL MAPIQ
  120     CONTINUE
C
C Mark the recomputed center points in green.
C
          CALL SFLUSH
          CALL GSPLCI (3)
C
          DO 122 I=1,100
            DO 121 J=1,116
              CALL MAPTRA (XLAT(I,J),XLON(I,J),X,Y)
              IF (X.NE.1.E12) CALL POINT (X,Y)
  121       CONTINUE
  122     CONTINUE
C
C Draw the edges of the corner-point grid in yellow.
C
          CALL SFLUSH
          CALL GSPLCI (5)
C
          DO 124 I=1,100,99
            DO 123 J=1,116
              IF (J.EQ.1) THEN
                IPEN=0
              ELSE IF (J.NE.116) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (ULAT(I,J),ULON(I,J),IPEN)
  123       CONTINUE
            CALL MAPIQ
  124     CONTINUE
C
          DO 126 J=1,116,115
            DO 125 I=1,100
              IF (I.EQ.1) THEN
                IPEN=0
              ELSE IF (I.NE.100) THEN
                IPEN=1
              ELSE
                IPEN=2
              END IF
              CALL MAPIT (ULAT(I,J),ULON(I,J),IPEN)
  125       CONTINUE
            CALL MAPIQ
  126     CONTINUE
C
C If requested, label the original center points in dark cyan, using
C very small letters.
C
          IF (ILBL.NE.0) THEN
C
            CALL SFLUSH
            CALL GSPLCI (6)
C
            DO 129 I=1,100
              DO 128 J=1,116
                CALL MAPTRN (TLAT(I,J),TLON(I,J),X,Y)
                IF (X.NE.1.E12) THEN
                  WRITE (CTMP,'(2I3)') I,J
                  DO 127 K=1,6
                    IF (CTMP(K:K).EQ.' ') CTMP(K:K)='0'
  127             CONTINUE
                  CALL PLCHMQ (X,Y,CTMP,.0005,0.,0.)
                END IF
  128         CONTINUE
  129       CONTINUE
C
          END IF
C
C Label the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),'THE POP GRID',.018,0.,0.)
C
          CALL PLCHHQ (CFUX(.5),CFUY(.035),'Corner-point grid in gray (i
     +nterior) and yellow (edge), center-point grid in pink (original) a
     +nd green (re-computed).',.01,0.,0.)
C
          CALL PLCHHQ (CFUX(.5),CFUY(.015),'Try zooming in on the actual
     + North Pole (using "idt") to see problem area.',.01,0.,0.)
C
C Advance the frame.
C
          CALL FRAME
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER PLOTTING POP GRID:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C I N I T I A L I Z E   C O N P A C K
C - - - - - - - - - -   - - - - - - -
C
C Initialize CONPACK for the contour plot, the fill area plot, the cell
C array plot, and/or the POP-grid-cell-fill plot.
C
        IF (ICON.NE.0.OR.IFIL.NE.0.OR.ICEL.NE.0.OR.IPGC.NE.0) THEN
C
C Tell CONPACK not to do its own call to SET.
C
          CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK which mapping to use and what the "out-of-range" value
C is for that mapping.
C
          CALL CPSETI ('MAP - MAPPING FLAG',3)
          CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Tell CONPACK to use DASHPACK.
C
          CALL CPSETI ('DPU - DASH PATTERN USE FLAG',-3)
C
C Tell CONPACK to use about 40 contour levels.
C
          CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',40)
C
C Tell CONPACK what real number to consider the "special value".
C
          CALL CPSETR ('SPV',-99.)
C
C Tell CONPACK to draw the boundary of the grid, in blue.
C
          CALL CPSETI ('PAI',-1)
          CALL CPSETI ('CLU',1)
          CALL CPSETI ('CLC',7)
C
C Tell CONPACK to draw the boundary of "special value" areas, in red.
C
          CALL CPSETI ('PAI',-2)
          CALL CPSETI ('CLU',1)
          CALL CPSETI ('CLC',4)
C
C Initialize CONPACK, telling it where the data array and the two work
C arrays are.
C
          CALL CPRECT (ZDAT,101,101,116,RWRK,LRWK,IWRK,LIWK)
C
C Force CONPACK to pick a set of contour values.
C
          CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER CONPACK INITIALIZATION:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C P L O T   T E M P E R A T U R E   C O N T O U R S
C - - - -   - - - - - - - - - - -   - - - - - - - -
C
C Plot contours, but only if a flag is set.
C
        IF (ICON.NE.0) THEN
C
C Reset the polyline color index to white.
C
          CALL SFLUSH
          CALL GSPLCI (1)
C
C Draw the simple EZMAP background again.
C
          CALL SFLUSH
          CALL GSPLCI (1)
C
          CALL MAPDRW
C
C Reset the polyline color index to yellow.
C
          CALL SFLUSH
          CALL GSPLCI (5)
C
C Draw the contour lines.
C
          CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Label the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),'TEMPERATURE CONTOURS',
     +                                                       .018,0.,0.)
C
          CALL PLCHHQ (CFUX(.5),CFUY(.025),'Contours are yellow, the edg
     +e of the grid is blue, and the edges of special-value areas are re
     +d.',.01,0.,0.)
C
C Advance the frame.
C
          CALL FRAME
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER CONTOUR PLOT:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C D O   F U R T H E R   C O N P A C K   I N I T I A L I Z A T I O N
C - -   - - - - - - -   - - - - - - -   - - - - - - - - - - - - - -
C
C Do initialization required for the fill area and/or cell array and/or
C POP-grid-cell-fill plots.
C
        IF (IFIL.NE.0.OR.ICEL.NE.0.OR.IPGC.NE.0) THEN
C
C Find out how many contour levels CONPACK chose to use, which tells us
C how many color indices need to be defined for a color-filled contour
C plot.
C
          CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
C
C Tell CONPACK what color index to use for areas outside the grid.
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE',NOCL+2)
C
C Tell CONPACK what color index to use for special-value areas.
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE',NOCL+3)
C
C Tell CONPACK what color index to use for "out-of-range" areas.
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-3)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE',NOCL+4)
C
C Tell CONPACK the bias to use for color indices in the cell array.
C
          CALL CPSETI ('CAF - CELL ARRAY FLAG',101)
C
C Define the color indices.
C
          DO 130 ICIN=101,101+NOCL+4
            IF      (ICIN.EQ.101) THEN
C             color for ambiguously-specified areas:
              CALL GSCR (IWKID,ICIN,0.,1.,1.)
            ELSE IF (ICIN.LE.101+NOCL+1) THEN
              RR=REAL(ICIN-102)/REAL(NOCL)
              GG=0.
              BB=1.-RR
C             colors for bands 1 to NOCL+1:
              CALL GSCR (IWKID,ICIN,RR,GG,BB)
            ELSE IF (ICIN.EQ.101+NOCL+2) THEN
C             color for areas outside the bounds of the grid:
              CALL GSCR (IWKID,ICIN,.2,.5,.2)
            ELSE IF (ICIN.EQ.101+NOCL+3) THEN
C             color for special-value areas:
              CALL GSCR (IWKID,ICIN,.5,.5,.5)
            ELSE IF (ICIN.EQ.101+NOCL+4) THEN
C             color for out-of-range areas:
              CALL GSCR (IWKID,ICIN,.2,.2,.2)
            END IF
  130     CONTINUE
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER FURTHER CONPACK INITIALIZATION:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C D O   C O N T O U R   B A N D S   U S I N G   F I L L   A R E A S
C - -   - - - - - - -   - - - - -   - - - - -   - - - -   - - - - -
C
C Do the fill area plot, but only if a flag is set.
C
        IF (IFIL.NE.0) THEN
C
C Turn on debug plots from AREAS and tweak the appearance a little.
C
C         CALL ARSETI ('DB',   -1)
C
C         CALL ARSETI ('DC',  200)
C
C         CALL ARSETR ('AL',.0020)
C         CALL ARSETR ('AW',.0005)
C         CALL ARSETR ('ID',.0010)
C         CALL ARSETR ('IS',.0005)
C
C Initialize the area map.
C
          CALL ARINAM (IAMA,LAMA)
C
C Put contour lines into the area map.
C
          CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Write the contents of the area map to unit 11.
C
C         CALL ARDAMN (IAMA,11)
C
C Color the map.
C
          CALL ARSCAM (IAMA,XCRD,YCRD,NCRD,IAIA,IGIA,2,COLRAM)
C
C Draw the EZMAP map on top of the filled area map.
C
          CALL MAPDRW
C
C Label the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                   'TEMPERATURE BANDS (USING FILL AREAS)',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
          CALL FRAME
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER FILL AREA PLOT:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C           PRINT * , '  AREA MAP SPACE: ',IAMA(1)-(IAMA(6)-IAMA(5)-1)
C         END IF
C
        END IF
C
C D O   C O N T O U R   B A N D S   U S I N G   A   C E L L   A R R A Y
C - -   - - - - - - -   - - - - -   - - - - -   -   - - - -   - - - - -
C
C Do the cell array plot, but only if a flag is set.
C
        IF (ICEL.NE.0) THEN
C
C Generate a cell array.
C
          FCCL=.05
          FCCR=.95
          FCCB=.05
          FCCT=.95
C
          CALL CPCICA (ZDAT,RWRK,IWRK,ICRA,MCRA,MCRA,NCRA,
     +                                              FCCL,FCCB,FCCR,FCCT)
C
C Display the cell array.
C
          CALL GCA (CFUX(FCCL),CFUY(FCCB),CFUX(FCCR),CFUY(FCCT),
     +                                     MCRA,NCRA,1,1,MCRA,NCRA,ICRA)
C
C Draw the EZMAP map on top of the cell array.
C
          CALL MAPDRW
C
C Label the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                   'TEMPERATURE BANDS (USING A CELL ARRAY)',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
          CALL FRAME
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER CELL ARRAY PLOT:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C F I L L   P O P   G R I D   C E L L S
C - - - -   - - -   - - - -   - - - - -
C
C Do the POP-grid-cell-fill plot, but only if a flag is set.
C
        IF (IPGC.NE.0) THEN
C
C Retrieve contour levels and associated area identifiers.
C
        DO 131 I=1,NOCL
          CALL CPSETI ('PAI',I)
          CALL CPGETR ('CLV',CLEV(I))
          CALL CPGETI ('AIA',KAIA(I))
          CALL CPGETI ('AIB',KAIB(I))
  131   CONTINUE
C
C Step through the cells defined by the corner-point positions of the
C POP grid, filling each one in the color implied by the temperature
C at the center of the cell.
C
        DO 133 I=1,100
          IP1=MOD(I,100)+1
          DO 132 J=1,115
            NPTS=0
            CALL MAPTRA (ULAT(I,J),ULON(I,J),XVAL,YVAL)
            IF (XVAL.NE.1.E12) THEN
              NPTS=NPTS+1
              XCRD(NPTS)=XVAL
              YCRD(NPTS)=YVAL
            END IF
            CALL MAPTRA (ULAT(IP1,J),ULON(IP1,J),XVAL,YVAL)
            IF (XVAL.NE.1.E12) THEN
              NPTS=NPTS+1
              XCRD(NPTS)=XVAL
              YCRD(NPTS)=YVAL
            END IF
            CALL MAPTRA (ULAT(IP1,J+1),ULON(IP1,J+1),XVAL,YVAL)
            IF (XVAL.NE.1.E12) THEN
              NPTS=NPTS+1
              XCRD(NPTS)=XVAL
              YCRD(NPTS)=YVAL
            END IF
            CALL MAPTRA (ULAT(I,J+1),ULON(I,J+1),XVAL,YVAL)
            IF (XVAL.NE.1.E12) THEN
              NPTS=NPTS+1
              XCRD(NPTS)=XVAL
              YCRD(NPTS)=YVAL
            END IF
            IF (NPTS.GT.2) THEN
              IF (TEMP(IP1,J+1).NE.-99.) THEN
                CALL GETAID (TEMP(IP1,J+1),IAID,NOCL,CLEV,KAIA,KAIB)
              ELSE
                IAID=NOCL+3
              END IF
              CALL GSFACI (101+IAID)
              CALL GFA    (NPTS,XCRD,YCRD)
            END IF
  132     CONTINUE
  133   CONTINUE
C
C Draw the EZMAP map on top of the plot.
C
          CALL MAPDRW
C
C Label the plot.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +                   'FILLED POP GRID CELLS',.018,0.,0.)
C
C Advance the frame.
C
          CALL FRAME
C
C Print elapsed time.
C
C         IF (ITIM.NE.0) THEN
C           PRINT * , 'AFTER FILLING POP GRID CELLS:'
C           IF (DTIME(TIME).GE.0.) THEN
C             PRINT * , '  USER TIME:  ',TIME(1)
C             PRINT * , '  SYSTEM TIME:',TIME(2)
C           ELSE
C             PRINT * , '  ERROR IN DTIME'
C           END IF
C         END IF
C
        END IF
C
C C L O S E   G K S   A N D   Q U I T
C - - - - -   - - -   - - -   - - - -
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END


      SUBROUTINE GETTRN (TST1,TST2,TST3)
C
C This subroutine generates and returns the three real numbers 1,
C 1+1E-10, and 1+2E-10.  On a machine with 64-bit arithmetic, all
C three of these numbers will be different from each other, while,
C on machines with 32-bit arithmetic, two or more of them will be
C equal.  Thus, we can use these numbers to determine whether we
C should use single-precision or double-precision arithmetic.
C
        TST1=1.
        TST2=TST1+1.E-10
        TST3=TST2+1.E-10
C
        RETURN
C
      END


      SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOUT,YOUT)
C
C This version of CPMPXY implements what has sometimes, in the past,
C been called a "parameterized distortion", but it does it on the
C surface of the globe and then maps the globe as determined by the
C current state of EZMAP.  If IMAP has the value 3, then, for given
C input values of XINP and YINP, where XINP lies in the interval
C [1,IDIM] and YINP lies in the interval [1,JDIM], CPMPXY generates
C output values of XOUT and YOUT, representing the transformed position
C of a point on an IDIMxJDIM grid on the surface of the globe.  If IMAP
C has the value -3, CPMPXY performs the inverse transformation.
C
C For each (I,J), where 1.LE.I.LE.IDIM, and 1.LE.J.LE.JDIM, one of the
C vectors (XQSP(K,I,J),K=1,4) and (XQDP(K,I,J),K=1,4) is a 4-element
C vector containing the cosine of the latitude, the sine of the
C latitude, the cosine of the longitude, and the sine of the longitude
C of a point of the grid.  If ISOD is 0, the calling program must have
C set the values in XQSP; otherwise, it must have set the values in
C XQDP.  IBEG, IEND, JBEG, and JEND keep track of which box the last
C point inverse-transformed turned out to be in.  The calling routine
C is responsible for initializing all these values properly.
C
        PARAMETER (IDIM=101,JDIM=116,IDM1=IDIM-1,JDM1=JDIM-1)
C
        COMMON /TESTCM/ XQSP(4,IDIM,JDIM),XQDP(4,IDIM,JDIM),ISOD,
     +                  IBEG,IEND,JBEG,JEND
          DOUBLE PRECISION XQDP
        SAVE   /TESTCM/
C
C Declare arrays needed to pass information to the routines ICEGSP and
C ICEGDP.
C
        DIMENSION        RQSP(4)
        DOUBLE PRECISION RQDP(4)
C
C Declare some other double-precision variables needed.
C
        DOUBLE PRECISION DRDP,RDDP,DLAT,DLON,XODP,YODP,XFDP,YFDP
C
C Define single-precision and double-precision multiplicative constants
C to convert from degrees to radians and vice-versa.
C
        DATA DRSP / .017453292519943 /
        DATA RDSP / 57.2957795130823 /
C
        DATA DRDP / .017453292519943D0 /
        DATA RDDP / 57.2957795130823D0 /
C
C Do it.  If IMAP = 0, the caller wants information about the mapping
C specified by INT(XINP).  Returning YINP = 3 says that both forward
C and inverse mappings are implemented, while returning YINP = 0 says
C that neither is, and returning YINP = 1 says that only the forward
C mapping is implemented.
C
        IF (IMAP.EQ.0) THEN
C
          IF (INT(XINP).EQ.3) THEN
            YINP=3.
          ELSE
            YINP=0.
          END IF
C
C If IMAP = 3, a forward "parameterized distortion" is requested.  The
C input value XINP is expected to be in the range [1,101] and the input
C value YINP is expected to be in the range [1,116].  The values of XOUT
C and YOUT returned are coordinates of a point on a map of the globe, as
C defined by the current state of EZMAP.
C
        ELSE IF (IMAP.EQ.3) THEN
C
C IGRD and JGRD are the indices of the lower left corner of the grid
C box in which the input point falls.
C
          IGRD=MAX(1,MIN(IDM1,INT(XINP)))
          JGRD=MAX(1,MIN(JDM1,INT(YINP)))
C
C Interpolate in the grid box to find the desired point, get its
C latitude and longitude, and call the EZMAP transformation routine
C to project the point to the U/V plane.
C
          IF (ISOD.EQ.0) THEN
C
            CALL IPIQSP (XQSP(1,IGRD  ,JGRD  ),XQSP(1,IGRD+1,JGRD  ),
     +                   XQSP(1,IGRD  ,JGRD+1),XQSP(1,IGRD+1,JGRD+1),
     +                   XINP-REAL(IGRD),YINP-REAL(JGRD),RQSP)
C
            RLAT=RDSP*ASIN(RQSP(2))
C
            IF (RQSP(3).NE.0..OR.RQSP(4).NE.0.) THEN
              RLON=RDSP*ATAN2(RQSP(4),RQSP(3))
            ELSE
              RLON=0.
            END IF
C
            CALL MAPTRA (RLAT,RLON,XOUT,YOUT)
C
          ELSE
C
            CALL IPIQDP (XQDP(1,IGRD  ,JGRD  ),XQDP(1,IGRD+1,JGRD  ),
     +                   XQDP(1,IGRD  ,JGRD+1),XQDP(1,IGRD+1,JGRD+1),
     +                   DBLE(XINP)-DBLE(IGRD),DBLE(YINP)-DBLE(JGRD),
     +                                                          RQDP)
C
            DLAT=RDDP*ASIN(RQDP(2))
C
            IF (RQDP(3).NE.0.D0.OR.RQDP(4).NE.0.D0) THEN
              DLON=RDDP*ATAN2(RQDP(4),RQDP(3))
            ELSE
              DLON=0.D0
            END IF
C
            CALL MDPTRA (DLAT,DLON,XODP,YODP)
C
            IF (XODP.NE.1.D12) THEN
              XOUT=REAL(XODP)
              YOUT=REAL(YODP)
            ELSE
              XOUT=1.E12
              YOUT=1.E12
            END IF
C
          END IF
C
C If IMAP = -3, the inverse of a "parameterized distortion" is desired,
C which is rather more difficult.  XINP and YINP are the coordinates of
C a point on the map and the values of XOUT and YOUT returned are the
C coordinates of a point on the grid.
C
        ELSE IF (IMAP.EQ.-3) THEN
C
C Use either single-precision or double-precision code.
C
          IF (ISOD.EQ.0) THEN
C
C First, call EZMAP to get the latitude and longitude of the inverse of
C the specified point.
C
            CALL MAPTRI (XINP,YINP,RLAT,RLON)
C
C If the area is not in a defined area of the EZMAP projection, return
C the "out-of-range" value.
C
            IF (RLAT.EQ.1.E12) THEN
C
              XOUT=1.E12
              YOUT=1.E12
C
C Otherwise ...
C
            ELSE
C
C ... compute quantities needed for tests below.
C
              RQSP(1)=COS(DRSP*RLAT)
              RQSP(2)=SIN(DRSP*RLAT)
              RQSP(3)=COS(DRSP*RLON)
              RQSP(4)=SIN(DRSP*RLON)
C
C Check a little area around the last box used (in hopes of quickly
C finding the correct box often enough so as to speed the process up).
C
              IF (IEND-IBEG.EQ.1.AND.JEND-JBEG.EQ.1) THEN
C
C Jump if in same box:
C
                ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICSP.EQ.0) GO TO 103
C
C Jump if in adjacent box:
C
                IBEG=MAX(   1,IBEG-1)
                IEND=MIN(IDIM,IEND+1)
                JBEG=MAX(   1,JBEG-1)
                JEND=MIN(JDIM,JEND+1)
                ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICSP.EQ.0) GO TO 103
C
C Jump if in near-by box:
C
                IBEG=MAX(   1,IBEG-1)
                IEND=MIN(IDIM,IEND+1)
                JBEG=MAX(   1,JBEG-1)
                JEND=MIN(JDIM,JEND+1)
                ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICSP.EQ.0) GO TO 103
C
              END IF
C
C If that didn't work, look for a smaller piece of the grid containing
C the point we want.  (Starting with the entire grid is problematical
C because of the way ICEGSP works.)
C
              IEND=1
C
              DO 102 I=1,3
                IBEG=IEND
                IEND=I*IDIM/3
                JEND=1
                DO 101 J=1,3
                  JBEG=JEND
                  JEND=J*JDIM/3
                  ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                  IF (ICSP.EQ.0) GO TO 103
  101           CONTINUE
  102         CONTINUE
C
C Point was not in any piece of the grid, so treat it as outside the
C grid.
C
              XOUT=0.
              YOUT=0.
C
              GO TO 107
C
C Now, find out what particular grid cell contains the point.
C
  103         IF (IEND-IBEG.GE.JEND-JBEG) THEN
                IF (IEND-IBEG.EQ.1) THEN
                  CALL FPIQSP (XQSP(1,IBEG,JBEG),XQSP(1,IEND,JBEG),
     +                         XQSP(1,IBEG,JEND),XQSP(1,IEND,JEND),
     +                         RQSP,XFSP,YFSP)
                  IF (XFSP.LT.0.) THEN
                    XOUT=0.
                    YOUT=0.
                  ELSE
                    XOUT=REAL(IBEG)+XFSP
                    YOUT=REAL(JBEG)+YFSP
                  END IF
                  GO TO 107
                END IF
                IMID=(IBEG+IEND)/2
                ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IMID,JBEG,JEND)
                IF (ICSP.NE.0) THEN
                  IBEG=IMID
                ELSE
                  IEND=IMID
                END IF
              ELSE
                JMID=(JBEG+JEND)/2
                ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JMID)
                IF (ICSP.NE.0) THEN
                  JBEG=JMID
                ELSE
                  JEND=JMID
                END IF
              END IF
C
C The following code may be uncommented to check for the following
C pathological situation: A point is found to be in a particular
C portion of the grid, but not in either "half" of that portion.
C (I have not yet seen this happen with this version of the code,
C that deals with the globe, but I did see it happen with a version
C in the plane.)
C
C*            ICSP=ICEGSP(RQSP,XQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
C*            IF (ICSP.NE.0) THEN
C*              PRINT * , 'ALGORITHM FAILURE'
C*              PRINT * , 'IBEG,IEND,JBEG,JEND,ICSP = ',
C*   +                     IBEG,IEND,JBEG,JEND,ICSP
C*              XOUT=0.
C*              YOUT=0.
C*              GO TO 107
C*            END IF
C
              GO TO 103
C
            END IF
C
          ELSE
C
C First, call EZMAP to get the latitude and longitude of the inverse of
C the specified point.
C
            CALL MDPTRI (DBLE(XINP),DBLE(YINP),DLAT,DLON)
C
C If the area is not in a defined area of the EZMAP projection, return
C the "out-of-range" value.
C
            IF (DLAT.EQ.1.D12) THEN
C
              XOUT=1.E12
              YOUT=1.E12
C
C Otherwise ...
C
            ELSE
C
C ... compute quantities needed for tests below.
C
              RQDP(1)=COS(DRDP*DLAT)
              RQDP(2)=SIN(DRDP*DLAT)
              RQDP(3)=COS(DRDP*DLON)
              RQDP(4)=SIN(DRDP*DLON)
C
C Check a little area around the last box used (in hopes of quickly
C finding the correct box often enough so as to speed the process up).
C
              IF (IEND-IBEG.EQ.1.AND.JEND-JBEG.EQ.1) THEN
                ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICDP.EQ.0) GO TO 106  !  Same box.
                IBEG=MAX(   1,IBEG-1)
                IEND=MIN(IDIM,IEND+1)
                JBEG=MAX(   1,JBEG-1)
                JEND=MIN(JDIM,JEND+1)
                ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICDP.EQ.0) GO TO 106  !  Adjacent box.
                IBEG=MAX(   1,IBEG-1)
                IEND=MIN(IDIM,IEND+1)
                JBEG=MAX(   1,JBEG-1)
                JEND=MIN(JDIM,JEND+1)
                ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                IF (ICDP.EQ.0) GO TO 106  !  Near-by box.
              END IF
C
C If that didn't work, look for a smaller piece of the grid containing
C the point we want.  (Starting with the entire grid is problematical
C because of the way ICEGDP works.)
C
              IEND=1
C
              DO 105 I=1,3
                IBEG=IEND
                IEND=I*IDIM/3
                JEND=1
                DO 104 J=1,3
                  JBEG=JEND
                  JEND=J*JDIM/3
                  ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
                  IF (ICDP.EQ.0) GO TO 106  !  Box (I,J).
  104           CONTINUE
  105         CONTINUE
C
C Point was not in any piece of the grid, so treat it as outside the
C grid.
C
              XOUT=0.
              YOUT=0.
C
              GO TO 107
C
C Now, find out what particular grid cell contains the point.
C
  106         IF (IEND-IBEG.GE.JEND-JBEG) THEN
                IF (IEND-IBEG.EQ.1) THEN
                  CALL FPIQDP (XQDP(1,IBEG,JBEG),XQDP(1,IEND,JBEG),
     +                         XQDP(1,IBEG,JEND),XQDP(1,IEND,JEND),
     +                         RQDP,XFDP,YFDP)
                  IF (XFDP.LT.0.D0) THEN
                    XOUT=0.
                    YOUT=0.
                  ELSE
                    XOUT=REAL(IBEG)+REAL(XFDP)
                    YOUT=REAL(JBEG)+REAL(YFDP)
                  END IF
                  GO TO 107
                END IF
                IMID=(IBEG+IEND)/2
                ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IMID,JBEG,JEND)
                IF (ICDP.NE.0) THEN
                  IBEG=IMID
                ELSE
                  IEND=IMID
                END IF
              ELSE
                JMID=(JBEG+JEND)/2
                ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JMID)
                IF (ICDP.NE.0) THEN
                  JBEG=JMID
                ELSE
                  JEND=JMID
                END IF
              END IF
C
C The following code may be uncommented to check for the following
C pathological situation: A point is found to be in a particular
C portion of the grid, but not in either "half" of that portion.
C (I have not yet seen this happen with this version of the code,
C that deals with the globe, but I did see it happen with a version
C in the plane.)
C
C*            ICDP=ICEGDP(RQDP,XQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
C*            IF (ICDP.NE.0) THEN
C*              PRINT * , 'ALGORITHM FAILURE'
C*              PRINT * , 'IBEG,IEND,JBEG,JEND,ICDP = ',
C*   +                     IBEG,IEND,JBEG,JEND,ICDP
C*              XOUT=0.
C*              YOUT=0.
C*              GO TO 107
C*            END IF
C
              GO TO 106
C
            END IF
          END IF
C
C If IMAP is anything else, log an error.  If error recovery is on,
C control returns to the caller; otherwise, SETER terminates execution.
C
        ELSE
C
          CALL SETER ('CPMPXY - UNKNOWN MAPPING',5,1)
C
        END IF
C
C Done.
C
  107   RETURN
C
      END


      SUBROUTINE IPIQSP (AQSP,BQSP,CQSP,DQSP,XFRA,YFRA,EQSP)
C
C (IPIQSP = Interpolate Point In Quadrilateral, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4),CQSP(4),DQSP(4),EQSP(4)
C
C This routine, given four points on the sphere (A, B, C, and D) forming
C a "quadrilateral" and two interpolation fractions (XFRA and YFRA, each
C between 0 and 1, inclusive), finds a point E defined by the following
C diagram and returns it.
C
C                              C------Q----D
C                              |      |    |
C                              |      E    |
C                              |      |    |
C                              |      |    |
C                              A------P----B
C
C P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
C positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
C "the shortest great circle distance from X to Y").
C
C It is assumed that the "quadrilateral" ABDC is "convex" (a working
C definition of which might be that none of the four great circles
C defined by its edges - the ones through A and B, B and D, D and C,
C and C and A - cross it anywhere.  However, this is not verified.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DIMENSION PQSP(4),QQSP(4)
C
C The code is easy:
C
        CALL IPGCSP (AQSP,BQSP,XFRA,PQSP)
        CALL IPGCSP (CQSP,DQSP,XFRA,QQSP)
        CALL IPGCSP (PQSP,QQSP,YFRA,EQSP)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE FPIQSP (AQSP,BQSP,CQSP,DQSP,EQSP,XFRA,YFRA)
C
C (FPIQSP = Find Point In Quadrilateral, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4),CQSP(4),DQSP(4),EQSP(4)
C
C This routine, given four points (A, B, C, and D) that form a
C "quadrilateral" on the surface of the globe, and a surface point
C (E) in its interior, returns the two interpolation fractions that
C one would use in a call to IPIQSP to get the point E.  See comments
C in IPIQSP; here's a duplicate of the diagram from that routine:
C
C                              C------Q----D
C                              |      |    |
C                              |      E    |
C                              |      |    |
C                              |      |    |
C                              A------P----B
C
C P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
C positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
C "the shortest great circle distance from X to Y").
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DIMENSION PQSP(4),QQSP(4)
C
C We use a fast iterative technique to find XFRA.
C
        XFR1=0.
        DST1=DPGCSP(AQSP,CQSP,EQSP)
        XFR2=1.
        DST2=DPGCSP(BQSP,DQSP,EQSP)
        IF (DST1*DST2.GE.0.) THEN
          IF (ABS(DST1).LT.ABS(DST2)) THEN
            XFRA=0.
          ELSE
            XFRA=1.
          END IF
          CALL IPGCSP (AQSP,BQSP,XFRA,PQSP)
          CALL IPGCSP (CQSP,DQSP,XFRA,QQSP)
        ELSE
  101     XFRA=(DST2*XFR1-DST1*XFR2)/(DST2-DST1)
          CALL IPGCSP (AQSP,BQSP,XFRA,PQSP)
          CALL IPGCSP (CQSP,DQSP,XFRA,QQSP)
          IF (XFRA.LE.XFR1.OR.XFRA.GE.XFR2) GO TO 104
          DSTA=DPGCSP(PQSP,QQSP,EQSP)
          IF (DSTA.EQ.0.) GO TO 104
          IF (DST1*DSTA.GT.0.) THEN
            XFR1=XFRA
            DST1=DSTA
          ELSE
            XFR2=XFRA
            DST2=DSTA
          END IF
          IF (XFR2-XFR1.GT.1.E-6) GO TO 101
        END IF
C
C Computing YFRA is easier:
C
  104   TMP1=ADGCSP(PQSP,EQSP)
        TMP2=ADGCSP(PQSP,QQSP)
        IF (TMP2.NE.0.) THEN
          YFRA=TMP1/TMP2
        ELSE
          YFRA=0.
        END IF
C
C Normal exit.
C
        RETURN
C
      END


      SUBROUTINE IPGCSP (AQSP,BQSP,FRAC,CQSP)
C
C (IPGCSP = Interpolate Point on Great Circle, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4),CQSP(4)
C
C This routine, given two points A and B on the surface of the globe and
C a fraction FRAC, between 0. and 1., interpolates a point C along the
C shortest great circle route joining A to B such that the distance from
C A to C, divided by the distance from A to B, is equal to FRAC.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQSP(1)*BQSP(3)
C       YCB1=BQSP(1)*BQSP(4)
C       ZCB1=BQSP(2)
C
C       XCB2=XCB1*AQSP(3)+YCB1*AQSP(4)
C       YCB2=YCB1*AQSP(3)-XCB1*AQSP(4)
C       ZCB2=ZCB1
C
        XCB2=BQSP(1)*BQSP(3)*AQSP(3)+BQSP(1)*BQSP(4)*AQSP(4)
        YCB2=BQSP(1)*BQSP(4)*AQSP(3)-BQSP(1)*BQSP(3)*AQSP(4)
        ZCB2=BQSP(2)
C
        XCB3=XCB2*AQSP(1)+ZCB2*AQSP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQSP(1)-XCB2*AQSP(2)
C
C Now, rotate about the X axis by the angle ALPH = -ATAN(ZCB3/YCB3),
C which leaves the position of A unchanged but carries B into a point
C in the XY plane.
C
        IF (YCB3.NE.0..OR.ZCB3.NE.0.) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          CALP=YCB3/DNOM
          SALP=ZCB3/DNOM
        ELSE
          CALP=1.
          SALP=0.
        END IF
C
        XCB4=XCB3
        YCB4=YCB3*CALP+ZCB3*SALP
C       ZCB4=ZCB3*CALP-YCB3*SALP
C
C The angle BETA from A to B can now be computed easily.
C
        IF (XCB4.NE.0..OR.YCB4.NE.0.) THEN
          BETA=ATAN2(YCB4,XCB4)
        ELSE
          BETA=0.
        END IF
C
C Interpolate a point C at the desired position between the points A and
C B and map it back to its original position on the great circle route
C from A to B.
C
C       GAMA=FRAC*BETA
C
C       XCC1=COS(GAMA)
C       YCC1=SIN(GAMA)
C       ZCC1=0.
C
C       XCC2=XCC1
C       YCC2=YCC1*CALP-ZCC1*SALP
C       ZCC2=ZCC1*CALP+YCC1*SALP
C
        XCC2=COS(FRAC*BETA)
        YCC2=SIN(FRAC*BETA)*CALP
        ZCC2=SIN(FRAC*BETA)*SALP
C
        XCC3=XCC2*AQSP(1)-ZCC2*AQSP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQSP(1)+XCC2*AQSP(2)
C
        XCC4=XCC3*AQSP(3)-YCC3*AQSP(4)
        YCC4=YCC3*AQSP(3)+XCC3*AQSP(4)
        ZCC4=ZCC3
C
        CQSP(1)=SQRT(XCC4*XCC4+YCC4*YCC4)
        CQSP(2)=ZCC4
        CQSP(3)=XCC4/CQSP(1)
        CQSP(4)=YCC4/CQSP(1)
C
C Done.
C
        RETURN
C
      END


      FUNCTION ACEGSP (PQSP,QQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
C
C (ACEGSP = Angular Change along Edge of Grid, Single Precision)
C
        DIMENSION PQSP(4),QQSP(4,IDIM,JDIM)
C
C The value of this function is the total angle swept out by a vector
C tangent to the sphere at the point P and pointing in the direction of
C the shortest great circle route to a point tracing Q, the outer edge
C of a "grid" defined by the points (QQSP(I,J)), for I from IBEG to IEND
C and J from JBEG to JEND.  (The edge of the grid is formed of shortest
C great circle routes from point to point.)
C
C The outer edge of the grid divides the surface of the sphere into two
C areas - one to the left, and one to the right, of Q.  Let P' denote
C the point opposite P on the sphere.  Note that P' = (-PLAT,PLON+180).
C In theory (ignoring computational inaccuracies), the function can only
C have three possible values: +360, if P is to the left of Q and P' is
C to the right of Q; -360, if P is to the right of Q and P' is to the
C left of Q; and zero, if both P and P' are on the same side of Q (left
C or right, but we don't know which).
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        ANCH=0.
C
C "Bottom" edge:
C
        DO 101 I=IBEG,IEND-1
          ANCH=ANCH+ABGCSP(PQSP,QQSP(1,I,JBEG),QQSP(1,I+1,JBEG))
  101   CONTINUE
C
C "Right" edge:
C
        DO 102 J=JBEG,JEND-1
          ANCH=ANCH+ABGCSP(PQSP,QQSP(1,IEND,J),QQSP(1,IEND,J+1))
  102   CONTINUE
C
C "Top" edge:
C
        DO 103 I=IEND,IBEG+1,-1
          ANCH=ANCH+ABGCSP(PQSP,QQSP(1,I,JEND),QQSP(1,I-1,JEND))
  103   CONTINUE
C
C "Left" edge:
C
        DO 104 J=JEND,JBEG+1,-1
          ANCH=ANCH+ABGCSP(PQSP,QQSP(1,IBEG,J),QQSP(1,IBEG,J-1))
  104   CONTINUE
C
C Set the function value.
C
        ACEGSP=ANCH
C
C Done.
C
        RETURN
C
      END


      FUNCTION ABGCSP (AQSP,BQSP,CQSP)
C
C (ABGCSP = Angle Between Great Circles, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4),CQSP(4)
C
C This function, given information about the points A, B, and C on the
C sphere, returns the angle, in degrees, from the great circle through
C A and B to the great circle through A and C, positive if the point C
C is in the hemisphere to the "left" of the great circle through A and
C B, negative otherwise.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQSP(1)*BQSP(3)
C       YCB1=BQSP(1)*BQSP(4)
C       ZCB1=BQSP(2)
C
C       XCB2=XCB1*AQSP(3)+YCB1*AQSP(4)
C       YCB2=YCB1*AQSP(3)-XCB1*AQSP(4)
C       ZCB2=ZCB1
C
        XCB2=BQSP(1)*BQSP(3)*AQSP(3)+BQSP(1)*BQSP(4)*AQSP(4)
        YCB2=BQSP(1)*BQSP(4)*AQSP(3)-BQSP(1)*BQSP(3)*AQSP(4)
        ZCB2=BQSP(2)
C
C       XCB3=XCB2*AQSP(1)+ZCB2*AQSP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQSP(1)-XCB2*AQSP(2)
C
C Do the same for the point C.
C
C       XCC1=CQSP(1)*CQSP(3)
C       YCC1=CQSP(1)*CQSP(4)
C       ZCC1=CQSP(2)
C
C       XCC2=XCC1*AQSP(3)+YCC1*AQSP(4)
C       YCC2=YCC1*AQSP(3)-XCC1*AQSP(4)
C       ZCC2=ZCC1
C
        XCC2=CQSP(1)*CQSP(3)*AQSP(3)+CQSP(1)*CQSP(4)*AQSP(4)
        YCC2=CQSP(1)*CQSP(4)*AQSP(3)-CQSP(1)*CQSP(3)*AQSP(4)
        ZCC2=CQSP(2)
C
C       XCC3=XCC2*AQSP(1)+ZCC2*AQSP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQSP(1)-XCC2*AQSP(2)
C
C Now, rotate C about the X axis by an amount which would put B on the
C equator.
C
        IF (YCB3.NE.0.OR.ZCB3.NE.0.) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          CANG=YCB3/DNOM
          SANG=ZCB3/DNOM
        ELSE
          CANG=1.
          SANG=0.
        END IF
C
C       XCC4=XCC3
        YCC4=YCC3*CANG+ZCC3*SANG
        ZCC4=ZCC3*CANG-YCC3*SANG
C
C The angle between the great circles is now easily computed.
C
        IF (YCC4.NE.0..OR.ZCC4.NE.0.) THEN
          ABGCSP=RTOD*ATAN2(ZCC4,YCC4)
        ELSE
          ABGCSP=0.
        END IF
C
C Done.
C
        RETURN
C
      END


      FUNCTION DPGCSP (AQSP,BQSP,CQSP)
C
C (DPGCSP = Distance of Point from Great Circle, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4),CQSP(4)
C
C This function, given points A, B, and C on the globe, returns the
C directed distance, in degrees of arc, from the great circle through
C A and B to the point C, positive if the point C is in the hemisphere
C to the "left" of the great circle and negative otherwise.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQSP(1)*BQSP(3)
C       YCB1=BQSP(1)*BQSP(4)
C       ZCB1=BQSP(2)
C
C       XCB2=XCB1*AQSP(3)+YCB1*AQSP(4)
C       YCB2=YCB1*AQSP(3)-XCB1*AQSP(4)
C       ZCB2=ZCB1
C
        XCB2=BQSP(1)*BQSP(3)*AQSP(3)+BQSP(1)*BQSP(4)*AQSP(4)
        YCB2=BQSP(1)*BQSP(4)*AQSP(3)-BQSP(1)*BQSP(3)*AQSP(4)
        ZCB2=BQSP(2)
C
C       XCB3=XCB2*AQSP(1)+ZCB2*AQSP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQSP(1)-XCB2*AQSP(2)
C
C Do the same for C.
C
C       XCC1=CQSP(1)*CQSP(3)
C       YCC1=CQSP(1)*CQSP(4)
C       ZCC1=CQSP(2)
C
C       XCC2=XCC1*AQSP(3)+YCC1*AQSP(4)
C       YCC2=YCC1*AQSP(3)-XCC1*AQSP(4)
C       ZCC2=ZCC1
C
        XCC2=CQSP(1)*CQSP(3)*AQSP(3)+CQSP(1)*CQSP(4)*AQSP(4)
        YCC2=CQSP(1)*CQSP(4)*AQSP(3)-CQSP(1)*CQSP(3)*AQSP(4)
        ZCC2=CQSP(2)
C
C       XCC3=XCC2*AQSP(1)+ZCC2*AQSP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQSP(1)-XCC2*AQSP(2)
C
C Rotate C about the X axis by the angle ALPH required to put B on the
C equator.
C
C       IF (YCB3.NE.0..OR.ZCB3.NE.0.) THEN
C         DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
C         CALP=YCB3/DNOM
C         SALP=ZCB3/DNOM
C       ELSE
C         CALP=1.
C         SALP=0.
C       END IF
C
C       XCC4=XCC3
C       YCC4=YCC3*CALP+ZCC3*SALP
C       ZCC4=ZCC3*CALP-YCC3*SALP
C
C Return the latitude of the point C as the value of the function.
C
C       DPGCSP=RTOD*ASIN(ZCC3*CALP-YCC3*SALP)
C
C The following code should be a little faster in the normal case and
C it returns the distance from A to C in the degenerate case when A
C and B are the same point.
C
        IF (YCB3.NE.0..OR.ZCB3.NE.0.) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          DPGCSP=RTOD*ASIN((ZCC3*YCB3-YCC3*ZCB3)/DNOM)
        ELSE
          DPGCSP=ADGCSP(AQSP,CQSP)
        END IF
C
C Done.
C
        RETURN
C
      END


      FUNCTION ADGCSP (AQSP,BQSP)
C
C (ADGCSP = Angle in Degrees along Great Circle, Single Precision)
C
        DIMENSION AQSP(4),BQSP(4)
C
C This function returns the shortest great circle distance, in degrees,
C between two points, A and B, on the surface of the globe.
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Define a multiplicative constant to convert from radians to degrees
C and multiply by two.
C
        DATA RDTT / 114.5915590261646 /
C
        ADGCSP=RDTT*
     +         ASIN(SQRT((AQSP(1)*AQSP(3)-BQSP(1)*BQSP(3))**2+
     +                   (AQSP(1)*AQSP(4)-BQSP(1)*BQSP(4))**2+
     +                   (AQSP(2)        -BQSP(2)        )**2)/2.)
C
        RETURN
C
      END


      FUNCTION ICEGSP(PQSP,QQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
C
C (ICEGSP = Integer Check along Edge of Grid, Single Precision)
C
        DIMENSION PQSP(4),QQSP(4,IDIM,JDIM)
C
C The value of this function is zero if and only if the point P is
C inside the smaller of the two portions of the sphere formed by the
C boundary Q, which is the outer edge of a "grid" defined by the points
C (QQSP(I,J)), for I from IBEG to IEND and J from JBEG to JEND.  (The
C edge of the grid is formed of shortest great circle routes from point
C to point.)
C
C All variables with names of the form XQSP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C If the total angle swept out by a vector tangent to the sphere at the
C point P and pointing in the direction of the shortest great circle
C route to a point tracing Q is near zero, then both the point P and its
C antipodal point P' are in the same area, which must therefore be the
C larger of the two areas created by Q.
C
        IF (ABS(ACEGSP(PQSP,QQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)).LT.
     +                                                        180.) THEN
          ICEGSP=1
          RETURN
        END IF
C
C Otherwise, P is in one of the two areas and P' is in the other, so
C we perform a somewhat heuristic test to see if P is in the smaller
C of the two areas: we compute the average distance from P to Q and
C see if it is less than 90 degrees or more than 90 degrees.
C
        ADST=0.
C
C "Bottom" edge:
C
        DO 101 I=IBEG,IEND-1
          ADST=ADST+ADGCSP(PQSP,QQSP(1,I,JBEG))
  101   CONTINUE
C
C "Right" edge:
C
        DO 102 J=JBEG,JEND-1
          ADST=ADST+ADGCSP(PQSP,QQSP(1,IEND,J))
  102   CONTINUE
C
C "Top" edge:
C
        DO 103 I=IEND,IBEG+1,-1
          ADST=ADST+ADGCSP(PQSP,QQSP(1,I,JEND))
  103   CONTINUE
C
C "Left" edge:
C
        DO 104 J=JEND,JBEG+1,-1
          ADST=ADST+ADGCSP(PQSP,QQSP(1,IBEG,J))
  104   CONTINUE
C
C Set the function value.
C
        ADST=ADST/REAL(IEND-IBEG+JEND-JBEG)/2.
C
        IF (ADST.LT.90.) THEN
          ICEGSP=0
        ELSE
          ICEGSP=1
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE IPIQDP (AQDP,BQDP,CQDP,DQDP,XFRA,YFRA,EQDP)
C
C (IPIQDP = Interpolate Point In Quadrilateral, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4),DQDP(4),EQDP(4)
        DOUBLE PRECISION XFRA,YFRA
C
C This routine, given four points on the sphere (A, B, C, and D) forming
C a "quadrilateral" and two interpolation fractions (XFRA and YFRA, each
C between 0 and 1, inclusive), finds a point E defined by the following
C diagram and returns it.
C
C                              C------Q----D
C                              |      |    |
C                              |      E    |
C                              |      |    |
C                              |      |    |
C                              A------P----B
C
C P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
C positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
C "the shortest great circle distance from X to Y").
C
C It is assumed that the "quadrilateral" ABDC is "convex" (a working
C definition of which might be that none of the four great circles
C defined by its edges - the ones through A and B, B and D, D and C,
C and C and A - cross it anywhere.  However, this is not verified.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION PQDP(4),QQDP(4)
C
C The code is easy:
C
        CALL IPGCDP (AQDP,BQDP,XFRA,PQDP)
        CALL IPGCDP (CQDP,DQDP,XFRA,QQDP)
        CALL IPGCDP (PQDP,QQDP,YFRA,EQDP)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE FPIQDP (AQDP,BQDP,CQDP,DQDP,EQDP,XFRA,YFRA)
C
C (FPIQDP = Find Point In Quadrilateral, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4),DQDP(4),EQDP(4)
        DOUBLE PRECISION XFRA,YFRA
C
C This routine, given four points (A, B, C, and D) that form a
C "quadrilateral" on the surface of the globe, and a surface point
C (E) in its interior, returns the two interpolation fractions that
C one would use in a call to IPIQSP to get the point E.  See comments
C in IPIQSP; here's a duplicate of the diagram from that routine:
C
C                              C------Q----D
C                              |      |    |
C                              |      E    |
C                              |      |    |
C                              |      |    |
C                              A------P----B
C
C P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
C positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
C "the shortest great circle distance from X to Y").
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION PQDP(4),QQDP(4),XFR1,XFR2,DST1,DST2,DSTA,TMP1,
     +                   TMP2,DPGCDP,ADGCDP
C
C We use a fast iterative technique to find XFRA.
C
        XFR1=0.D0
        DST1=DPGCDP(AQDP,CQDP,EQDP)
        XFR2=1.D0
        DST2=DPGCDP(BQDP,DQDP,EQDP)
        IF (DST1*DST2.GE.0.D0) THEN
          IF (ABS(DST1).LT.ABS(DST2)) THEN
            XFRA=0.D0
          ELSE
            XFRA=1.D0
          END IF
          CALL IPGCDP (AQDP,BQDP,XFRA,PQDP)
          CALL IPGCDP (CQDP,DQDP,XFRA,QQDP)
        ELSE
  101     XFRA=(DST2*XFR1-DST1*XFR2)/(DST2-DST1)
          CALL IPGCDP (AQDP,BQDP,XFRA,PQDP)
          CALL IPGCDP (CQDP,DQDP,XFRA,QQDP)
          IF (XFRA.LE.XFR1.OR.XFRA.GE.XFR2) GO TO 104
          DSTA=DPGCDP(PQDP,QQDP,EQDP)
          IF (DSTA.EQ.0.D0) GO TO 104
          IF (DST1*DSTA.GT.0.D0) THEN
            XFR1=XFRA
            DST1=DSTA
          ELSE
            XFR2=XFRA
            DST2=DSTA
          END IF
          IF (XFR2-XFR1.GT.1.D-12) GO TO 101
        END IF
C
C Computing YFRA is easier:
C
  104   TMP1=ADGCDP(PQDP,EQDP)
        TMP2=ADGCDP(PQDP,QQDP)
        IF (TMP2.NE.0.D0) THEN
          YFRA=TMP1/TMP2
        ELSE
          YFRA=0.D0
        END IF
C
C Normal exit.
C
        RETURN
C
      END


      SUBROUTINE IPGCDP (AQDP,BQDP,FRAC,CQDP)
C
C (IPGCDP = Interpolate Point on Great Circle, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4),FRAC
C
C This routine, given two points A and B on the surface of the globe and
C a fraction FRAC, between 0. and 1., interpolates a point C along the
C shortest great circle route joining A to B such that the distance from
C A to C, divided by the distance from A to B, is equal to FRAC.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
C       DOUBLE PRECISION XCB1,YCB1,ZCB1
        DOUBLE PRECISION XCB2,YCB2,ZCB2
        DOUBLE PRECISION XCB3,YCB3,ZCB3
        DOUBLE PRECISION XCB4,YCB4
        DOUBLE PRECISION XCC2,YCC2,ZCC2
        DOUBLE PRECISION XCC3,YCC3,ZCC3
        DOUBLE PRECISION XCC4,YCC4,ZCC4
        DOUBLE PRECISION DNOM,CALP,SALP,BETA
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQDP(1)*BQDP(3)
C       YCB1=BQDP(1)*BQDP(4)
C       ZCB1=BQDP(2)
C
C       XCB2=XCB1*AQDP(3)+YCB1*AQDP(4)
C       YCB2=YCB1*AQDP(3)-XCB1*AQDP(4)
C       ZCB2=ZCB1
C
        XCB2=BQDP(1)*BQDP(3)*AQDP(3)+BQDP(1)*BQDP(4)*AQDP(4)
        YCB2=BQDP(1)*BQDP(4)*AQDP(3)-BQDP(1)*BQDP(3)*AQDP(4)
        ZCB2=BQDP(2)
C
        XCB3=XCB2*AQDP(1)+ZCB2*AQDP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQDP(1)-XCB2*AQDP(2)
C
C Now, rotate about the X axis by the angle ALPH = -ATAN(ZCB3/YCB3),
C which leaves the position of A unchanged but carries B into a point
C in the XY plane.
C
        IF (YCB3.NE.0.D0.OR.ZCB3.NE.0.D0) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          CALP=YCB3/DNOM
          SALP=ZCB3/DNOM
        ELSE
          CALP=1.D0
          SALP=0.D0
        END IF
C
        XCB4=XCB3
        YCB4=YCB3*CALP+ZCB3*SALP
C       ZCB4=ZCB3*CALP-YCB3*SALP
C
C The angle BETA from A to B can now be computed easily.
C
        IF (XCB4.NE.0.D0.OR.YCB4.NE.0.D0) THEN
          BETA=ATAN2(YCB4,XCB4)
        ELSE
          BETA=0.D0
        END IF
C
C Interpolate a point C at the desired position between the points A and
C B and map it back to its original position on the great circle route
C from A to B.
C
C       GAMA=FRAC*BETA
C
C       XCC1=COS(GAMA)
C       YCC1=SIN(GAMA)
C       ZCC1=0.
C
C       XCC2=XCC1
C       YCC2=YCC1*CALP-ZCC1*SALP
C       ZCC2=ZCC1*CALP+YCC1*SALP
C
        XCC2=COS(FRAC*BETA)
        YCC2=SIN(FRAC*BETA)*CALP
        ZCC2=SIN(FRAC*BETA)*SALP
C
        XCC3=XCC2*AQDP(1)-ZCC2*AQDP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQDP(1)+XCC2*AQDP(2)
C
        XCC4=XCC3*AQDP(3)-YCC3*AQDP(4)
        YCC4=YCC3*AQDP(3)+XCC3*AQDP(4)
        ZCC4=ZCC3
C
        CQDP(1)=SQRT(XCC4*XCC4+YCC4*YCC4)
        CQDP(2)=ZCC4
        CQDP(3)=XCC4/CQDP(1)
        CQDP(4)=YCC4/CQDP(1)
C
C Done.
C
        RETURN
C
      END


      DOUBLE PRECISION FUNCTION ACEGDP (PQDP,QQDP,IDIM,JDIM,
     +                                  IBEG,IEND,JBEG,JEND)
C
C (ACEGDP = Angular Change along Edge of Grid, Double Precision)
C
        DOUBLE PRECISION PQDP(4),QQDP(4,IDIM,JDIM)
C
C The value of this function is the total angle swept out by a vector
C tangent to the sphere at the point P and pointing in the direction of
C the shortest great circle route to a point tracing Q, the outer edge
C of a "grid" defined by the points (QQDP(I,J)), for I from IBEG to IEND
C and J from JBEG to JEND.  (The edge of the grid is formed of shortest
C great circle routes from point to point.)
C
C The outer edge of the grid divides the surface of the sphere into two
C areas - one to the left, and one to the right, of Q.  Let P' denote
C the point opposite P on the sphere.  Note that P' = (-PLAT,PLON+180).
C In theory (ignoring computational inaccuracies), the function can only
C have three possible values: +360, if P is to the left of Q and P' is
C to the right of Q; -360, if P is to the right of Q and P' is to the
C left of Q; and zero, if both P and P' are on the same side of Q (left
C or right, but we don't know which).
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION ANCH,ABGCDP
C
        ANCH=0.D0
C
C "Bottom" edge:
C
        DO 101 I=IBEG,IEND-1
          ANCH=ANCH+ABGCDP(PQDP,QQDP(1,I,JBEG),QQDP(1,I+1,JBEG))
  101   CONTINUE
C
C "Right" edge:
C
        DO 102 J=JBEG,JEND-1
          ANCH=ANCH+ABGCDP(PQDP,QQDP(1,IEND,J),QQDP(1,IEND,J+1))
  102   CONTINUE
C
C "Top" edge:
C
        DO 103 I=IEND,IBEG+1,-1
          ANCH=ANCH+ABGCDP(PQDP,QQDP(1,I,JEND),QQDP(1,I-1,JEND))
  103   CONTINUE
C
C "Left" edge:
C
        DO 104 J=JEND,JBEG+1,-1
          ANCH=ANCH+ABGCDP(PQDP,QQDP(1,IBEG,J),QQDP(1,IBEG,J-1))
  104   CONTINUE
C
C Set the function value.
C
        ACEGDP=ANCH
C
C Done.
C
        RETURN
C
      END


      DOUBLE PRECISION FUNCTION ABGCDP (AQDP,BQDP,CQDP)
C
C (ABGCDP = Angle Between Great Circles, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4)
C
C This function, given information about the points A, B, and C on the
C sphere, returns the angle, in degrees, from the great circle through
C A and B to the great circle through A and C, positive if the point C
C is in the hemisphere to the "left" of the great circle through A and
C B, negative otherwise.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
        DOUBLE PRECISION RTOD
C       DOUBLE PRECISION XCB1,YCB1,ZCB1
        DOUBLE PRECISION XCB2,YCB2,ZCB2
        DOUBLE PRECISION      YCB3,ZCB3
C       DOUBLE PRECISION XCC1,YCC1,ZCC1
        DOUBLE PRECISION XCC2,YCC2,ZCC2
        DOUBLE PRECISION      YCC3,ZCC3
        DOUBLE PRECISION      YCC4,ZCC4
        DOUBLE PRECISION DNOM,CANG,SANG
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823D0 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQDP(1)*BQDP(3)
C       YCB1=BQDP(1)*BQDP(4)
C       ZCB1=BQDP(2)
C
C       XCB2=XCB1*AQDP(3)+YCB1*AQDP(4)
C       YCB2=YCB1*AQDP(3)-XCB1*AQDP(4)
C       ZCB2=ZCB1
C
        XCB2=BQDP(1)*BQDP(3)*AQDP(3)+BQDP(1)*BQDP(4)*AQDP(4)
        YCB2=BQDP(1)*BQDP(4)*AQDP(3)-BQDP(1)*BQDP(3)*AQDP(4)
        ZCB2=BQDP(2)
C
C       XCB3=XCB2*AQDP(1)+ZCB2*AQDP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQDP(1)-XCB2*AQDP(2)
C
C Do the same for the point C.
C
C       XCC1=CQDP(1)*CQDP(3)
C       YCC1=CQDP(1)*CQDP(4)
C       ZCC1=CQDP(2)
C
C       XCC2=XCC1*AQDP(3)+YCC1*AQDP(4)
C       YCC2=YCC1*AQDP(3)-XCC1*AQDP(4)
C       ZCC2=ZCC1
C
        XCC2=CQDP(1)*CQDP(3)*AQDP(3)+CQDP(1)*CQDP(4)*AQDP(4)
        YCC2=CQDP(1)*CQDP(4)*AQDP(3)-CQDP(1)*CQDP(3)*AQDP(4)
        ZCC2=CQDP(2)
C
C       XCC3=XCC2*AQDP(1)+ZCC2*AQDP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQDP(1)-XCC2*AQDP(2)
C
C Now, rotate C about the X axis by an amount which would put B on the
C equator.
C
        IF (YCB3.NE.0D0.OR.ZCB3.NE.0.D0) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          CANG=YCB3/DNOM
          SANG=ZCB3/DNOM
        ELSE
          CANG=1.D0
          SANG=0.D0
        END IF
C
C       XCC4=XCC3
        YCC4=YCC3*CANG+ZCC3*SANG
        ZCC4=ZCC3*CANG-YCC3*SANG
C
C The angle between the great circles is now easily computed.
C
        IF (YCC4.NE.0.D0.OR.ZCC4.NE.0.D0) THEN
          ABGCDP=RTOD*ATAN2(ZCC4,YCC4)
        ELSE
          ABGCDP=0.D0
        END IF
C
C Done.
C
        RETURN
C
      END


      DOUBLE PRECISION FUNCTION DPGCDP (AQDP,BQDP,CQDP)
C
C (DPGCDP = Distance of Point from Great Circle, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4)
C
C This function, given points A, B, and C on the globe, returns the
C directed distance, in degrees of arc, from the great circle through
C A and B to the point C, positive if the point C is in the hemisphere
C to the "left" of the great circle and negative otherwise.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
C Code that is commented out either produces unneeded results or is
C superseded by code that executes faster.
C
        DOUBLE PRECISION RTOD
C       DOUBLE PRECISION XCB1,YCB1,ZCB1
        DOUBLE PRECISION XCB2,YCB2,ZCB2
        DOUBLE PRECISION      YCB3,ZCB3
C       DOUBLE PRECISION XCC1,YCC1,ZCC1
        DOUBLE PRECISION XCC2,YCC2,ZCC2
        DOUBLE PRECISION      YCC3,ZCC3
        DOUBLE PRECISION DNOM
C
        DOUBLE PRECISION ADGCDP
C
C Define a multiplicative constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823D0 /
C
C Get XYZ coordinates for B, rotate them by -ALON about the Z axis (which
C would put A on the prime meridian), and then rotate them by ALAT about
C the Y axis (which would put A on the equator).
C
C       XCB1=BQDP(1)*BQDP(3)
C       YCB1=BQDP(1)*BQDP(4)
C       ZCB1=BQDP(2)
C
C       XCB2=XCB1*AQDP(3)+YCB1*AQDP(4)
C       YCB2=YCB1*AQDP(3)-XCB1*AQDP(4)
C       ZCB2=ZCB1
C
        XCB2=BQDP(1)*BQDP(3)*AQDP(3)+BQDP(1)*BQDP(4)*AQDP(4)
        YCB2=BQDP(1)*BQDP(4)*AQDP(3)-BQDP(1)*BQDP(3)*AQDP(4)
        ZCB2=BQDP(2)
C
C       XCB3=XCB2*AQDP(1)+ZCB2*AQDP(2)
        YCB3=YCB2
        ZCB3=ZCB2*AQDP(1)-XCB2*AQDP(2)
C
C Do the same for C.
C
C       XCC1=CQDP(1)*CQDP(3)
C       YCC1=CQDP(1)*CQDP(4)
C       ZCC1=CQDP(2)
C
C       XCC2=XCC1*AQDP(3)+YCC1*AQDP(4)
C       YCC2=YCC1*AQDP(3)-XCC1*AQDP(4)
C       ZCC2=ZCC1
C
        XCC2=CQDP(1)*CQDP(3)*AQDP(3)+CQDP(1)*CQDP(4)*AQDP(4)
        YCC2=CQDP(1)*CQDP(4)*AQDP(3)-CQDP(1)*CQDP(3)*AQDP(4)
        ZCC2=CQDP(2)
C
C       XCC3=XCC2*AQDP(1)+ZCC2*AQDP(2)
        YCC3=YCC2
        ZCC3=ZCC2*AQDP(1)-XCC2*AQDP(2)
C
C Rotate C about the X axis by the angle ALPH required to put B on the
C equator.
C
C       IF (YCB3.NE.0.D0.OR.ZCB3.NE.0.D0) THEN
C         DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
C         CALP=YCB3/DNOM
C         SALP=ZCB3/DNOM
C       ELSE
C         CALP=1.D0
C         SALP=0.D0
C       END IF
C
C       XCC4=XCC3
C       YCC4=YCC3*CALP+ZCC3*SALP
C       ZCC4=ZCC3*CALP-YCC3*SALP
C
C Return the latitude of the point C as the value of the function.
C
C       DPGCDP=RTOD*ASIN(ZCC3*CALP-YCC3*SALP)
C
C The following code should be a little faster in the normal case and
C it returns the distance from A to C in the degenerate case when A
C and B are the same point.
C
        IF (YCB3.NE.0.D0.OR.ZCB3.NE.0.D0) THEN
          DNOM=SQRT(YCB3*YCB3+ZCB3*ZCB3)
          DPGCDP=RTOD*ASIN((ZCC3*YCB3-YCC3*ZCB3)/DNOM)
        ELSE
          DPGCDP=ADGCDP(AQDP,CQDP)
        END IF
C
C Done.
C
        RETURN
C
      END


      DOUBLE PRECISION FUNCTION ADGCDP (AQDP,BQDP)
C
C (ADGCDP = Angle in Degrees along Great Circle, Double Precision)
C
        DOUBLE PRECISION AQDP(4),BQDP(4)
C
C This function returns the shortest great circle distance, in degrees,
C between two points, A and B, on the surface of the globe.
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION RDTT
C
C Define a multiplicative constant to convert from radians to degrees
C and multiply by two.
C
        DATA RDTT / 114.5915590261646D0 /
C
        ADGCDP=RDTT*
     +         ASIN(SQRT((AQDP(1)*AQDP(3)-BQDP(1)*BQDP(3))**2+
     +                   (AQDP(1)*AQDP(4)-BQDP(1)*BQDP(4))**2+
     +                   (AQDP(2)        -BQDP(2)        )**2)/2.D0)
C
        RETURN
C
      END


      FUNCTION ICEGDP(PQDP,QQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
C
C (ICEGDP = Integer Check along Edge of Grid, Double Precision)
C
        DOUBLE PRECISION PQDP(4),QQDP(4,IDIM,JDIM)
C
C The value of this function is zero if and only if the point P is
C inside the smaller of the two portions of the sphere formed by the
C boundary Q, which is the outer edge of a "grid" defined by the points
C (QQDP(I,J)), for I from IBEG to IEND and J from JBEG to JEND.  (The
C edge of the grid is formed of shortest great circle routes from point
C to point.)
C
C All variables with names of the form XQDP are four-element arrays
C containing the cosine and sine of the latitude and the cosine and
C sine of the longitude, in that order, of the point X.  Describing
C the point positions in this way makes this routine execute faster
C than if the latitudes and longitudes themselves are used.
C
        DOUBLE PRECISION ACEGDP,ADGCDP,ADST
C
C If the total angle swept out by a vector tangent to the sphere at the
C point P and pointing in the direction of the shortest great circle
C route to a point tracing Q is near zero, then both the point P and its
C antipodal point P' are in the same area, which must therefore be the
C larger of the two areas created by Q.
C
        IF (ABS(ACEGDP(PQDP,QQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)).LT.
     +                                                      180.D0) THEN
          ICEGDP=1
          RETURN
        END IF
C
C Otherwise, P is in one of the two areas and P' is in the other, so
C we perform a somewhat heuristic test to see if P is in the smaller
C of the two areas: we compute the average distance from P to Q and
C see if it is less than 90 degrees or more than 90 degrees.
C
        ADST=0.D0
C
C "Bottom" edge:
C
        DO 101 I=IBEG,IEND-1
          ADST=ADST+ADGCDP(PQDP,QQDP(1,I,JBEG))
  101   CONTINUE
C
C "Right" edge:
C
        DO 102 J=JBEG,JEND-1
          ADST=ADST+ADGCDP(PQDP,QQDP(1,IEND,J))
  102   CONTINUE
C
C "Top" edge:
C
        DO 103 I=IEND,IBEG+1,-1
          ADST=ADST+ADGCDP(PQDP,QQDP(1,I,JEND))
  103   CONTINUE
C
C "Left" edge:
C
        DO 104 J=JEND,JBEG+1,-1
          ADST=ADST+ADGCDP(PQDP,QQDP(1,IBEG,J))
  104   CONTINUE
C
C Set the function value.
C
        ADST=ADST/DBLE(IEND-IBEG+JEND-JBEG)/2.D0
C
        IF (ADST.LT.90.D0) THEN
          ICEGDP=0
        ELSE
          ICEGDP=1
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAIA,IGIA,NAIA)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C The arrays XCRA and YCRA, for indices 1 to NCRA, contain the X and Y
C coordinates of points defining a polygon.  The area identifiers in
C the array IAIA, each with an associated group identifier in the array
C IGIA, tell us whether the polygon is to be color-filled or not.
C
C Assume the polygon will be filled until we find otherwise.
C
        IFLL=1
C
C If any of the area identifiers is negative, don't fill the polygon.
C
        DO 101 I=1,NAIA
          IF (IAIA(I).LT.0) IFLL=0
  101   CONTINUE
C
C Otherwise, fill the polygon in the color implied by its area
C identifier relative to edge group 3 (the contour-line group).
C
        IF (IFLL.NE.0) THEN
          IFLL=0
          DO 102 I=1,NAIA
            IF (IGIA(I).EQ.3) IFLL=IAIA(I)
  102     CONTINUE
          IF (IFLL.GE.0) THEN
            CALL GSFACI (101+IFLL)
            CALL GFA (NCRA-1,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE GETAID (ZVAL,IAID,NCLV,CLEV,IAIA,IAIB)
C
        DIMENSION CLEV(NCLV),IAIA(NCLV),IAIB(NCLV)
C
C Given a field value ZVAL, GETAID searches the current contour list to
C determine the area identifier IAID to be associated with that value.
C
C NCLV is the number of contour levels CONPACK is using, CLEV is a list
C of the levels, in ascending order, and IAIA and IAIB contain the area
C identifiers above and below the specified contour levels.
C
C Zero will be returned if nothing better turns up.
C
        IAID=0
C
C First, search forward for a value in the parameter array 'AIB'.  This
C search is complicated by the latitude the user is given in the way he
C or she defines the area identifiers to be associated with the contour
C bands.
C
        JCLV=0
C
        DO 101 KCLV=1,NCLV
          IF (ZVAL.LE.CLEV(KCLV)) THEN
            IF (JCLV.NE.0) THEN
              IF (CLEV(KCLV).NE.CLEV(JCLV)) GO TO 102
            END IF
            IF (IAIB(KCLV).NE.0) THEN
              IAID=IAIB(KCLV)
              GO TO 104
            ELSE IF (IAIA(KCLV).NE.0) THEN
              JCLV=KCLV
            END IF
          END IF
  101   CONTINUE
C
C If necessary, search backward, in the same way, for a value in the
C parameter array 'AIA'.
C
  102  JCLV=0
C
        DO 103 KCLV=NCLV,1,-1
          IF (ZVAL.GE.CLEV(KCLV)) THEN
            IF (JCLV.NE.0) THEN
              IF (CLEV(KCLV).NE.CLEV(JCLV)) GO TO 104
            END IF
            IF (IAIA(KCLV).NE.0) THEN
              IAID=IAIA(KCLV)
              GO TO 104
            ELSE IF (IAIB(KCLV).NE.0) THEN
              JCLV=KCLV
            END IF
          END IF
  103   CONTINUE
C
C Done.
C
  104   RETURN
C
      END


      SUBROUTINE ARDAMN (IAMA,IFLN)
        DIMENSION IAMA(*)
        WRITE (IFLN,'(2I10)') (I,IAMA(I),I=1,IAMA(5))
        WRITE (IFLN,'(2I10)') (I,IAMA(I),I=IAMA(6),IAMA(1))
        RETURN
      END
