
      PROGRAM CCPMAP
C 
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
      
      PARAMETER (M=30,N=20,LRWK=3500,LIWK=3500)
C 
C United States corner latitude and longitudes
C 
      PARAMETER (RMNLON=-190.,RMXLON=20.,RMNLAT=-20.,RMXLAT=50.)
      
      REAL Z(M,N), RWRK(LRWK), RLAT1(2), RLAT2(2), RLON1(2), RLON2(2)
      INTEGER IWRK(LIWK)
      
      DATA RLAT1 /RMNLAT,0.0/
      DATA RLAT2 /RMXLAT,0.0/
      DATA RLON1 /RMNLON,0.0/
      DATA RLON2 /RMXLON,0.0/
      
      CALL MKDAT (1, Z, M, N) 
C 
C Open GKS
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C 
C Turn off clipping.
C 
      CALL GSCLIP (0)
C 
C Draw Lat/Lon lines at 10 degree intervals.
C Draw political & continental outlines.
C 
      CALL MAPSTI ('GR - GRID',10)
      CALL MAPSTC ('OU - OUTLINE DATASET','PO')
C 
C Draw a Satellite view over the United States
C 
      CALL MAPROJ ('SV - SATELLITE-VIEW',35.,-100.,0.)
      CALL MAPSET ('MA',RLAT1,RLON1,RLAT2,RLON2)
C 
C Don't draw a square around the globe
C 
      CALL MAPSTI ('PE - PERIMETER FLAG', 0)
C 
C Draw map.
C 
      CALL MAPDRW
C 
C Tell CONPACK that the SET call has been done, force it to generate X
C coordinates that are longitudes and Y coordinates that are latitudes,
C turn on mapping to an EZMAP background. Define the out-of-range value
C (returned by MAPTRN for an unprojectable point).
C 
      CALL CPSETI ('SET - DO SET-CALL FLAG',0)
      CALL CPSETR ('XC1 - X COORDINATE AT I = 1',RMNLON)
      CALL CPSETR ('XCM - X COORDINATE AT I = M',RMXLON)
      CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',RMNLAT)
      CALL CPSETR ('YCN - Y COORDINATE AT J = N',RMXLAT)
      CALL CPSETI ('MAP - MAPPING FLAG',1)
      CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C 
C Initialize the drawing of the first contour plot.
C 
      CALL CPRECT (Z,M,M,N,RWRK,LRWK,IWRK,LIWK)
C 
C Draw Contours
C 
      CALL CPCLDR(Z,RWRK,IWRK)
C Close frame and close GKS
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE MKDAT(NCTFR,ZDAT,M,N)
C 
C Create some data to contour
C 
      REAL ZDAT(M,N)
C 
C NCTFR is used to generate a random number, ZDAT is the data array
C 
      CALL GGDINI (0.,1.,NCTFR,.9)
      ZMIN= 1.E36
      ZMAX=-1.E36
      DO 10 I=1,M
         RLON=.017453292519943*(-180.+360.*REAL(I-1)/REAL(M-1))
         DO 20 J=1,N
            RLAT=.017453292519943*(-90.+180.*REAL(J-1)/REAL(N-1))
            ZDAT(I,J)=GGDPNT(RLAT,RLON)+.5*COS(4.*RLAT)
            ZMIN=MIN(ZMIN,ZDAT(I,J))
            ZMAX=MAX(ZMAX,ZDAT(I,J))
 20      CONTINUE
 10   CONTINUE
      DO 30 I=1,M
         DO 40 J=1,N
            ZDAT(I,J)=((ZDAT(I,J)-ZMIN)/(ZMAX-ZMIN))*130.-50.
 40      CONTINUE
 30   CONTINUE
      
      RETURN
      END
      
      
