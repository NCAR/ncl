C
C $Id: mputin.f,v 1.3 1999-04-19 22:09:57 kennison Exp $
C
      SUBROUTINE MPUTIN (IPRJ,IZON,ISPH,PADP,UMIN,UMAX,VMIN,VMAX)
C
        DOUBLE PRECISION PADP(15),UMIN,UMAX,VMIN,VMAX
C
        REAL PASP(15)
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
          DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
          INTEGER IPRF
        SAVE   /USGSC1/
C
C Declare the USGS "BLOCK DATA" external to force it to load.
C
        EXTERNAL GTPZBD
C
C Initialize minimum and maximum values in the USGS code's common block.
C
        UUMN=UMIN
        UUMX=UMAX
        UVMN=VMIN
        UVMX=VMAX
C
C Copy some input parameters for the call to PJINSP.
C
        ISSP=ISPH
C
        DO 101 I=1,15
          PASP(I)=REAL(PADP(I))
  101   CONTINUE
C
C Call PJINDP to initialize the double-precision form of the USGS code.
C
        CALL PJINDP (IPRJ,IZON,ISPH,PADP)
C
C Call PJINSP to initialize the single-precision form of the USGS code.
C
        CALL PJINSP (IPRJ,IZON,ISSP,PASP)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Done.
C
        RETURN
C
      END
