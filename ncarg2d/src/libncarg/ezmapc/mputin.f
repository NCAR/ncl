C
C $Id: mputin.f,v 1.1 1999-04-02 23:05:52 kennison Exp $
C
      SUBROUTINE MPUTIN (IPRJ,IZON,ISPH,PARA,UMIN,UMAX,VMIN,VMAX)
C
C Declare the USGS block data external to force it to load.
C
        EXTERNAL GTPZBD
C
        DOUBLE PRECISION PARA(15)
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /ERRMZ0/ IERR
        SAVE   /ERRMZ0/
C
        COMMON /ELLPZ0/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
          DOUBLE PRECISION AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
        SAVE   /ELLPZ0/
C
        COMMON /PROJZ0/ IPRO
        SAVE   /PROJZ0/
C
        COMMON /SPCSIR/ ISPHER,LU27,LU83,LEN
        SAVE   /SPCSIR/
C
        COMMON /USGSC1/ IPRF,UTPA(15),UUMN,UUMX,UVMN,UVMX
          DOUBLE PRECISION UTPA
        SAVE   /USGSC1/
C
C Initialize the error flag.
C
        IERR=0
C
C Initialize the projection number in the USGS code's common block.
C
        IPRO=IPRJ
C
C Initialize minimum and maximum values in the USGS code's common block.
C
        UUMN=UMIN
        UUMX=UMAX
        UVMN=VMIN
        UVMX=VMAX
C
C Initialize the spheroid parameters.
C
        ISPHER=ISPH
C
        IF (IPRJ.NE.2) CALL SPHDZ0 (ISPH,PARA)
C
C Call PJINIT to finish initializing the USGS code.
C
        CALL PJINIT (IPRJ,IZON,PARA)
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
