C
C $Id: mdritd.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRITD (IAXS,ANGL,UCRD,VCRD,WCRD)
C
        INTEGER          IAXS
        DOUBLE PRECISION ANGL,UCRD,VCRD,WCRD
C
C Declare common block containing math constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
C Declare local variables.
C
        DOUBLE PRECISION SINA,COSA,UTMP,VTMP,WTMP
C
C This routine rotates the point with coordinates (UCRD,VCRD,WCRD) by
C the angle ANGL about the axis specified by IAXS (1 for the U axis,
C 2 for the V axis, 3 for the W axis).  A right-handed coordinate
C system is assumed.
C
        SINA=SIN(DTOR*ANGL)
        COSA=COS(DTOR*ANGL)
C
        UTMP=UCRD
        VTMP=VCRD
        WTMP=WCRD
C
        IF (IAXS.EQ.1) THEN
          VCRD=VTMP*COSA-WTMP*SINA
          WCRD=WTMP*COSA+VTMP*SINA
        ELSE IF (IAXS.EQ.2) THEN
          UCRD=UTMP*COSA+WTMP*SINA
          WCRD=WTMP*COSA-UTMP*SINA
        ELSE
          UCRD=UTMP*COSA-VTMP*SINA
          VCRD=VTMP*COSA+UTMP*SINA
        END IF
C
        RETURN
C
      END
