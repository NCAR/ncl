C
C $Id: ngritd.f,v 1.1 1994-04-13 22:37:27 kennison Exp $
C
      SUBROUTINE NGRITD (IAXS,ANGL,UCRD,VCRD,WCRD)
C
C Define a multiplicative constant to convert from degrees to radians.
C
        DATA DTOR / .017453292519943 /
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
