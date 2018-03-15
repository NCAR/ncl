C
C $Id: ngritd.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C This is a modified version of ngritd.f that was generated using
C nag_apt to convert from single to double precision.
C
      SUBROUTINE DNGRITD (IAXS,ANGL,UCRD,VCRD,WCRD)
      DOUBLE PRECISION ANGL
      DOUBLE PRECISION UCRD
      DOUBLE PRECISION VCRD
      DOUBLE PRECISION WCRD
      DOUBLE PRECISION DTOR
      DOUBLE PRECISION SINA
      DOUBLE PRECISION COSA
      DOUBLE PRECISION UTMP
      DOUBLE PRECISION VTMP
      DOUBLE PRECISION WTMP
C
C Define a multiplicative constant to convert from degrees to radians.
C
        DATA DTOR / .017453292519943D0 /
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
