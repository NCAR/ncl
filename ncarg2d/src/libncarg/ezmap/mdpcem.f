C
C $Id: mdpcem.f,v 1.8 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPCEM (IEM1,IEM2,IERR,IFLG)
C
        CHARACTER*(*) IEM1,IEM2
        INTEGER       IERR,IFLG
C
C MDPCEM is called to do a call to SETER when the error message to be
C printed is in two parts which need to be concatenated.  FORTRAN-77
C rules make it necessary to concatenate the two parts of the message
C into a local character variable.
C
        CHARACTER*100 IEMC
C
        IEMC=IEM1//IEM2
        CALL SETER (IEMC,IERR,IFLG)
C
        RETURN
C
      END
