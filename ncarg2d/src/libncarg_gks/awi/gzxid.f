C
C	$Id: gzxid.f,v 1.5 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZXID(WKID,XID,IER)
C
C  Return the local X window identifier associated with the
C  WKID supplied.  IER is 0 if success.
C
      include 'gkscom.h'
C
      INTEGER WKID,XID,IER
C
      IER = 0
C
C  Check to make sure that WKID flags an X workstation type.
C
      IF (WKID .LT. 0) THEN
        IER = 20
        RETURN
      ENDIF
C
      CALL GQWKC(WKID,IER,ICONID,ITYPE)
      IF (IER .NE. 0) RETURN
      CALL GQWKCA(ITYPE,IER,ICAT)
      IF (IER .NE. 0) RETURN
C
      DO 10 I=1,NOPWK
        IF (WKID .EQ. SOPWK(I)) THEN
          XID = LXWKID(I)
C**RLB          IF (XID .EQ. -1) IER = 25
          IF (XID .EQ. -1 .and. SWKTP(I).NE.GCGM) IER = 25
          RETURN
        ENDIF
   10 CONTINUE
      IER = 25
C
      RETURN
      END
