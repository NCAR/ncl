C
C $Id: gzgte1.f,v 1.6 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZGTE1(INDEX,NUM,ERMSG)
C
C  Given an index INDEX into the error table, this subroutine
C  returns the NCAR GKS error number in NUM and the error
C  message string in ERMSG (the length of ERMSG will be a
C  maximum of 210 characters).
C
      CHARACTER*(*) ERMSG
C
      include 'gkscom.h'
C
      IF (INDEX.LE.NUMERS .AND. INDEX.GE.1) THEN
        NUM = IERNMS(INDEX)
        ERMSG = ERMSGS(INDEX)
      ELSE
        NUM = -100
        ERMSG = ERMSGS(79)
      ENDIF
C
      RETURN
      END
