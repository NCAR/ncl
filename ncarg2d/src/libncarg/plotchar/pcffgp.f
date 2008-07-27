C
C	$Id: pcffgp.f,v 1.5 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCFFGP(NUM,IOFF,PFLG,X,Y)
C
C  Get NUM packets from the fontcap starting at bit offset IOFF.
C  Store the packet(s) in the arrays PFLG, X, and Y.
C
      include 'pcffme.h'
      include 'pcffdx.h'
C
      INTEGER PFLG(*),X(*),Y(*)
C
      CALL GBYTES(IBFC,PFLG,IOFF,PKFLWD,XBITWD+YBITWD,NUM)
      CALL GBYTES(IBFC,X,IOFF+PKFLWD,XBITWD,YBITWD+PKFLWD,NUM)
      CALL GBYTES(IBFC,Y,IOFF+PKFLWD+XBITWD,YBITWD,PKFLWD+XBITWD,NUM)
C
      RETURN
      END
