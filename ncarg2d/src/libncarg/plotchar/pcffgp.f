C
C	$Id: pcffgp.f,v 1.2 1992-11-19 01:34:37 fred Exp $
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
