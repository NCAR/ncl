C
C	$Id: pcffme.f,v 1.2 1992-11-19 01:34:43 fred Exp $
C
      SUBROUTINE PCFFME (CHGT)
C
C Extract the informational part of the fontcap.
C
      include 'pcffme.h'
      include 'pcffdx.h'
C
C  Type flag.
C
      CALL GBYTES(IBFC,TYPFLG,0,16,0,1)
C
C  Other fields.
C
      CALL GBYTES(IBFC,FNINFO(2),336,16,0,NUMNTR-1)
C
C  Generate negative numbers if required.
C
      DO 80 I=2,NUMNTR
        CALL PCGNEG (FNINFO(I),ITMP)
        FNINFO(I) = ITMP
   80 CONTINUE
C
C  Get the byte pointers for the characters.
C
      NUMCHR = CHREND-CHRSTR+1
      CALL GBYTES(IBFC,CHRPNT,8*TABPNT,16,0,NUMCHR)
C
      RETURN
      END
