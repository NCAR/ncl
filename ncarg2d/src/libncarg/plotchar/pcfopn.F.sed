C
C	$Id: pcfopn.F.sed,v 1.1 1992-09-24 19:50:04 ncargd Exp $
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
      CHARACTER*128 PARANM,FILENM
      DATA PARANM / 'DBDIR' /
      IF (NFNT.EQ.0) THEN
         CALL GNGPAT (PARANM,FILENM,ISTATUS)
         IF (ISTATUS .NE. -1) THEN
            DO 101 I=1,119
               IF (FILENM(I:I).EQ.' ') THEN
                  FILENM(I:I+9)='/pwritdata'
                  GO TO 102
               END IF
 101        CONTINUE
            GO TO 104
         ELSE
            GO TO 103
         ENDIF
#if defined(ultrix) && defined(mips)
 102     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +        READONLY,ERR=104)
#else
 102     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +        ERR=104)
#endif
         REWIND IBNU
      ELSE IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
         CALL BOFRED (IBNU,NFNT,IOST,ISTA)
         IF (ISTA.NE.0) THEN
            PRINT * , 'PCFOPN - ERROR OPENING FONT ',NFNT
            STOP
         END IF
      ELSE
         PRINT * , 'PCFOPN - REQUEST FOR UNAVAILBLE FONT ',NFNT
         STOP
      END IF
      RETURN
 103  PRINT *, FILENM
      GO TO 105
 104  PRINT * , 'PCFOPN - ERROR OPENING PWRITX DATA FILE ',FILENM
 105  CONTINUE
      STOP
      END
