C
C   $Id: pcfopn.F.sed,v 1.3 1992-10-20 22:41:38 haley Exp $
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
      CHARACTER*128 FILENM
	  DATA FILENM / ' ' /
      IF (NFNT.EQ.0) THEN
         CALL GNGPAT (FILENM,'SED_DBDIR',ISTAT)
         IF (ISTAT .NE. -1) THEN
             DO 101 I=1,119
                IF (FILENM(I:I).EQ.CHAR(0)) THEN
                  FILENM(I:I+9)='/pwritdata'
                  GO TO 104
                ENDIF
101          CONTINUE
             GO TO 105
         ELSE
	         DO 102 I=2,128
    	         LENEM=I
        	     IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
102          CONTINUE
103          PRINT * , 'PCFOPN - ',FILENM(1:LENEM-1)
             STOP
         ENDIF
#if defined(ultrix) && defined(mips)
 104     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +        READONLY,ERR=105)
#else
 104     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +        ERR=105)
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
 105  PRINT * , 'PCFOPN - ERROR OPENING PWRITX DATA FILE ',FILENM
      STOP
      END
