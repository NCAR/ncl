C
C $Id: agsetc.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSETC (TPID,CUSR)
C
      CHARACTER*(*) TPID,CUSR
C
      DIMENSION FURA(1)
C
C The routine AGSETC is used to set the values of individual AUTOGRAPH
C parameters which intrinsically represent character strings.  TPID is a
C parameter identifier.  CUSR is a character string.  The situation is
C complicated by the fact that the character string may be either a dash
C pattern, the name of a label, the line-end character, or the text of a
C line, all of which are treated differently.
C
C Define a local variable to hold the "line-end" character.
C
      CHARACTER*1 LEND
C
C See what kind of parameter is being set.
C
      CALL AGCTCS (TPID,ITCS)
C
C If the parameter is not intrinsically of type character, log an error.
C
      IF (ITCS.EQ.0) GO TO 901
C
C Find the length of the string, which may or may not actually be used.
C (On the Cray, at least, it may be zero if the wrong type of argument
C was used.)
C
      ILEN=LEN(CUSR)
C
C Retrieve the current (integer) value of the parameter.
C
      CALL AGGETI (TPID,ITMP)
C
C Check for a dash pattern.
C
      IF (ITCS.EQ.1) THEN
        CALL AGGETI ('DASH/LENG.',NCHR)
        IF (ILEN.GT.0.AND.ILEN.LT.NCHR) NCHR=ILEN
        CALL AGRPCH (CUSR,NCHR,ITMP)
C
C Check for a label name.
C
      ELSE IF (ITCS.EQ.2) THEN
        CALL AGSTCH (CUSR,MAX(1,ILEN),ITMP)
C
C Check for the line-end character.
C
      ELSE IF (ITCS.EQ.3) THEN
        CALL AGRPCH (CUSR,1,ITMP)
C
C Check for the text of a label.
C
      ELSE IF (ITCS.EQ.4) THEN
        CALL AGGETI ('LINE/MAXI.',NCHR)
        IF (ILEN.GT.0) NCHR=MIN(NCHR,ILEN)
        CALL AGGETC ('LINE/END .',LEND)
        DO 101 I=1,NCHR
          IF (CUSR(I:I).EQ.LEND) THEN
            NCHR=I-1
            GO TO 102
          END IF
  101   CONTINUE
C
  102   IF (NCHR.LE.0) THEN
          CALL AGRPCH (' ',1,ITMP)
        ELSE
          CALL AGRPCH (CUSR,NCHR,ITMP)
        END IF
C
      END IF
C
C Transfer the generated value to the list of AUTOGRAPH parameters.
C
      FURA(1)=REAL(ITMP)
      CALL AGSETP (TPID,FURA,1)
C
C Done.
C
      RETURN
C
C Error exit.
C
  901 CALL AGPPID (TPID)
      CALL SETER ('AGSETC - PARAMETER TO SET IS NOT INTRINSICALLY OF TYP
     +E CHARACTER',14,2)
C
      END
