C
C $Id: argpai.f,v 1.1 1995-04-28 19:41:01 kennison Exp $
C
      SUBROUTINE ARGPAI (CHS,IBG,IVL)
C
C This routine looks for a subscript of the form "(n)" in positions IBG
C and following of the character string CHS.  It returns the value of
C "n" (which may be negative) in IVL.  If no subscript is found, IVL
C is zeroed.
C
      CHARACTER*(*) CHS
      CHARACTER*1 ICH
C
      IVL=0
      ISN=1
C
      IST=0
C
      DO 101 ICHS=IBG,LEN(CHS)
      ICH=CHS(ICHS:ICHS)
      IF (.NOT.(ICH.NE.' ')) GO TO 10001
        IF (.NOT.(IST.EQ.0)) GO TO 10002
          IF (ICH.NE.'(') GO TO 102
          IST=1
        GO TO 10003
10002   CONTINUE
        IF (.NOT.(IST.EQ.1)) GO TO 10004
          IF (.NOT.(ICHAR(CHS(ICHS:ICHS)).GE.ICHAR('0').AND.ICHAR(CHS(IC
     +HS:ICHS)).LE.ICHAR('9'))) GO TO 10005
            IVL=ICHAR(CHS(ICHS:ICHS))-ICHAR('0')
          GO TO 10006
10005     CONTINUE
          IF (.NOT.(ICH.EQ.'-')) GO TO 10007
            ISN=-1
          GO TO 10006
10007     CONTINUE
          IF (.NOT.(ICH.NE.'+')) GO TO 10008
            GO TO 102
10006     CONTINUE
10008     CONTINUE
          IST=2
        GO TO 10003
10004   CONTINUE
        IF (.NOT.(IST.EQ.2)) GO TO 10009
          IF (.NOT.(ICHAR(CHS(ICHS:ICHS)).GE.ICHAR('0').AND.ICHAR(CHS(IC
     +HS:ICHS)).LE.ICHAR('9'))) GO TO 10010
            IVL=IVL*10+ICHAR(CHS(ICHS:ICHS))-ICHAR('0')
          GO TO 10011
10010     CONTINUE
          IF (.NOT.(ICH.NE.')')) GO TO 10012
            GO TO 102
10012     CONTINUE
            GO TO 103
10011     CONTINUE
10003   CONTINUE
10009   CONTINUE
10001 CONTINUE
  101 CONTINUE
C
  102 IVL=0
      RETURN
C
  103 IVL=ISN*IVL
      RETURN
C
      END
