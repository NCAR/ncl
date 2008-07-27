C
C $Id: gaplch.f,v 1.3 2008-07-27 00:17:13 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GAPLCH (XPOS,YPOS,LBL1,RCHW,RORI,RCEN)
        CHARACTER*(*) LBL1
        CHARACTER*128 LBL2
        CHARACTER*1 ISFC
        NCH1=LEN(LBL1)
  101   IF (NCH1.GT.1) THEN
          IF (LBL1(NCH1:NCH1).EQ.' ') THEN
            NCH1=NCH1-1
            GO TO 101
          END IF
        END IF
        IF (NCH1.GT.116) THEN
          CALL PLCHHQ (XPOS,YPOS,LBL1(1:NCH1),RCHW,RORI,RCEN)
        ELSE
          CALL PCGETC ('FC',ISFC)
          CALL PCSETC ('FC',':')
          NCH2=0
          IEXP=0
          IRLZ=0
          DO 104 I=1,NCH1
            IF (LBL1(I:I).EQ.' ') GO TO 104
            IF (LBL1(I:I).NE.'E'.AND.LBL1(I:I).NE.'e') THEN
              IF (IRLZ.EQ.0.OR.LBL1(I:I).EQ.'-') THEN
                NCH2=NCH2+1
                LBL2(NCH2:NCH2)=LBL1(I:I)
              ELSE IF (I.EQ.NCH1.OR.(LBL1(I:I).NE.'+'.AND.
     +                               LBL1(I:I).NE.'0')) THEN
                NCH2=NCH2+1
                LBL2(NCH2:NCH2)=LBL1(I:I)
                IRLZ=0
              END IF
            ELSE
              IEXP=1
              IRLZ=1
              IBEG=1
              IF (LBL2(1:1).EQ.'+'.OR.LBL2(1:1).EQ.'-') IBEG=2
              IF (LBL2(IBEG:IBEG).EQ.'1') THEN
                IF (IBEG.EQ.NCH2) THEN
                  NCH2=IBEG+4
                  LBL2(IBEG:IBEG+4)='10:S:'
                ELSE IF (LBL2(IBEG+1:IBEG+1).EQ.'.') THEN
                  DO 102 J=IBEG+2,NCH2
                    IF (LBL2(J:J).NE.'0') GO TO 103
  102             CONTINUE
                  NCH2=IBEG+4
                  LBL2(IBEG:IBEG+4)='10:S:'
                  GO TO 104
  103             NCH2=NCH2+10
                  LBL2(NCH2-9:NCH2)=':L1:410:S:'
                ELSE
                  NCH2=NCH2+10
                  LBL2(NCH2-9:NCH2)=':L1:410:S:'
                END IF
              ELSE
                NCH2=NCH2+10
                LBL2(NCH2-9:NCH2)=':L1:410:S:'
              END IF
            END IF
  104     CONTINUE
          IF (IEXP.NE.0) THEN
            NCH2=NCH2+3
            LBL2(NCH2-2:NCH2)=':N:'
          END IF
          CALL PLCHHQ (XPOS,YPOS,LBL2(1:NCH2),RCHW,RORI,RCEN)
          CALL PCSETC ('FC',ISFC)
        END IF
        RETURN
      END
