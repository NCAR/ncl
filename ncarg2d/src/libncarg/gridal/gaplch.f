C
C $Id: gaplch.f,v 1.1 2000-10-25 22:27:36 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE GAPLCH (XPOS,YPOS,LBL1,RCHW,RORI,RCEN)
        CHARACTER*(*) LBL1
        CHARACTER*128 LBL2
        CHARACTER*1 ISFC
        NCH1=LEN(LBL1)
        IF (NCH1.GT.116) THEN
          CALL PLCHHQ (XPOS,YPOS,LBL1(1:NCH1),RCHW,RORI,RCEN)
        ELSE
          CALL PCGETC ('FC',ISFC)
          CALL PCSETC ('FC',':')
          NCH2=0
          IRLZ=0
          DO 101 I=1,NCH1
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
              NCH2=NCH2+10
              LBL2(NCH2-9:NCH2)=':L1:410:S:'
              IRLZ=1
            END IF
  101     CONTINUE
          IF (NCH2.NE.NCH1) THEN
            NCH2=NCH2+3
            LBL2(NCH2-2:NCH2)=':N:'
          END IF
          CALL PLCHHQ (XPOS,YPOS,LBL2(1:NCH2),RCHW,RORI,RCEN)
          CALL PCSETC ('FC',ISFC)
        END IF
        RETURN
      END
