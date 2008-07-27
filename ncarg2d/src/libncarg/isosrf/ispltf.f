C
C $Id: ispltf.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISPLTF (RXN,RYN,IENT)
C
C This routine is ISOSRF's analogue of the SPPS routine PLOTIF; it
C receives the fractional coordinates of points on line to be drawn
C (usually closed contour lines).  When IENT = 1, (RXN,RYN) is the
C first point on such a line; when IENT = 2, (RXN,RYN) is the next
C point on such a line.
C
C ISPLTF draws visible portions of each line as defined by the screen
C model ISCA and marks the entire line on the screen model ISCR.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Certain variables need to be saved from call to call.
C
      SAVE RXO,RYO,IXO,IYO,IBO,IWO,IVO
C
C Arithmetic statement function for taking the average of two reals.
C
      AVE(A,B)=.5*(A+B)
C
      IXN=1+INT(RXN*RNX)
      IYN=1+INT(RYN*RNY)
      IBN=NFPW-MOD(IXN-1,NFPW)
      IWN=(IXN+NFPW-1)/NFPW
      IVN=IAND(ISHIFT(ISCA(IWN,IYN),2*(1-IBN)),3)
C
      IF (IVN.NE.0) THEN
        IF (MOD(RXN*RNX,1.).LT..5) THEN
          IF (IXN.GT.1) THEN
            IF (IAND(ISHIFT(ISCA((IXN+NFPW-2)/NFPW,IYN),
     +                        2*(1+MOD(IXN-2,NFPW)-NFPW)),3).EQ.0) IVN=0
          END IF
        ELSE
          IF (IXN.LT.NX) THEN
            IF (IAND(ISHIFT(ISCA((IXN+NFPW  )/NFPW,IYN),
     +                        2*(1+MOD(IXN  ,NFPW)-NFPW)),3).EQ.0) IVN=0
          END IF
        END IF
      END IF
C
      IF (IVN.NE.0) THEN
        IF (MOD(RYN*RNY,1.).LT..5) THEN
          IF (IYN.GT.1) THEN
            IF (IAND(ISHIFT(ISCA(IWN,IYN-1),2*(1-IBN)),3).EQ.0) IVN=0
          END IF
        ELSE
          IF (IYN.LT.NY) THEN
            IF (IAND(ISHIFT(ISCA(IWN,IYN+1),2*(1-IBN)),3).EQ.0) IVN=0
          END IF
        END IF
      END IF
C
      IF (IENT.EQ.1) THEN
        IF (ISDR.EQ.0.AND.IVN.EQ.0)
     +         CALL PLOTIF (XVPL+(XVPR-XVPL)*RXN,YVPB+(YVPT-YVPB)*RYN,0)
        RXO=RXN
        RYO=RYN
        IXO=IXN
        IYO=IYN
        IBO=IBN
        IWO=IWN
        IVO=IVN
      ELSE
        RXT=RXO
        RYT=RYO
        IF (ISDR.EQ.0) THEN
          NPTI=MAX(ABS(IXN-IXO),ABS(IYN-IYO))-1
          IF (NPTI.GT.0) THEN
            DO 101 IPTI=1,NPTI
              RXI=RXT+(REAL(IPTI)/REAL(NPTI+1))*(RXN-RXT)
              RYI=RYT+(REAL(IPTI)/REAL(NPTI+1))*(RYN-RYT)
              IXI=1+INT(RXI*RNX)
              IYI=1+INT(RYI*RNY)
              IBI=NFPW-MOD(IXI-1,NFPW)
              IWI=(IXI+NFPW-1)/NFPW
              IVI=IAND(ISHIFT(ISCA(IWI,IYI),2*(1-IBI)),3)
C
              IF (IVI.NE.0) THEN
                IF (MOD(RXI*RNX,1.).LT..5) THEN
                  IF (IXI.GT.1) THEN
                    IF (IAND(ISHIFT(ISCA((IXI+NFPW-2)/NFPW,IYI),
     +                        2*(1+MOD(IXI-2,NFPW)-NFPW)),3).EQ.0) IVI=0
                  END IF
                ELSE
                  IF (IXI.LT.NX) THEN
                    IF (IAND(ISHIFT(ISCA((IXI+NFPW  )/NFPW,IYI),
     +                        2*(1+MOD(IXI  ,NFPW)-NFPW)),3).EQ.0) IVI=0
                  END IF
                END IF
              END IF
C
              IF (IVI.NE.0) THEN
                IF (MOD(RYI*RNY,1.).LT..5) THEN
                  IF (IYI.GT.1) THEN
                    IF (IAND(ISHIFT(ISCA(IWI,IYI-1),
     +                                         2*(1-IBI)),3).EQ.0) IVI=0
                  END IF
                ELSE
                  IF (IYI.LT.NY) THEN
                    IF (IAND(ISHIFT(ISCA(IWI,IYI+1),
     +                                         2*(1-IBI)),3).EQ.0) IVI=0
                  END IF
                END IF
              END IF
C
              IF (IVO.EQ.0.AND.IVI.EQ.0)
     +                              CALL PLOTIF (XVPL+(XVPR-XVPL)*RXI,
     +                                           YVPB+(YVPT-YVPB)*RYI,1)
              IF (IVO.EQ.0.AND.IVI.NE.0)
     +                     CALL PLOTIF (XVPL+(XVPR-XVPL)*AVE(RXO,RXI),
     +                                  YVPB+(YVPT-YVPB)*AVE(RYO,RYI),1)
              IF (IVO.NE.0.AND.IVI.EQ.0)
     +                     CALL PLOTIF (XVPL+(XVPR-XVPL)*AVE(RXO,RXI),
     +                                  YVPB+(YVPT-YVPB)*AVE(RYO,RYI),0)
              RXO=RXI
              RYO=RYI
              IVO=IVI
  101       CONTINUE
          END IF
          IF (IVO.EQ.0.AND.IVN.EQ.0)
     +                              CALL PLOTIF (XVPL+(XVPR-XVPL)*RXN,
     +                                           YVPB+(YVPT-YVPB)*RYN,1)
          IF (IVO.EQ.0.AND.IVN.NE.0)
     +                     CALL PLOTIF (XVPL+(XVPR-XVPL)*AVE(RXO,RXN),
     +                                  YVPB+(YVPT-YVPB)*AVE(RYO,RYN),1)
          IF (IVO.NE.0.AND.IVN.EQ.0)
     +                     CALL PLOTIF (XVPL+(XVPR-XVPL)*AVE(RXO,RXN),
     +                                  YVPB+(YVPT-YVPB)*AVE(RYO,RYN),0)
        END IF
        RXO=RXN
        RYO=RYN
        IVO=IVN
        IF (IXN.NE.IXO.OR.IYN.NE.IYO) THEN
          IF (RYN.GT.RYT) THEN
            ISCR(IWO,IYO)=IOR(ISCR(IWO,IYO),ISHIFT(  IFLIP,2*(IBO-1)))
            DO 102 IYI=IYO,IYN-1
              RXI=RXT+((RXN-RXT)/(RYN-RYT))*(REAL(IYI)/RNY-RYT)
              IXI=1+INT(RXI*RNX)
              IBI=NFPW-MOD(IXI-1,NFPW)
              IWI=(IXI+NFPW-1)/NFPW
              ISCR(IWI,IYI  )=IOR(ISCR(IWI,IYI  ),
     +                                      ISHIFT(  IFLIP,2*(IBI-1)))
              ISCR(IWI,IYI+1)=IOR(ISCR(IWI,IYI+1),
     +                                      ISHIFT(  IFLIP,2*(IBI-1)))
  102       CONTINUE
            ISCR(IWN,IYN)=IOR(ISCR(IWN,IYN),ISHIFT(  IFLIP,2*(IBN-1)))
          ELSE IF (RYN.LT.RYT) THEN
            ISCR(IWO,IYO)=IOR(ISCR(IWO,IYO),ISHIFT(3-IFLIP,2*(IBO-1)))
            DO 103 IYI=IYN,IYO-1
              RXI=RXN+((RXT-RXN)/(RYT-RYN))*(REAL(IYI)/RNY-RYN)
              IXI=1+INT(RXI*RNX)
              IBI=NFPW-MOD(IXI-1,NFPW)
              IWI=(IXI+NFPW-1)/NFPW
              ISCR(IWI,IYI  )=IOR(ISCR(IWI,IYI  ),
     +                                      ISHIFT(3-IFLIP,2*(IBI-1)))
              ISCR(IWI,IYI+1)=IOR(ISCR(IWI,IYI+1),
     +                                      ISHIFT(3-IFLIP,2*(IBI-1)))
  103       CONTINUE
            ISCR(IWN,IYN)=IOR(ISCR(IWN,IYN),ISHIFT(3-IFLIP,2*(IBN-1)))
          ELSE
            ISCR(IWO,IYO)=IOR(ISCR(IWO,IYO),ISHIFT(3      ,2*(IBO-1)))
            ISCR(IWN,IYN)=IOR(ISCR(IWN,IYN),ISHIFT(3      ,2*(IBN-1)))
          END IF
          IXO=IXN
          IYO=IYN
          IBO=IBN
          IWO=IWN
        END IF
      END IF
      IFILL=1
      RETURN
      END
