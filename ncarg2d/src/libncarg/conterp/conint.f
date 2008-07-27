C
C	$Id: conint.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONINT (NDP,XD,YD,ZD,NCP,IPC,PD)
C
C THIS SUBROUTINE ESTIMATES PARTIAL DERIVATIVES OF THE FIRST AND
C SECOND ORDER AT THE DATA POINTS.
C THE INPUT PARAMETERS ARE
C
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD,ZD = ARRAYS CONTAINING THE X, Y, AND Z COORDI-
C                NATES OF DATA POINTS,
C     NCP = NUMBER OF DATA POINTS TO BE USED FOR ESTIMATION
C           OF PARTIAL DERIVATIVES AT EACH DATA POINT,
C     IPC = INTEGER ARRAY CONTAINING THE POINT NUMBERS OF
C           NCP DATA POINTS CLOSEST TO EACH OF THE NDP DATA
C           POINT.
C THE OUTPUT PARAMETER IS
C
C     PD  = ARRAY OF DIMENSION 5*NDP, WHERE THE ESTIMATED
C
C           ZX, ZY, ZXX, ZXY, AND ZYY VALUES AT THE DATA
C           POINTS ARE TO BE STORED.
C DECLARATION STATEMENTS
C
C
      DIMENSION       XD(NDP)    ,YD(NDP)    ,ZD(NDP)    ,IPC(1)     ,
     1                PD(1)
      REAL            NMX        ,NMY        ,NMZ        ,NMXX       ,
     1                NMXY        ,NMYX       ,NMYY
C
      SAVE
C
C PRELIMINARY PROCESSING
C
C
      NCPM1 = NCP-1
C
C ESTIMATION OF ZX AND ZY
C
C
      DO  130 IP0=1,NDP
         X0 = XD(IP0)
         Y0 = YD(IP0)
         Z0 = ZD(IP0)
         NMX = 0.0
         NMY = 0.0
         NMZ = 0.0
         JIPC0 = NCP*(IP0-1)
         DO  120 IC1=1,NCPM1
            JIPC = JIPC0+IC1
            IPI = IPC(JIPC)
            DX1 = XD(IPI)-X0
            DY1 = YD(IPI)-Y0
            DZ1 = ZD(IPI)-Z0
            IC2MN = IC1+1
            DO  110 IC2=IC2MN,NCP
               JIPC = JIPC0+IC2
               IPI = IPC(JIPC)
               DX2 = XD(IPI)-X0
               DY2 = YD(IPI)-Y0
               DNMZ = DX1*DY2-DY1*DX2
               IF (DNMZ .EQ. 0.0) GO TO  110
               DZ2 = ZD(IPI)-Z0
               DNMX = DY1*DZ2-DZ1*DY2
               DNMY = DZ1*DX2-DX1*DZ2
               IF (DNMZ .GE. 0.0) GO TO  100
               DNMX = -DNMX
               DNMY = -DNMY
               DNMZ = -DNMZ
  100          NMX = NMX+DNMX
               NMY = NMY+DNMY
               NMZ = NMZ+DNMZ
  110       CONTINUE
  120    CONTINUE
         JPD0 = 5*IP0
         PD(JPD0-4) = -NMX/NMZ
         PD(JPD0-3) = -NMY/NMZ
  130 CONTINUE
C
C ESTIMATION OF ZXX, ZXY, AND ZYY
C
C
      DO  170 IP0=1,NDP
         JPD0 = JPD0+5
         X0 = XD(IP0)
         JPD0 = 5*IP0
         Y0 = YD(IP0)
         ZX0 = PD(JPD0-4)
         ZY0 = PD(JPD0-3)
         NMXX = 0.0
         NMXY = 0.0
         NMYX = 0.0
         NMYY = 0.0
         NMZ = 0.0
         JIPC0 = NCP*(IP0-1)
         DO  160 IC1=1,NCPM1
            JIPC = JIPC0+IC1
            IPI = IPC(JIPC)
            DX1 = XD(IPI)-X0
            DY1 = YD(IPI)-Y0
            JPD = 5*IPI
            DZX1 = PD(JPD-4)-ZX0
            DZY1 = PD(JPD-3)-ZY0
            IC2MN = IC1+1
            DO  150 IC2=IC2MN,NCP
               JIPC = JIPC0+IC2
               IPI = IPC(JIPC)
               DX2 = XD(IPI)-X0
               DY2 = YD(IPI)-Y0
               DNMZ = DX1*DY2-DY1*DX2
               IF (DNMZ .EQ. 0.0) GO TO  150
               JPD = 5*IPI
               DZX2 = PD(JPD-4)-ZX0
               DZY2 = PD(JPD-3)-ZY0
               DNMXX = DY1*DZX2-DZX1*DY2
               DNMXY = DZX1*DX2-DX1*DZX2
               DNMYX = DY1*DZY2-DZY1*DY2
               DNMYY = DZY1*DX2-DX1*DZY2
               IF (DNMZ .GE. 0.0) GO TO  140
               DNMXX = -DNMXX
               DNMXY = -DNMXY
               DNMYX = -DNMYX
               DNMYY = -DNMYY
               DNMZ = -DNMZ
  140          NMXX = NMXX+DNMXX
               NMXY = NMXY+DNMXY
               NMYX = NMYX+DNMYX
               NMYY = NMYY+DNMYY
               NMZ = NMZ+DNMZ
  150       CONTINUE
  160    CONTINUE
         PD(JPD0-2) = -NMXX/NMZ
         PD(JPD0-1) = -(NMXY+NMYX)/(2.0*NMZ)
         PD(JPD0) = -NMYY/NMZ
  170 CONTINUE
      RETURN
      END
