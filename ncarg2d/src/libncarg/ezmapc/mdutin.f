C
C $Id: mdutin.f,v 1.4 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDUTIN (IPRJ,IZON,ISPH,PADP,UMIN,UMAX,VMIN,VMAX)
C
        DOUBLE PRECISION PADP(15),UMIN,UMAX,VMIN,VMAX
C
        REAL PASP(15)
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER IPRF
        SAVE   /USGSC1/
C
C Declare some double precision variables that we need.
C
        DOUBLE PRECISION UVDP,VVDP,DTMP
C
C Declare arrays in which to put State Plane limit information.
C
        DIMENSION IZN0(139),IZN8(139),ILM0(139,4),ILM8(139,4)
C
C Define State Plane limit information.
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I=  1, 10)/
     +              0101,0101,   0, 260,  30, 520,  48, 308,  30, 520,
     +              0102,0102,  40, 300,  10, 580, 488, 748,  10, 580,
     +              5001,5001, 380,1080, 300,1000, 380,1080, 300,1000,
     +              5002,5002,   0, 300, 580,1830, 348, 648, 580,1830,
     +              5003,5003,   0, 300, 630,1860, 348, 648, 630,1860,
     +              5004,5004,   0, 300, 360,1910, 348, 648, 360,1910,
     +              5005,5005,   0, 300, 250,1960, 348, 648, 250,1960,
     +              5006,5006,   0, 300, 160,1950, 348, 648, 160,1950,
     +              5007,5007,   0, 340,  20,1960, 287, 627,  20,1960,
     +              5008,5008,   0, 300,-100,1820, 348, 648,-100,1820/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 11, 20)/
     +              5009,5009,   0, 320,-160,1480, 317, 637,-160,1480,
     +              5010,5010,  50,1250,   0, 400, 120,1320,   0, 400,
     +              0201,0201,   0, 280,  10, 690,  61, 341,  10, 690,
     +              0202,0202,   0, 300,  10, 690,  61, 361,  10, 690,
     +              0203,0203,  30, 300,  40, 690,  91, 361,  40, 690,
     +              0301,0301, 360, 840,  20, 260, 150, 630,  20, 260,
     +              0302,0302, 360, 800,  20, 290, 150, 590, 420, 690,
     +              0401,0401, 390, 800,   0, 330,1780,2190, 500, 830,
     +              0402,0402, 390, 960,   0, 320,1780,2350, 500, 820,
     +              0403,0403, 310, 980,   0, 280,1700,2370, 500, 780/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 21, 30)/
     +              0404,0404, 280, 980,   0, 280,1670,2370, 500, 780,
     +              0405,0405, 280, 990,   0, 280,1670,2380, 500, 780,
     +              0406,0406, 200, 830,   0, 260,1590,2220, 500, 760,
     +              0407,   0, 970,1700,1070,1470,   0,   0,   0,   0,
     +              0501,0501, 280, 930,   0, 210, 584,1234, 305, 515,
     +              0502,0502, 280, 930,  40, 240, 584,1234, 345, 545,
     +              0503,0503, 280, 930,  20, 240, 584,1234, 325, 545,
     +              0600,0600,  80, 280,   0, 160, 202, 402, 152, 312,
     +              0700,0700,  90, 210,  30, 230, 138, 258,  30, 230,
     +              1901,1901, 226, 254, 103, 131, 383, 411, 122, 150/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 31, 40)/
     +              0901,0901,   0, 270,   0, 740,  48, 318,   0, 740,
     +              0902,0902,  40, 300,   0, 740,  88, 348,   0, 740,
     +              0903,0903, 290, 980,  20, 240, 280, 970,  20, 240,
     +              1001,1001,   0, 300,  20, 580,  48, 348,  20, 580,
     +              1002,1002,   0, 300,  40, 580, 548, 848,  40, 580,
     +              5101,5101,  60, 240,   0, 180, 408, 588,   0, 180,
     +              5102,5102,  70, 240,   0, 120, 418, 588,   0, 120,
     +              5103,5103, 110, 200,   0,  70, 458, 548,   0,  70,
     +              5104,5104, 110, 180,   0,  50, 458, 528,   0,  50,
     +              5105,5105, 130, 175,   5,  50, 478, 523,   5,  50/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 41, 50)/
     +              1101,1101,   0, 260,  20, 470,  48, 308,  20, 470,
     +              1102,1102,   0, 300,  20, 720, 348, 648,  20, 720,
     +              1103,1103,  10, 300,  20, 840, 658, 948,  20, 840,
     +              1201,1201,   0, 250,  20, 670, 148, 398,  20, 670,
     +              1202,1202,  20, 300,  20, 670, 568, 848,  20, 670,
     +              1301,1301,   0, 250,  10, 500, -52, 198, 260, 750,
     +              1302,1302,  40, 300,  10, 500, 788,1048, 260, 750,
     +              1401,1401, 330, 910,  20, 240,1220,1800,1020,1240,
     +              1402,1402, 350, 910,  20, 250, 240, 800,  20, 250,
     +              1501,1501, 230, 930,   0, 210,  20, 720,   0, 210/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 51, 60)/
     +              1502,1502, 280, 970,  20, 260,  70, 760, 420, 660,
     +              1601,1601, 210, 830, -20, 200, 100, 720, -20, 200,
     +              1602,1602, 260, 960,   0, 230, 150, 850, 500, 730,
     +              1701,1701, 440, 890,   0, 280, 830,1280,  20, 300,
     +              1702,1702, 340, 860,  10, 280, 730,1250,  30, 300,
     +              1703,1703, 340, 860,   0, 520, 730,1250,  20, 540,
     +              1801,1801,   0, 300, -40, 420, 148, 448, -20, 440,
     +              1802,1802,  60, 300,   0, 540, 808,1048,   0, 540,
     +              1900,1900,  10, 430, -10, 230, 166, 586,  10, 250,
     +              2001,2001,   0, 330,  10, 230,  17, 347, 760, 980/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 61, 70)/
     +              2002,2002,   0, 140,  10, 110, 439, 579,  10, 110,
     +              2101,   0,   0, 300,   0, 610,   0,   0,   0,   0,
     +              2102,   0,   0, 300,   0, 620,   0,   0,   0,   0,
     +              2103,   0,   0, 300, 380, 760,   0,   0,   0,   0,
     +              2111,2111, 320, 920,  10, 320,7710,8310,  10, 320,
     +              2112,2112, 220, 780,  30, 330,5610,6170,  30, 330,
     +              2113,2113, 380, 790,   0, 310,3770,4180,   0, 310,
     +              2201,2201, 290, 900,   0, 350, 480,1090, 100, 450,
     +              2202,2202, 390, 900,   0, 300, 580,1090, 100, 400,
     +              2203,2203, 370, 850,  30, 300, 560,1040, 130, 400/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 71, 80)/
     +              2301,2301,   0, 240,  40, 610, 148, 388,  60, 630,
     +              2302,2302,   0, 300, -50, 520, 548, 848,  60, 630,
     +              2401,2401,   0, 300,   0, 550,  98, 398,   0, 550,
     +              2402,2402,   0, 300,  50, 550, 348, 648,  50, 550,
     +              2403,2403,  20, 300,  20, 510, 718, 998,  20, 510,
     +                 0,2500,   0,   0,   0,   0,  90,1050,   0, 560,
     +              2501,   0, 100,1040,  40, 260,   0,   0,   0,   0,
     +              2502,   0, 100,1050,  20, 280,   0,   0,   0,   0,
     +              2503,   0, 190,1060,  20, 340,   0,   0,   0,   0,
     +                 0,2600,   0,   0,   0,   0, 140, 920,   0, 380/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 81, 90)/
     +              2601,   0, 250, 970,   0, 220,   0,   0,   0,   0,
     +              2602,   0, 210, 990,  20, 280,   0,   0,   0,   0,
     +              2701,2701,   0, 310,  10, 830,  48, 358,8010,8830,
     +              2702,2702,   0, 300,  50, 830, 348, 648,6050,6830,
     +              2703,2703,   0, 300, 200, 830, 648, 948,4200,4830,
     +              2800,2800,  60, 250,   0, 340, 208, 398,   0, 340,
     +              2900,2900, 510, 690, -10, 300,  40, 220, -10, 300,
     +              3001,3001,   0, 300,  90, 690,  13, 313,  90, 690,
     +              3002,3002,   0, 300,  60, 690, 348, 648,  60, 690,
     +              3003,3003,  10, 300,  20, 690, 688, 978,  20, 690/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I= 91,100)/
     +              3101,3101,   0, 260,  40, 580,  -2, 258, 170, 710,
     +              3102,3102,   0, 300, 140, 580,  98, 398, 140, 580,
     +              3103,3103,  20, 300, 200, 400, 218, 498, 200, 400,
     +              3104,3104, 570, 810,  20, 110, 260, 500,  30, 120,
     +              3200,3200, 100, 950, -10, 340, 100, 950, -10, 340,
     +              3301,3301, 320, 910, -10, 250, 310, 900, -10, 250,
     +              3302,3302, 310, 940,  10, 250, 300, 930,  10, 250,
     +              3401,3401, 390, 800,  40, 280, 380, 790,  40, 280,
     +              3402,3402, 390, 790,  20, 290, 380, 780,  20, 290,
     +              3501,3501, 140, 960,  10, 250, 130, 950,  10, 250/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I=101,110)/
     +              3502,3502, 400, 960,  20, 260, 390, 950,  20, 260,
     +              3601,3601, 290, 950,  20, 310,2180,2840,  20, 310,
     +              3602,3602, 260, 920,  20, 310,1150,1810,  20, 310,
     +              3701,3701, 350, 880,  20, 260, 340, 870,  20, 260,
     +              3702,3702, 350, 890,  20, 230, 340, 880,  20, 230,
     +              3800,3800, 102, 202, -10, 120,  50, 150, -10, 120,
     +                 0,3900,   0,   0,   0,   0, 370, 860,   0, 400,
     +              3901,   0, 370, 860,  40, 260,   0,   0,   0,   0,
     +              3902,   0, 430, 860,   0, 260,   0,   0,   0,   0,
     +              4001,4001, 260, 920,  10, 260, 250, 910,  10, 260/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I=111,120)/
     +              4002,4002, 280, 950,   0, 280, 270, 940,   0, 280,
     +              4100,4100, 200,1020,  40, 270, 190,1010,  50, 280,
     +              4201,4201, 440, 850,  20, 300,  30, 440,1020,1300,
     +              4202,4202,  60, 980,  10, 300, 150,1070,2010,2300,
     +              4203,4203, -10,1280,   0, 300,  80,1370,3000,3300,
     +              4204,4204,  30,1140,  10, 310,  20,1130,4010,4310,
     +              4205,4205, 420, 820,   0, 290, 110, 510,5000,5290,
     +              4301,4301, 370, 840, -10, 210, 260, 730, 990,1210,
     +              4302,4302, 370, 840,  30, 310, 260, 730,2030,2310,
     +              4303,4303, 360, 850,  20, 240, 250, 740,3020,3240/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I=121,130)/
     +              4400,4400,  60, 240,  10, 300, 408, 588,  10, 300,
     +              4501,4501, 430, 830, -10, 220,3320,3720,1990,2220,
     +              4502,4502, 130, 910,   0, 230,3020,3800,1000,1230,
     +              4601,4601, 300, 920,   0, 250, 190, 810,   0, 250,
     +              4602,4602, 300, 910,   0, 280, 190, 800,   0, 280,
     +              4701,4701, 340, 780,   0, 260, 330, 770,   0, 260,
     +              4702,4702, 440, 860,   0, 270, 430, 850,   0, 270,
     +              4801,4801, 360, 880, -20, 240, 350, 870, -20, 240,
     +              4802,4802, 360, 880,   0, 230, 350, 870,   0, 230,
     +              4803,4803, 410, 830,  30, 280, 400, 820,  30, 280/
C
        DATA (IZN0(I),IZN8(I),ILM0(I,1),ILM0(I,2),ILM0(I,3),ILM0(I,4),
     +                        ILM8(I,1),ILM8(I,2),ILM8(I,3),ILM8(I,4),
     +                                                     I=131,139)/
     +              4901,4901,  30, 270,  20, 500,  78, 318,  40, 520,
     +              4902,4902,   0, 300,  20, 500, 248, 548, 140, 620,
     +              4903,4903,   0, 300,  20, 500, 448, 748,  40, 520,
     +              4904,4904,  50, 250,  20, 500, 698, 898, 140, 620,
     +              5201,5200,   0, 390, -80, 140,  48, 438, 120, 340,
     +              5201,5200,   0, 390, -80, 140,  48, 438, 120, 340,
     +              5202,5200,   0, 390, -50, 170,  48, 438, 120, 340,
     +              5300,   0,   0, 800,   0, 800,   0,   0,   0,   0,
     +              5400,   0,   0, 800,   0, 800,   0,   0,   0,   0/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GTPZBD
C
C Set the value of the projection selector to be passed to PJIN[SD]P.
C
        JPRJ=MAX(1,MIN(23,IPRJ))
C
C Set the value of the zone number to be passed to PJIN[SD]P.  UTM zone
C numbers are limited to the legal range.  For State Plane Zone number
C "1901" (the District of Columbia), we use "1900" (Maryland).
C
        IF (JPRJ.EQ.1) THEN
          JZON=SIGN(MAX(1,MIN(60,ABS(IZON))),IZON)
        ELSE IF (JPRJ.EQ.2) THEN
          JZON=IZON
          IF (JZON.EQ.1901) JZON=1900
        ELSE
          JZON=0
        END IF
C
C Set the value of the spheroid selector to be passed to PJIN[SD]P.
C
        IF (JPRJ.EQ.2) THEN
          IF (ISPH.EQ.0) THEN
            JSPH=0
          ELSE
            JSPH=8
          END IF
        ELSE IF (JPRJ.EQ.23) THEN
          JSPH=0
        ELSE
          JSPH=MAX(-1,MIN(19,ISPH))
        END IF
C
C Decide whether to use real or double-precision arithmetic and call the
C appropriate initialization routine.
C
        CALL MDPIN2 (TST1,TST2,TST3)
C
        IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
C
          IROD=0
C
          DO 101 I=1,15
            PASP(I)=REAL(PADP(I))
  101     CONTINUE
C
          CALL PJINSP (JPRJ,JZON,JSPH,PASP)
C
          DO 102 I=1,15
            PADP(I)=DBLE(PASP(I))
  102     CONTINUE
C
        ELSE
C
          IROD=1
C
          CALL PJINDP (JPRJ,JZON,JSPH,PADP)
C
        END IF
C
C Initialize minimum and maximum values in the USGS code's common block.
C
        IF (UMIN.LT.UMAX.AND.VMIN.LT.VMAX) THEN
C
          UUMN=UMIN
          UUMX=UMAX
          UVMN=VMIN
          UVMX=VMAX
C
        ELSE IF (JPRJ.EQ.1) THEN
C
          UUMN=0.D0
          UUMX=1.D6
          UVMN=0.D0
          UVMX=1.D7
C
        ELSE IF (JPRJ.EQ.2) THEN
C
          UUMN=0.D0
          UUMX=1.D6
          UVMN=0.D0
          UVMX=1.D6
C
          IF (JSPH.EQ.0) THEN
            DO 103 I=1,139
              IF (IZON.EQ.IZN0(I)) THEN
                UUMN=1.D3*DBLE(ILM0(I,1))
                UUMX=1.D3*DBLE(ILM0(I,2))
                UVMN=1.D3*DBLE(ILM0(I,3))
                UVMX=1.D3*DBLE(ILM0(I,4))
                GO TO 107
              END IF
  103       CONTINUE
          ELSE
            DO 104 I=1,139
              IF (IZON.EQ.IZN8(I)) THEN
                UUMN=1.D3*DBLE(ILM8(I,1))
                UUMX=1.D3*DBLE(ILM8(I,2))
                UVMN=1.D3*DBLE(ILM8(I,3))
                UVMX=1.D3*DBLE(ILM8(I,4))
                GO TO 107
              END IF
  104       CONTINUE
          END IF
C
        ELSE IF (JPRJ.EQ. 4.OR.
     +           JPRJ.EQ. 6.OR.
     +           JPRJ.EQ.10.OR.
     +           JPRJ.EQ.13    ) THEN
C
          UUMN=PADP(7)-PADP(1)
          UUMX=PADP(7)+PADP(1)
          UVMN=PADP(8)-PADP(1)
          UVMX=PADP(8)+PADP(1)
C
        ELSE IF (JPRJ.EQ.9) THEN
C
          UUMN=PADP(7)- .0873D0*PADP(1)
          UUMX=PADP(7)+ .0873D0*PADP(1)
          UVMN=PADP(8)-1.3963D0*PADP(1)
          UVMX=PADP(8)+1.4661D0*PADP(1)
C
        ELSE IF (JPRJ.EQ.22) THEN
C
          UUMN=PADP(7)+1.68D0*PADP(1)
          UUMX=PADP(7)+4.74D0*PADP(1)
          UVMN=PADP(8)-0.75D0*PADP(1)
          UVMX=PADP(8)+0.75D0*PADP(1)
C
        ELSE IF (JPRJ.EQ.23) THEN
C
          UUMN=PADP(7)-1.75D6
          UUMX=PADP(7)+1.75D6
          UVMN=PADP(8)-1.75D6
          UVMX=PADP(8)+1.75D6
C
        ELSE
C
C JPRJ is in the group {3,5,7,8,11,12,14,15,16,17,18,19,20,21}.
C
          UUMN=+1.D12
          UUMX=-1.D12
          UVMN=+1.D12
          UVMX=-1.D12
C
          DO 106 I=-90,90
            DO 105 J=-180,180
              IF (IROD.EQ.0) THEN
                CALL MDUTFS (REAL(I),REAL(J),UVSP,VVSP)
                IF (ABS(UVSP).LT.1.E12) THEN
                  IF ((JPRJ.EQ.5.OR.JPRJ.EQ.20).AND.
     +                (UVSP-PASP(7))**2+(VVSP-PASP(7))**2.GT.
     +                9.8696E0*PASP(1)**2) GO TO 105
                  UUMN=MIN(UUMN,DBLE(UVSP))
                  UUMX=MAX(UUMX,DBLE(UVSP))
                  UVMN=MIN(UVMN,DBLE(VVSP))
                  UVMX=MAX(UVMX,DBLE(VVSP))
                END IF
              ELSE
                CALL MDUTFD (DBLE(I),DBLE(J),UVDP,VVDP)
                IF (ABS(UVDP).LT.1.D12) THEN
                  IF ((JPRJ.EQ.5.OR.JPRJ.EQ.20).AND.
     +                (UVDP-PADP(7))**2+(VVDP-PADP(7))**2.GT.
     +                9.8696D0*PADP(1)**2) GO TO 105
                  UUMN=MIN(UUMN,UVDP)
                  UUMX=MAX(UUMX,UVDP)
                  UVMN=MIN(UVMN,VVDP)
                  UVMX=MAX(UVMX,VVDP)
                END IF
              END IF
  105       CONTINUE
  106     CONTINUE
C
          IF (UUMX-UUMN.LT.UVMX-UVMN) THEN
            DTMP=.5D0*(UVMX-UVMN-UUMX+UUMN)
            UUMN=UUMN-DTMP
            UUMX=UUMX+DTMP
          ELSE IF (UUMX-UUMN.GT.UVMX-UVMN) THEN
            DTMP=.5D0*(UUMX-UUMN-UVMX+UVMN)
            UVMN=UVMN-DTMP
            UVMX=UVMX+DTMP
          END IF
C
          DTMP=.01*(UUMX-UUMN)
C
          UUMN=UUMN-DTMP
          UUMX=UUMX+DTMP
          UVMN=UVMN-DTMP
          UVMX=UVMX+DTMP
C
        END IF
C
C Initialize EZMAP.
C
  107   CALL MDPINT
C
C Done.
C
        RETURN
C
      END
