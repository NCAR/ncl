C
C $Id: pjindp.f,v 1.7 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJINDP (ISYS,ZONE,ISPH,DATA)
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      REAL SECS(5)
      INTEGER ITEMP
      INTEGER LAND, PATH, LIMIT, IND02, IND06, IND09, ISYS, KEEPZN
      INTEGER I,ZONE,ISPH,DEGS(5),MINS(5)
      INTEGER ID, IND, MODE, N
      INTEGER NAD27(134), NAD83(134)
C
      COMMON /STPLCH/ NM27,NM83
        CHARACTER*32 NM27(134),NM83(134)
      SAVE   /STPLCH/
      COMMON /STPLDP/ PA27,PA83
        DOUBLE PRECISION PA27(9,134),PA83(9,134)
      SAVE   /STPLDP/
      COMMON /STPLIR/ ID27,ID83
        INTEGER ID27(134),ID83(134)
      SAVE   /STPLIR/
C
      CHARACTER*32 PNAME
      CHARACTER*1 SGNA(5)
C
      DIMENSION DATA(15),BUFFL(15)
      DIMENSION TABLE(9)
      DIMENSION PR(20),XLR(20)
      DIMENSION ACOEF(6),BCOEF(6)
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /ELLPDP/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
      COMMON /SPHRDP/ AZZ
      COMMON /NORMDP/ Q,T,U,W,ES22,P22,SA,CA,XJ
      COMMON /PC03DP/ A03,LON003,X003,Y003,C,E03,ES03,NS03,RH003
      COMMON /PC04DP/ A04,LON004,X004,Y004,E04,F04,NS04,RH004
      COMMON /PC05DP/ A05,LON005,X005,Y005,E05,M1
      COMMON /PC06DP/ A06,LON006,X006,Y006,E06,E4,FAC,MCS,TCS,IND06
      COMMON /PC07DP/ A07,LON007,X007,Y007,E07,E007,E107,E207,E307,ES07,
     .                ML007
      COMMON /PC08DP/ A08,LON008,X008,Y008,E008,E108,E208,E308,GL,NS08,
     .                RH008
      COMMON /PC09DP/ A09,LON009,X009,Y009,ES09,ESP,E009,E109,E209,E309,
     .                KS009,LAT009,ML009,IND09
      COMMON /PC10DP/ A10,LON010,X010,Y010,COSP10,LAT010,SINP10
      COMMON /PC11DP/ A11,LON011,X011,Y011,COSP11,LAT011,SINP11
      COMMON /PC12DP/ A12,LON012,X012,Y012,COSP12,LAT012,SINP12
      COMMON /PC13DP/ A13,LON013,X013,Y013,COSP13,LAT013,SINP13
      COMMON /PC14DP/ A14,LON014,X014,Y014,COSP14,LAT014,SINP14
      COMMON /PC15DP/ A15,LON015,X015,Y015,COSP15,LAT015,P,SINP15
      COMMON /PC16DP/ A16,LON016,X016,Y016
      COMMON /PC17DP/ A17,LON017,X017,Y017,LAT1
      COMMON /PC18DP/ A18,LON018,X018,Y018
      COMMON /PC19DP/ A19,LON019,X019,Y019
      COMMON /PC20DP/ LON020,X020,Y020,AL,BL,COSALF,COSGAM,E20,EL,
     +                SINALF,SINGAM,U0
      COMMON /PC21DP/ A21,LON021,X021,Y021,PR,XLR
      COMMON /PC22DP/ A22,X022,Y022,A2,A4,B,C1,C3,LAND,PATH
      COMMON /PC23DP/ A23,LON023,X023,Y023,ACOEF,BCOEF,EC,LAT023,
     .                CCHIO,SCHIO,N
C
      COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
      DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
      INTEGER IPRF
      SAVE   /USGSC1/
C
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA ZERO,HALF,ONE,TWO /0.0D0,0.5D0,1.0D0,2.0D0/
      DATA EPSLN /1.0D-10/
      DATA TOL /1.0D-7/
      DATA TOL09 /1.0D-5/
      DATA NINTYD /90000000.0D0/
      DATA DG1 /0.01745329252D0/
C
      DATA NAD27/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,0407,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2501,2502,2503,2601,2602,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3901,3902,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5201,
     .           5202,5400/
C
      DATA NAD83/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,   0,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2500,   0,   0,2600,   0,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3900,   0,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5200,
     .              0,5400/
C
C -- I N I T I A L I Z A T I O N   O F   P R O J E C T I O N S
C
      IPRF=0
      IERR=0
      IF (ISYS.NE.2) CALL SPHDDP (ISPH,DATA)
C
      GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     +       1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     +       2100,2200,2300) , ISYS
C
C -- U T M
C
  100    KEEPZN = ZONE
         ZONE = ABS(ZONE)
         IF (ZONE.LT.1 .OR. ZONE.GT.60) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,140) KEEPZN
  140       FORMAT (/' ERROR PJ01DP'/
     .               ' ILLEGAL ZONE NO. : ',I10)
            IERR = 011
            RETURN
         END IF
         DATA(3) = 0.9996D0
         DATA(4) = ZERO
         DATA(5) = DBLE(6*ZONE-183)
         DATA(6) = ZERO
         DATA(7) = 500000.0D0
         DATA(8) = ZERO
         IF (KEEPZN .LT. 0) DATA(8) = 10000000.0D0
         ITEMP = IPPARM
         IPPARM = 1
         AZ = DATA(1)
         EZ = DATA(2)
         GO TO 900
C
C -- S T A T E   P L A N E
C
  200    IF (ISPH.NE.0.AND.ISPH.NE.8) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,205) ISPH
  205       FORMAT(/' ERROR PJ02DP'/
     .              ' SPHEROID NO. ',I4,' IS INVALID FOR STATE PLANE',
     .              ' TRANSFORMATIONS')
            IERR = 020
            RETURN
         END IF
         IF (ZONE .GT. 0) THEN
            IND02 = 0
            IF (ISPH.EQ.0) THEN
               DO 210 I = 1,134
                  IF (ZONE .EQ. NAD27(I)) IND02 = I
  210          CONTINUE
            END IF
            IF (ISPH.EQ.8) THEN
               DO 220 I = 1,134
                  IF (ZONE .EQ. NAD83(I)) IND02 = I
  220          CONTINUE
            END IF
            IF (IND02 .EQ. 0) THEN
               IF (IPEMSG .EQ. 0) WRITE (IPELUN,240) ZONE,ISPH
               IERR = 021
               RETURN
            END IF
         ELSE
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,240) ZONE,ISPH
            IERR = 021
            RETURN
         END IF
         IF (ISPH.EQ.0) THEN
            PNAME=NM27(IND02)
            ID=ID27(IND02)
            TABLE(1)=PA27(1,IND02)
            TABLE(2)=PA27(2,IND02)
            TABLE(3)=PA27(3,IND02)
            TABLE(4)=PA27(4,IND02)
            TABLE(5)=PA27(5,IND02)
            TABLE(6)=PA27(6,IND02)
            TABLE(7)=PA27(7,IND02)
            TABLE(8)=PA27(8,IND02)
            TABLE(9)=PA27(9,IND02)
         ELSE IF (ISPH.EQ.8) THEN
            PNAME=NM83(IND02)
            ID=ID83(IND02)
            TABLE(1)=PA83(1,IND02)
            TABLE(2)=PA83(2,IND02)
            TABLE(3)=PA83(3,IND02)
            TABLE(4)=PA83(4,IND02)
            TABLE(5)=PA83(5,IND02)
            TABLE(6)=PA83(6,IND02)
            TABLE(7)=PA83(7,IND02)
            TABLE(8)=PA83(8,IND02)
            TABLE(9)=PA83(9,IND02)
         END IF
         IF (ID .LE. 0) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,240) ZONE,ISPH
  240       FORMAT(/' ERROR PJ02DP'/
     .              ' ILLEGAL ZONE NO. : ',I8,' FOR SPHEROID NO. : ',I4)
            IERR = 021
            RETURN
         END IF
         AZ = TABLE(1)
         ES = TABLE(2)
         ESZ = ES
         EZ  = SQRT(ES)
         E0Z = E0FNDP(ES)
         E1Z = E1FNDP(ES)
         E2Z = E2FNDP(ES)
         E3Z = E3FNDP(ES)
         E4Z = E4FNDP(EZ)
         ITEMP = IPPARM
         IPPARM = 1
C
C TRANSVERSE MERCATOR PROJECTION
C
         IF (ID.EQ.1) THEN
            DATA(3) = TABLE(4)
            DATA(5) = PAKRDP(PAKCDP(TABLE(3)))/.017453292519943D0
            DATA(6) = PAKRDP(PAKCDP(TABLE(7)))/.017453292519943D0
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            GO TO 900
         END IF
C
C LAMBERT CONFORMAL PROJECTION
C
         IF (ID.EQ.2) THEN
            DATA(3) = PAKRDP(PAKCDP(TABLE(6)))/.017453292519943D0
            DATA(4) = PAKRDP(PAKCDP(TABLE(5)))/.017453292519943D0
            DATA(5) = PAKRDP(PAKCDP(TABLE(3)))/.017453292519943D0
            DATA(6) = PAKRDP(PAKCDP(TABLE(7)))/.017453292519943D0
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            GO TO 400
         END IF
C
C POLYCONIC PROJECTION
C
         IF (ID.EQ.3) THEN
            DATA(5) = PAKRDP(PAKCDP(TABLE(3)))/.017453292519943D0
            DATA(6) = PAKRDP(PAKCDP(TABLE(4)))/.017453292519943D0
            DATA(7) = TABLE(5)
            DATA(8) = TABLE(6)
            GO TO 700
         END IF
C
C OBLIQUE MERCATOR PROJECTION
C
         IF (ID.EQ.4) THEN
            DATA(3) = TABLE(4)
            DATA(4) = PAKRDP(PAKCDP(TABLE(6)))/.017453292519943D0
            DATA(5) = PAKRDP(PAKCDP(TABLE(3)))/.017453292519943D0
            DATA(6) = PAKRDP(PAKCDP(TABLE(7)))/.017453292519943D0
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            DATA(13) = ONE
            GO TO 2000
         END IF
C
C -- A L B E R S   C O N I C A L   E Q U A L   A R E A
C
  300    A03 = AZ
         E03 = EZ
         ES03 = ESZ
         LAT1 = .017453292519943D0 * DATA(3)
         LAT2 = .017453292519943D0 * DATA(4)
         IF (ABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,340)
  340       FORMAT (/' ERROR PJ03DP'/
     .               ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .               ' SIDES OF EQUATOR')
            IERR = 031
            RETURN
         END IF
         LON003 = .017453292519943D0 * DATA(5)
         LAT003 = .017453292519943D0 * DATA(6)
         X003 = DATA(7)
         Y003 = DATA(8)
         SINP03 = SIN (LAT1)
         CON = SINP03
         COSP03 = COS (LAT1)
         MS1 = MSFNDP (E03,SINP03,COSP03)
         QS1 = QSFNDP (E03,SINP03,COSP03)
         SINP03 = SIN (LAT2)
         COSP03 = COS (LAT2)
         MS2 = MSFNDP (E03,SINP03,COSP03)
         QS2 = QSFNDP (E03,SINP03,COSP03)
         SINP03 = SIN (LAT003)
         COSP03 = COS (LAT003)
         QS0 = QSFNDP (E03,SINP03,COSP03)
         IF (ABS(LAT1-LAT2) .GE. EPSLN) THEN
            NS03 = (MS1 * MS1 - MS2 * MS2) / (QS2 - QS1)
         ELSE
            NS03 = CON
         END IF
         C = MS1 * MS1 + NS03 * QS1
         RH003 = A03 * SQRT (C - NS03 * QS0) / NS03
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDDP (LON003,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDDP (LAT003,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,350) A03,ES03,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .            X003,Y003
  350   FORMAT (/' INITIALIZATION PARAMETERS (ALBERS CONICAL EQUAL-AREA'
     .          ,' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=3
         GO TO 9900
C
C -- L A M B E R T   C O N F O R M A L   C O N I C
C
  400    A04 = AZ
         E04 = EZ
         ES = ESZ
         LAT1 = .017453292519943D0 * DATA(3)
         LAT2 = .017453292519943D0 * DATA(4)
         IF (ABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,440)
  440       FORMAT (/' ERROR PJ04DP'/
     .               ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .               ' SIDES OF EQUATOR')
            IERR = 041
            RETURN
         END IF
         LON004 = .017453292519943D0 * DATA(5)
         LAT004 = .017453292519943D0 * DATA(6)
         X004 = DATA(7)
         Y004 = DATA(8)
         SINP04 = SIN (LAT1)
         CON = SINP04
         COSP04 = COS (LAT1)
         MS1 = MSFNDP (E04,SINP04,COSP04)
         TS1 = TSFNDP (E04,LAT1,SINP04)
         SINP04 = SIN (LAT2)
         COSP04 = COS (LAT2)
         MS2 = MSFNDP (E04,SINP04,COSP04)
         TS2 = TSFNDP (E04,LAT2,SINP04)
         SINP04 = SIN (LAT004)
         TS0 = TSFNDP (E04,LAT004,SINP04)
         IF (ABS(LAT1-LAT2) .GE. EPSLN) THEN
            NS04 = LOG (MS1 / MS2) / LOG (TS1 / TS2)
         ELSE
            NS04 = CON
         END IF
         F04 = MS1 / (NS04 * TS1 ** NS04)
         RH004 = A04 * F04 * TS0 ** NS04
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDDP (LON004,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDDP (LAT004,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,450) A04,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .            X004,Y004
  450    FORMAT (/' INITIALIZATION PARAMETERS (LAMBERT CONFORMAL CONIC',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .            ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .            ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A04
         DATA(2) = ES
C
C LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
  470    FORMAT (/' INITIALIZATION PARAMETERS (STATE PLANE PROJECTION)'/
     .            ' ZONE NUMBER = ',I4,5X,' ZONE NAME = ',A32)
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,451) A04,ES,
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .               X004,Y004
  451    FORMAT ( ' USING LAMBERT CONFORMAL CONIC PROJECTION'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .            ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .            ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         END IF
C
         IPRF=4
         GO TO 9900
C
C -- M E R C A T O R
C
  500    A05 = AZ
         E05 = EZ
         ES = ESZ
         LON005 = .017453292519943D0 * DATA(5)
         LAT1 = .017453292519943D0 * DATA(6)
         M1 = COS(LAT1) / (SQRT( ONE - ES * SIN(LAT1) **2))
         X005 = DATA(7)
         Y005 = DATA(8)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LON005,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,550) A05,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X005,Y005
  550    FORMAT (/' INITIALIZATION PARAMETERS (MERCATOR',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LATITUDE OF TRUE SCALE       = ',A1,2I3,F7.3/
     .            ' CENTRAL LONGITUDE            = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=5
         GO TO 9900
C
C -- P O L A R   S T E R E O G R A P H I C
C
  600    A06 = AZ
         E06 = EZ
         ES = ESZ
         E4 = E4Z
         LON006 = .017453292519943D0 * DATA(5)
         SAVE = DATA(6)
         LATC = .017453292519943D0 * SAVE
         X006 = DATA(7)
         Y006 = DATA(8)
         FAC = ONE
         IF (SAVE .LT. ZERO) FAC =-ONE
         IND06 = 0
         IF (ABS(SAVE) .NE. NINTYD) THEN
            IND06 = 1
            CON1 = FAC * LATC
            SINPHI = SIN (CON1)
            COSPHI = COS (CON1)
            MCS = MSFNDP (E06,SINPHI,COSPHI)
            TCS = TSFNDP (E06,CON1,SINPHI)
         END IF
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON006,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LATC,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,650) A06,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X006,Y006
  650    FORMAT (/' INITIALIZATION PARAMETERS (POLAR STEREOGRAPHIC',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LONGITUDE OF Y-AXIS          = ',A1,2I3,F7.3/
     .            ' LATITUDE OF TRUE SCALE       = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=6
         GO TO 9900
C
C -- P O L Y C O N I C
C
  700    A07 = AZ
         E07 = EZ
         ES07 = ESZ
         E007 = E0Z
         E107 = E1Z
         E207 = E2Z
         E307 = E3Z
         LON007 = .017453292519943D0 * DATA(5)
         LAT007 = .017453292519943D0 * DATA(6)
         X007 = DATA(7)
         Y007 = DATA(8)
         ML007 = MLFNDP (E007,E107,E207,E307,LAT007)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON007,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT007,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,750) A07,ES07,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X007,Y007
  750    FORMAT (/' INITIALIZATION PARAMETERS (POLYCONIC',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A07
         DATA(2) = ES07
C
C LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,751) A07,ES07,
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .               X007,Y007
  751    FORMAT ( ' USING POLYCONIC PROJECTION'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         END IF
C
         IPRF=7
         GO TO 9900
C
C -- E Q U I D I S T A N T   C O N I C
C
  800    A08 = AZ
         E = EZ
         ES = ESZ
         E008 = E0Z
         E108 = E1Z
         E208 = E2Z
         E308 = E3Z
         LAT1 = .017453292519943D0 * DATA(3)
         LAT2 = .017453292519943D0 * DATA(4)
         IF (ABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,840)
  840       FORMAT (/' ERROR PJ08DP'/
     .               ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .               ' SIDES OF EQUATOR')
            IERR = 081
            RETURN
         END IF
         LON008 = .017453292519943D0 * DATA(5)
         LAT0 = .017453292519943D0 * DATA(6)
         X008 = DATA(7)
         Y008 = DATA(8)
         SINPHI = SIN (LAT1)
         COSPHI = COS (LAT1)
         MS1 = MSFNDP (E,SINPHI,COSPHI)
         ML1 = MLFNDP (E008,E108,E208,E308,LAT1)
         IND = 0
         IF (DATA(9) .NE. ZERO) THEN
            IND = 1
            SINPHI = SIN (LAT2)
            COSPHI = COS (LAT2)
            MS2 = MSFNDP (E,SINPHI,COSPHI)
            ML2 = MLFNDP (E008,E108,E208,E308,LAT2)
            IF (ABS(LAT1-LAT2) .GE. EPSLN) THEN
               NS08 = (MS1 - MS2) / (ML2 - ML1)
            ELSE
               NS08 = SINPHI
            END IF
         ELSE
            NS08 = SINPHI
         END IF
         GL = ML1 + MS1 / NS08
         ML0 = MLFNDP (E008,E108,E208,E308,LAT0)
         RH008 = A08 * (GL - ML0)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDDP (LON008,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDDP (LAT0,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IND .NE. 0) THEN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,850) A08,ES,
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .               X008,Y008
  850       FORMAT (/' INITIALIZATION PARAMETERS (EQUIDISTANT CONIC',
     .               ' PROJECTION)'/
     .               ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .               ' ECCENTRICITY SQUARED         =',F12.9/
     .               ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .               ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .               ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .               ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .               ' FALSE EASTING                =',F12.2,' METERS'/
     .               ' FALSE NORTHING               =',F12.2,' METERS')
         ELSE
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,860) A08,ES,
     .                SGNA(1),DEGS(1),MINS(1),SECS(1),
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=3,4),
     .               X008,Y008
  860       FORMAT (/' INITIALIZATION PARAMETERS (EQUIDISTANT CONIC',
     .               ' PROJECTION)'/
     .               ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .               ' ECCENTRICITY SQUARED         =',F12.9/
     .               ' LATITUDE OF ST. PARALLEL     = ',A1,2I3,F7.3/
     .               ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .               ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .               ' FALSE EASTING                =',F12.2,' METERS'/
     .               ' FALSE NORTHING               =',F12.2,' METERS')
         END IF
         IPRF=8
         GO TO 9900
C
C -- T R A N S V E R S E   M E R C A T O R
C
  900    A09 = AZ
         E09 = EZ
         ES09 = ESZ
         E009 = E0Z
         E109 = E1Z
         E209 = E2Z
         E309 = E3Z
         KS009 = DATA(3)
         LON009 = .017453292519943D0 * DATA(5)
         LAT009 = .017453292519943D0 * DATA(6)
         X009 = DATA(7)
         Y009 = DATA(8)
         ML009 = A09 * MLFNDP (E009,E109,E209,E309,LAT009)
         IND09 = 1
         ESP = ES09
         IF (E09 .GE. TOL09) THEN
            IND09 = 0
            ESP = ES09 / (ONE - ES09)
         END IF
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON009,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT009,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,950) A09,ES09,KS009,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X009,Y009
  950    FORMAT (/' INITIALIZATION PARAMETERS (TRANSVERSE MERCATOR',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' SCALE FACTOR AT C. MERIDIAN  =',F9.6/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A09
         DATA(2) = ES09
C
C LIST UTM PROJECTION INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 1) THEN
            IPPARM = ITEMP
            BUFFL(1) = A09
            BUFFL(2) = ES09
            ZONE = KEEPZN
            IF (IERR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,960) ZONE,BUFFL(1),
     .            BUFFL(2),BUFFL(3),
     .            SGNA(1),DEGS(1),MINS(1),SECS(1),
     .            BUFFL(7),BUFFL(8)
  960    FORMAT (/' INITIALIZATION PARAMETERS (U T M PROJECTION)'/
     .            ' ZONE = ',I3/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID = ',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         = ',F18.15/
     .            ' SCALE FACTOR AT C. MERIDIAN  = ',F9.6/
     .            ' LONGITUDE OF CENTRAL MERIDIAN= ',A1,2I3,F7.3/
     .            ' FALSE EASTING                = ',F12.2,' METERS'/
     .            ' FALSE NORTHING               = ',F12.2,' METERS')
         END IF
C
C LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,951) A09,ES09,KS009,
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .               X009,Y009
  951    FORMAT ( ' USING TRANSVERSE MERCATOR PROJECTION'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' SCALE FACTOR AT C. MERIDIAN  =',F9.6/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         END IF
C
         IPRF=9
         GO TO 9900
C
C -- S T E R E O G R A P H I C
C
 1000    A10 = AZZ
         LON010 = .017453292519943D0 * DATA(5)
         LAT010 = .017453292519943D0 * DATA(6)
         X010 = DATA(7)
         Y010 = DATA(8)
         SINP10 = SIN (LAT010)
         COSP10 = COS (LAT010)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON010,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT010,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1050) A10,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X010,Y010
 1050    FORMAT (/' INITIALIZATION PARAMETERS (STEREOGRAPHIC',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=10
         GO TO 9900
C
C -- L A M B E R T   A Z I M U T H A L   E Q U A L - A R E A
C
 1100    A11 = AZZ
         LON011 = .017453292519943D0 * DATA(5)
         LAT011 = .017453292519943D0 * DATA(6)
         X011 = DATA(7)
         Y011 = DATA(8)
         SINP11 = SIN (LAT011)
         COSP11 = COS (LAT011)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON011,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT011,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1150) A11,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X011,Y011
 1150    FORMAT (/' INITIALIZATION PARAMETERS (LAMBERT AZIMUTHAL EQUAL',
     .            '-AREA PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=11
         GO TO 9900
C
C -- A Z I M U T H A L   E Q U I D I S T A N T
C
 1200    A12 = AZZ
         LON012 = .017453292519943D0 * DATA(5)
         LAT012 = .017453292519943D0 * DATA(6)
         X012 = DATA(7)
         Y012 = DATA(8)
         SINP12 = SIN (LAT012)
         COSP12 = COS (LAT012)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON012,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT012,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1250) A12,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X012,Y012
 1250    FORMAT (/' INITIALIZATION PARAMETERS (AZIMUTHAL EQUIDISTANT',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=12
         GO TO 9900
C
C -- G N O M O N I C
C
 1300    A13 = AZZ
         LON013 = .017453292519943D0 * DATA(5)
         LAT013 = .017453292519943D0 * DATA(6)
         X013 = DATA(7)
         Y013 = DATA(8)
         SINP13 = SIN (LAT013)
         COSP13 = COS (LAT013)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON013,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT013,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1350) A13,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X013,Y013
 1350    FORMAT (/' INITIALIZATION PARAMETERS (GNOMONIC',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=13
         GO TO 9900
C
C -- O R T H O G R A P H I C
C
 1400    A14 = AZZ
         LON014 = .017453292519943D0 * DATA(5)
         LAT014 = .017453292519943D0 * DATA(6)
         X014 = DATA(7)
         Y014 = DATA(8)
         SINP14 = SIN (LAT014)
         COSP14 = COS (LAT014)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON014,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT014,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1450) A14,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X014,Y014
 1450    FORMAT (/' INITIALIZATION PARAMETERS (ORTHOGRAPHIC',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=14
         GO TO 9900
C
C -- V E R T I C A L   N E A R - S I D E   P E R S P E C T I V E
C
 1500    A15 = AZZ
         P = ONE + DATA(3) / A15
         LON015 = .017453292519943D0 * DATA(5)
         LAT015 = .017453292519943D0 * DATA(6)
         X015 = DATA(7)
         Y015 = DATA(8)
         SINP15 = SIN (LAT015)
         COSP15 = COS (LAT015)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON015,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT015,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1550) A15,DATA(3),
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X015,Y015
 1550    FORMAT (/' INITIALIZATION PARAMETERS (GENERAL VERTICAL NEAR-S',
     .            'IDE PERSPECTIVE PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' HEIGHT OF PERSPECTIVE POINT'/
     .            ' ABOVE SPHERE                 =',F12.2,' METERS'/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=15
         GO TO 9900
C
C -- S I N U S O I D A L
C
 1600    A16 = AZZ
         LON016 = .017453292519943D0 * DATA(5)
         X016 = DATA(7)
         Y016 = DATA(8)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON016,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1650) A16,
     .            SGNA(1),DEGS(1),MINS(1),SECS(1),
     .            X016,Y016
 1650    FORMAT (/' INITIALIZATION PARAMETERS (SINUSOIDAL',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=16
         GO TO 9900
C
C -- E Q U I R E C T A N G U L A R
C
 1700    A17 = AZZ
         LAT1 = .017453292519943D0 * DATA(6)
         LON017 = .017453292519943D0 * DATA(5)
         X017 = DATA(7)
         Y017 = DATA(8)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LON017,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1750) A17,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X017,Y017
 1750    FORMAT (/' INITIALIZATION PARAMETERS (EQUIRECTANGULAR PROJECT',
     .            'ION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LATITUDE OF TRUE SCALE       = ',A1,2I2,F7.3/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=17
         GO TO 9900
C
C -- M I L L E R   C Y L I N D R I C A L
C
 1800    A18 = AZZ
         LON018 = .017453292519943D0 * DATA(5)
         X018 = DATA(7)
         Y018 = DATA(8)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON018,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1850) A18,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X018,Y018
 1850    FORMAT (/' INITIALIZATION PARAMETERS (MILLER CYLINDRICAL',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=18
         GO TO 9900
C
C -- V A N   D E R   G R I N T E N   I
C
 1900    A19 = AZZ
         LON019 = .017453292519943D0 * DATA(5)
         X019 = DATA(7)
         Y019 = DATA(8)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON019,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1950) A19,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X019,Y019
 1950    FORMAT (/' INITIALIZATION PARAMETERS (VAN DER GRINTEN I',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=19
         GO TO 9900
C
C -- O B L I Q U E   M E R C A T O R   ( H O T I N E )
C
 2000    MODE = 0
         IF (DATA(13) .NE. ZERO) MODE = 1
         A = AZ
         E20 = EZ
         ES = ESZ
         KS0 = DATA(3)
         LAT0 = .017453292519943D0 * DATA(6)
         X020 = DATA(7)
         Y020 = DATA(8)
         SINPH0 = SIN (LAT0)
         COSPH0 = COS (LAT0)
         CON = ONE - ES * SINPH0 * SINPH0
         COM = SQRT (ONE - ES)
         BL = SQRT (ONE + ES * COSPH0 ** 4 / (ONE - ES))
         AL = A * BL * KS0 * COM / CON
         IF (ABS(LAT0).LT.EPSLN) TS0 = 1.0D0
         IF (ABS(LAT0).LT.EPSLN) D=1.0D0
         IF (ABS(LAT0).LT.EPSLN) EL=1.0D0
         IF (ABS(LAT0).GE.EPSLN) THEN
            TS0 = TSFNDP (E20,LAT0,SINPH0)
            CON = SQRT (CON)
            D = BL * COM / (COSPH0 * CON)
            F = D + SIGN (SQRT (MAX ((D * D - ONE), 0.0D0)) , LAT0)
            EL = F * TS0 ** BL
         END IF
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2050) A,ES,KS0,X020,Y020
 2050    FORMAT (/' INITIALIZATION PARAMETERS (OBLIQUE MERCATOR ''HOTI',
     .            'NE'' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' SCALE AT CENTER              =',F12.9/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IF (MODE .NE. 0) THEN
            ALPHA = .017453292519943D0 * DATA(4)
            LONC = .017453292519943D0 * DATA(5)
            G = HALF * (F - ONE / F)
            GAMMA = ASINDP (SIN (ALPHA) / D)
            LON020 = LONC - ASINDP (G * TAN (GAMMA)) / BL
C
C LIST INITIALIZATION PARAMETERS (CASE B).
C
            CALL RADDDP (ALPHA,SGNA(1),DEGS(1),MINS(1),SECS(1))
            CALL RADDDP (LONC,SGNA(2),DEGS(2),MINS(2),SECS(2))
            CALL RADDDP (LAT0,SGNA(3),DEGS(3),MINS(3),SECS(3))
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,2060)
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,3)
 2060       FORMAT (' AZIMUTH OF CENTRAL LINE      = ',A1,2I3,F7.3/
     .              ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .              ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3)
            CON = ABS (LAT0)
            IF (CON.GT.EPSLN .AND. ABS(CON - HALFPI).GT.EPSLN) THEN
               SINGAM = SIN (GAMMA)
               COSGAM = COS (GAMMA)
               SINALF = SIN (ALPHA)
               COSALF = COS (ALPHA)
               U0 = SIGN((AL/BL)*ATAN(SQRT(D*D-ONE)/COSALF),LAT0)
               IF (IPPARM .EQ. 0) WRITE (IPPLUN,2080) X020,Y020
               DATA(1) = A
               DATA(2) = ES
C
C LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
               IF (ISYS .EQ. 2) THEN
                  IPPARM = ITEMP
                  IF (IERR .NE. 0) RETURN
                  IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
                  IF (IPPARM .EQ. 0) WRITE (IPPLUN,2051) A,ES,KS0,X020,
     .                                                            Y020
 2051 FORMAT ( ' USING OBLIQUE MERCATOR (''HOTINE'') PROJECTION'/
     .         ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .         ' ECCENTRICITY SQUARED         =',F12.9/
     .         ' SCALE AT CENTER              =',F12.9/
     .         ' FALSE EASTING                =',F12.2,' METERS'/
     .         ' FALSE NORTHING               =',F12.2,' METERS')
                  IF (IPPARM .EQ. 0) WRITE (IPPLUN,2060)
     .                     (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,3)
               END IF
C
               IPRF=20
               GO TO 9900
            ELSE
               IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
 2040          FORMAT (/' ERROR PJ20DP'/
     .                  ' INPUT DATA ERROR')
               IERR = 201
               RETURN
            END IF
         END IF
         LON1 = .017453292519943D0 * DATA(9)
         LAT1 = .017453292519943D0 * DATA(10)
         LON2 = .017453292519943D0 * DATA(11)
         LAT2 = .017453292519943D0 * DATA(12)
         SINPHI = SIN (LAT1)
         TS1 = TSFNDP (E20,LAT1,SINPHI)
         SINPHI = SIN (LAT2)
         TS2 = TSFNDP (E20,LAT2,SINPHI)
         H = TS1 ** BL
         L = TS2 ** BL
         F = EL / H
         G = HALF * (F - ONE / F)
         J = (EL * EL - L * H) / (EL * EL + L * H)
         P = (L - H) / (L + H)
         CALL RADDDP (LON2,SGNA(3),DEGS(3),MINS(3),SECS(3))
         DLON = LON1 - LON2
         IF (DLON .LT. -PI) LON2 = LON2 - 2.D0 * PI
         IF (DLON .GT.  PI) LON2 = LON2 + 2.D0 * PI
         DLON = LON1 - LON2
         LON020 = HALF * (LON1 + LON2) - ATAN (J * TAN (HALF * BL *
     .          DLON) / P) / BL
         DLON = ADJLDP (LON1 - LON020)
         GAMMA = ATAN (SIN (BL * DLON) / G)
         ALPHA = ASINDP (D * SIN (GAMMA))
         CALL RADDDP (LON1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT1,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDDP (LAT2,SGNA(4),DEGS(4),MINS(4),SECS(4))
         CALL RADDDP (LAT0,SGNA(5),DEGS(5),MINS(5),SECS(5))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2070)
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,5)
 2070    FORMAT (' LONGITUDE OF 1ST POINT       = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 1ST POINT        = ',A1,2I3,F7.3/
     .           ' LONGITUDE OF 2ND POINT       = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 2ND POINT        = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3)
         IF (ABS(LAT1 - LAT2) .LE. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
            IERR = 202
            RETURN
         ELSE
            CON = ABS (LAT1)
         END IF
         IF (CON.LE.EPSLN .OR. ABS(CON - HALFPI).LE.EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
            IERR = 202
            RETURN
         ELSE
            IF (ABS(ABS(LAT0) - HALFPI) .LE. EPSLN) THEN
               IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
               IERR = 202
               RETURN
            END IF
         END IF
         SINGAM = SIN (GAMMA)
         COSGAM = COS (GAMMA)
         SINALF = SIN (ALPHA)
         COSALF = COS (ALPHA)
         U0 = SIGN((AL/BL)*ATAN(SQRT(D*D-ONE)/COSALF),LAT0)
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2080) X020,Y020
 2080    FORMAT (' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=20
         GO TO 9900
C
C -- R O B I N S O N
C
 2100    A21 = AZZ
         LON021 = .017453292519943D0 * DATA(5)
         X021 = DATA(7)
         Y021 = DATA(8)
         PR(1)=-0.062D0
         XLR(1)=0.9986D0
         PR(2)=0.D0
         XLR(2)=1.D0
         PR(3)=0.062D0
         XLR(3)=0.9986D0
         PR(4)=0.124D0
         XLR(4)=0.9954D0
         PR(5)=0.186D0
         XLR(5)=0.99D0
         PR(6)=0.248D0
         XLR(6)=0.9822D0
         PR(7)=0.31D0
         XLR(7)=0.973D0
         PR(8)=0.372D0
         XLR(8)=0.96D0
         PR(9)=0.434D0
         XLR(9)=0.9427D0
         PR(10)=0.4958D0
         XLR(10)=0.9216D0
         PR(11)=0.5571D0
         XLR(11)=0.8962D0
         PR(12)=0.6176D0
         XLR(12)=0.8679D0
         PR(13)=0.6769D0
         XLR(13)=0.835D0
         PR(14)=0.7346D0
         XLR(14)=0.7986D0
         PR(15)=0.7903D0
         XLR(15)=0.7597D0
         PR(16)=0.8435D0
         XLR(16)=0.7186D0
         PR(17)=0.8936D0
         XLR(17)=0.6732D0
         PR(18)=0.9394D0
         XLR(18)=0.6213D0
         PR(19)=0.9761D0
         XLR(19)=0.5722D0
         PR(20)=1.0D0
         XLR(20)=0.5322D0
         DO 2110 I=1,20
 2110    XLR(I)=XLR(I) * 0.9858D0
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON021,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2150) A21,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X021,Y021
 2150    FORMAT (/' INITIALIZATION PARAMETERS (ROBINSON',
     .            ' PROJECTION)'/
     .            ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .            ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=21
         GO TO 9900
C
C -- S P A C E   O B L I Q U E   M E R C A T O R
C
 2200    A22 = AZ
         E = EZ
         ES22 = ESZ
         X022 = DATA(7)
         Y022 = DATA(8)
         LAND = INT(DATA(3)+TOL)
         PATH = INT(DATA(4)+TOL)
C
C CHECK IF LANDSAT NUMBER IS WITHIN RANGE 1 - 5
C
         IF (LAND .GT. 0 .AND. LAND .LE. 5) THEN
            IF (LAND .LE. 3) LIMIT = 251
            IF (LAND .GE. 4) LIMIT = 233
         ELSE
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2240) LAND, PATH
            IERR = 221
            RETURN
         END IF
C
C CHECK IF PATH NUMBER IS WITHIN RANGE 1 - 251 FOR LANDSATS 1 - 3
C OR RANGE 1 - 233 FOR LANDSATS 4 - 5
C
         IF (PATH .LE. 0 .OR. PATH .GT. LIMIT) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2240) LAND, PATH
 2240       FORMAT (/' ERROR PJ22DP'/
     .               ' LANDSAT NUMBER ',I2,' AND / OR PATH NUMBER ',I4,
     .               ' ARE OUT OF RANGE')
            IERR = 221
            RETURN
         END IF
         P1=1440.0D0
         IF (LAND.LE.3) THEN
            P2=103.2669323D0
            ALF=99.092D0*DG1
         ELSE
            P2=98.8841202D0
            ALF=98.20D0*DG1
         END IF
         SA=SIN(ALF)
         CA=COS(ALF)
         IF (ABS(CA).LT.1.D-9) CA=1.D-9
         ESC=ES22*CA*CA
         ESS=ES22*SA*SA
         W=((ONE-ESC)/(ONE-ES22))**TWO-ONE
         Q=ESS/(ONE-ES22)
         T=(ESS*(TWO-ES22))/(ONE-ES22)**TWO
         U=ESC/(ONE-ES22)
         XJ=(ONE-ES22)**3
         P22=P2/P1
C
C COMPUTE FOURIER COEFFICIENTS.  LAM IS CURRENT VALUE OF
C LAMBDA DOUBLE-PRIME.
C
         LAM=0
         CALL SERADP (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=FA2
         SUMA4=FA4
         SUMB=FB
         SUMC1=FC1
         SUMC3=FC3
         DO 2210 I=9,81,18
         LAM=I
         CALL SERADP (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+4.0D0*FA2
         SUMA4=SUMA4+4.0D0*FA4
         SUMB=SUMB+4.0D0*FB
         SUMC1=SUMC1+4.0D0*FC1
         SUMC3=SUMC3+4.0D0*FC3
 2210    CONTINUE
         DO 2220 I=18,72,18
         LAM=I
         CALL SERADP (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+TWO*FA2
         SUMA4=SUMA4+TWO*FA4
         SUMB=SUMB+TWO*FB
         SUMC1=SUMC1+TWO*FC1
         SUMC3=SUMC3+TWO*FC3
 2220    CONTINUE
         LAM=90.0D0
         CALL SERADP (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+FA2
         SUMA4=SUMA4+FA4
         SUMB=SUMB+FB
         SUMC1=SUMC1+FC1
         SUMC3=SUMC3+FC3
C
C THESE ARE THE VALUES OF FOURIER CONSTANTS.
C
         A2=SUMA2/30.D0
         A4=SUMA4/60.D0
         B=SUMB/30.D0
         C1=SUMC1/15.D0
         C3=SUMC3/45.D0
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2250) A22,ES22,LAND,PATH,
     .                                          X022,Y022
 2250    FORMAT (/' INITIALIZATION PARAMETERS (SPACE OBL. MERCATOR',
     .            ' PROJECTION)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LANDSAT NO.                  = ',I3/
     .            ' PATH                         = ',I5/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS'/)
         IPRF=22
         GO TO 9900
C
C -- M O D I F I E D   S T E R E O G R A P H I C   -   A L A S K A
C
 2300    A23 = AZ
         EC2 = 0.6768657997291094D-02
         EC  = SQRT (EC2)
         N=6
         LON023 = -152.0D0*DG1
         LAT023 = 64.0D0*DG1
         X023 = DATA(7)
         Y023 = DATA(8)
         ACOEF(1)=0.9945303D0
         ACOEF(2)=0.0052083D0
         ACOEF(3)=0.0072721D0
         ACOEF(4)=-0.0151089D0
         ACOEF(5)=0.0642675D0
         ACOEF(6)=0.3582802D0
         BCOEF(1)=0.0D0
         BCOEF(2)=-.0027404D0
         BCOEF(3)=0.0048181D0
         BCOEF(4)=-0.1932526D0
         BCOEF(5)=-0.1381226D0
         BCOEF(6)=-0.2884586D0
         ESPHI=EC*SIN(LAT023)
         CHIO=TWO*ATAN(TAN((HALFPI+LAT023)/TWO)*((ONE-ESPHI)/
     .       (ONE+ESPHI))**(EC/TWO)) - HALFPI
         SCHIO=SIN(CHIO)
         CCHIO=COS(CHIO)
C
C LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDDP (LON023,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDDP (LAT023,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2350) A23,EC2,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X023,Y023
 2350    FORMAT (/' INITIALIZATION PARAMETERS (MOD. STEREOGRAPHIC',
     .            ' CONFORMAL PROJECTION, ALASKA)'/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         =',F12.9/
     .            ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .            ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .            ' FALSE EASTING                =',F12.2,' METERS'/
     .            ' FALSE NORTHING               =',F12.2,' METERS')
         IPRF=23
         GO TO 9900
C
 9900 DO 9999 I=1,15
        UTPA(I)=DATA(I)
 9999 CONTINUE
C
      RETURN
C
      END
