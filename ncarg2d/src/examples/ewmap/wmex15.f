
      PROGRAM WMEX15
C
C  Example of adjusting wind barb directions when drawing
C  station model data.
C
C  City names and locations, station model data.
C
C   NUMC    -  the number of cities.
C   ICITYS  -  city names.
C   CITYUX  -  latitude of city.
C   CITYUY  -  longitude of city.
C   IMDAT   -  station model date for city.
C   SV2C    -  variable in which to save two characters.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)

      PARAMETER(NUMC=27)
      DIMENSION CITYUX(NUMC),CITYUY(NUMC)
      CHARACTER*13 ICITYS(NUMC)
      CHARACTER*5  IMDAT(10,NUMC)
      CHARACTER*2  SV2C
C
C  Constants used to convert from degrees to radians and from radians
C  to degrees.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C  City names.
C
      DATA (ICITYS(II),II=1,NUMC)/
     +           'NCAR',       'Seattle', 'San Francisco',
     +    'Los Angeles',      'Billings',       'El Paso',
     +        'Houston',   'Kansas City',   'Minneapolis',
     +        'Chicago',       'Detroit',       'Atlanta',
     +          'Miami',      'New York',        'Eugene',
     +          'Boise',     'Salt Lake',       'Phoenix',
     +    'Albuquerque',      'Bismarck',         'Tulsa',
     +         'Dallas',   'Little Rock',     'Lexington',
     +      'Charlotte',       'Norfolk',        'Bangor'/
C
C  City locations.
C
      DATA (CITYUX(II),II=1,NUMC)/
     +    40.0,   47.6,   37.8,   34.1,   45.8,   31.8,   29.8,
     +    39.1,   45.0,   41.9,   42.3,   33.8,   25.8,   40.8,
     +    44.1,   43.6,   40.7,   33.5,   35.1,   46.7,   36.0,
     +    32.8,   34.7,   38.1,   35.2,   36.8,   44.8/
C
      DATA (CITYUY(II),II=1,NUMC)/
     +  -105.0, -122.3, -122.4, -118.3, -108.5, -106.5, -095.3,
     +  -094.1, -093.8, -087.6, -083.1, -084.4, -080.2, -074.0,
     +  -123.1, -116.2, -111.9, -112.1, -106.6, -100.8, -096.0,
     +  -096.8, -092.3, -084.1, -080.8, -076.3, -068.8/
C
C  Station model data.  All wind directions have been set to zero, which
C  implies a northerly wind (from the north).
C
      DATA (IMDAT(L, 1),L=1,10)/'11000','00075','11260','21360','30000',
     +                          '49550','54054','60000','77570','87712'/
      DATA (IMDAT(L, 2),L=1,10)/'11103','10001','11040','21080','30000',
     +                          '49590','55050','60051','70430','80369'/
      DATA (IMDAT(L, 3),L=1,10)/'11206','20003','11020','21040','30000',
     +                          '49630','56046','60151','70840','81470'/
      DATA (IMDAT(L, 4),L=1,10)/'11309','30006','10000','21020','30000',
     +                          '49670','57042','60201','71250','82581'/
      DATA (IMDAT(L, 5),L=1,10)/'11412','40009','10020','21010','30000',
     +                          '49710','58038','60251','71660','83592'/
      DATA (IMDAT(L, 6),L=1,10)/'11515','50012','10040','20000','30000',
     +                          '49750','50034','60301','72070','84703'/
      DATA (IMDAT(L, 7),L=1,10)/'11618','60015','10060','20030','30000',
     +                          '49790','51030','60350','72480','85814'/
      DATA (IMDAT(L, 8),L=1,10)/'11721','70018','10080','20050','30000',
     +                          '49830','52026','60400','72890','86925'/
      DATA (IMDAT(L, 9),L=1,10)/'11824','80021','10090','20070','30000',
     +                          '49870','53022','60450','73230','87036'/
      DATA (IMDAT(L,10),L=1,10)/'11927','90024','10110','20110','30000',
     +                          '49910','54018','60501','73640','88147'/
      DATA (IMDAT(L,11),L=1,10)/'11030','00027','10130','20130','30000',
     +                          '49950','55014','60551','74050','89258'/
      DATA (IMDAT(L,12),L=1,10)/'11133','10030','10150','20170','30000',
     +                          '49990','56010','60601','74460','80369'/
      DATA (IMDAT(L,13),L=1,10)/'11236','20033','10170','20200','30000',
     +                          '40000','57006','60651','74870','81470'/
      DATA (IMDAT(L,14),L=1,10)/'11339','30036','10190','20230','30000',
     +                          '40040','58002','60701','75280','82581'/
      DATA (IMDAT(L,15),L=1,10)/'11442','40039','10210','20250','30000',
     +                          '40080','50000','60751','75690','83692'/
      DATA (IMDAT(L,16),L=1,10)/'11545','50042','10230','20270','30000',
     +                          '40120','51040','60801','76030','84703'/
      DATA (IMDAT(L,17),L=1,10)/'11648','60045','10250','20290','30000',
     +                          '40170','52008','60851','76440','85814'/
      DATA (IMDAT(L,18),L=1,10)/'11751','70048','10270','20310','30000',
     +                          '40210','53012','60901','76850','86925'/
      DATA (IMDAT(L,19),L=1,10)/'11854','80051','10290','20330','30000',
     +                          '40250','54016','60950','77260','87036'/
      DATA (IMDAT(L,20),L=1,10)/'11958','90054','10310','20360','30000',
     +                          '40290','55018','61000','77670','88147'/
      DATA (IMDAT(L,21),L=1,10)/'11060','00057','10330','20380','30000',
     +                          '40330','56030','61050','78080','89258'/
      DATA (IMDAT(L,22),L=1,10)/'11163','10060','10350','20410','30000',
     +                          '40370','57034','61100','78490','80369'/
      DATA (IMDAT(L,23),L=1,10)/'11266','20063','10370','20430','30000',
     +                          '40410','58043','61150','78830','81470'/
      DATA (IMDAT(L,24),L=1,10)/'11369','30066','10390','20470','30000',
     +                          '40450','50041','61200','79240','82581'/
      DATA (IMDAT(L,25),L=1,10)/'11472','40069','10410','20500','30000',
     +                          '40480','51025','61250','79650','83692'/
      DATA (IMDAT(L,26),L=1,10)/'11575','50072','10430','20530','30000',
     +                          '40510','52022','61350','79960','84703'/
      DATA (IMDAT(L,27),L=1,10)/'11678','60075','10480','21580','30000',
     +                          '40550','53013','61400','73370','85814'/
C
C  Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Set the foreground and background colors.
C
      CALL GSCR (1,0,1.,1.,1.)
      CALL GSCR (1,1,0.,0.,0.)
C
C  Tell EZMAP what part of the frame to use.
C
      CALL MAPPOS (.05,.95,.05,.95)
C
C  Draw a satellite view projection.
C

      CALL MAPSTR ('SA',1.3)
      CALL MAPROJ ('SV',40.,-97.,35.)
      CALL MAPSET ('MA',0.,0.,0.,0.)
C     CALL MAPSTC ('OU','CO')
      CALL MAPSTC ('OU','US')
C
C  Draw the EZMAP background.
C
	CALL MAPDRW
C
C  In the middle of Nebraska, draw a wind barb for a northeasterly wind
C  with a magnitude of 15 knots.
C
	CALL MAPTRN (42.,    -99.,XO,YO)
	CALL MAPTRN (42.+0.1,-99.,XA,YA)
	UW=(XA-XO)/SQRT((XA-XO)**2+(YA-YO)**2)
	VW=(YA-YO)/SQRT((XA-XO)**2+(YA-YO)**2)
	CALL WMBARB (XO,YO,15.*UW,15.*VW)
C
C  Plot station models at selected cities.  Prior to calling WMSTNM, the
C  wind direction in the array IMDAT is modified so that the wind barb
C  will point in the correct direction relative to the map.  This is done
C  by projecting both ends of a tiny wind vector and then computing the
C  angle that the projected wind vector makes with the sides of the map.
C  After the call to WMSTNM, the original wind direction is restored.
C
C  (Note that, because the contents of IMDAT have been defined in DATA
C  statements above so that all winds are northerlies, all wind barbs
C  will point to the north.  Uncommenting one of the other lines setting
C  ANGR will make all of the wind barbs point in some other direction.
C  This is useful for testing purposes.)
C
      DO 101 I=1,NUMC
C
C  Find the position of the city on the map.
C
        CALL MAPTRN (CITYUX(I),CITYUY(I),XO,YO)
C
C  Modify the wind speed before calling WMSTNM.
C
        SV2C=IMDAT(2,I)(2:3)
        READ (SV2C,'(I2)') IANG
C
C  Modify the direction of the wind barb.
C
        ANGR=DTOR*REAL(10*IANG)
        CALL MAPTRN(CITYUX(I)+.1*                    COS(ANGR),
     +              CITYUY(I)+.1/COS(DTOR*CITYUX(I))*SIN(ANGR),XA,YA)
        ANGD=MOD(RTOD*ATAN2(XA-XO,YA-YO)+360.,360.)
        IANG=MAX(0,MIN(35,NINT(ANGD/10.)))
        WRITE (IMDAT(2,I)(2:3),'(I2)') IANG
C
C  Call WMSTNM.
C
        CALL WMSTNM (XO,YO,IMDAT(1,I))
C
C  Restore the original wind speed after calling WMSTNM.
C
        IMDAT(2,I)(2:3)=SV2C
C
  101 CONTINUE
C
      CALL FRAME
C
C  Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END
