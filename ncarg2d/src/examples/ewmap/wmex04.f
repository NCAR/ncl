C
C	$Id: wmex04.f,v 1.1 1994-09-09 23:59:12 fred Exp $
C
      PROGRAM WMEX04
C
C  Example of using station model data.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C  City names and locations, Station model data.
C
C   NUMC    - the number of cities.
C   ICITYS  - City names.
C   CITYUX  - X user coordinates for city locations.
C   CITYUY  - Y user coordinates for city locations.
C
      PARAMETER(NUMC=27)
      DIMENSION CITYUX(NUMC),CITYUY(NUMC)
      CHARACTER*13 ICITYS(NUMC)
      CHARACTER*5  IMDAT(10,NUMC)
C
C  Data on city locations and daily temperature labels.
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
      DATA (CITYUX(II),II=1,NUMC)/ 
     +    40.0,   47.6,   37.8,   34.1,   45.8,   31.8,   29.8, 
     +    39.1,   45.0,   41.9,   42.3,   33.8,   25.8,   40.8,
     +    44.1,   43.6,   40.7,   33.5,   35.1,   46.7,   36.0,
     +    32.8,   34.7,   38.1,   35.2,   36.8,   44.8/
      DATA (CITYUY(II),II=1,NUMC)/ 
     +  -105.0, -122.3, -122.4, -118.3, -108.5, -106.5, -095.3, 
     +  -094.1, -093.8, -087.6, -083.1, -084.4, -080.2, -074.0,
     +  -123.1, -116.2, -111.9, -112.1, -106.6, -100.8, -096.0,
     +  -096.8, -092.3, -084.1, -080.8, -076.3, -068.8/
C
      DATA (IMDAT(L, 1),L=1,10)/'11000','00000','11260','21360','30000',       
     +                          '49550','54054','60000','77570','87712'/
      DATA (IMDAT(L, 2),L=1,10)/'11103','11101','11040','21080','30000',
     +                          '49590','55050','60051','70430','80369'/
      DATA (IMDAT(L, 3),L=1,10)/'11206','21003','11020','21040','30000',
     +                          '49630','56046','60151','70840','81470'/
      DATA (IMDAT(L, 4),L=1,10)/'11309','31106','10000','21020','30000',
     +                          '49670','57042','60201','71250','82581'/
      DATA (IMDAT(L, 5),L=1,10)/'11412','41209','10020','21010','30000',
     +                          '49710','58038','60251','71660','83592'/
      DATA (IMDAT(L, 6),L=1,10)/'11515','51312','10040','20000','30000',
     +                          '49750','50034','60301','72070','84703'/
      DATA (IMDAT(L, 7),L=1,10)/'11618','61415','10060','20030','30000',
     +                          '49790','51030','60350','72480','85814'/
      DATA (IMDAT(L, 8),L=1,10)/'11721','71518','10080','20050','30000',
     +                          '49830','52026','60400','72890','86925'/
      DATA (IMDAT(L, 9),L=1,10)/'11824','81621','10090','20070','30000',
     +                          '49870','53022','60450','73230','87036'/
      DATA (IMDAT(L,10),L=1,10)/'11927','91724','10110','20110','30000',
     +                          '49910','54018','60501','73640','88147'/
      DATA (IMDAT(L,11),L=1,10)/'11030','01827','10130','20130','30000',
     +                          '49950','55014','60551','74050','89258'/
      DATA (IMDAT(L,12),L=1,10)/'11133','11930','10150','20170','30000',
     +                          '49990','56010','60601','74460','80369'/
      DATA (IMDAT(L,13),L=1,10)/'11236','22033','10170','20200','30000',
     +                          '40000','57006','60651','74870','81470'/
      DATA (IMDAT(L,14),L=1,10)/'11339','32136','10190','20230','30000',
     +                          '40040','58002','60701','75280','82581'/
      DATA (IMDAT(L,15),L=1,10)/'11442','42239','10210','20250','30000',
     +                          '40080','50000','60751','75690','83692'/
      DATA (IMDAT(L,16),L=1,10)/'11545','52342','10230','20270','30000',
     +                          '40120','51040','60801','76030','84703'/
      DATA (IMDAT(L,17),L=1,10)/'11648','62445','10250','20290','30000',
     +                          '40170','52008','60851','76440','85814'/
      DATA (IMDAT(L,18),L=1,10)/'11751','72548','10270','20310','30000',
     +                          '40210','53012','60901','76850','86925'/
      DATA (IMDAT(L,19),L=1,10)/'11854','82651','10290','20330','30000',
     +                          '40250','54016','60950','77260','87036'/
      DATA (IMDAT(L,20),L=1,10)/'11958','92754','10310','20360','30000',
     +                          '40290','55018','61000','77670','88147'/
      DATA (IMDAT(L,21),L=1,10)/'11060','02857','10330','20380','30000',
     +                          '40330','56030','61050','78080','89258'/
      DATA (IMDAT(L,22),L=1,10)/'11163','12960','10350','20410','30000',
     +                          '40370','57034','61100','78490','80369'/
      DATA (IMDAT(L,23),L=1,10)/'11266','23063','10370','20430','30000',
     +                          '40410','58043','61150','78830','81470'/
      DATA (IMDAT(L,24),L=1,10)/'11369','33166','10390','20470','30000',
     +                          '40450','50041','61200','79240','82581'/
      DATA (IMDAT(L,25),L=1,10)/'11472','43269','10410','20500','30000',
     +                          '40480','51025','61250','79650','83692'/
      DATA (IMDAT(L,26),L=1,10)/'11575','51172','10430','20530','30000',
     +                          '40510','52022','61350','79960','84703'/
      DATA (IMDAT(L,27),L=1,10)/'11678','63475','10480','21580','30000',
     +                          '40550','53013','61400','73370','85814'/
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
C
C  Calls to position the output (applicable only to PostScript output).
C
      CALL NGSETI('LX',-90)
      CALL NGSETI('UX',710)
      CALL NGSETI('LY',-15)
      CALL NGSETI('UY',785)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      CALL GSCR(IWKID,3,0.,0.,1.)
      CALL GSCR(IWKID,4,.85,.85,.85)
      CALL GSCR(IWKID,5,.60,.60,.60)
C
      CALL GSFAIS(1)
      CALL GSFACI(2)
C
      CALL PCSETI('CC',1)
      CALL PLCHHQ(0.5,0.82,':F21:Sample Plotted Station Model Data',
     +            0.02,0.,0.)
      CALL PLCHHQ(0.5,0.77,':F21:(Data are not from actual readings)',
     +            0.02,0.,0.)
C
C  Set up the parameters for drawing the U.S. state outlines.
C
C   Position the plot.
      CALL MAPPOS(0.05, 0.95, 0.00, 0.90)
C
C   Specify U.S. state outlines.
      CALL MAPSTC('OU','US')
C
C   Choose Lambert conformal projection with two standard parallels.
      CALL MAPROJ('LC',30.,-100.,45.)
C
C   Reduce the value of 'MV' to make what MAPDRW produces match what
C   comes out of MAPBLA/ARSCAM.
      CALL MAPSTI ('MV',1)
C
C   Specify the corner points of the plot as lat/lon pairs.
      CALL MAPSET('CO',22.6,-120.,46.9,-64.2)
C
C   Initialize the transformations.
      CALL MAPINT()
C
C  Set the flag for MAPEOD to just consider boundary lines.
C
      IEODF = 1
      CALL GSPLCI(5)
      CALL MAPLOT
C  
C--------------------------------------
C  Station models at selected cities  |
C--------------------------------------
C
C   NUMC    - the number of cities.
C   ICITYS  - City names.
C   IDLYTS  - Daily hi/low labels for cities.
C   CITYUX  - X user coordinates for city locations.
C   CITYUY  - Y user coordinates for city locations.

      DO 110 I=1,NUMC
        CALL WMSETR('WBS - Wind barb size',0.035)
        IF (I.EQ.10 .OR. I.EQ.21 .OR. I.EQ.22 .OR. I.EQ.23 .OR.
     +      I.EQ.25) THEN
              CALL WMSETR('WBS - Wind barb size',0.028)
        ENDIF
        CALL MAPTRN(CITYUX(I),CITYUY(I),XO,YO)
        CALL WMSTNM(XO,YO,IMDAT(1,I))
  110 CONTINUE
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
