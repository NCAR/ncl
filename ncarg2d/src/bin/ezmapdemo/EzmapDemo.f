C
C $Id: EzmapDemo.f,v 1.16 2008-09-18 00:38:09 kennison Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      PROGRAM EMDEMO
C
C Declarations required to do color fill.
C
        PARAMETER (LAMA=7500000,NCRA=75000,NGPS=10,LRWK=2*NCRA)
C
        DIMENSION IAMA(LAMA),XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
        DIMENSION RWRK(LRWK)
        EQUIVALENCE (RWRK(1),XCRA(1)),(RWRK(NCRA+1),YCRA(1))
C
        EXTERNAL COLORA,DRAWLA,COLORB,DRAWLB
C
C Declare some internal common blocks so as to be able to retrieve
C various values for the State Plane projection.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
          DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
          INTEGER IPRF
        SAVE   /USGSC1/
C
C Declare a common block in which the directory name will be passed.
C
        COMMON /EDRGDI/ USNM
          CHARACTER*60 USNM
        SAVE   /EDRGDI/
C
C Declare a common block in which to keep track of the largest fill
C polygon generated.
C
        COMMON /RASIZE/ LLPG
        SAVE   /RASIZE/
C
C Declare a couple of variables that determine what projection will be
C used and how the portion of the projection to be shown is selected.
C
        CHARACTER*2 PTYP,LTYP,CTMP
C
C Declare a variable to determine what outline dataset will be used.
C
        CHARACTER*2 ODNM
C
C Declare some two-element arrays to hold values determining what
C portion of the projection is to be shown.
C
        DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C Declare a character variable into which to read commands.
C
        CHARACTER*1 COMD
C
C Declare a character variable to hold a plot label.
C
        CHARACTER*60 LABL
C
C Declare a temporary character variable to read labels and names into.
C
        CHARACTER*60 CHRT
C
C Declare the function and variable necessary to deal with area names.
C
        CHARACTER*128 MDFNME,FNME
C
C Declare a couple of arrays in which to define boxes to be filled.
C
        DIMENSION XBOX(4),YBOX(4)
C
C Declare the double-precision parameter array required for initializing
C the USGS transformations.
C
        DOUBLE PRECISION PARA(15)
C
C Declare arrays in which to put State Plane zone-number information.
C
        DIMENSION IZN0(139),IZN8(139)
C
C Declare arrays in which to save the coordinates of zone boundaries.
C
        DIMENSION PLT0(200,139),PLN0(200,139)
        DIMENSION PLT8(200,139),PLN8(200,139)
C
C Declare arrays in which to save the coordinates of zone center points
C (used to position the labels).
C
        DIMENSION CLT0(139),CLN0(139),CLT8(139),CLN8(139)
C
C Declare some double precision temporaries to be used in generating
C the Piet Hein super-ellipses that are used for the zone borders in
C place of rectangles.
C
        DOUBLE PRECISION DBLP,DANG,DSNA,DCSA
C
C Declare arrays to hold color indices to be passed to MDRGSC, defining
C the colors to be used on maps drawn from the RANGS/GSHHS database.
C
        DIMENSION LCOL(5),LCSF(5)
C
C Set the default value of the parameter that says whether or not the
C X workstation will be updated as changes are made.  (When making a
C movie, it's a good idea to set this to zero.)
C
        DATA IUXW / 1 /
C
C Set the default value of the flag that says whether the basic color
C scheme will be white on black (IBOW=0) or black on white (IBOW=1).
C
        DATA IBOW / 0 /
C
C Declare the "style" of plot to be done.
C
        DATA ISTY / 1 /
C
C Set the default values of the variables that position the map on the
C plotter frame.
C
        DATA XVPL,XVPR,YVPB,YVPT / .1,.9,.1,.9 /
C
C Set the default values of the parameters that determine the projection
C to be used.
C
        DATA PTYP,PLAT,PLON,ROTA / 'CE',3*0. /
C
C Set the default values of the parameters that determine what portion
C of the projection is shown.
C
        DATA LTYP,PLM1,PLM2,PLM3,PLM4 / 'MA',8*0. /
C
C Set the default value of the satellite altitude and the angles that
C determine where its camera is pointed.
C
        DATA SALT,SAN1,SAN2 / 3*0. /
C
C Set the default value of the parameter that determines what outline
C dataset will be used.
C
        DATA ODNM / 'CO' /
C
C Set the default value of the parameter saying at what level the new
C database "Earth..1" should be used.
C
        DATA ILVL / 3 /
C
C Set the default value of the parameter saying at what level the new
C RANGS/GSHHS database should be used.
C
        DATA IRGL / 5 /
C
C Set the default value of the miscellaneous minor parameters.
C
        DATA IPER,IELL,ILBL,IDOT,IDBD,GSPC,GSLA,GSLO,GLAT,GLON
     +     /    1,   0,   1,   0,  96,  5.,  0.,  0.,  0., 90. /
C
        DATA IINT,IRPF,SLAT
     +     /   50,   1, -1. /
C
C Set a couple of counters that keep track of how many NCGM and
C PostScript frames have been saved.
C
        DATA NFNG,NFPS / 0 , 0 /
C
C Set the default values of the parameters required to initialize the
C USGS transformations.
C
        DATA IPRJ,IZON,ISPH,PARA / 2 , 4400 , 8 , 15*0.D0 /
C
C Set the default value of the index that can be used to select a
C pre-defined plot.
C
        DATA IPDP / 1 /
C
C Set the default value of the flag that says whether to draw an
C informational label at the bottom of the plot.
C
        DATA ILPT / 1 /
C
C Set the default value of the major label at the top of the plot.
C
        DATA LABL / ' ' /
C
C Define the flag that controls whether or not lat/lon labels are to
C be displayed and the variable that specifies the width of characters
C to use in them.
C
        DATA ILLB,CSLL / -1,.012 /
C
C Define some quantities for the test during which the projection of
C a global spiral is drawn.
C
        DATA ISPC,SPLT,SPLN,NSPT / 5 , 30. , 30. , 400 /
C
C Define some quantities affecting the test during which fields of
C latitude and longitudes generated by using the inverse routines
C are contoured.
C
        DATA ICNC,NCDM / 6 , 501 /
C
C Define a constant that says how nearly rectangular the Piet Hein
C super-ellipses are to be.  (They are used in place of rectangles
C for the State Plane zones.)
C
        DATA DBLP / 10.D0 /
C
C Define a flag that says which set of State Plane zones to display.
C The value "-1" means "neither"; "0" means "NAD1927"; and "8" means
C "NAD1983".  Also define a variable that specifies the width of the
C characters to use in labeling the zones.
C
        DATA ISPG,CHSZ / -1 , .1 /
C
C Define the State Plane zone numbers.
C
        DATA (IZN0(I),IZN8(I),I=1,50)/
     +    0101,0101,0102,0102,5001,5001,5002,5002,5003,5003,
     +    5004,5004,5005,5005,5006,5006,5007,5007,5008,5008,
     +    5009,5009,5010,5010,0201,0201,0202,0202,0203,0203,
     +    0301,0301,0302,0302,0401,0401,0402,0402,0403,0403,
     +    0404,0404,0405,0405,0406,0406,0407,   0,0501,0501,
     +    0502,0502,0503,0503,0600,0600,0700,0700,1901,1901,
     +    0901,0901,0902,0902,0903,0903,1001,1001,1002,1002,
     +    5101,5101,5102,5102,5103,5103,5104,5104,5105,5105,
     +    1101,1101,1102,1102,1103,1103,1201,1201,1202,1202,
     +    1301,1301,1302,1302,1401,1401,1402,1402,1501,1501/
C
        DATA (IZN0(I),IZN8(I),I=51,100)/
     +    1502,1502,1601,1601,1602,1602,1701,1701,1702,1702,
     +    1703,1703,1801,1801,1802,1802,1900,1900,2001,2001,
     +    2002,2002,2101,   0,2102,   0,2103,   0,2111,2111,
     +    2112,2112,2113,2113,2201,2201,2202,2202,2203,2203,
     +    2301,2301,2302,2302,2401,2401,2402,2402,2403,2403,
     +       0,2500,2501,   0,2502,   0,2503,   0,   0,2600,
     +    2601,   0,2602,   0,2701,2701,2702,2702,2703,2703,
     +    2800,2800,2900,2900,3001,3001,3002,3002,3003,3003,
     +    3101,3101,3102,3102,3103,3103,3104,3104,3200,3200,
     +    3301,3301,3302,3302,3401,3401,3402,3402,3501,3501/
C
        DATA (IZN0(I),IZN8(I),I=101,139)/
     +    3502,3502,3601,3601,3602,3602,3701,3701,3702,3702,
     +    3800,3800,   0,3900,3901,   0,3902,   0,4001,4001,
     +    4002,4002,4100,4100,4201,4201,4202,4202,4203,4203,
     +    4204,4204,4205,4205,4301,4301,4302,4302,4303,4303,
     +    4400,4400,4501,4501,4502,4502,4601,4601,4602,4602,
     +    4701,4701,4702,4702,4801,4801,4802,4802,4803,4803,
     +    4901,4901,4902,4902,4903,4903,4904,4904,5201,5200,
     +    5201,5200,5202,5200,5300,   0,5400,   0/
C
C Open GKS.
C
        CALL GOPKS (6,0)
C
C Open and activate an X workstation.
C
        CALL GOPWK (1,0,8)
        CALL GACWK (1)
C
C Open and activate an NCGM workstation.
C
        CALL GOPWK (2,0,1)
        CALL GACWK (2)
C
C Open and activate a PostScript workstation.
C
        CALL GOPWK (3,0,20)
        CALL GACWK (3)
C
C Turn clipping off.
C
        CALL GSCLIP (0)
C
C Select a "fill area interior style" of "solid".
C
        CALL GSFAIS (1)
C
C Define colors on all the workstations.
C
        DO 101 IWID=1,3
          IF (IBOW.EQ.0) THEN
            CALL GSCR (IWID,  0,0.,0.,0.)
            CALL GSCR (IWID,  1,1.,1.,1.)
          ELSE
            CALL GSCR (IWID,  0,1.,1.,1.)
            CALL GSCR (IWID,  1,0.,0.,0.)
          END IF
          CALL GSCR (IWID,  2,1.,0.,0.)
          CALL GSCR (IWID,  3,0.,1.,0.)
          CALL GSCR (IWID,  4,0.,0.,1.)
          CALL GSCR (IWID,  5,0.,1.,1.)
          CALL GSCR (IWID,  6,1.,0.,1.)
          CALL GSCR (IWID,  7,1.,1.,0.)
          CALL GSCR (IWID,  8,.7,.7,0.)
          CALL GSCR (IWID,  9,1.,1.,0.)
          CALL GSCR (IWID, 10,0.,0.,0.)
          CALL GSCR (IWID, 11,1.,1.,.6)
          CALL GSCR (IWID, 12,1.,0.,0.)
          CALL GSCR (IWID,101,.2,.2,.8)
          CALL GSCR (IWID,102,.6,.4,.2)
          CALL GSCR (IWID,103,.2,.6,.4)
          CALL GSCR (IWID,104,.2,.8,.2)
          CALL GSCR (IWID,105,.4,.6,.2)
          CALL GSCR (IWID,106,.2,.4,.6)
          CALL GSCR (IWID,107,.6,.6,.6)
          CALL GSCR (IWID,108,.9,1.,1.)
          CALL GSCR (IWID,201,0.,0.,1.)  !  RANGS/GSHHS - Ocean
          CALL GSCR (IWID,202,0.,1.,0.)  !  RANGS/GSHHS - Land
          CALL GSCR (IWID,203,0.,1.,1.)  !  RANGS/GSHHS - Lake
          CALL GSCR (IWID,204,0.,1.,0.)  !  RANGS/GSHHS - Island in lake
          CALL GSCR (IWID,205,0.,1.,1.)  !  RANGS/GSHHS - Pond on island
          CALL GSCR (IWID,206,1.,1.,1.)  !  RANGS/GSHHS - Outlines
  101   CONTINUE
C
C Deactivate the NCGM and PostScript workstations.
C
        CALL GDAWK (2)
        CALL GDAWK (3)
C
C Put SETER into recovery mode.  No real error recovery is done in
C this program, but attempts to draw a plot that lead to an error
C are detected and the user is given another chance to avoid the
C error by changing parameter values.
C
        CALL ENTSR (IELD,1)
C
C Initialize PLOTCHAR font, character color, outline color, and box
C fill parameters.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('CC - CHARACTER COLOR',11)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETI ('OC - OUTLINE LINE COLOR',11)
        CALL PCSETI ('BF - BOX FLAG',3)
        CALL PCSETI ('BC(1) - BOX COLOR 1',12)
        CALL PCSETI ('BC(2) - BOX COLOR 2',10)
        CALL PCSETR ('BL - BOX LINE WIDTH',2.)
        CALL PCSETR ('BM - BOX MARGIN WIDTH',.5)
C
C Tell EZMAP what colors to use for the outlines and filled areas
C produced from the RANGS/GSHHS data.
C
        LCOL(1)=206
        LCOL(2)=206
        LCOL(3)=206
        LCOL(4)=206
        LCOL(5)=206
C
        LCSF(1)=201
        LCSF(2)=202
        LCSF(3)=203
        LCSF(4)=204
        LCSF(5)=205
C
        CALL MDRGSC (LCOL,LCSF)
C
C Generate boundary-line data for each of the State Plane zones.
C
        PRINT * , ' '
        PRINT * , 'Generating boundary-line data for State Plane zones.'
C
        CALL MAPROJ ('UT',0.,0.,0.)
C
        DO 104 ISPI=1,139
C
          IF (IZN0(ISPI).NE.0) THEN
C
            CALL ZERODA (PARA,15)
            CALL MPUTIN (2,IZN0(ISPI),0,PARA,0.D0,0.D0,0.D0,0.D0)
C
            EPSI=.001*(UMAX-UMIN)
C
            UMID=(UMIN+UMAX)/2.
            VMID=(VMIN+VMAX)/2.
            UDIF=.999*(UMAX-UMID)
            VDIF=.999*(VMAX-VMID)
C
            CALL MAPTRI (UMID,VMID,CLT0(ISPI),CLN0(ISPI))
C
            DO 102 I=1,200
C
              DANG=1.8D0*DBLE(I-1)*.017453292519943D0
              DSNA=SIN(DANG)
              DCSA=COS(DANG)
              R=1./REAL(((ABS(DCSA)/DBLE(UDIF))**DBLP+
     +                   (ABS(DSNA)/DBLE(VDIF))**DBLP)**(1.D0/DBLP))
              CALL MAPTRI (UMID+R*REAL(DCSA),VMID+R*REAL(DSNA),
     +                     PLT0(I,ISPI),PLN0(I,ISPI))
  102       CONTINUE
C
          END IF
C
          IF (IZN8(ISPI).NE.0) THEN
C
            CALL ZERODA (PARA,15)
            CALL MPUTIN (2,IZN8(ISPI),8,PARA,0.D0,0.D0,0.D0,0.D0)
C
            EPSI=.001*(UMAX-UMIN)
C
            UMID=(UMIN+UMAX)/2.
            VMID=(VMIN+VMAX)/2.
            UDIF=.999*(UMAX-UMID)
            VDIF=.999*(VMAX-VMID)
C
            CALL MAPTRI (UMID,VMID,CLT8(ISPI),CLN8(ISPI))
C
            DO 103 I=1,200
              DANG=1.8D0*DBLE(I-1)*.017453292519943D0
              DSNA=SIN(DANG)
              DCSA=COS(DANG)
              R=1./REAL(((ABS(DCSA)/DBLE(UDIF))**DBLP+
     +                   (ABS(DSNA)/DBLE(VDIF))**DBLP)**(1.D0/DBLP))
              CALL MAPTRI (UMID+R*REAL(DCSA),VMID+R*REAL(DSNA),
     +                     PLT8(I,ISPI),PLN8(I,ISPI))
  103       CONTINUE
C
          END IF
C
  104   CONTINUE
C
C Initialize the variable that will be returned to EZMAP when it asks
C for the name of the directory containing the RANGS/GSHHS data.
C
        USNM=' '
C
C If the X workstation is not to be updated, jump to get user input.
C
  105   IF (IUXW.EQ.0) GO TO 106
C
C Clear the X workstation.
C
        PRINT * , ' '
        PRINT * , 'Clearing the workstation.'
C
        CALL GCLRWK (1,1)
C
C Draw a map.
C
        PRINT * , ' '
        PRINT * , 'Drawing the map.'
C
        CALL MPSETI ('PE',IPER)
        CALL MPSETI ('EL',IELL)
        CALL MPSETI ('LA',ILBL)
        CALL MPSETI ('DO',IDOT)
        CALL MPSETI ('DD',IDBD)
        CALL MPSETR ('GR',GSPC)
        CALL MPSETR ('GT',GSLA)
        CALL MPSETR ('GN',GSLO)
        CALL MPSETR ('GP',1000.*GLAT+GLON)
        CALL MPSETI ('II',1000*IINT+IINT)
        CALL MPSETI ('RP',IRPF)
        CALL MPSETR ('SL',SLAT)
C
        CALL MAPPOS (XVPL,XVPR,YVPB,YVPT)
C
        CALL MAPROJ (PTYP,PLAT,PLON,ROTA)
C
        IF (PTYP.EQ.'SV') THEN
          CALL MPSETR ('SA',SALT)
          CALL MPSETR ('S1',SAN1)
          CALL MPSETR ('S2',SAN2)
        END IF
C
        CALL MAPSET (LTYP,PLM1,PLM2,PLM3,PLM4)
C
        IF (PTYP.NE.'UT') THEN
          CALL MAPINT
        ELSE
          CALL MPUTIN (IPRJ,IZON,ISPH,PARA,0.D0,0.D0,0.D0,0.D0)
        END IF
C
        IF (NERRO(NERR).NE.0) GO TO 901
        CALL GETSET (RVPL,RVPR,RVPB,RVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        EPSI=1.E-6*MIN(RVPR-RVPL,RVPT-RVPB)
        RVPL=RVPL+EPSI
        RVPR=RVPR-EPSI
        RVPB=RVPB+EPSI
        RVPT=RVPT-EPSI
        PRINT * , ' '
        PRINT * , 'U/V limits used: ', XWDL,XWDR,YWDB,YWDT
        IF (ODNM.EQ.'RG'.AND.IRGL.GE.5) THEN
          CALL MDRGDL (IRGL)
          IRGL=IRGL+5
          PRINT * , ' '
          PRINT * , 'RANGS/GSHHS data level picked:',MOD(IRGL,5)
        END IF
        IF (ISTY.EQ.0) THEN
          CALL MAPGRD
          CALL MAPLBL
          IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +        ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
            IF (ODNM.EQ.'E1') CALL MPLNDR ('Earth..1',ILVL)
            IF (ODNM.EQ.'E2') CALL MPLNDR ('Earth..2',ILVL)
            IF (ODNM.EQ.'E3') CALL MPLNDR ('Earth..3',ILVL)
            IF (ODNM.EQ.'E4') CALL MPLNDR ('Earth..4',ILVL)
          ELSE IF (ODNM.EQ.'RG') THEN
            CALL MDRGOL (MOD(IRGL,5),RWRK,LRWK)
            IF (NERRO(NERR).NE.0) GO TO 901
          ELSE
            CALL MAPLOT
            IF (NERRO(NERR).NE.0) GO TO 901
          END IF
          CALL DRSPGD (ISPG,IZN0,PLT0,PLN0,CLT0,CLN0,
     +                      IZN8,PLT8,PLN8,CLT8,CLN8,CHSZ)
        ELSE
          IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +        ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
            CALL MPSETI ('VS',20)
            CALL ARINAM (IAMA,LAMA)
            IF (NERRO(NERR).NE.0) GO TO 901
            IF (ODNM.EQ.'E1') CALL MPLNAM ('Earth..1',ILVL,IAMA)
            IF (ODNM.EQ.'E2') CALL MPLNAM ('Earth..2',ILVL,IAMA)
            IF (ODNM.EQ.'E3') CALL MPLNAM ('Earth..3',ILVL,IAMA)
            IF (ODNM.EQ.'E4') CALL MPLNAM ('Earth..4',ILVL,IAMA)
            IF (NERRO(NERR).NE.0) GO TO 901
            LLPG=0
            CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLORB)
            IF (NERRO(NERR).NE.0) GO TO 901
            PRINT * , ' '
            PRINT * , 'Area map length: ',LAMA-IAMA(6)+IAMA(5)+1
            PRINT * , 'Longest polygon: ',LLPG
            IF (ODNM.EQ.'E1') CALL MPLNDR ('Earth..1',ILVL)
            IF (ODNM.EQ.'E2') CALL MPLNDR ('Earth..2',ILVL)
            IF (ODNM.EQ.'E3') CALL MPLNDR ('Earth..3',ILVL)
            IF (ODNM.EQ.'E4') CALL MPLNDR ('Earth..4',ILVL)
            CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DRAWLB)
            IF (NERRO(NERR).NE.0) GO TO 901
            IAIC=IGETAI (IAMA,CFUX(.5),CFUY(.5),IAAI,IAGI,NGPS,NOGU,1)
            PRINT * , ' '
            PRINT * , 'AREA IDENTIFIER AT CENTER: ',IAIC
            FNME=MDFNME(IAIC,1)
            PRINT * , 'NAME OF AREA AT CENTER: ',
     +                                   FNME(MDIFNB(FNME):MDILNB(FNME))
          ELSE IF (ODNM.EQ.'RG') THEN
            CALL MDRGSF (MOD(IRGL,5),RWRK,LRWK,IAMA,LAMA)
            IF (NERRO(NERR).NE.0) GO TO 901
            CALL MAPGRD
            IF (NERRO(NERR).NE.0) GO TO 901
          ELSE
            CALL MPSETC ('OU',ODNM)
            CALL MPSETI ('VS',0)
            CALL ARINAM (IAMA,LAMA)
            CALL MAPBLA (IAMA)
            IF (NERRO(NERR).NE.0) GO TO 901
            LLPG=0
            CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLORA)
            IF (NERRO(NERR).NE.0) GO TO 901
            PRINT * , ' '
            PRINT * , 'Area map length: ',LAMA-IAMA(6)+IAMA(5)+1
            PRINT * , 'Longest polygon: ',LLPG
            CALL MAPLOT
            IF (NERRO(NERR).NE.0) GO TO 901
            CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DRAWLA)
            IF (NERRO(NERR).NE.0) GO TO 901
            IAIC=IGETAI (IAMA,CFUX(.5),CFUY(.5),IAAI,IAGI,NGPS,NOGU,1)
            PRINT * , ' '
            PRINT * , 'AREA IDENTIFIER AT CENTER: ',IAIC
          END IF
          CALL MAPLMB
          CALL MAPLBL
          CALL DRSPGD (ISPG,IZN0,PLT0,PLN0,CLT0,CLN0,
     +                      IZN8,PLT8,PLN8,CLT8,CLN8,CHSZ)
        END IF
        IF (ILLB.NE.0) THEN
          P=REAL(SIGN(1,ILLB))
          CALL PCSETI ('BF - BOX FLAG',0)
          CALL MDLBLT (RVPL,RVPB,RVPL,RVPT,+P*CSLL,    0.,CSLL,0.,-P)
          CALL MDLBLT (RVPR,RVPB,RVPR,RVPT,-P*CSLL,    0.,CSLL,0.,+P)
          CALL MDLBLN (RVPL,RVPB,RVPR,RVPB,0.,+1.5*P*CSLL,CSLL,0.,0.)
          CALL MDLBLN (RVPL,RVPT,RVPR,RVPT,0.,-1.5*P*CSLL,CSLL,0.,0.)
          CALL PCSETI ('BF - BOX FLAG',3)
        END IF
        IF (LABL.NE.' ') CALL PLCHHQ (CFUX(.5),CFUY(.965),
     +                                LABL(1:MPILNB(LABL)),
     +                                          .018,0.,0.)
        IF (ILPT.NE.0) CALL DRPTLB (PTYP,IPRJ,IZON,ISPH,PARA,IPRF,
     +                                                  ODNM,ILVL)
C
C Flush buffers and update the X workstation.
C
        CALL SFLUSH
        CALL GUWK (1,0)
C
C Let the user enter a command.
C
  106   PRINT * , ' '
        PRINT * , 'Enter a command (H for help, Q for quit):'
C
        READ  '(A1)', COMD
C
        IF      (COMD.EQ.'H'.OR.COMD.EQ.'h') THEN
C
          PRINT * , ' '
          PRINT * , 'Possible commands are as follows:'
          PRINT * , ' '
          PRINT * , '  H  =>  Help (this is it)'
          PRINT * , '  C  =>  Change parameters (in several groups)'
          PRINT * , '  P  =>  Plot (specified predefined plots)'
          PRINT * , '  Q  =>  Quit (stop, halt, cease and desist)'
          PRINT * , '  R  =>  Redraw (for use after resizing X window)'
          PRINT * , '  S  =>  Save (to an NCGM or Postscript file)'
          PRINT * , '  T  =>  Tests (of three different kinds)'
          PRINT * , ' '
          PRINT * , 'In user responses, case is ignored.  When a yes'
          PRINT * , 'or no answer is requested, hitting RETURN means'
          PRINT * , 'no.  When a numeric value is requested, hitting'
          PRINT * , 'RETURN implies no change in the current value.'
          PRINT * , ' '
          PRINT * , 'Most EZMAP errors will not result in termination'
          PRINT * , 'of the demo.  It should usually be obvious what'
          PRINT * , 'needs to be changed to avoid the error.'
C
          GO TO 106
C
        ELSE IF (COMD.EQ.'C'.OR.COMD.EQ.'c') THEN
C
          PRINT * , ' '
          PRINT * , 'Changing internal parameter values.'
C
          PRINT * , ' '
          PRINT * , 'Change position of map on plotter frame (Y or N)?'
          READ  '(A1)', COMD
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
            PRINT * , ' '
            PRINT * , 'Current left-edge value:  ',XVPL
            PRINT * , 'Current right-edge value: ',XVPR
            PRINT * , 'Current bottom-edge value:',YVPB
            PRINT * , 'Current top-edge value:   ',YVPT
            PRINT * , ' '
            PRINT * , 'Enter new left-edge value (between 0 and 1):'
            CALL EMRDRN (XVPL,XVPL)
            XVPL=MAX(0.,MIN(1.,XVPL))
            PRINT * , ' '
            PRINT * , 'Enter new right-edge value (between 0 and 1):'
            CALL EMRDRN (XVPR,XVPR)
            XVPR=MAX(0.,MIN(1.,XVPR))
            PRINT * , ' '
            PRINT * , 'Enter new bottom-edge value (between 0 and 1):'
            CALL EMRDRN (YVPB,YVPB)
            YVPB=MAX(0.,MIN(1.,YVPB))
            PRINT * , ' '
            PRINT * , 'Enter new top-edge value (between 0 and 1):'
            CALL EMRDRN (YVPT,YVPT)
            YVPT=MAX(0.,MIN(1.,YVPT))
          END IF
C
          PRINT * , ' '
          PRINT * , 'Change projection type, center, rotation angle,'
          PRINT * , 'other values for USGS transformations (Y or N)?'
          READ  '(A1)', COMD
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
            PRINT * , ' '
            PRINT * , 'These are the possible projection types:'
            PRINT * , ' '
            PRINT * , '  AE => Azimuthal Equidistant'
            PRINT * , '  AI => Aitoff'
            PRINT * , '  CE => Cylindrical Equidistant'
            PRINT * , '  EA => Cylindrical Equal-Area'
            PRINT * , '  ER => Equirectangular'
            PRINT * , '  GN => Gnomonic'
            PRINT * , '  HA => Hammer'
            PRINT * , '  LC => Lambert Conformal Conic'
            PRINT * , '  LE => Lambert Equal-area'
            PRINT * , '  ME => Mercator'
            PRINT * , '  MO => Mollweide'
            PRINT * , '  MT => "Mollweide-type"'
            PRINT * , '  OR => Orthographic'
            PRINT * , '  RM => Rotated Mercator'
            PRINT * , '  RO => Robinson'
            PRINT * , '  ST => Stereographic'
            PRINT * , '  SV => Satellite-View'
            PRINT * , '  UT => USGS Transformations'
            PRINT * , '  WT => Winkel Tripel'
            PRINT * , ' '
            PRINT * , 'Current projection type: ',PTYP
            PRINT * , ' '
            PRINT * , 'Enter new projection type:'
            READ  '(A2)', CTMP
            CALL MUPPER (CTMP)
            IF (CTMP.EQ.'AE'.OR.CTMP.EQ.'AI'.OR.CTMP.EQ.'CE'.OR.
     +          CTMP.EQ.'EA'.OR.CTMP.EQ.'ER'.OR.CTMP.EQ.'GN'.OR.
     +          CTMP.EQ.'HA'.OR.CTMP.EQ.'LC'.OR.CTMP.EQ.'LE'.OR.
     +          CTMP.EQ.'ME'.OR.CTMP.EQ.'MO'.OR.CTMP.EQ.'MT'.OR.
     +          CTMP.EQ.'OR'.OR.CTMP.EQ.'RM'.OR.CTMP.EQ.'RO'.OR.
     +          CTMP.EQ.'ST'.OR.CTMP.EQ.'SV'.OR.CTMP.EQ.'UT'.OR.
     +          CTMP.EQ.'WT') PTYP=CTMP
            IF (PTYP.EQ.'LC') THEN
              PRINT * , ' '
              PRINT * , 'Current central meridian:      ',PLON
              PRINT * , 'Current 1st standard parallel: ',PLAT
              PRINT * , 'Current 2nd standard parallel: ',ROTA
              PRINT * , ' '
              PRINT * , 'Enter new central meridian (between -180 and 18
     +0):'
              CALL EMRDRN (PLON,PLON)
              PLON=MAX(-180.,MIN(180.,PLON))
              PRINT * , ' '
              PRINT * , 'Enter new 1st standard parallel (between -90 an
     +d 90):'
              CALL EMRDRN (PLAT,PLAT)
              PLAT=MAX(-90.,MIN(90.,PLAT))
              PRINT * , ' '
              PRINT * , 'Enter new 2nd standard parallel (between -90 an
     +d 90):'
              CALL EMRDRN (ROTA,ROTA)
              ROTA=MAX(-90.,MIN(90.,ROTA))
            ELSE IF (PTYP.EQ.'RM') THEN
              PRINT * , ' '
              PRINT * , 'Current central meridian:      ',PLON
              PRINT * , 'Current rotation angle:        ',ROTA
              PRINT * , ' '
              PRINT * , 'Enter new central meridian (between -180 and 18
     +0):'
              CALL EMRDRN (PLON,PLON)
              PLON=MAX(-180.,MIN(180.,PLON))
              PRINT * , ' '
              PRINT * , 'Enter new rotation angle (between -180 and 180)
     +:'
              CALL EMRDRN (ROTA,ROTA)
              ROTA=MAX(-180.,MIN(180.,ROTA))
            ELSE IF (PTYP.EQ.'UT') THEN
              PRINT * , ' '
              PRINT * , 'For most of the USGS transformations, one may '
              PRINT * , 'select any one of various "spheroids"; each of'
              PRINT * , 'these is a particular ellipsoid having semi-  '
              PRINT * , 'major and semiminor axes of specified lengths '
              PRINT * , '(in meters).  A couple of the transformations '
              PRINT * , 'require using a particular spheroid and some  '
              PRINT * , 'require a "sphere of reference" with a radius '
              PRINT * , 'equal to the semimajor axis of the selected   '
              PRINT * , 'spheroid.  The possible spheroids are these:  '
              PRINT * , '                                              '
              PRINT * , '  -1 => arbitrary, user-specified             '
              PRINT * , '   0 => Clarke 1866                           '
              PRINT * , '   1 => Clarke 1880                           '
              PRINT * , '   2 => Bessel                                '
              PRINT * , '   3 => New International 1967                '
              PRINT * , '   4 => International 1909                    '
              PRINT * , '   5 => WGS 72                                '
              PRINT * , '   6 => Everest                               '
              PRINT * , '   7 => WGS 66                                '
              PRINT * , '   8 => GRS 1980                              '
              PRINT * , '   9 => Airy                                  '
              PRINT * , '  10 => Modified Everest                      '
              PRINT * , '  11 => Modified Airy                         '
              PRINT * , '  12 => WGS 84                                '
              PRINT * , '  13 => Southeast Asia                        '
              PRINT * , '  14 => Australian National                   '
              PRINT * , '  15 => Krassovsky                            '
              PRINT * , '  16 => Hough                                 '
              PRINT * , '  17 => Mercury 1960                          '
              PRINT * , '  18 => Modified Mercury 1968                 '
              PRINT * , '  19 => Normal Sphere                         '
              PRINT * , ' '
              PRINT * , 'The spheroid currently selected:',ISPH
              PRINT * , ' '
              PRINT * , 'Enter the desired spheroid number:'
              CALL EMRDIN (ISPH,ISPH)
              ISPH=MAX(-1,MIN(19,ISPH))
              IF (ISPH.LT.0) THEN
                PRINT * , ' '
                PRINT * , 'We need the length of the semimajor axis of '
                PRINT * , 'the ellipsoid (the equatorial radius), in   '
                PRINT * , 'meters.  The value must be greater than or  '
                PRINT * , 'equal to 1. (The actual equatorial radius   '
                PRINT * , 'of the earth is about 6,378,000 meters, but '
                PRINT * , 'a smaller value can be used if the scale of '
                PRINT * , 'the map is unimportant.)                    '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(1)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(1)))
                PARA(1)=DBLE(MAX(.1,TEMP))
                PRINT * , ' '
                PRINT * , 'We need the value of the square of the      '
                PRINT * , 'eccentricy of the ellipsoid; it must be     '
                PRINT * , 'less than .1 (generally, it''s much less).  '
                PRINT * , 'A value greater than 1 is treated as the    '
                PRINT * , 'length of the semiminor axis (the polar     '
                PRINT * , 'radius), in meters.  (The actual polar      '
                PRINT * , 'radius of the earth is about 6,357,000      '
                PRINT * , 'meters.)                                    '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(2)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(2)))
                PARA(2)=DBLE(MAX(0.,TEMP))
              END IF
              PRINT * , ' '
              PRINT * , 'Possible USGS transformations are as follows: '
              PRINT * , '                                              '
              PRINT * , '   1 => Universal Transverse Mercator (UTM)   '
              PRINT * , '   2 => State Plane Coordinates               '
              PRINT * , '   3 => Alber''s Equal-Area Conic             '
              PRINT * , '   4 => Lambert Conformal Conic               '
              PRINT * , '   5 => Mercator                              '
              PRINT * , '   6 => Polar Stereographic                   '
              PRINT * , '   7 => Polyconic                             '
              PRINT * , '   8 => Equidistant Conic                     '
              PRINT * , '   9 => Transverse Mercator                   '
              PRINT * , '  10 => Stereographic                         '
              PRINT * , '  11 => Lambert Azimuthal Equal-Area          '
              PRINT * , '  12 => Azimuthal Equidistant                 '
              PRINT * , '  13 => Gnomonic                              '
              PRINT * , '  14 => Orthographic                          '
              PRINT * , '  15 => Perspective                           '
              PRINT * , '  16 => Sinusoidal                            '
              PRINT * , '  17 => Equirectangular                       '
              PRINT * , '  18 => Miller Cylindrical                    '
              PRINT * , '  19 => Van der Grinten I                     '
              PRINT * , '  20 => Oblique Mercator                      '
              PRINT * , '  21 => Robinson                              '
              PRINT * , '  22 => Space Oblique Mercator                '
              PRINT * , '  23 => Modified Stereographic for Alaska     '
              PRINT * , ' '
              PRINT * , 'The transformation currently selected:',IPRJ
              PRINT * , ' '
              PRINT * , 'Enter the desired transformation number:'
              CALL EMRDIN (IPRJ,IPRJ)
              IPRJ=MAX(1,MIN(23,IPRJ))
              IF      (IPRJ.EQ.1) THEN  !  UTM Coordinates
                PRINT * , ' '
                PRINT * , 'The UTM system requires a zone number with  '
                PRINT * , 'an absolute value from 1 to 60, inclusive.  '
                PRINT * , 'Each zone is a strip 6 degrees of longitude '
                PRINT * , 'in width.  Zone 1 is from 180W to 174W, zone'
                PRINT * , '2 from 174W to 168W, and so on; zone 60 is  '
                PRINT * , 'from 174E to 180E.  Positive zone numbers   '
                PRINT * , 'imply the Northern hemisphere and negative  '
                PRINT * , 'ones the Southern hemisphere.               '
                PRINT * , ' '
                PRINT * , 'The zone number currently selected:',IZON
                PRINT * , ' '
                PRINT * , 'Enter the desired zone number:'
                CALL EMRDIN (IZON,IZON)
                IZON=MAX(-60,MIN(60,IZON))
                IF (IZON.EQ.0) IZON=13
              ELSE IF (IPRJ.EQ.2) THEN  !  State Plane Coordinates
                PRINT * , ' '
                PRINT * , 'The older version of the State Plane system '
                PRINT * , 'is based on the North American Datum of 1927'
                PRINT * , '(NAD1927) and the newer one is based on the '
                PRINT * , 'North American Datum of 1983 (NAD1983).  The'
                PRINT * , 'first is selected by using a spheroid with  '
                PRINT * , 'index 0 and the second is selected by using '
                PRINT * , 'a spheroid with index 8.                    '
                PRINT * , ' '
                PRINT * , 'The current spheroid index:',ISPH
                PRINT * , ' '
                PRINT * , 'Enter the desired spheroid index:'
                CALL EMRDIN (ISPH,ISPH)
                IF (ISPH.NE.0.AND.ISPH.NE.8) THEN
                  PRINT * , ' '
                  PRINT * , 'The value entered is invalid; spheroid 8  '
                  PRINT * , 'will be used.                             '
                  ISPH=8
                END IF
                PRINT * , ' '
                PRINT * , 'Each version of the State Plane system uses '
                PRINT * , 'zone numbers to identify a particular state '
                PRINT * , 'or portion of a state.  The zone numbers are'
                PRINT * , 'four digits long; values may be found in the'
                PRINT * , 'on-line documentation and on some predefined'
                PRINT * , 'plots available from this demo program.     '
                PRINT * , ' '
                PRINT * , 'The current zone number:',IZON
                PRINT * , ' '
                PRINT * , 'Enter the desired zone number:'
                CALL EMRDIN (IZON,IZON)
                IF (ISPH.EQ.0) THEN
                  DO 114 I=1,139
                    IF (IZON.EQ.IZN0(I)) GO TO 116
  114             CONTINUE
                ELSE
                  DO 115 I=1,139
                    IF (IZON.EQ.IZN8(I)) GO TO 116
  115             CONTINUE
                END IF
                PRINT * , ' '
                PRINT * , 'The selected zone number is invalid.  The   '
                PRINT * , 'value 2800 (New Hampshire) will be used as  '
                PRINT * , 'an illustration.                            '
                IZON=2800
  116           CONTINUE
              ELSE IF (IPRJ.EQ.3) THEN  !  Alber's Equal-Area Conic
                PRINT * , ' '
                PRINT * , 'We need the latitude of the first standard  '
                PRINT * , 'parallel (between -90 and +90).             '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the second standard '
                PRINT * , 'parallel (between -90 and +90).             '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(4)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(4)))
                PARA(4)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the origin of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.4) THEN  !  Lambert Conformal Conic
                PRINT * , ' '
                PRINT * , 'We need the latitude of the first standard  '
                PRINT * , 'parallel (between -90 and +90).             '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the second standard '
                PRINT * , 'parallel (between -90 and +90).             '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(4)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(4)))
                PARA(4)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the origin of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.5) THEN  !  Mercator
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of true scale (between '
                PRINT * , '-90 and +90).                               '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.6) THEN  !  Polar Stereographic
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of true scale (between '
                PRINT * , '-90 and +90).                               '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.7) THEN  !  Polyconic
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the origin of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.8) THEN  !  Equidistant Conic
                PRINT * , ' '
                PRINT * , 'This transformation has a flag which is 0   '
                PRINT * , 'if there is only one standard parallel and  '
                PRINT * , '1 if there are two standard parallels.      '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',INT(PARA(9)
     +)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDIN (ITMP,INT(PARA(9)))
                PARA(9)=DBLE(MAX(0,MIN(1,ITMP)))
                PRINT * , ' '
                IF (PARA(9).EQ.0.D0) THEN
                  PRINT * , 'We need the latitude of the standard      '
                  PRINT * , 'parallel (between -90 and +90).           '
                ELSE
                  PRINT * , 'We need the latitude of the first         '
                  PRINT * , 'standard parallel (between -90 and +90).  '
                END IF
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                IF (PARA(9).NE.0.D0) THEN
                  PRINT * , ' '
                  PRINT * , 'We need the latitude of the second        '
                  PRINT * , 'standard parallel (between -90 and +90).  '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(4)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(4)))
                  PARA(4)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                END IF
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the origin of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.9) THEN  !  Transverse Mercator
                PRINT * , ' '
                PRINT * , 'We need the scale factor at the central     '
                PRINT * , 'meridian (use a value close to 1).          '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(.1,MIN(10.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the origin (between '
                PRINT * , '-90 and +90).                               '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.10) THEN  !  Stereographic
                PRINT * , ' '
                PRINT * , 'The Stereographic projection uses a sphere  '
                PRINT * , 'of reference, rather than an ellipsoid.     '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.11) THEN  !  Lambert Azimuthal Equal-Area
                PRINT * , ' '
                PRINT * , 'The Lambert Azimuthal Equal-Area projection '
                PRINT * , 'uses a sphere of reference, rather than an  '
                PRINT * , 'ellipsoid.                                  '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.12) THEN  !  Azimuthal Equidistant
                PRINT * , ' '
                PRINT * , 'The Azimuthal Equidistant projection uses   '
                PRINT * , 'a sphere of reference, rather than an       '
                PRINT * , 'ellipsoid.                                  '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.13) THEN  !  Gnomonic
                PRINT * , ' '
                PRINT * , 'The Gnomonic projection uses a sphere of    '
                PRINT * , 'reference, rather than an ellipsoid.        '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.14) THEN  !  Orthographic
                PRINT * , ' '
                PRINT * , 'The Orthographic projection uses a sphere of'
                PRINT * , 'reference, rather than an ellipsoid.        '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.15) THEN  !  Perspective
                PRINT * , ' '
                PRINT * , 'The Perspective projection uses a sphere of '
                PRINT * , 'reference, rather than an ellipsoid.        '
                PRINT * , ' '
                PRINT * , 'We need the height of the perspective point '
                PRINT * , 'above the surface (in meters).              '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(0.,TEMP))
                PRINT * , ' '
                PRINT * , 'We need the longitude of the center of the  '
                PRINT * , 'projection (between -180 and +180).         '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of the center of the   '
                PRINT * , 'projection (between -90 and +90).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.16) THEN  !  Sinusoidal
                PRINT * , ' '
                PRINT * , 'The Sinusoidal projection uses a sphere of  '
                PRINT * , 'reference, rather than an ellipsoid.        '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.17) THEN  !  Equirectangular
                PRINT * , ' '
                PRINT * , 'The Equirectangular projection uses a       '
                PRINT * , 'sphere of reference, rather than an         '
                PRINT * , 'ellipsoid.                                  '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need the latitude of true scale (between '
                PRINT * , '-90 and +90).                               '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(6)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(6)))
                PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.18) THEN  !  Miller Cylindrical
                PRINT * , ' '
                PRINT * , 'The Miller Cylindrical projection uses a    '
                PRINT * , 'sphere of reference, rather than an         '
                PRINT * , 'ellipsoid.                                  '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.19) THEN  !  Van der Grinten I
                PRINT * , ' '
                PRINT * , 'The Van der Grinten projection uses a sphere'
                PRINT * , 'of reference, rather than an ellipsoid.     '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.20) THEN  !  Oblique Mercator
                PRINT * , ' '
                PRINT * , 'The Oblique Mercator transformation has a   '
                PRINT * , 'flag which is 0 if the azimuth angle is to  '
                PRINT * , 'be specified indirectly and 1 if it is to   '
                PRINT * , 'be specified directly.                      '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',INT(PARA(13
     +))
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDIN (ITMP,INT(PARA(13)))
                PARA(13)=DBLE(MAX(0,MIN(1,ITMP)))
                PRINT * , ' '
                PRINT * , 'We need the scale factor at the center of   '
                PRINT * , 'the projection.                             '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(3)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(3)))
                PARA(3)=DBLE(MAX(.1,MIN(10.,TEMP)))
                IF (PARA(13).EQ.0.D0) THEN
                  PRINT * , ' '
                  PRINT * , 'We need the latitude of the origin of the '
                  PRINT * , 'projection (between -90 and +90).         '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(6)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(6)))
                  PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the longitude of point 1 on a line'
                  PRINT * , 'defining the azimuth angle (between -180  '
                  PRINT * , 'and +180).                                '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA( 9)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA( 9)))
                  PARA( 9)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the latitude of point 1 on a line '
                  PRINT * , 'defining the azimuth angle (between -90   '
                  PRINT * , 'and +90).                                 '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(10)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(10)))
                  PARA(10)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the longitude of point 2 on a line'
                  PRINT * , 'defining the azimuth angle (between -180  '
                  PRINT * , 'and +180).                                '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(11)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(11)))
                  PARA(11)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the latitude of point 2 on a line '
                  PRINT * , 'defining the azimuth angle (between -90   '
                  PRINT * , 'and +90).                                 '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(12)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(12)))
                  PARA(12)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                ELSE
                  PRINT * , ' '
                  PRINT * , 'We need the azimuth angle of the center   '
                  PRINT * , 'line, measured east of north (between -180'
                  PRINT * , 'and +180).                                '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(4)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(4)))
                  PARA(4)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the longitude of the origin of the'
                  PRINT * , 'projection (between -180 and +180).       '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(5)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(5)))
                  PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                  PRINT * , ' '
                  PRINT * , 'We need the latitude of the origin of the '
                  PRINT * , 'projection (between -90 and +90).         '
                  PRINT * , ' '
                  PRINT * , 'The current value is as follows:',PARA(6)
                  PRINT * , ' '
                  PRINT * , 'Enter the desired new value:'
                  CALL EMRDRN (TEMP,REAL(PARA(6)))
                  PARA(6)=DBLE(MAX(-90.,MIN(90.,TEMP)))
                END IF
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.21) THEN  !  Robinson
                PRINT * , ' '
                PRINT * , 'The Robinson projection uses a sphere of    '
                PRINT * , 'reference, rather than an ellipsoid.  This  '
                PRINT * , 'projection is much used by the National     '
                PRINT * , 'Geographic Society for world maps.          '
                PRINT * , ' '
                PRINT * , 'We need the longitude of the central        '
                PRINT * , 'meridian (between -180 and +180).           '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(5)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(5)))
                PARA(5)=DBLE(MAX(-180.,MIN(180.,TEMP)))
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.22) THEN  !  Space Oblique Mercator
                PRINT * , ' '
                PRINT * , 'The Space Oblique Mercator appears to be    '
                PRINT * , 'set up so that one can specify a particular '
                PRINT * , 'Landsat series and a particular orbit and   '
                PRINT * , 'get a map that shows with minimal distortion'
                PRINT * , 'the part of the earth passed over during    '
                PRINT * , 'that particular orbit.                      '
                PRINT * , ' '
                PRINT * , 'We need a Landsat number (an integer between'
                PRINT * , '1 and 5, inclusive).                        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',INT(PARA(3)
     +)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDIN (ITMP,INT(PARA(3)))
                PARA(3)=DBLE(MAX(1,MIN(5,ITMP)))
                PRINT * , ' '
                PRINT * , 'We need a path number (an integer between 1 '
                PRINT * , 'and 251, inclusive, for Landsat numbers 1, 2'
                PRINT * , 'and 3, and between 1 and 233, inclusive, for'
                PRINT * , 'Landsat numbers 4 and 5).                   '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',INT(PARA(4)
     +)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDIN (ITMP,INT(PARA(4)))
                IF (INT(PARA(3)).LE.3) THEN
                  PARA(4)=DBLE(MAX(1,MIN(251,ITMP)))
                ELSE
                  PARA(4)=DBLE(MAX(1,MIN(233,ITMP)))
                END IF
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE IF (IPRJ.EQ.23) THEN  !  Modified Stereographic (AK)
                PRINT * , ' '
                PRINT * , 'The Modified Stereographic projection for   '
                PRINT * , 'Alaska uses the Clarke 1866 spheroid, no    '
                PRINT * , 'matter what.                                '
                PRINT * , ' '
                PRINT * , 'We need a false easting (in meters).        '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(7)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(7)))
                PARA(7)=DBLE(TEMP)
                PRINT * , ' '
                PRINT * , 'We need a false northing (in meters).       '
                PRINT * , ' '
                PRINT * , 'The current value is as follows:',PARA(8)
                PRINT * , ' '
                PRINT * , 'Enter the desired new value:'
                CALL EMRDRN (TEMP,REAL(PARA(8)))
                PARA(8)=DBLE(TEMP)
              ELSE
                IZON=0
              END IF
            ELSE
              PRINT * , ' '
              PRINT * , 'Current pole latitude:  ',PLAT
              PRINT * , 'Current pole longitude: ',PLON
              PRINT * , 'Current rotation angle: ',ROTA
              PRINT * , ' '
              PRINT * , 'Enter new pole latitude (between -90 and 90):'
              CALL EMRDRN (PLAT,PLAT)
              PLAT=MAX(-90.,MIN(90.,PLAT))
              PRINT * , ' '
              PRINT * , 'Enter new pole longitude (between -180 and 180)
     +:'
              CALL EMRDRN (PLON,PLON)
              PLON=MAX(-180.,MIN(180.,PLON))
              PRINT * , ' '
              PRINT * , 'Enter new rotation angle (between -180 and 180)
     +:'
              CALL EMRDRN (ROTA,ROTA)
              ROTA=MAX(-180.,MIN(180.,ROTA))
            END IF
            IF (PTYP.EQ.'SV') THEN
              PRINT * , ' '
              PRINT * , 'Satellite-view projection will be used.'
              PRINT * , ' '
              PRINT * , 'Current satellite height :',SALT
              PRINT * , 'Current satellite angle 1:',SAN1
              PRINT * , 'Current satellite angle 2:',SAN2
              PRINT * , ' '
              PRINT * , 'Enter new satellite height (measured from'
              PRINT * , 'earth center, 1 or greater):'
              CALL EMRDRN (SALT,SALT)
              IF (SALT.LE.1.) SALT=0.
              PRINT * , ' '
              PRINT * , 'Enter new satellite angle 1 (0 to 180):'
              CALL EMRDRN (SAN1,SAN1)
              SAN1=MAX(0.,MIN(180.,SAN1))
              PRINT * , ' '
              PRINT * , 'Enter new satellite angle 2 (-180 to 180):'
              CALL EMRDRN (SAN2,SAN2)
              SAN2=MAX(-180.,MIN(180.,SAN2))
            ELSE IF (PTYP.EQ.'EA'.OR.PTYP.EQ.'ER'.OR.PTYP.EQ.'WT') THEN
              PRINT * , ' '
              PRINT * , 'The exact nature of this projection depends on'
              PRINT * , 'the position of a "standard parallel", which'
              PRINT * , 'is determined by the value of its latitude.'
              PRINT * , 'The parameter specifying this has the default'
              PRINT * , 'value -1, allowing Ezmap to choose for itself'
              PRINT * , 'where to put the standard parallel.'
              PRINT * , ' '
              PRINT * , 'Current latitude of standard parallel: ',SLAT
              PRINT * , ' '
              PRINT * , 'Be aware that, if you give this parameter a'
              PRINT * , 'non-negative value, it affects the display of'
              PRINT * , 'projections of types CE, EA, ER, and WT.'
              PRINT * , ' '
              PRINT * , 'Enter new value, between -1 and 90 (-1 to allow
     + use of built-in defaults):'
              CALL EMRDRN (SLAT,SLAT)
              SLAT=MAX(-1.,MIN(90.,SLAT))
            END IF
          END IF
C
          PRINT * , ' '
          PRINT * , 'Change limits selecting part of projection shown (Y
     + or N)?'
          READ  '(A1)', COMD
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
  108       PRINT * , ' '
            PRINT * , 'Possible limit selection types:'
            PRINT * , ' '
            PRINT * , '  MA => Maximum useful area'
            PRINT * , '  CO => Corners opposite one another'
            PRINT * , '  PO => Points on the edges'
            PRINT * , '  AN => Angular extents'
            PRINT * , '  LI => Limits in the U/V plane'
            PRINT * , '  GR => Grid of lat/lon values'
            PRINT * , ' '
            PRINT * , 'Current limit selection type: ',LTYP
            PRINT * , ' '
            PRINT * , 'Enter new limit selection type:'
            READ  '(A2)', CTMP
            CALL MUPPER (CTMP)
            IF (PTYP.EQ.'LC'.AND.CTMP.EQ.'AN') THEN
              PRINT * , ' '
              PRINT * , 'Can''t use angular extents with a Lambert confo
     +rmal conic projection.'
              GO TO 108
            END IF
            IF (CTMP.EQ.'MA'.OR.CTMP.EQ.'CO'.OR.CTMP.EQ.'PO'.OR.
     +          CTMP.EQ.'AN'.OR.CTMP.EQ.'LI'.OR.CTMP.EQ.'GR') LTYP=CTMP
            IF (LTYP.EQ.'CO') THEN
              PRINT * , ' '
              PRINT * , 'Current latitude, 1st corner point: ',PLM1(1)
              PRINT * , 'Current longitude, 1st corner point:',PLM2(1)
              PRINT * , 'Current latitude, 2nd corner point: ',PLM3(1)
              PRINT * , 'Current longitude, 2nd corner point:',PLM4(1)
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 1st corner point:'
              CALL EMRDRN (PLM1(1),PLM1(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 1st corner point:'
              CALL EMRDRN (PLM2(1),PLM2(1))
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 2nd corner point:'
              CALL EMRDRN (PLM3(1),PLM3(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 2nd corner point:'
              CALL EMRDRN (PLM4(1),PLM4(1))
              PLM1(2)=0.
              PLM2(2)=0.
              PLM3(2)=0.
              PLM4(2)=0.
            ELSE IF (LTYP.EQ.'PO') THEN
              PRINT * , ' '
              PRINT * , 'Current latitude of 1st edge point: ',PLM1(1)
              PRINT * , 'Current longitude of 1st edge point:',PLM1(2)
              PRINT * , 'Current latitude of 2nd edge point: ',PLM2(1)
              PRINT * , 'Current longitude of 2nd edge point:',PLM2(2)
              PRINT * , 'Current latitude of 3rd edge point: ',PLM3(1)
              PRINT * , 'Current longitude of 3rd edge point:',PLM3(2)
              PRINT * , 'Current latitude of 4th edge point: ',PLM4(1)
              PRINT * , 'Current longitude of 4th edge point:',PLM4(2)
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 1st edge point:'
              CALL EMRDRN (PLM1(1),PLM1(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 1st edge point:'
              CALL EMRDRN (PLM1(2),PLM1(2))
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 2nd edge point:'
              CALL EMRDRN (PLM2(1),PLM2(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 2nd edge point:'
              CALL EMRDRN (PLM2(2),PLM2(2))
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 3rd edge point:'
              CALL EMRDRN (PLM3(1),PLM3(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 3rd edge point:'
              CALL EMRDRN (PLM3(2),PLM3(2))
              PRINT * , ' '
              PRINT * , 'Enter new latitude of 4th edge point:'
              CALL EMRDRN (PLM4(1),PLM4(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude of 4th edge point:'
              CALL EMRDRN (PLM4(2),PLM4(2))
            ELSE IF (LTYP.EQ.'AN') THEN
              PRINT * , ' '
              PRINT * , 'Current angular distance to left:  ',PLM1(1)
              PRINT * , 'Current angular distance to right: ',PLM2(1)
              PRINT * , 'Current angular distance to bottom:',PLM3(1)
              PRINT * , 'Current angular distance to top:   ',PLM4(1)
              PRINT * , ' '
              PRINT * , 'Enter new angular distance to left:'
              CALL EMRDRN (PLM1(1),PLM1(1))
              PRINT * , ' '
              PRINT * , 'Enter new angular distance to right:'
              CALL EMRDRN (PLM2(1),PLM2(1))
              PRINT * , ' '
              PRINT * , 'Enter new angular distance to bottom:'
              CALL EMRDRN (PLM3(1),PLM3(1))
              PRINT * , ' '
              PRINT * , 'Enter new angular distance to top:'
              CALL EMRDRN (PLM4(1),PLM4(1))
              PLM1(2)=0.
              PLM2(2)=0.
              PLM3(2)=0.
              PLM4(2)=0.
            ELSE IF (LTYP.EQ.'LI') THEN
              PRINT * , ' '
              PRINT * , 'Current U coordinate of left edge:  ',PLM1(1)
              PRINT * , 'Current U coordinate of right edge: ',PLM2(1)
              PRINT * , 'Current V coordinate of bottom edge:',PLM3(1)
              PRINT * , 'Current V coordinate of top edge:   ',PLM4(1)
              PRINT * , ' '
              IF (PTYP.EQ.'LC'.OR.PTYP.EQ.'OR'.OR.PTYP.EQ.'SV') THEN
                PRINT * , 'U and V values should range from -1 to +1.'
              ELSE IF (PTYP.EQ.'ST'.OR.PTYP.EQ.'LE'.OR.PTYP.EQ.'GN')THEN
                PRINT * , 'U and V values should range from -2 to +2.'
              ELSE IF (PTYP.EQ.'AE'.OR.PTYP.EQ.'ME') THEN
                PRINT * , 'U and V values should range from -pi to +pi.'
              ELSE IF (PTYP.EQ.'CE') THEN
                PRINT * , 'U values should range from -180 to +180, V va
     +lues from -90/CSLS to +90/CSLS.'
                PRINT * , '(CSLS is the square of the cosine of the curr
     +ent value of the standard latitude.)'
              ELSE IF (PTYP.EQ.'EA') THEN
                PRINT * , 'U values should range from -pi to +pi, V valu
     +es from -1/CSLT to +1/CSLT.'
                PRINT * , '(CSLT is the cosine of the current value of t
     + he standard latitude.)'
              ELSE IF (PTYP.EQ.'MT') THEN
                PRINT * , 'U values should range from -2 to +2, V values
     + from -1 to +1.'
              ELSE IF (PTYP.EQ.'RO') THEN
                PRINT * , 'U values should range from -1 to +1, V values
     + from -.5072 to +.5072.'
              ELSE IF (PTYP.EQ.'AI') THEN
                PRINT * , 'U values should range from -pi to +pi, V valu
     +es from -pi/2 to +pi/2.'
              ELSE IF (PTYP.EQ.'HA'.OR.PTYP.EQ.'MO') THEN
                PRINT * , 'U values should range from -2*sqrt(2) to +2*s
     +qrt(2), V values from -sqrt(2) to +sqrt(2).'
              ELSE IF (PTYP.EQ.'WT') THEN
                PRINT * , 'U values should range from -(1+pi/2) to +(1+p
     +i/2), V values from -pi/2 to +pi/2.'
                PRINT * , '(The exact range depends on the current value
     + of the standard latitude.)'
              ELSE IF (PTYP.EQ.'UT') THEN
                PRINT * , ' '
                PRINT * , 'The ranges of U and V values will depend on'
                PRINT * , 'the particular USGS transformation chosen;'
                PRINT * , 'UMIN, UMAX, VMIN, and VMAX from the last'
                PRINT * , 'EZMAP initialization were as follows:'
                PRINT * , ' '
                PRINT * , ' ',XWDL,XWDR,YWDB,YWDT
              END IF
              PRINT * , ' '
              PRINT * , 'Enter new U coordinate of left edge:'
              CALL EMRDRN (PLM1(1),PLM1(1))
              PRINT * , ' '
              PRINT * , 'Enter new U coordinate of right edge:'
              CALL EMRDRN (PLM2(1),PLM2(1))
              PRINT * , ' '
              PRINT * , 'Enter new V coordinate of bottom edge:'
              CALL EMRDRN (PLM3(1),PLM3(1))
              PRINT * , ' '
              PRINT * , 'Enter new V coordinate of top edge:'
              CALL EMRDRN (PLM4(1),PLM4(1))
              PLM1(2)=0.
              PLM2(2)=0.
              PLM3(2)=0.
              PLM4(2)=0.
            ELSE IF (LTYP.EQ.'GR') THEN
              PRINT * , ' '
              PRINT * , 'Latitude now at bottom of range: ',PLM1(1)
              PRINT * , 'Longitude now at bottom of range:',PLM2(1)
              PRINT * , 'Latitude now at top of range:    ',PLM3(1)
              PRINT * , 'Longitude now at top of range:   ',PLM4(1)
              PRINT * , ' '
              PRINT * , 'Enter new latitude at bottom of range:'
              CALL EMRDRN (PLM1(1),PLM1(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude at bottom of range:'
              CALL EMRDRN (PLM2(1),PLM2(1))
              PRINT * , ' '
              PRINT * , 'Enter new latitude at top of range:'
              CALL EMRDRN (PLM3(1),PLM3(1))
              PRINT * , ' '
              PRINT * , 'Enter new longitude at top of range:'
              CALL EMRDRN (PLM4(1),PLM4(1))
              PLM1(2)=0.
              PLM2(2)=0.
              PLM3(2)=0.
              PLM4(2)=0.
            END IF
          END IF
C
          PRINT * , ' '
          PRINT * , 'Change outline dataset parameters (Y or N)?'
          READ  '(A1)', COMD
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
            PRINT * , ' '
            PRINT * , 'Possible outline dataset selectors:'
            PRINT * , ' '
            PRINT * , '  NO => None'
            PRINT * , '  CO => Continental outlines only'
            PRINT * , '  US => US outlines only'
            PRINT * , '  PS => Continental, political, and US outlines'
            PRINT * , '  PO => Continental and political outlines'
            PRINT * , '  E1 => The database "Earth..1"'
            PRINT * , '  E2 => The database "Earth..2"'
            PRINT * , '  E3 => The database "Earth..3"'
            PRINT * , '  E4 => The database "Earth..4"'
            PRINT * , '  RG => The RANGS/GSHHS database'
            PRINT * , ' '
            PRINT * , 'Current outline dataset selector: ',ODNM
            PRINT * , ' '
            PRINT * , 'Enter new outline dataset selector:'
            READ  '(A2)', CTMP
            CALL MUPPER (CTMP)
            IF (CTMP.EQ.'NO'.OR.CTMP.EQ.'CO'.OR.CTMP.EQ.'US'.OR.
     +          CTMP.EQ.'PS'.OR.CTMP.EQ.'PO'.OR.CTMP.EQ.'E1'.OR.
     +          CTMP.EQ.'E2'.OR.CTMP.EQ.'E3'.OR.CTMP.EQ.'E4'.OR.
     +          CTMP.EQ.'RG') ODNM=CTMP
            IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +          ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
              PRINT * , ' '
              PRINT * , 'Current database level specifier:',ILVL
              PRINT * , ' '
              PRINT * , 'Enter new database level specifier:'
              CALL EMRDIN (ILVL,ILVL)
            ELSE IF (ODNM.EQ.'RG') THEN
              PRINT * , ' '
              PRINT * , 'Current name of data directory: ',USNM
              PRINT * , ' '
              PRINT * , 'Enter new name of data directory:'
              READ (*,'(A60)') CHRT
              IF (CHRT.NE.' ') USNM=CHRT
              PRINT * , ' '
              PRINT * , 'Current database level specifier:',MIN(5,IRGL)
              PRINT * , ' '
              PRINT * , 'Enter new database level specifier (0 => use'
              PRINT * , 'highest resolution, 1 to 4 => use successively'
              PRINT * , 'lower resolutions, 5 => pick a value for me):'
              CALL EMRDIN (IRGL,IRGL)
              IRGL=MAX(0,MIN(5,IRGL))
              PRINT * , ' '
              PRINT * , 'Change any color-fill flags (Y or N)?'
              READ  '(A1)', COMD
              IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
                PRINT * , ' '
                IF (LCSF(1).GE.0) THEN
                  PRINT * , 'Ocean will be filled.'
                ELSE
                  PRINT * , 'Ocean will not be filled.'
                END IF
                PRINT * , ' '
                PRINT * , 'Toggle this fill flag (Y or N)?'
                READ  '(A1)', COMD
                IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') LCSF(1)=-LCSF(1)
                PRINT * , ' '
                IF (LCSF(2).GE.0) THEN
                  PRINT * , 'Land will be filled.'
                ELSE
                  PRINT * , 'Land will not be filled.'
                END IF
                PRINT * , ' '
                PRINT * , 'Toggle this fill flag (Y or N)?'
                READ  '(A1)', COMD
                IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') LCSF(2)=-LCSF(2)
                PRINT * , ' '
                IF (LCSF(3).GE.0) THEN
                  PRINT * , 'Lakes will be filled.'
                ELSE
                  PRINT * , 'Lakes will not be filled.'
                END IF
                PRINT * , ' '
                PRINT * , 'Toggle this fill flag (Y or N)?'
                READ  '(A1)', COMD
                IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') LCSF(3)=-LCSF(3)
                PRINT * , ' '
                IF (LCSF(4).GE.0) THEN
                  PRINT * , 'Islands in lakes will be filled.'
                ELSE
                  PRINT * , 'Islands in lakes will not be filled.'
                END IF
                PRINT * , ' '
                PRINT * , 'Toggle this fill flag (Y or N)?'
                READ  '(A1)', COMD
                IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') LCSF(4)=-LCSF(4)
                PRINT * , ' '
                IF (LCSF(5).GE.0) THEN
                  PRINT * , 'Ponds on islands in lakes will be filled.'
                ELSE
                  PRINT * , 'Ponds on islands in lakes will not be fille
     +d.'
                END IF
                PRINT * , ' '
                PRINT * , 'Toggle this fill flag (Y or N)?'
                READ  '(A1)', COMD
                IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') LCSF(5)=-LCSF(5)
                CALL MDRGSC (LCOL,LCSF)
              END IF
            END IF
          END IF
C
C Other parameters to provide access to:
C
C   GD LS
C
          PRINT * , ' '
          PRINT * , 'Change miscellaneous minor EZMAP parameters (Y or N
     +)?'
          READ  '(A1)', COMD
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
            PRINT * , ' '
            PRINT * , 'Current perimeter flag:           ',IPER
            PRINT * , 'Current elliptical-perimeter flag:',IELL
            PRINT * , 'Current labelling flag:           ',ILBL
            PRINT * , 'Current dotted-outline flag:      ',IDOT
            PRINT * , 'Current distance between dots:    ',IDBD
            PRINT * , 'Current grid spacing:             ',GSPC
            PRINT * , 'Current latitude line spacing:    ',GSLA
            PRINT * , 'Current longitude line spacing:   ',GSLO
            PRINT * , 'Current polar latitude control:   ',GLAT
            PRINT * , 'Current polar longitude control:  ',GLON
            PRINT * , 'Current standard latitude:        ',SLAT
            PRINT * , 'Current RG interpolation control: ',IINT
            PRINT * , 'Current RG data processing flag:  ',IRPF
            PRINT * , ' '
            PRINT * , 'Enter new perimeter flag (0 or 1):'
            CALL EMRDIN (IPER,IPER)
            IPER=MAX(0,MIN(1,IPER))
            PRINT * , ' '
            PRINT * , 'Enter new elliptical-perimeter flag (0 or 1):'
            CALL EMRDIN (IELL,IELL)
            IELL=MAX(0,MIN(1,IELL))
            PRINT * , ' '
            PRINT * , 'Enter new labelling flag (0 or 1):'
            CALL EMRDIN (ILBL,ILBL)
            ILBL=MAX(0,MIN(1,ILBL))
            PRINT * , ' '
            PRINT * , 'Enter new dotted-outline flag (0 or 1):'
            CALL EMRDIN (IDOT,IDOT)
            IDOT=MAX(0,MIN(1,IDOT))
            PRINT * , ' '
            PRINT * , 'Enter new distance between dots (out of 32768):'
            CALL EMRDIN (IDBD,IDBD)
            IDBD=MAX(1,IDBD)
            PRINT * , ' '
            PRINT * , 'Enter new grid spacing (0 turns grid off):'
            CALL EMRDRN (GSPC,GSPC)
            IGRD=MAX(0.,MIN(90.,GSPC))
            PRINT * , ' '
            PRINT * ,  'Enter new latitude line spacing (zero if same as
     + grid spacing):'
            CALL EMRDRN (GSLA,GSLA)
            GSLA=MAX(0.,MIN(90.,GSLA))
            PRINT * , ' '
            PRINT * , 'Enter new longitude line spacing (zero if same as
     + grid spacing):'
            CALL EMRDRN (GSLO,GSLO)
            GSLO=MAX(0.,MIN(90.,GSLO))
            PRINT * , ' '
            PRINT * , 'Enter latitude at edge of polar region (integer f
     +rom 0 to 90):'
            CALL EMRDRN (GLAT,GLAT)
            GLAT=MAX(0.,MIN(90.,REAL(INT(GLAT))))
            PRINT * , ' '
            PRINT * , 'Enter polar-region longitude-line spacing (0 to s
     +uppress them at poles):'
            CALL EMRDRN (GLON,GLON)
            GLON=MAX(0.,MIN(90.,GLON))
            PRINT * , ' '
            PRINT * , 'Current latitude of standard parallel: ',SLAT
            PRINT * , ' '
            PRINT * , 'Enter new value, between -1 and 90 (-1 to allow u
     +se of built-in defaults):'
            CALL EMRDRN (SLAT,SLAT)
            SLAT=MAX(-1.,MIN(90.,SLAT))
            PRINT * , ' '
            PRINT * , 'Enter RANGS/GSHHS interpolation flag:'
            CALL EMRDIN (IINT,IINT)
            IINT=MAX(1,MIN(999,IINT))
            PRINT * , ' '
            PRINT * , 'Enter RANGS/GSHHS data processing flag:'
            CALL EMRDIN (IRPF,IRPF)
            IRPF=MAX(0,MIN(2,IRPF))
          END IF
C
          PRINT * , ' '
          PRINT * , 'Change miscellaneous (non-EZMAP) parameters'
          PRINT * , 'within this demo program (Y or N)?'
          READ  '(A1)', COMD
C
          IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
C
            PRINT * , ' '
            IF (IBOW.EQ.0) THEN
              PRINT * , 'The color scheme flag is 0 (white on black).'
            ELSE
              PRINT * , 'The color scheme flag is 1 (black on white).'
            END IF
            PRINT * , ' '
            PRINT * , 'Enter new color scheme flag (0=>WOB, 1=>BOW):'
            CALL EMRDIN (ITMP,IBOW)
            ITMP=MAX(0,MIN(1,ITMP))
            IF (ITMP.NE.IBOW) THEN
              IBOW=ITMP
              CALL GCLRWK (1,1)
              DO 107 IWID=1,3
                IF (IWID.NE.1) CALL GACWK (IWID)
                IF (IBOW.EQ.0) THEN
                  CALL GSCR (IWID,  0,0.,0.,0.)
                  CALL GSCR (IWID,  1,1.,1.,1.)
                ELSE
                  CALL GSCR (IWID,  0,1.,1.,1.)
                  CALL GSCR (IWID,  1,0.,0.,0.)
                END IF
                CALL GSCR (IWID,  2,1.,0.,0.)
                CALL GSCR (IWID,  3,0.,1.,0.)
                CALL GSCR (IWID,  4,0.,0.,1.)
                CALL GSCR (IWID,  5,0.,1.,1.)
                CALL GSCR (IWID,  6,1.,0.,1.)
                CALL GSCR (IWID,  7,1.,1.,0.)
                CALL GSCR (IWID,  8,.7,.7,0.)
                CALL GSCR (IWID,  9,1.,1.,0.)
                CALL GSCR (IWID, 10,0.,0.,0.)
                CALL GSCR (IWID, 11,1.,1.,.6)
                CALL GSCR (IWID, 12,1.,0.,0.)
                CALL GSCR (IWID,101,.2,.2,.8)
                CALL GSCR (IWID,102,.6,.4,.2)
                CALL GSCR (IWID,103,.2,.6,.4)
                CALL GSCR (IWID,104,.2,.8,.2)
                CALL GSCR (IWID,105,.4,.6,.2)
                CALL GSCR (IWID,106,.2,.4,.6)
                CALL GSCR (IWID,107,.6,.6,.6)
                CALL GSCR (IWID,108,.9,1.,1.)
                IF (IWID.NE.1) CALL GDAWK (IWID)
  107         CONTINUE
            END IF
C
            PRINT * , ' '
            IF (ISTY.EQ.0) THEN
              PRINT * , 'The style flag is 0 (simple map).'
            ELSE
              PRINT * , 'The style flag is 1 (color-filled map).'
            END IF
            PRINT * , ' '
            PRINT * , 'Enter new style flag (0 => simple map, 1 => color
     +-filled map):'
            CALL EMRDIN (ISTY,ISTY)
            ISTY=MAX(0,MIN(1,ISTY))
C
            PRINT * , ' '
            IF (IUXW.EQ.0) THEN
              PRINT * , 'The X-workstation-update flag is 0 (off).'
            ELSE
              PRINT * , 'The X-workstation-update flag is 1 (on).'
            END IF
            PRINT * , ' '
            PRINT * , 'Enter new X-workstation-update flag (0=>off, 1=>o
     +n):'
            CALL EMRDIN (IUXW,IUXW)
            IUXW=MAX(0,MIN(1,IUXW))
C
            PRINT * , ' '
            IF (ILPT.EQ.0) THEN
              PRINT * , 'The informational label flag is 0 (no label).
     +'
            ELSE
              PRINT * , 'The informational label flag is 1 (do label).
     +'
            END IF
            PRINT * , ' '
            PRINT * , 'Enter new informational label flag (0=>off, 1=>on
     +):'
            CALL EMRDIN (ILPT,ILPT)
            ILPT=MAX(0,MIN(1,ILPT))
C
            PRINT * , ' '
            IF (LABL.EQ.' ') THEN
              PRINT * , 'No plot label is currently defined.'
            ELSE
              PRINT * , 'The following plot label is currently defined:'
              WRITE (*,'(2X,A60)') LABL
            END IF
            PRINT * , ' '
            PRINT * , 'Enter one of the following:'
            PRINT * , ' '
            PRINT * , '  A blank line (use the current label)'
            PRINT * , '  A new label (60 or fewer characters)'
            PRINT * , '  The word "NONE" or "none" (no label)'
            PRINT * , ' '
            READ (*,'(A60)') CHRT
            IF (CHRT.EQ.'NONE'.OR.CHRT.EQ.'none') THEN
              LABL=' '
            ELSE IF (CHRT.NE.' ') THEN
              LABL=CHRT
            END IF
C
            PRINT * , ' '
            IF (ILLB.EQ.0) THEN
              PRINT * , 'The lat/lon label flag is 0 (no labels).'
            ELSE IF (ILLB.EQ.-1) THEN
              PRINT * , 'The lat/lon label flag is -1 (labels outside ma
     +p edge).'
            ELSE IF (ILLB.EQ.-1) THEN
              PRINT * , 'The lat/lon label flag is +1 (labels inside map
     + edge.)'
            END IF
            PRINT * , 'Current width of characters in labels: ',CSLL
            PRINT * , ' '
            PRINT * , 'Enter new lat/lon label flag:'
            PRINT * , ' '
            PRINT * , '   0=>no lat/lon labels'
            PRINT * , '  -1=>labels outside map edge'
            PRINT * , '  +1=>labels inside map edge'
            PRINT * , ' '
            CALL EMRDIN (ILLB,ILLB)
            ILLB=MAX(-1,MIN(+1,ILLB))
            PRINT * , ' '
            PRINT * , 'Enter new character width:'
            PRINT * , ' '
            CALL EMRDRN (CSLL,CSLL)
            CSLL=MAX(.001,MIN(.1,CSLL))
C
            PRINT * , ' '
            IF (ISPG.LT.0) THEN
              PRINT * , 'State Plane zones will not be drawn.'
            ELSE IF (ISPG.EQ.0) THEN
              PRINT * , 'State Plane zones for NAD1927 will be drawn.'
            ELSE
              PRINT * , 'State Plane zones for NAD1983 will be drawn.'
            END IF
            PRINT * , ' '
            PRINT * , 'Enter new State Plane zone flag:'
            PRINT * , ' '
            PRINT * , '  -n => off'
            PRINT * , '   0 => NAD1927'
            PRINT * , '  +n => NAD1983'
            CALL EMRDIN (ISPG,ISPG)
            IF (ISPG.LT.0) THEN
              ISPG=-1
            ELSE IF (ISPG.GT.0) THEN
              ISPG=8
            END IF
            IF (ISPG.GE.0) THEN
              PRINT * , ' '
              PRINT * , 'Current value of character width for labels:',
     +CHSZ
              PRINT * , ' '
              PRINT * , 'Enter new character width (from .01 to 10 degre
     +es):'
              CALL EMRDRN (CHSZ,CHSZ)
              CHSZ=MAX(.01,MIN(10.,CHSZ))
            END IF
C
          END IF
C
          GO TO 105
C
        ELSE IF (COMD.EQ.'P'.OR.COMD.EQ.'p') THEN
C
          PRINT * , ' '
          PRINT * , 'Selecting a new predefined example plot.  Enter'
          PRINT * , 'one of the following:'
          PRINT * , ' '
          PRINT * , '  N => next plot in the list'
          PRINT * , '  P => previous plot in the list'
          PRINT * , '  S => specified plot in the list'
          PRINT * , ' '
          PRINT * , '  E => expand current plot into a square'
C
          READ  '(A1)', COMD
C
          IF      (COMD.EQ.'E'.OR.COMD.EQ.'e') THEN
            PRINT * , ' '
            PRINT * , 'Expanding the current plot into a square.'
            LTYP='LI'
            PLM1(1)=.5*(XWDL+XWDR)-.5*MAX(XWDR-XWDL,YWDT-YWDB)
            PLM2(1)=.5*(XWDL+XWDR)+.5*MAX(XWDR-XWDL,YWDT-YWDB)
            PLM3(1)=.5*(YWDB+YWDT)-.5*MAX(XWDR-XWDL,YWDT-YWDB)
            PLM4(1)=.5*(YWDB+YWDT)+.5*MAX(XWDR-XWDL,YWDT-YWDB)
            PLM1(2)=0.
            PLM2(2)=0.
            PLM3(2)=0.
            PLM4(2)=0.
            GO TO 105
          ELSE IF (COMD.EQ.'N'.OR.COMD.EQ.'n') THEN
            PRINT * , ' '
            PRINT * , 'Selected next one in list.'
          ELSE IF (COMD.EQ.'P'.OR.COMD.EQ.'p') THEN
            PRINT * , ' '
            PRINT * , 'Selected previous one in list.'
            IPDP=MOD(IPDP+69,72)+1
          ELSE IF (COMD.EQ.'S'.OR.COMD.EQ.'s') THEN
            PRINT * , ' '
            PRINT * , 'Enter an integer between 1 and 72 (use 1-17 for'
            PRINT * , 'original EZMAP projection types, 18-50 for USGS'
            PRINT * , 'transformations, and 51-72 for State Plane Grid'
            PRINT * , 'plots):'
            PRINT * , ' '
            PRINT * , '  1 => LC      25 => UT-04  49 => UT-22        '
            PRINT * , '  2 => GN      26 => UT-04  50 => UT-23        '
            PRINT * , '  3 => ME      27 => UT-05  51 => SPG-US-NE--27'
            PRINT * , '  4 => ST      28 => UT-06  52 => SPG-US-NE--83'
            PRINT * , '  5 => LE      29 => UT-06  53 => SPG-US-SE--27'
            PRINT * , '  6 => OR      30 => UT-07  54 => SPG-US-SE--83'
            PRINT * , '  7 => SV      31 => UT-08  55 => SPG-US-NEC-27'
            PRINT * , '  8 => AE      32 => UT-08  56 => SPG-US-NEC-83'
            PRINT * , '  9 => RO      33 => UT-08  57 => SPG-US-SEC-27'
            PRINT * , ' 10 => MT      34 => UT-08  58 => SPG-US-SEC-83'
            PRINT * , ' 11 => CE      35 => UT-09  59 => SPG-US-NWC-27'
            PRINT * , ' 12 => EA      36 => UT-10  60 => SPG-US-NWC-83'
            PRINT * , ' 13 => RM      37 => UT-11  61 => SPG-US-SWC-27'
            PRINT * , ' 14 => AI      38 => UT-12  62 => SPG-US-SWC-83'
            PRINT * , ' 15 => HA      39 => UT-13  63 => SPG-US-NW--27'
            PRINT * , ' 16 => MO      40 => UT-14  64 => SPG-US-NW--83'
            PRINT * , ' 17 => WT      41 => UT-15  65 => SPG-US-SW--27'
            PRINT * , ' 18 => UT-01   42 => UT-16  66 => SPG-US-SW--83'
            PRINT * , ' 19 => UT-01   43 => UT-17  67 => SPG-ALASKA-27'
            PRINT * , ' 20 => UT-02   44 => UT-18  68 => SPG-ALASKA-83'
            PRINT * , ' 21 => UT-02   45 => UT-19  69 => SPG-HAWAII-27'
            PRINT * , ' 22 => UT-02   46 => UT-20  70 => SPG-HAWAII-83'
            PRINT * , ' 23 => UT-03   47 => UT-20  71 => SPG-ALL----27'
            PRINT * , ' 24 => UT-03   48 => UT-21  72 => SPG-ALL----83'
            PRINT * , ' '
            CALL EMRDIN (IPDP,IPDP)
            IPDP=MAX(1,MIN(72,IPDP))
          END IF
C
          PRINT * , ' '
          PRINT * , 'Number of predefined example plot selected:',IPDP
C
          CALL INNPDP (IPDP,PTYP,PLAT,PLON,ROTA,LTYP,PLM1,PLM2,PLM3,
     +                 PLM4,IPRJ,IZON,ISPH,PARA,IDSL,ISPG,ODNM,ILVL,
     +                 CHSZ,LABL,ILLB)
C
          GO TO 105
C
        ELSE IF (COMD.EQ.'Q'.OR.COMD.EQ.'q') THEN
C
          CALL GDAWK (1)
          CALL GCLWK (1)
          CALL GCLWK (2)
          IF (NFPS.NE.0) THEN
            CALL GACWK (3)
            CALL GCLRWK (3,1)
            CALL GDAWK (3)
          END IF
          CALL GCLWK (3)
          CALL GCLKS
          STOP
C
        ELSE IF (COMD.EQ.'R'.OR.COMD.EQ.'r') THEN
C
          PRINT * , ' '
          PRINT * , 'Redrawing the picture.'
C
          GO TO 105
C
        ELSE IF (COMD.EQ.'S'.OR.COMD.EQ.'s') THEN
C
          PRINT * , ' '
          PRINT * , 'Save the current frame.  Note that parts of the'
          PRINT * , 'image resulting from "T" commands are not saved.'
          PRINT * , ' '
          PRINT * , 'One may save to an NCGM, to a Postscript file, or'
          PRINT * , 'both.'
          PRINT * , ' '
          PRINT * , 'Do you want to save to an NCAR CGM file (Y or N)?'
          READ  '(A1)', COMD
          IF (COMD.NE.'Y'.AND.COMD.NE.'y') THEN
            ISNG=0
          ELSE
            ISNG=1
            CALL GACWK (2)
            NFNG=NFNG+1
            IF (NFNG.GT.1) CALL GCLRWK (2,1)
          END IF
          PRINT * , ' '
          PRINT * , 'Do you want to save to a PostScript file (Y or N)?'
          READ  '(A1)', COMD
          IF (COMD.NE.'Y'.AND.COMD.NE.'y') THEN
            ISPS=0
          ELSE
            ISPS=1
            CALL GACWK (3)
            NFPS=NFPS+1
            IF (NFPS.GT.1) CALL GCLRWK (3,1)
          END IF
          IF (ISNG.NE.0.OR.ISPS.NE.0) THEN
            CALL GDAWK(1)
            CALL MAPPOS (XVPL,XVPR,YVPB,YVPT)
            CALL MAPROJ (PTYP,PLAT,PLON,ROTA)
            IF (PTYP.EQ.'SV') THEN
              CALL MPSETR ('SA',SALT)
              CALL MPSETR ('S1',SAN1)
              CALL MPSETR ('S2',SAN2)
            END IF
            CALL MAPSET (LTYP,PLM1,PLM2,PLM3,PLM4)
            CALL MPSETI ('PE',IPER)
            CALL MPSETI ('EL',IELL)
            CALL MPSETI ('LA',ILBL)
            CALL MPSETI ('DO',IDOT)
            CALL MPSETI ('DD',IDBD)
            CALL MPSETR ('GR',GSPC)
            CALL MPSETR ('GT',GSLA)
            CALL MPSETR ('GN',GSLO)
            CALL MPSETR ('GP',1000.*GLAT+GLON)
            CALL MPSETI ('II',1000*IINT+IINT)
            CALL MPSETI ('RP',IRPF)
            CALL MPSETR ('SL',SLAT)
            IF (PTYP.NE.'UT') THEN
              CALL MAPINT
            ELSE
              CALL MPUTIN (IPRJ,IZON,ISPH,PARA,0.D0,0.D0,0.D0,0.D0)
            END IF
            IF (ISTY.EQ.0) THEN
              CALL MAPGRD
              CALL MAPLBL
              IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +            ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
                IF (ODNM.EQ.'E1') CALL MPLNDR ('Earth..1',ILVL)
                IF (ODNM.EQ.'E2') CALL MPLNDR ('Earth..2',ILVL)
                IF (ODNM.EQ.'E3') CALL MPLNDR ('Earth..3',ILVL)
                IF (ODNM.EQ.'E4') CALL MPLNDR ('Earth..4',ILVL)
              ELSE IF (ODNM.EQ.'RG') THEN
                CALL MDRGOL (MOD(IRGL,5 ),RWRK,LRWK)
              ELSE
                CALL MAPLOT
              END IF
              CALL DRSPGD (ISPG,IZN0,PLT0,PLN0,CLT0,CLN0,
     +                          IZN8,PLT8,PLN8,CLT8,CLN8,CHSZ)
            ELSE
              IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +            ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
                CALL MPSETI ('VS',20)
                CALL ARINAM (IAMA,LAMA)
                IF (ODNM.EQ.'E1') CALL MPLNAM ('Earth..1',ILVL,IAMA)
                IF (ODNM.EQ.'E2') CALL MPLNAM ('Earth..2',ILVL,IAMA)
                IF (ODNM.EQ.'E3') CALL MPLNAM ('Earth..3',ILVL,IAMA)
                IF (ODNM.EQ.'E4') CALL MPLNAM ('Earth..4',ILVL,IAMA)
                CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLORB)
                IF (ODNM.EQ.'E1') CALL MPLNDR ('Earth..1',ILVL)
                IF (ODNM.EQ.'E2') CALL MPLNDR ('Earth..2',ILVL)
                IF (ODNM.EQ.'E3') CALL MPLNDR ('Earth..3',ILVL)
                IF (ODNM.EQ.'E4') CALL MPLNDR ('Earth..4',ILVL)
                CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DRAWLB)
              ELSE IF (ODNM.EQ.'RG') THEN
                CALL MDRGSF (MOD(IRGL,5),RWRK,LRWK,IAMA,LAMA)
                CALL MAPGRD
              ELSE
                CALL MPSETC ('OU',ODNM)
                CALL MPSETI ('VS',0)
                CALL ARINAM (IAMA,LAMA)
                CALL MAPBLA (IAMA)
                CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLORA)
                CALL MAPLOT
                CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DRAWLA)
              END IF
              CALL MAPLMB
              CALL MAPLBL
              CALL DRSPGD (ISPG,IZN0,PLT0,PLN0,CLT0,CLN0,
     +                          IZN8,PLT8,PLN8,CLT8,CLN8,CHSZ)
            END IF
            IF (ILLB.NE.0) THEN
              P=REAL(SIGN(1,ILLB))
              CALL PCSETI ('BF - BOX FLAG',0)
              CALL MDLBLT(RVPL,RVPB,RVPL,RVPT,+P*CSLL,    0.,CSLL,0.,-P)
              CALL MDLBLT(RVPR,RVPB,RVPR,RVPT,-P*CSLL,    0.,CSLL,0.,+P)
              CALL MDLBLN(RVPL,RVPB,RVPR,RVPB,0.,+1.5*P*CSLL,CSLL,0.,0.)
              CALL MDLBLN(RVPL,RVPT,RVPR,RVPT,0.,-1.5*P*CSLL,CSLL,0.,0.)
              CALL PCSETI ('BF - BOX FLAG',3)
            END IF
            IF (LABL.NE.' ') CALL PLCHHQ (CFUX(.5),CFUY(.965),
     +                                    LABL(1:MPILNB(LABL)),
     +                                              .018,0.,0.)
            IF (ILPT.NE.0) CALL DRPTLB (PTYP,IPRJ,IZON,ISPH,PARA,IPRF,
     +                                                      ODNM,ILVL)
            CALL SFLUSH
            IF (ISNG.NE.0) CALL GDAWK (2)
            IF (ISPS.NE.0) CALL GDAWK (3)
            CALL GACWK(1)
          END IF
C
          GO TO 106
C
        ELSE IF (COMD.EQ.'T'.OR.COMD.EQ.'t') THEN
C
          PRINT * , ' '
          PRINT * , 'Test some aspect of EZMAP.'
          PRINT * , ' '
          PRINT * , 'Enter one of the following commands:'
          PRINT * , ' '
          PRINT * , '  C  =>  Contour test'
          PRINT * , '  I  =>  Inverse routine test'
          PRINT * , '  S  =>  Spiral test'
C
          READ  '(A1)', COMD
C
          IF (COMD.EQ.'C'.OR.COMD.EQ.'c') THEN
            PRINT * , ' '
            PRINT * , 'This test uses EZMAP''s inverse transformation'
            PRINT * , 'routines to generate large arrays of latitudes'
            PRINT * , 'and longitudes as functions of the fractional'
            PRINT * , 'coordinates of points in the plotter frame.  The'
            PRINT * , 'arrays are contoured.  The contour lines should'
            PRINT * , 'be observed to lie on top of the lat/lon grid'
            PRINT * , 'previously drawn.'
            PRINT * , ' '
            PRINT * , 'As the latitudes and longitudes are computed,'
            PRINT * , 'points where the inverse transformation is not'
            PRINT * , 'defined are marked with little X''s whose color'
            PRINT * , 'implies the reason the inverse was not defined.'
            PRINT * , ' '
            PRINT * , 'One may change the color of the contours and the'
            PRINT * , 'dimensions of the arrays used.  Using larger'
            PRINT * , 'arrays will more accurately duplicate the grid'
            PRINT * , 'line positions, but is time-consuming.'
            PRINT * , ' '
            PRINT * , 'Change contouring parameters (Y or N)?:'
            READ  '(A1)', COMD
            IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
              PRINT * , ' '
              PRINT * , 'Current contouring color:          ',ICNC
              PRINT * , 'Current dimension of contour array:',NCDM
              PRINT * , ' '
              PRINT * , 'Enter new contouring color (between 0 and 7):'
              PRINT * , ' '
              IF (IBOW.EQ.0) THEN
                PRINT * , '  0 => black'
                PRINT * , '  1 => white'
              ELSE
                PRINT * , '  0 => white'
                PRINT * , '  1 => black'
              END IF
              PRINT * , '  2 => red'
              PRINT * , '  3 => green'
              PRINT * , '  4 => blue'
              PRINT * , '  5 => cyan'
              PRINT * , '  6 => magenta'
              PRINT * , '  7 => yellow'
              CALL EMRDIN (ICNC,ICNC)
              ICNC=MAX(0,MIN(7,ICNC))
              PRINT * , ' '
              PRINT * , 'Enter new contour array dimension (between 51 a
     +nd 501):'
              CALL EMRDIN (NCDM,NCDM)
              NCDM=MAX(51,MIN(501,NCDM))
            END IF
            PRINT * , ' '
            PRINT * , 'Contouring latitudes and longitudes.'
            CALL CONINV (ICNC,NCDM)
            CALL MAPINT
            GO TO 106
          ELSE IF (COMD.EQ.'I'.OR.COMD.EQ.'i') THEN
            IF (ISTY.NE.0.AND.ODNM.NE.'RG') THEN
              CALL GSPLCI (2)
              DO 109 I=0,9
                CALL PLCHMQ (CFUX(REAL(I)/10.+.05),CFUY(.05),
     +                       CHAR(ICHAR('0')+I),.015,0.,0.)
                CALL PLCHMQ (CFUX(.05),CFUY(REAL(I)/10.+.05),
     +                       CHAR(ICHAR('0')+I),.015,0.,0.)
  109         CONTINUE
              PRINT * , ' '
              PRINT * , 'This test allows one to repeatedly select one'
              PRINT * , 'of 100 subsquares of the plotter frame; each'
              PRINT * , 'subsquare so selected is further subdivided'
              PRINT * , 'into 100 subsubsquares and each of those is'
              PRINT * , 'filled with a color.  The color is selected'
              PRINT * , 'by a process that thoroughly tests the EZMAP'
              PRINT * , 'transformation routines (in both directions).'
              PRINT * , 'The effect should be to leave the map colored'
              PRINT * , 'as it was by the initial color fill.'
  110         PRINT * , ' '
              PRINT * , 'Enter X position (0-9, -1 to quit):'
              CALL EMRDIN (IHOR,0)
              IHOR=MAX(-1,MIN(9,IHOR))
              IF (IHOR.EQ.-1) GO TO 106
              PRINT * , ' '
              PRINT * , 'Enter Y position (0-9, -1 to quit):'
              CALL EMRDIN (IVER,0)
              IVER=MAX(-1,MIN(9,IVER))
              IF (IVER.EQ.-1) GO TO 106
              ICSF=1
              DO 113 I=0,99
                DO 112 J=0,99
                  XBOX(1)=CFUX(REAL(IHOR)/10.+REAL(I  )/1000.)
                  YBOX(1)=CFUY(REAL(IVER)/10.+REAL(J  )/1000.)
                  XBOX(2)=CFUX(REAL(IHOR)/10.+REAL(I+1)/1000.)
                  YBOX(2)=CFUY(REAL(IVER)/10.+REAL(J  )/1000.)
                  XBOX(3)=CFUX(REAL(IHOR)/10.+REAL(I+1)/1000.)
                  YBOX(3)=CFUY(REAL(IVER)/10.+REAL(J+1)/1000.)
                  XBOX(4)=CFUX(REAL(IHOR)/10.+REAL(I  )/1000.)
                  YBOX(4)=CFUY(REAL(IVER)/10.+REAL(J+1)/1000.)
                  XPOS=CFUX(REAL(IHOR)/10.+REAL(I  )/1000.+.0005)
                  YPOS=CFUY(REAL(IVER)/10.+REAL(J+1)/1000.+.0005)
                  CALL MAPTRI (XPOS,YPOS,RLAT,RLON)
                  IF (RLAT.NE.1.E12.AND.RLON.NE.1.E12) THEN
                    CALL MAPTRA (RLAT,RLON,XPOS,YPOS)
                    IF (XPOS.NE.1.E12.AND.YPOS.NE.1.E12) THEN
                      CALL ARGTAI (IAMA,XPOS,YPOS,IAAI,IAGI,NGPS,NAIR,
     +                                                           ICSF)
                      ICSF=0
                      IAI1=-1
                      DO 111 IAIR=1,NAIR
                        IF (IAGI(IAIR).EQ.1) IAI1=IAAI(IAIR)
  111                 CONTINUE
                      IF (IAI1.GT.0) THEN
                        IF (ODNM.NE.'E1'.AND.ODNM.NE.'E2'.AND.
     +                      ODNM.NE.'E3'.AND.ODNM.NE.'E4') THEN
                          CALL GSFACI (MAPACI(IAI1)+100)
                        ELSE
                          CALL GSFACI (MPISCI(IAI1)+100)
                        END IF
                        CALL GFA (4,XBOX,YBOX)
                      END IF
                    END IF
                  END IF
  112           CONTINUE
  113         CONTINUE
              CALL PLOTIF (REAL(IHOR  )/10.,REAL(IVER  )/10.,0)
              CALL PLOTIF (REAL(IHOR+1)/10.,REAL(IVER  )/10.,1)
              CALL PLOTIF (REAL(IHOR+1)/10.,REAL(IVER+1)/10.,1)
              CALL PLOTIF (REAL(IHOR  )/10.,REAL(IVER+1)/10.,1)
              CALL PLOTIF (REAL(IHOR  )/10.,REAL(IVER  )/10.,1)
              CALL SFLUSH
              CALL GUWK (1,0)
              GO TO 110
            ELSE
              PRINT * , ' '
              PRINT * , 'Can''t do that test with "style" flag = 0 or'
              PRINT * , 'when using outline dataset selector "RG".'
              GO TO 106
            END IF
          ELSE IF (COMD.EQ.'S'.OR.COMD.EQ.'s') THEN
            PRINT * , ' '
            PRINT * , 'This test allows one to draw the projection of'
            PRINT * , 'a spiral beginning at a specified point on the'
            PRINT * , 'globe and ending at the point exactly opposite'
            PRINT * , 'that initial point.  One may change the color of'
            PRINT * , 'the spiral, the number of complete turns that it'
            PRINT * , 'makes, and the location of its starting point.'
            PRINT * , 'This tests the forward-transformation routines'
            PRINT * , 'and the proper drawing of lines as they appear'
            PRINT * , 'and disappear at the edges of a map.'
            PRINT * , ' '
            PRINT * , 'Change parameters of the spiral (Y or N)?:'
            READ  '(A1)', COMD
            IF (COMD.EQ.'Y'.OR.COMD.EQ.'y') THEN
              PRINT * , ' '
              PRINT * , 'Current spiral color:      ',ISPC
              PRINT * , 'Current number of turns:   ',NSPT
              PRINT * , 'Current latitude at start: ',SPLT
              PRINT * , 'Current longitude at start:',SPLN
              PRINT * , ' '
              PRINT * , 'Enter new spiral color (between 0 and 7):'
              PRINT * , ' '
              IF (IBOW.EQ.0) THEN
                PRINT * , '  0 => black'
                PRINT * , '  1 => white'
              ELSE
                PRINT * , '  0 => white'
                PRINT * , '  1 => black'
              END IF
              PRINT * , '  2 => red'
              PRINT * , '  3 => green'
              PRINT * , '  4 => blue'
              PRINT * , '  5 => cyan'
              PRINT * , '  6 => magenta'
              PRINT * , '  7 => yellow'
              CALL EMRDIN (ISPC,ISPC)
              ISPC=MAX(0,MIN(7,ISPC))
              PRINT * , ' '
              PRINT * , 'Enter new number of turns (between 1 and 1000):
     +'
              CALL EMRDIN (NSPT,NSPT)
              NSPT=MAX(1,MIN(1000,NSPT))
              PRINT * , ' '
              PRINT * , 'Enter new latitude at start (between -90 and 90
     +):'
              CALL EMRDRN (SPLT,SPLT)
              SPLT=MAX(-90.,MIN(90.,SPLT))
              PRINT * , ' '
              PRINT * ,  'Enter new longitude at start (between -180 and
     + 180):'
              CALL EMRDRN (SPLN,SPLN)
              SPLN=MAX(-180.,MIN(180.,SPLN))
            END IF
            PRINT * , ' '
            PRINT * , 'Drawing spiral.'
            CALL DRAWSP (ISPC,SPLT,SPLN,NSPT)
            GO TO 106
          ELSE
            PRINT * , ' '
            PRINT * , 'Illegal test command.'
            GO TO 106
          END IF
C
        ELSE
C
          PRINT * , ' '
          PRINT * , 'Illegal command.  Try again.'
          GO TO 106
C
        END IF
C
C An error occurred in a graphics call.  Inform user and go back to
C let him enter another command.
C
  901   PRINT * , ' '
        PRINT * , 'An error has occurred in a graphics call.  The error'
        PRINT * , 'message was as follows:'
        PRINT * , ' '
        CALL EPRIN
        PRINT * , ' '
        PRINT * , 'You will probably have to change some parameter to'
        PRINT * , 'avoid this error.'
        CALL ERROF
        GO TO 106
C
      END



      SUBROUTINE EMRDIN (IVAL,IDEF)
C
C This routine reads a line of input to get an integer value IVAL.  If
C the line is entirely blank, the default value IDEF is returned.  If
C the line contains an illegal character, another line is obtained.
C
        CHARACTER*128 LINE
C
C Assume the default value will be returned until we find otherwise.
C
        IVAL=IDEF
C
C Get a line from standard input.
C
        READ '(A128)' , LINE
C
        IF (LINE.NE.' ') THEN
          READ (LINE,*) IVAL
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE EMRDRN (RVAL,RDEF)
C
C This routine reads a line of input to get a real number RVAL.  If
C the line is entirely blank, the default value RDEF is returned.  If
C the line contains an illegal character, another line is obtained.
C
        CHARACTER*128 LINE
C
C Assume the default value will be returned until we find otherwise.
C
        RVAL=RDEF
C
C Get a line from standard input.
C
        READ '(A128)' , LINE
C
        IF (LINE.NE.' ') THEN
          READ (LINE,*) RVAL
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLORA (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        COMMON /RASIZE/ LLPG
        SAVE   /RASIZE/
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          CALL GSFACI (MAPACI(IAI1)+100)
          CALL GFA    (NCRA-1,XCRA,YCRA)
          LLPG=MAX(LLPG,NCRA-1)
        END IF
        RETURN
      END



      SUBROUTINE DRAWLA (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MAPACI(IAI1).EQ.1) THEN
            CALL GPL    (NCRA,XCRA,YCRA)
          END IF
        END IF
        RETURN
      END



      SUBROUTINE COLORB (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        COMMON /RASIZE/ LLPG
        SAVE   /RASIZE/
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          CALL GSFACI (MPISCI(IAI1)+100)
          CALL GFA    (NCRA-1,XCRA,YCRA)
          LLPG=MAX(LLPG,NCRA-1)
        END IF
        RETURN
      END



      SUBROUTINE DRAWLB (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MPIPAN(IAI1,'Water').NE.0) THEN
            CALL GPL    (NCRA,XCRA,YCRA)
          END IF
        END IF
        RETURN
      END



      SUBROUTINE MUPPER (CHRS)
        CHARACTER*(*) CHRS
        CHARACTER*26 LOWR,UPPR
        DATA LOWR / 'abcdefghijklmnopqrstuvwxyz' /
        DATA UPPR / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
        DO 102 I=1,LEN(CHRS)
          DO 101 J=1,26
            IF (CHRS(I:I).EQ.LOWR(J:J)) CHRS(I:I)=UPPR(J:J)
  101     CONTINUE
  102   CONTINUE
        RETURN
      END


      SUBROUTINE ZERODA (DPRA,LNTH)
        DOUBLE PRECISION DPRA(LNTH)
        DO 101 I=1,LNTH
          DPRA(I)=0.D0
  101   CONTINUE
        RETURN
      END


      SUBROUTINE DRSPGD (ISPG,IZN0,PLT0,PLN0,CLT0,CLN0,
     +                        IZN8,PLT8,PLN8,CLT8,CLN8,CHSZ)
C
        DIMENSION IZN0(139)
        DIMENSION PLT0(200,139),PLN0(200,139),CLT0(139),CLN0(139)
        DIMENSION IZN8(139)
        DIMENSION PLT8(200,139),PLN8(200,139),CLT8(139),CLN8(139)
C
C Declare a common block making it possible to set PLOTCHAR's internal
C parameter 'MA' = 4.
C
        COMMON /PCMP04/ PANG,PLAT,PLON
        SAVE   /PCMP04/
C
C Declare a character temporary.
C
        CHARACTER*5 CTMP
C
C Draw NAD1927 zone, if it's selected.
C
        IF (ISPG.EQ.0) THEN
C
          DO 103 ISPI=1,139
            IF (IZN0(ISPI).NE.0) THEN
              CALL SFLUSH
              CALL GSLWSC (6.)
              CALL GSPLCI (10)
              CALL MAPIT (PLT0(1,ISPI),PLN0(1,ISPI),0)
              DO 101 I=2,200
                CALL MAPIT (PLT0(I,ISPI),PLN0(I,ISPI),1)
  101         CONTINUE
              CALL MAPIT (PLT0(1,ISPI),PLN0(1,ISPI),2)
              CALL SFLUSH
              CALL GSLWSC (2.)
              CALL GSPLCI (12)
              CALL MAPIT (PLT0(1,ISPI),PLN0(1,ISPI),0)
              DO 102 I=2,200
                CALL MAPIT (PLT0(I,ISPI),PLN0(I,ISPI),1)
  102         CONTINUE
              CALL MAPIT (PLT0(1,ISPI),PLN0(1,ISPI),2)
            END IF
  103     CONTINUE
C
          CALL SFLUSH
          CALL GSLWSC (1.)
C
          CALL PCSETI ('MA',4)
          CALL PCSETR ('OR',1.E12)
C
          DO 104 ISPI=1,139
            IF (IZN0(ISPI).NE.0.AND.IZN0(ISPI).NE.1901) THEN
              WRITE (CTMP,'(I5)') 10000+IZN0(ISPI)
              PANG=0.
              PLAT=CLT0(ISPI)
              PLON=CLN0(ISPI)
              CALL PLCHHQ (0.,0.,CTMP(2:5),CHSZ,0.,0.)
            END IF
  104     CONTINUE
C
          CALL PCSETI ('MA',0)
          CALL PCSETR ('OR',0.)
C
          CALL SFLUSH
          CALL GSPLCI (1)
C
C Draw NAD1983 zone, if it's selected.
C
        ELSE IF (ISPG.EQ.8) THEN
C
          DO 107 ISPI=1,139
            IF (IZN8(ISPI).NE.0) THEN
              CALL SFLUSH
              CALL GSLWSC (6.)
              CALL GSPLCI (10)
              CALL MAPIT (PLT8(1,ISPI),PLN8(1,ISPI),0)
              DO 105 I=2,200
                CALL MAPIT (PLT8(I,ISPI),PLN8(I,ISPI),1)
  105         CONTINUE
              CALL MAPIT (PLT8(1,ISPI),PLN8(1,ISPI),2)
              CALL SFLUSH
              CALL GSLWSC (2.)
              CALL GSPLCI (12)
              CALL MAPIT (PLT8(1,ISPI),PLN8(1,ISPI),0)
              DO 106 I=2,200
                CALL MAPIT (PLT8(I,ISPI),PLN8(I,ISPI),1)
  106         CONTINUE
              CALL MAPIT (PLT8(1,ISPI),PLN8(1,ISPI),2)
            END IF
  107     CONTINUE
C
          CALL SFLUSH
          CALL GSLWSC (1.)
C
          CALL PCSETI ('MA',4)
          CALL PCSETR ('OR',1.E12)
C
          DO 108 ISPI=1,139
            IF (IZN8(ISPI).NE.0.AND.IZN8(ISPI).NE.1901) THEN
              WRITE (CTMP,'(I5)') 10000+IZN8(ISPI)
              PANG=0.
              PLAT=CLT8(ISPI)
              PLON=CLN8(ISPI)
              CALL PLCHHQ (0.,0.,CTMP(2:5),CHSZ,0.,0.)
            END IF
  108     CONTINUE
C
          CALL PCSETI ('MA',0)
          CALL PCSETR ('OR',0.)
C
          CALL SFLUSH
          CALL GSPLCI (1)
C
        END IF
C
        RETURN
C
      END


      SUBROUTINE INNPDP (IPDP,PTYP,PLAT,PLON,ROTA,LTYP,PLM1,PLM2,PLM3,
     +                   PLM4,IPRJ,IZON,ISPH,PARA,IDSL,ISPG,ODNM,ILVL,
     +                   CHSZ,LABL,ILLB)
C
        CHARACTER*(*) PTYP,LTYP,ODNM,LABL
        DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
        DOUBLE PRECISION PARA(15)
C
        PTYP='UT'
C
        PLAT=0.
        PLON=0.
        ROTA=0.
C
        LTYP='MA'
        PLM1(1)=0.
        PLM1(2)=0.
        PLM2(1)=0.
        PLM2(2)=0.
        PLM3(1)=0.
        PLM3(2)=0.
        PLM4(1)=0.
        PLM4(2)=0.
C
        IPRJ=0
        IZON=0
        ISPH=0
C
        IDSL=1
C
        ISPG=-1
C
        LABL=' '
C
        ILLB=0
C
        CALL ZERODA (PARA,15)
C
        PRINT * , ' '
C
        IF      (IPDP.EQ. 1) THEN
          PRINT * , 'Example 1 shows an original EZMAP transformation'
          PRINT * , 'called the Lambert Conformal Conic.'
          PTYP='LC'
          PLAT=55.
          ROTA=85.
        ELSE IF (IPDP.EQ. 2) THEN
          PRINT * , 'Example 2 shows an original EZMAP transformation'
          PRINT * , 'called the Gnomonic.'
          PTYP='GN'
          PLAT=40.
          PLON=-105.
        ELSE IF (IPDP.EQ. 3) THEN
          PRINT * , 'Example 3 shows an original EZMAP transformation'
          PRINT * , 'called the Mercator.'
          PTYP='ME'
          ILLB=-1
        ELSE IF (IPDP.EQ. 4) THEN
          PRINT * , 'Example 4 shows an original EZMAP transformation'
          PRINT * , 'called the Stereographic.'
          PTYP='ST'
        ELSE IF (IPDP.EQ. 5) THEN
          PRINT * , 'Example 5 shows an original EZMAP transformation'
          PRINT * , 'called the Lambert Equal-Area.'
          PTYP='LE'
        ELSE IF (IPDP.EQ. 6) THEN
          PRINT * , 'Example 6 shows an original EZMAP transformation'
          PRINT * , 'called the Orthographic.'
          PTYP='OR'
          PLAT=40.
          PLON=-105.
        ELSE IF (IPDP.EQ. 7) THEN
          PRINT * , 'Example 7 shows an original EZMAP transformation'
          PRINT * , 'called the Satellite View or Perspective.'
          PTYP='SV'
          PLAT=40.
          PLON=-105.
        ELSE IF (IPDP.EQ. 8) THEN
          PRINT * , 'Example 8 shows an original EZMAP transformation'
          PRINT * , 'called the Azimuthal Equidistant.'
          PTYP='AE'
        ELSE IF (IPDP.EQ. 9) THEN
          PRINT * , 'Example 9 shows an original EZMAP transformation'
          PRINT * , 'called the Robinson, which was developed by the'
          PRINT * , 'National Geographic Society for use in world maps.'
          PTYP='RO'
        ELSE IF (IPDP.EQ.10) THEN
          PRINT * , 'Example 10 shows an original EZMAP transformation'
          PRINT * , 'which is characterized as a "Mollweide type".'
          PTYP='MT'
        ELSE IF (IPDP.EQ.11) THEN
          PRINT * , 'Example 11 shows an original EZMAP transformation'
          PRINT * , 'called the Cylindrical Equidistant.'
          PTYP='CE'
          ILLB=-1
        ELSE IF (IPDP.EQ.12) THEN
          PRINT * , 'Example 12 shows an original EZMAP transformation'
          PRINT * , 'called the Cylindrical Equal-Area.'
          PTYP='EA'
          ILLB=-1
        ELSE IF (IPDP.EQ.13) THEN
          PRINT * , 'Example 13 shows an original EZMAP transformation'
          PRINT * , 'called the Rotated Mercator.'
          PTYP='RM'
          ROTA=45.
          LTYP='CO'
          PLM1(1)=   2.
          PLM2(1)=-135.
          PLM3(1)=  37.
          PLM4(1)=  67.
        ELSE IF (IPDP.EQ.14) THEN
          PRINT * , 'Example 14 shows an original EZMAP transformation'
          PRINT * , 'called the Aitoff.'
          PTYP='AI'
        ELSE IF (IPDP.EQ.15) THEN
          PRINT * , 'Example 15 shows an original EZMAP transformation'
          PRINT * , 'called the Hammer.'
          PTYP='HA'
        ELSE IF (IPDP.EQ.16) THEN
          PRINT * , 'Example 16 shows an original EZMAP transformation'
          PRINT * , 'called the Mollweide.'
          PTYP='MO'
        ELSE IF (IPDP.EQ.17) THEN
          PRINT * , 'Example 17 shows an original EZMAP transformation'
          PRINT * , 'called the Winkel tripel.'
          PTYP='WT'
        ELSE IF (IPDP.EQ.18) THEN
          PRINT * , 'Example 18 shows a USGS transformation called the'
          PRINT * , 'UTM system (UTM="Universal Transverse Mercator"),'
          PRINT * , 'in the Northern Hemisphere.  The area shown is'
          PRINT * , 'that for which distortion is minimal, on either'
          PRINT * , 'side of a particular meridian; normally, the UTM'
          PRINT * , 'system is used for large-scale maps which are more'
          PRINT * , 'nearly square.'
          IPRJ=1
          IZON=13
          IDSL=4
        ELSE IF (IPDP.EQ.19) THEN
          PRINT * , 'Example 19 shows a USGS transformation called the'
          PRINT * , 'UTM system (UTM="Universal Transverse Mercator"),'
          PRINT * , 'in the Southern Hemisphere.  The area shown is'
          PRINT * , 'that for which distortion is minimal, on either'
          PRINT * , 'side of a particular meridian; normally, the UTM'
          PRINT * , 'system is used for large-scale maps which are more'
          PRINT * , 'nearly square.'
          IPRJ=1
          IZON=-35
          IDSL=4
        ELSE IF (IPDP.EQ.20) THEN
          PRINT * , 'Example 20 shows a USGS transformation called the'
          PRINT * , 'State Plane coordinate system.  The zone shown is'
          PRINT * , 'that for New Hampshire.'
          IPRJ=2
          IZON=2800
          ISPH=0
          IDSL=4
          ILLB=-1
        ELSE IF (IPDP.EQ.21) THEN
          PRINT * , 'Example 21 shows a USGS transformation called the'
          PRINT * , 'State Plane coordinate system.  The zone shown is'
          PRINT * , 'that for Nebraska.'
          IPRJ=2
          IZON=2600
          ISPH=8
          IDSL=4
          ILLB=-1
        ELSE IF (IPDP.EQ.22) THEN
          PRINT * , 'Example 22 shows a USGS transformation called the'
          PRINT * , 'State Plane coordinate system.  The zone shown is'
          PRINT * , 'that for the Alaskan panhandle.'
          IPRJ=2
          IZON=5001
          ISPH=8
          IDSL=4
          ILLB=-1
        ELSE IF (IPDP.EQ.23) THEN
          PRINT * , 'Example 23 shows a USGS transformation called the'
          PRINT * , 'Alber''s Equal-Area Conic, in the Northern'
          PRINT * , 'Hemisphere.'
          IPRJ=3
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=         50.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=         60.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=         90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.24) THEN
          PRINT * , 'Example 24 shows a USGS transformation called the'
          PRINT * , 'Alber''s Equal-Area Conic, in the Southern'
          PRINT * , 'Hemisphere.'
          IPRJ=3
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=        -50.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=        -60.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=        -90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.25) THEN
          PRINT * , 'Example 25 shows a USGS transformation called the'
          PRINT * , 'Lambert Conformal Conic, with two standard'
          PRINT * , 'parallels, in the Northern Hemisphere.'
          IPRJ=4
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=         50.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=         60.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=         55.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.26) THEN
          PRINT * , 'Example 26 shows a USGS transformation called the'
          PRINT * , 'Lambert Conformal Conic, with two standard'
          PRINT * , 'parallels, in the Southern Hemisphere.'
          IPRJ=4
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=        -50.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=        -60.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=        -55.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.27) THEN
          PRINT * , 'Example 27 shows a USGS transformation called the'
          PRINT * , 'Mercator.'
          IPRJ=5
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=          0.D0  !  LATITUDE OF TRUE SCALE
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.28) THEN
          PRINT * , 'Example 28 shows a USGS transformation called the'
          PRINT * , 'Polar Stereographic, in the Northern Hemisphere.'
          IPRJ=6
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 5)=       -105.D0  !  LONGITUDE DOWN FROM NORTH, UP FROM SOUTH
          PARA( 6)=        +45.D0  !  LATITUDE OF TRUE SCALE
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.29) THEN
          PRINT * , 'Example 29 shows a USGS transformation called the'
          PRINT * , 'Polar Stereographic, in the Southern Hemisphere.'
          IPRJ=6
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 5)=       -105.D0  !  LONGITUDE DOWN FROM NORTH, UP FROM SOUTH
          PARA( 6)=        -45.D0  !  LATITUDE OF TRUE SCALE
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.30) THEN
          PRINT * , 'Example 30 shows a USGS transformation called the'
          PRINT * , 'Polyconic.'
          IPRJ=7
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=          0.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.31) THEN
          PRINT * , 'Example 31 shows a USGS transformation called the'
          PRINT * , 'Equidistant Conic, with one standard parallel, in'
          PRINT * , 'the Northern Hemisphere.'
          IPRJ=8
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=         40.D0  !  LATITUDE OF STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=         90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA( 9)=          0.D0  !  ZERO
        ELSE IF (IPDP.EQ.32) THEN
          PRINT * , 'Example 32 shows a USGS transformation called the'
          PRINT * , 'Equidistant Conic, with one standard parallel, in'
          PRINT * , 'the Southern Hemisphere.'
          IPRJ=8
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=        -40.D0  !  LATITUDE OF STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=        -90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA( 9)=          0.D0  !  ZERO
        ELSE IF (IPDP.EQ.33) THEN
          PRINT * , 'Example 33 shows a USGS transformation called the'
          PRINT * , 'Equidistant Conic, with two standard parallels, in'
          PRINT * , 'the Northern Hemisphere.'
          IPRJ=8
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=         40.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=         55.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=         90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA( 9)=          1.D0  !  NON-ZERO
        ELSE IF (IPDP.EQ.34) THEN
          PRINT * , 'Example 34 shows a USGS transformation called the'
          PRINT * , 'Equidistant Conic, with two standard parallels, in'
          PRINT * , 'the Southern Hemisphere.'
          IPRJ=8
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=        -40.D0  !  LATITUDE OF 1ST STANDARD PARALLEL
          PARA( 4)=        -55.D0  !  LATITUDE OF 2ND STANDARD PARALLEL
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=        -90.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA( 9)=          1.D0  !  NON-ZERO
        ELSE IF (IPDP.EQ.35) THEN
          PRINT * , 'Example 35 shows a USGS transformation called the'
          PRINT * , 'Transverse Mercator.  The area shown is that for'
          PRINT * , 'which distortion is minimal, on either side of a'
          PRINT * , 'particular meridian; normally, this system is used'
          PRINT * , 'for large-scale maps which are more nearly square.'
          IPRJ=9
          ISPH=-1
          PARA( 1)=   6378206.4D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=   6356583.8D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=          1.D0  !  SCALE FACTOR AT CENTRAL MERIDIAN
          PARA( 5)=         25.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=          0.D0  !  LATITUDE OF ORIGIN
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.36) THEN
          PRINT * , 'Example 36 shows a USGS transformation called the'
          PRINT * , 'Stereographic.'
          IPRJ=10
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.37) THEN
          PRINT * , 'Example 37 shows a USGS transformation called the'
          PRINT * , 'Lambert Azimuthal Equal-Area.'
          IPRJ=11
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.38) THEN
          PRINT * , 'Example 38 shows a USGS transformation: the'
          PRINT * , 'Azimuthal Equidistant.'
          IPRJ=12
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.39) THEN
          PRINT * , 'Example 39 shows a USGS transformation called the'
          PRINT * , 'Gnomonic.'
          IPRJ=13
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.40) THEN
          PRINT * , 'Example 40 shows a USGS transformation called the'
          PRINT * , 'Orthographic.'
          IPRJ=14
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.41) THEN
          PRINT * , 'Example 41 shows a USGS transformation called the'
          PRINT * , 'Perspective (the same as the Satellite-View, but'
          PRINT * , 'only allows for a vertical view).'
          IPRJ=15
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 3)=          3.D0  !  HEIGHT OF PERSPECTIVE POINT ABOVE SURFACE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF CENTER OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.42) THEN
          PRINT * , 'Example 42 shows a USGS transformation called the'
          PRINT * , 'Sinusoidal.'
          IPRJ=16
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.43) THEN
          PRINT * , 'Example 43 shows a USGS transformation called the'
          PRINT * , 'Equirectangular (same as Cylindrical Equidistant).'
          IPRJ=17
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 6)=          0.D0  !  LATITUDE OF TRUE SCALE
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.44) THEN
          PRINT * , 'Example 44 shows a USGS transformation called the'
          PRINT * , 'Miller Cylindrical.'
          IPRJ=18
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.45) THEN
          PRINT * , 'Example 45 shows a USGS transformation called the'
          PRINT * , 'Van der Grinten I.'
          IPRJ=19
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.46) THEN
          PRINT * , 'Example 46 shows a USGS transformation called the'
          PRINT * , 'Oblique Mercator.  The projection''s center line'
          PRINT * , 'is specified by giving two points that project'
          PRINT * , 'into it.  Currently, there is a bug which prevents'
          PRINT * , 'this specification method from working properly.'
          IPRJ=20
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=          1.D0  !  SCALE FACTOR AT CENTER OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA( 9)=       -100.D0  !  LONGITUDE, POINT 1
          PARA(10)=         30.D0  !  LATITUDE, POINT 1
          PARA(11)=        -80.D0  !  LONGITUDE, POINT 2
          PARA(12)=         50.D0  !  LATITUDE, POINT 2
          PARA(13)=          0.D0  !  ZERO
        ELSE IF (IPDP.EQ.47) THEN
          PRINT * , 'Example 47 shows a USGS transformation called the'
          PRINT * , 'Oblique Mercator.  The projection''s center line'
          PRINT * , 'is specified by specifying its azimuth angle.'
          IPRJ=20
          ISPH=-1
          PARA( 1)=          1.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=        .007D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=          1.D0  !  SCALE FACTOR AT CENTER OF PROJECTION
          PARA( 4)=         45.D0  !  AZIMUTH ANGLE OF CENTER LINE (E OF N)
          PARA( 5)=       -105.D0  !  LONGITUDE OF ORIGIN OF PROJECTION
          PARA( 6)=         40.D0  !  LATITUDE OF ORIGIN OF PROJECTION
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          PARA(13)=          1.D0  !  NON-ZERO
        ELSE IF (IPDP.EQ.48) THEN
          PRINT * , 'Example 48 shows a USGS transformation called the'
          PRINT * , 'Robinson, which was developed by the National'
          PRINT * , 'Geographic Society for use in world maps.'
          IPRJ=21
          ISPH=-1
          PARA( 1)=          1.D0  !  RADIUS OF SPHERE OF REFERENCE
          PARA( 5)=       -105.D0  !  LONGITUDE OF CENTRAL MERIDIAN
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.49) THEN
          PRINT * , 'Example 49 shows a USGS transformation called the'
          PRINT * , 'Space Oblique Mercator.  Each of the views that'
          PRINT * , 'one can select shows the area passed over by a'
          PRINT * , 'chosen Landsat satellite during a particular'
          PRINT * , 'orbit.'
          IPRJ=22
          PARA( 1)=          0.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=          0.D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 3)=          1.D0  !  LANDSAT NUMBER (1-5)
          PARA( 4)=         84.D0  !  PATH (1-251 for LN 1-3, 1-233 for LN 4-5)
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
        ELSE IF (IPDP.EQ.50) THEN
          PRINT * , 'Example 50 shows a USGS transformation called the'
          PRINT * , 'Modified Stereographic for Alaska.  It is centered'
          PRINT * , 'at Lat 64N, Long 152W and stretched in such a way'
          PRINT * , 'as to allow all of Alaska to be shown on a single'
          PRINT * , 'map with relatively little distortion.'
          IPRJ=23
          PARA( 1)=          0.D0  !  SEMIMAJOR AXIS OF ELLIPSOID
          PARA( 2)=          0.D0  !  SQUARE OF ELLIPSOID'S ECCENTRICITY
          PARA( 7)=          0.D0  !  FALSE EASTING
          PARA( 8)=          0.D0  !  FALSE NORTHING
          IDSL=4
          ILLB=-1
        ELSE IF (IPDP.EQ.51) THEN
          PRINT * , 'Example 51 shows the State Plane zones for the'
          PRINT * , 'northeastern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1927.'
          PTYP='ST'
          PLAT=41.
          PLON=-76.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHEAST - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.52) THEN
          PRINT * , 'Example 52 shows the State Plane zones for the'
          PRINT * , 'northeastern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1983.'
          PTYP='ST'
          PLAT=41.
          PLON=-76.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHEAST - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.53) THEN
          PRINT * , 'Example 53 shows the State Plane zones for the'
          PRINT * , 'southeastern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1927.'
          PTYP='ST'
          PLAT=31.
          PLON=-84.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHEAST - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.54) THEN
          PRINT * , 'Example 54 shows the State Plane zones for the'
          PRINT * , 'southeastern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1983.'
          PTYP='ST'
          PLAT=31.
          PLON=-84.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHEAST - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.55) THEN
          PRINT * , 'Example 55 shows the State Plane zones for the'
          PRINT * , 'northeast central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1927.'
          PTYP='ST'
          PLAT=42.
          PLON=-92.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHEAST CENTRAL - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.56) THEN
          PRINT * , 'Example 56 shows the State Plane zones for the'
          PRINT * , 'northeast central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1983.'
          PTYP='ST'
          PLAT=42.
          PLON=-92.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHEAST CENTRAL - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.57) THEN
          PRINT * , 'Example 57 shows the State Plane zones for the'
          PRINT * , 'southeast central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1927.'
          PTYP='ST'
          PLAT=33.
          PLON=-92.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHEAST CENTRAL - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.58) THEN
          PRINT * , 'Example 58 shows the State Plane zones for the'
          PRINT * , 'southeast central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1983.'
          PTYP='ST'
          PLAT=33.
          PLON=-92.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHEAST CENTRAL - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.59) THEN
          PRINT * , 'Example 59 shows the State Plane zones for the'
          PRINT * , 'northwest central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1927.'
          PTYP='ST'
          PLAT=42.
          PLON=-104.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHWEST CENTRAL - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.60) THEN
          PRINT * , 'Example 60 shows the State Plane zones for the'
          PRINT * , 'northwest central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1983.'
          PTYP='ST'
          PLAT=42.
          PLON=-104.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHWEST CENTRAL - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.61) THEN
          PRINT * , 'Example 61 shows the State Plane zones for the'
          PRINT * , 'southwest central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1927.'
          PTYP='ST'
          PLAT=33.
          PLON=-104.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHWEST CENTRAL - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.62) THEN
          PRINT * , 'Example 62 shows the State Plane zones for the'
          PRINT * , 'southwest central states of the lower 48, using'
          PRINT * , 'the North American Datum of 1983.'
          PTYP='ST'
          PLAT=33.
          PLON=-104.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHWEST CENTRAL - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.63) THEN
          PRINT * , 'Example 63 shows the State Plane zones for the'
          PRINT * , 'northwestern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1927.'
          PTYP='ST'
          PLAT=42.
          PLON=-116.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHWEST - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.64) THEN
          PRINT * , 'Example 64 shows the State Plane zones for the'
          PRINT * , 'northwestern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1983.'
          PTYP='ST'
          PLAT=42.
          PLON=-116.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - NORTHWEST - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.65) THEN
          PRINT * , 'Example 65 shows the State Plane zones for the'
          PRINT * , 'southwestern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1927.'
          PTYP='ST'
          PLAT=38.
          PLON=-116.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=0
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHWEST - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.66) THEN
          PRINT * , 'Example 66 shows the State Plane zones for the'
          PRINT * , 'southwestern states of the lower 48, using the'
          PRINT * , 'North American Datum of 1983.'
          PTYP='ST'
          PLAT=38.
          PLON=-116.
          LTYP='AN'
          PLM1(1)=8.
          PLM2(1)=8.
          PLM3(1)=8.
          PLM4(1)=8.
          IDSL=4
          ISPG=8
          CHSZ=.24
          LABL='STATE PLANE ZONES - SOUTHWEST - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.67) THEN
          PRINT * , 'Example 67 shows the State Plane zones for Alaska,'
          PRINT * , 'using the North American Datum of 1927.'
          PTYP='ST'
          PLAT=64.
          PLON=-152.
          LTYP='AN'
          PLM1(1)=23.
          PLM2(1)=15.
          PLM3(1)=19.
          PLM4(1)=19.
          IDSL=4
          ISPG=0
          CHSZ=.75
          LABL='STATE PLANE ZONES - ALASKA - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.68) THEN
          PRINT * , 'Example 68 shows the State Plane zones for Alaska,'
          PRINT * , 'using the North American Datum of 1983.'
          PTYP='ST'
          PLAT=64.
          PLON=-152.
          LTYP='AN'
          PLM1(1)=23.
          PLM2(1)=15.
          PLM3(1)=19.
          PLM4(1)=19.
          IDSL=4
          ISPG=8
          CHSZ=.75
          LABL='STATE PLANE ZONES - ALASKA - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.69) THEN
          PRINT * , 'Example 69 shows the State Plane zones for Hawaii,'
          PRINT * , 'using the North American Datum of 1927.'
          PTYP='ST'
          PLAT=21.
          PLON=-157.5
          LTYP='AN'
          PLM1(1)=3.
          PLM2(1)=3.
          PLM3(1)=3.
          PLM4(1)=3.
          IDSL=4
          ISPG=0
          CHSZ=.075
          LABL='STATE PLANE ZONES - HAWAII - NAD1927'
          ILLB=-1
        ELSE IF (IPDP.EQ.70) THEN
          PRINT * , 'Example 70 shows the State Plane zones for Hawaii,'
          PRINT * , 'using the North American Datum of 1927.'
          PTYP='ST'
          PLAT=21.
          PLON=-157.5
          LTYP='AN'
          PLM1(1)=3.
          PLM2(1)=3.
          PLM3(1)=3.
          PLM4(1)=3.
          IDSL=4
          ISPG=8
          CHSZ=.075
          LABL='STATE PLANE ZONES - HAWAII - NAD1983'
          ILLB=-1
        ELSE IF (IPDP.EQ.71) THEN
          PRINT * , 'Example 71 shows all the State Plane zones, using'
          PRINT * , 'the North American Datum of 1927.'
          PTYP='LE'
          PLAT=40.
          PLON=-140.
          LTYP='AN'
          PLM1(1)=70.
          PLM2(1)=70.
          PLM3(1)=70.
          PLM4(1)=70.
          IDSL=4
          ISPG=0
          CHSZ=1.75
          LABL='STATE PLANE ZONES - ALL - NAD1927'
        ELSE IF (IPDP.EQ.72) THEN
          PRINT * , 'Example 72 shows all the State Plane zones, using'
          PRINT * , 'the North American Datum of 1983.'
          PTYP='LE'
          PLAT=45.
          PLON=-112.
          LTYP='AN'
          PLM1(1)=48.
          PLM2(1)=48.
          PLM3(1)=48.
          PLM4(1)=48.
          IDSL=4
          ISPG=8
          CHSZ=1.7
          LABL='STATE PLANE ZONES - ALL - NAD1983'
        END IF
C
        IF (IPDP.GE.51.AND.IPDP.LE.72) THEN
          PRINT * , ' '
          PRINT * , 'The area for which each of the State Plane zones'
          PRINT * , 'shown is meant to be used is shown as a rectangle'
          PRINT * , 'with rounded corners, so that one can more easily'
          PRINT * , 'sort out how the zones overlap.  Each zone number'
          PRINT * , 'is approximately centered within its zone.'
        END IF
C
        IF (IDSL.EQ.1) THEN
          IF (ODNM.NE.'E1'.AND.ODNM.NE.'E2'.AND.
     +        ODNM.NE.'E3'.AND.ODNM.NE.'E4') THEN
            ODNM='CO'
          ELSE
            ILVL=1
          END IF
        ELSE
          IF (ODNM.NE.'E1'.AND.ODNM.NE.'E2'.AND.
     +        ODNM.NE.'E3'.AND.ODNM.NE.'E4') THEN
            ODNM='PS'
          ELSE
            ILVL=4
          END IF
        END IF
C
        IPDP=MOD(IPDP,72)+1
C
        RETURN
C
      END


      SUBROUTINE DRPTLB (PTYP,IPRJ,IZON,ISPH,PARA,IPRF,ODNM,ILVL)
C
        CHARACTER*(*) PTYP,ODNM
        DOUBLE PRECISION PARA(15)
C
        CHARACTER*82 PTLB
C
        PTLB=' '
C
        IF      (PTYP.EQ.'LC') THEN
          PTLB='LAMBERT CONFORMAL CONIC'
        ELSE IF (PTYP.EQ.'GN') THEN
          PTLB='GNOMONIC'
        ELSE IF (PTYP.EQ.'ME') THEN
          PTLB='MERCATOR'
        ELSE IF (PTYP.EQ.'RM') THEN
          PTLB='ROTATED MERCATOR'
        ELSE IF (PTYP.EQ.'ST') THEN
          PTLB='STEREOGRAPHIC'
        ELSE IF (PTYP.EQ.'LE') THEN
          PTLB='LAMBERT EQUAL-AREA'
        ELSE IF (PTYP.EQ.'OR') THEN
          PTLB='ORTHOGRAPHIC'
        ELSE IF (PTYP.EQ.'SV') THEN
          PTLB='SATELLITE-VIEW'
        ELSE IF (PTYP.EQ.'AE') THEN
          PTLB='AZIMUTHAL EQUIDISTANT'
        ELSE IF (PTYP.EQ.'RO') THEN
          PTLB='ROBINSON'
        ELSE IF (PTYP.EQ.'MT') THEN
          PTLB='"MOLLWEIDE-TYPE"'
        ELSE IF (PTYP.EQ.'CE') THEN
          PTLB='CYLINDRICAL EQUIDISTANT'
        ELSE IF (PTYP.EQ.'ER') THEN
          PTLB='EQUIRECTANGULAR'
        ELSE IF (PTYP.EQ.'EA') THEN
          PTLB='CYLINDRICAL EQUAL-AREA'
        ELSE IF (PTYP.EQ.'AI') THEN
          PTLB='AITOFF'
        ELSE IF (PTYP.EQ.'HA') THEN
          PTLB='HAMMER'
        ELSE IF (PTYP.EQ.'MO') THEN
          PTLB='MOLLWEIDE'
        ELSE IF (PTYP.EQ.'WT') THEN
          PTLB='WINKEL TRIPEL'
        ELSE IF (PTYP.EQ.'UT') THEN
          IF (IPRJ.EQ.1) THEN
            IF (IZON.GT.0) THEN
              PTLB='USGS - UTM COORDINATES - Zone 00 - North'
            ELSE
              PTLB='USGS - UTM COORDINATES - Zone 00 - South'
            END IF
            CALL PUTINT (PTLB(31:32),IZON)
          ELSE IF (IPRJ.EQ.2) THEN
            PTLB( 1:38)='USGS - STATE PLANE COORDINATES (XX) - '
            PTLB(39:57)='NAD0000 - Zone 0000'
            IF (IPRF.EQ.4) THEN
              PTLB(33:34)='LC'
            ELSE IF (IPRF.EQ.7) THEN
              PTLB(33:34)='PC'
            ELSE IF (IPRF.EQ.9) THEN
              PTLB(33:34)='TM'
            ELSE IF (IPRF.EQ.20) THEN
              PTLB(33:34)='OM'
            END IF
            IF (ISPH.EQ.0) THEN
              PTLB(42:45)='1927'
            ELSE
              PTLB(42:45)='1983'
            END IF
            CALL PUTINT (PTLB(54:57),IZON)
          ELSE IF (IPRJ.EQ.3) THEN
            IF (PARA(3)+PARA(4).GT.0.D0) THEN
              PTLB='USGS - ALBER''S EQUAL-AREA CONIC - North'
            ELSE
              PTLB='USGS - ALBER''S EQUAL-AREA CONIC - South'
            END IF
          ELSE IF (IPRJ.EQ.4) THEN
            IF (PARA(3)+PARA(4).GT.0.D0) THEN
                PTLB='USGS - LAMBERT CONFORMAL CONIC - North'
            ELSE
                PTLB='USGS - LAMBERT CONFORMAL CONIC - South'
            END IF
          ELSE IF (IPRJ.EQ.5) THEN
            PTLB='USGS - MERCATOR'
          ELSE IF (IPRJ.EQ.6) THEN
            IF (PARA(6).GT.0.D0) THEN
              PTLB='USGS - POLAR STEREOGRAPHIC - North'
            ELSE
              PTLB='USGS - POLAR STEREOGRAPHIC - South'
            END IF
          ELSE IF (IPRJ.EQ.7) THEN
            PTLB='USGS - POLYCONIC'
          ELSE IF (IPRJ.EQ.8) THEN
            IF (PARA(9).EQ.0.D0) THEN
              IF (PARA(3).GT.0.D0) THEN
                PTLB='USGS - EQUIDISTANT CONIC - Type A - North'
              ELSE
                PTLB='USGS - EQUIDISTANT CONIC - Type A - South'
              END IF
            ELSE
              IF (PARA(3)+PARA(4).GT.0.D0) THEN
                PTLB='USGS - EQUIDISTANT CONIC - Type B - North'
              ELSE
                PTLB='USGS - EQUIDISTANT CONIC - Type B - South'
              END IF
            END IF
          ELSE IF (IPRJ.EQ.9) THEN
            PTLB='USGS - TRANSVERSE MERCATOR'
          ELSE IF (IPRJ.EQ.10) THEN
            PTLB='USGS - STEREOGRAPHIC'
          ELSE IF (IPRJ.EQ.11) THEN
            PTLB='USGS - LAMBERT EQUAL-AREA'
          ELSE IF (IPRJ.EQ.12) THEN
            PTLB='USGS - AZIMUTHAL EQUIDISTANT'
          ELSE IF (IPRJ.EQ.13) THEN
            PTLB='USGS - GNOMONIC'
          ELSE IF (IPRJ.EQ.14) THEN
            PTLB='USGS - ORTHOGRAPHIC'
          ELSE IF (IPRJ.EQ.15) THEN
            PTLB='USGS - PERSPECTIVE'
          ELSE IF (IPRJ.EQ.16) THEN
            PTLB='USGS - SINUSOIDAL'
          ELSE IF (IPRJ.EQ.17) THEN
            PTLB='USGS - EQUIRECTANGULAR'
          ELSE IF (IPRJ.EQ.18) THEN
            PTLB='USGS - MILLER CYLINDRICAL'
          ELSE IF (IPRJ.EQ.19) THEN
            PTLB='USGS - VAN DER GRINTEN I'
          ELSE IF (IPRJ.EQ.20) THEN
            IF (PARA(13).EQ.0.D0) THEN
              PTLB='USGS - OBLIQUE MERCATOR - Type A'
            ELSE
              PTLB='USGS - OBLIQUE MERCATOR - Type B'
            END IF
          ELSE IF (IPRJ.EQ.21) THEN
            PTLB='USGS - ROBINSON'
          ELSE IF (IPRJ.EQ.22) THEN
            PTLB='USGS - SPACE OBLIQUE MERCATOR'
          ELSE IF (IPRJ.EQ.23) THEN
            PTLB='USGS - MODIFIED STEREOGRAPHIC FOR ALASKA'
          END IF
        END IF
C
C Add the dataset specifier.
C
        ILST=MPILNB(PTLB)
        PTLB(ILST+ 1:ILST+12)=' - Database '
        PTLB(ILST+13:ILST+14)=ODNM(1:2)
        ILST=ILST+14
        IF (ODNM.EQ.'E1'.OR.ODNM.EQ.'E2'.OR.
     +      ODNM.EQ.'E3'.OR.ODNM.EQ.'E4') THEN
          PTLB(ILST+1:ILST+10)=' at level '
          CALL PUTINT (PTLB(ILST+11:ILST+11),ILVL)
          ILST=ILST+11
        END IF
C
        IF (PTLB.NE.' ') CALL PLCHHQ (CFUX(.5),CFUY(.05),PTLB(1:ILST),
     +                                                     .012,0.,0.)
C
        RETURN
C
      END


      SUBROUTINE PUTINT (CHRS,IVAL)
        CHARACTER*(*) CHRS
        CHARACTER*10 DGTS
        DATA DGTS / '0123456789' /
        IIII=ABS(IVAL)
        DO 101 I=LEN(CHRS),1,-1
          IDIG=MOD(IIII,10)+1
          CHRS(I:I)=DGTS(IDIG:IDIG)
          IIII=IIII/10
  101   CONTINUE
        RETURN
      END


      SUBROUTINE DRAWSP (ICLR,PLAT,PLON,NOTA)
C
C Draw a spiral, in the color specified by ICLR, which starts at the
C point with latitude PLAT and longitude PLON and ends at the point
C opposite it on the globe.  NOTA is the number of times the spiral
C winds around the globe.
C
        NPTS=720*NOTA-1
C
        RDIV=REAL(NPTS+1)
C
        CALL SFLUSH
        CALL GSPLCI (ICLR)
C
        DO 101 IPNT=0,NPTS+1
          RLAT=-90.+180.*REAL(IPNT)/RDIV
          RLON=.5*REAL(MOD(IPNT,720))
          CALL ROTATE (2,-PLAT-90.,RLAT,RLON)
          CALL ROTATE (3, PLON    ,RLAT,RLON)
          IF (IPNT.EQ.0) THEN
            CALL MAPIT (RLAT,RLON,0)
          ELSE IF (IPNT.LE.NPTS) THEN
            CALL MAPIT (RLAT,RLON,1)
          ELSE
            CALL MAPIT (RLAT,RLON,2)
          END IF
  101   CONTINUE
C
        CALL SFLUSH
        CALL GSPLCI (1)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE ROTATE (IAXS,ANGL,RLAT,RLON)
C
C Define multiplicative constants to convert from degrees to radians
C and from radians to degrees.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C This routine rotates the point with latitude RLAT and longitude RLON
C by the angle ANGL about the axis specified by IAXS (1 for the U axis,
C 2 for the V axis, 3 for the W axis).  A right-handed coordinate system
C is assumed.
C
        UCRD=COS(DTOR*RLAT)*COS(DTOR*RLON)
        VCRD=COS(DTOR*RLAT)*SIN(DTOR*RLON)
        WCRD=SIN(DTOR*RLAT)
C
        SINA=SIN(DTOR*ANGL)
        COSA=COS(DTOR*ANGL)
C
        UTMP=UCRD
        VTMP=VCRD
        WTMP=WCRD
C
        IF (IAXS.EQ.1) THEN
          VCRD=VTMP*COSA-WTMP*SINA
          WCRD=WTMP*COSA+VTMP*SINA
        ELSE IF (IAXS.EQ.2) THEN
          UCRD=UTMP*COSA+WTMP*SINA
          WCRD=WTMP*COSA-UTMP*SINA
        ELSE
          UCRD=UTMP*COSA-VTMP*SINA
          VCRD=VTMP*COSA+UTMP*SINA
        END IF
C
        RLAT=RTOD*ASIN(WCRD)
        RLON=RTOD*ATAN2(VCRD,UCRD)
C
        RETURN
C
      END


      SUBROUTINE CONINV (ICLR,NPTS)
C
C Declare one of the USGS code's common blocks to gain access to its
C error flag.
C
        COMMON /ERRMZ0/ IERR
        SAVE   /ERRMZ0/
C
C Declare the arrays required for contouring values returned by MAPTRI.
C
        PARAMETER (MPTS=501,LRWK=10000,LIWK=10000)
C
        DIMENSION RLAT(MPTS,MPTS),RLON(MPTS,MPTS),RWRK(LRWK),IWRK(LIWK)
C
        CALL CPSETR ('VPL - VIEWPORT LEFT  ',0.)
        CALL CPSETR ('VPR - VIEWPORT RIGHT ',1.)
        CALL CPSETR ('VPB - VIEWPORT BOTTOM',0.)
        CALL CPSETR ('VPT - VIEWPORT TOP   ',1.)
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETI ('CIS - CONTOUR INTERVAL SELECTOR',5)
        CALL CPSETI ('LIS - LABEL INTERVAL SELECTOR',100)
        CALL CPSETR ('SPV - SPECIAL VALUE',1.E12)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',5000)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
        CALL SFLUSH
        CALL GSPLCI (ICLR)
        CALL GSTXCI (ICLR)
        CALL GSMKSC (50./REAL(NPTS-1))
C
        DO 102 I=1,NPTS
          XVAL=CFUX(REAL(I-1)/REAL(NPTS-1))
          DO 101 J=1,NPTS
            YVAL=CFUY(REAL(J-1)/REAL(NPTS-1))
            IERR=0
            CALL MAPTRI (XVAL,YVAL,RLAT(I,J),RLON(I,J))
            IF (RLAT(I,J).EQ.1.E12) THEN
              IF (IERR.EQ.0) THEN
                CALL GSPMCI (4)
                CALL GPM (1,XVAL,YVAL)
              ELSE IF (IERR.EQ.1) THEN
                CALL GSPMCI (5)
                CALL GPM (1,XVAL,YVAL)
              ELSE
                CALL GSPMCI (6)
                CALL GPM (1,XVAL,YVAL)
              END IF
            ELSE
              IF (ABS(RLAT(I,J)).GT. 90..OR.ABS(RLON(I,J)).GT.180.) THEN
                RLAT(I,J)=1.E12
                RLON(I,J)=1.E12
                CALL GSPMCI (7)
                CALL GPM (1,XVAL,YVAL)
              END IF
            END IF
  101     CONTINUE
  102   CONTINUE
C
        CALL CPRECT (RLAT,MPTS,NPTS,NPTS,RWRK,LRWK,IWRK,LIWK)
        CALL CPCLDR (RLAT,RWRK,IWRK)
        CALL CPRECT (RLON,MPTS,NPTS,NPTS,RWRK,LRWK,IWRK,LIWK)
        CALL CPCLDR (RLON,RWRK,IWRK)
C
        CALL SFLUSH
        CALL GSPLCI (1)
        CALL GSTXCI (1)
        CALL GSMKSC (1.)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE MDRGDI (DINM)
C
C This is a user-replaceable routine that returns the name of the
C directory in which the RANGS/GSHHS data files have been placed.
C
        CHARACTER*(*) DINM
C
C Declare a common block in which the directory name will be passed.
C
        COMMON /EDRGDI/ USNM
          CHARACTER*60 USNM
        SAVE   /EDRGDI/
C
C Return the name of the directory.
C
        DINM=USNM
C
C Done.
C
        RETURN
C
      END


      FUNCTION IGETAI (IAMA,XCOP,YCOP,IAAI,IAGI,MNAI,NOAI,IGRP)
C
C This function retrieves from an area map the area identifier, relative
C to group IGRP, of the area containing the point (XCOP,YCOP).
C
        DIMENSION IAAI(MNAI),IAGI(MNAI)
C
        CALL ARGTAI (IAMA,XCOP,YCOP,IAAI,IAGI,MNAI,NOAI,1)
C
        DO 101 I=1,MNAI
          IF (IAGI(I).EQ.IGRP) THEN
            IGETAI=IAAI(I)
            RETURN
          END IF
  101   CONTINUE
C
        IGETAI=-1
        RETURN
C
      END
