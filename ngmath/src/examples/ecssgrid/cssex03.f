
      PROGRAM CSSEX03
C
C  Do a comparison of CSSGRID with NATGRIDS for interpolation on
C  a sphere, using an analytic function.  This program produces
C  four plots:
C
C     1.)  Contour plot of the analytic function.
C     2.)  Contour plot of the interpolated function using NATGRIDS
C          with no periodic points.
C     3.)  Contour plot of the interpolated function using NATGRIDS
C          with periodic points added to the longitudes.
C     4.)  Contour plot of the interpolated function using CSSGRID.
C
C  External for Conpack calls.
C
      EXTERNAL DRAWCL
C
C  NUMORG - Number of points in the original dataset.
C  NMAX   - Number of points in the dataset with periodic points added.
C  NUMXOUT - Number of latitudes in the interpolated dataset.
C  NUMYOUT - Number of longitudes in the interpolated dataset.
C
      PARAMETER(NUMORG=750, NMAX = 1111, NUMXOUT = 71, NUMYOUT = 145,
     +          IDIM=2*NUMXOUT*NUMYOUT)
C
C  Conversion factors and constants.
C
      PARAMETER(D2R=0.017453293, R2D=57.29578)
      PARAMETER( PI=3.1415927, PIH= 1.5707963)
C
C  Specify GKS output data.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=2)
C
C  Sizes of work arrays for plotting.
C
      PARAMETER (IRWRK=5200, IIWRK=11250, IAMASZ=20000)
      DIMENSION RWRK(IRWRK), IWRK(IIWRK), IAMA(IAMASZ)
C
C  Arrays for original data.
C
      REAL X(NMAX), Y(NMAX), FVAL(NMAX)
C
C  Arrays for output data.
C
      REAL RLAT(NMAX), RLON(NMAX)
      REAL XI(NUMXOUT), YI(NUMYOUT), ZDAT(NUMYOUT,NUMXOUT)
      REAL XR(NUMXOUT), YR(NUMYOUT)
      REAL FF(NUMXOUT,NUMYOUT), FFN(NUMXOUT,NUMYOUT)
      REAL FFS(NUMXOUT,NUMYOUT)
C
C  Work arrays for CSSGRID interpolation.
C
      PARAMETER (NIWKC=27*NMAX, NRWKC=13*NMAX)
      INTEGER IWKC(NIWKC)    
      DOUBLE PRECISION RWKC(NRWKC)    
C
C  Array to store the values of the analytic function.
C
      REAL FF_ANA(NUMXOUT,NUMYOUT)
C
C  Generate a random dataset on the unit sphere.
C
      N = NUMORG
      CALL GENSS(N,RLAT,RLON)
C
C  Generate functional values at the input nodes.
C
      CALL CRGENI(5,10,-200.,500.)
      DO 10 I=1,N
        FVAL(I) = CRGENPNT(RLAT(I),RLON(I))
   10 CONTINUE
C
C  Convert to radians for NATGRIDS calls.
C
      DO 2 K = 1,N
        X(K) = D2R*RLAT(K)
        Y(K) = D2R*RLON(K)
    2 CONTINUE
C
C  Create the output grid.
C
      DO 20 I=1,NUMXOUT
        XI(I) = -87.5+(I-1)*2.5
        XR(I) = D2R*XI(I)
   20 CONTINUE
      DO 30 J=1,NUMYOUT
        YI(J) = -180.+(J-1)*2.5
        YR(J) = D2R*YI(J)
   30 CONTINUE
C
C  Store analytic function values.
C
      DO 600 I=1,NUMXOUT
        DO 610 J=1,NUMYOUT
          FF_ANA(I,J) = CRGENPNT(XI(I),YI(J))
  610   CONTINUE
  600 CONTINUE
C
C  Use Natgrid to interpolate, without using any periodic points
C  in longitude.  There are significant problems along the zero 
C  longitude.
C
      CALL NATGRIDS(N,X,Y,FVAL,NUMXOUT,NUMYOUT,XR,YR,FFN,IER)
C
C  Add in periodic points in longitude for the Natgrid call.
C
      IJ = NUMORG
      DO 299 I=1,N
        IF(Y(I).GT.PIH .AND. Y(I).LE.PI) THEN
          IJ = IJ+1
          X(IJ) = X(I)
          Y(IJ) = Y(I)-2.0*PI
          FVAL(IJ) = FVAL(I)
        ELSE IF(Y(I).GE.-PI .AND. Y(I).LT.-PIH) THEN
          IJ = IJ+1
          X(IJ) = X(I)
          Y(IJ) = Y(I)+2.0*PI
          FVAL(IJ) = FVAL(I)
        ENDIF
  299 CONTINUE
C
C  Use Natgrid to interpolate, using the additional periodic points.
C  
      N = NMAX
      CALL NATGRIDS(N,X,Y,FVAL,NUMXOUT,NUMYOUT,XR,YR,FF,IER)
C
C  Use Cssgrid to interpolate on the sphere.
C
      N = NUMORG
      CALL CSSGRID(N,RLAT,RLON,FVAL,NUMXOUT,NUMYOUT,XI,YI,FFS,
     +             IWKC,RWKC,IER)
C
C  Plot the results.
C
C
C  Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Color table.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      CALL GSCR(IWKID,3,0.,0.,1.)
C
C  Draw a contour map of the analytic function.
C
      CALL GSPLCI(3)
      CALL GSTXCI(3)
      CALL GSTXFP(-13,2)
C
C  Use a satellite projection and view the globe looking at the
C  north pole.
C
      CALL MAPSTR ('SA - SATELLITE HEIGHT',4.)
      CALL MAPROJ ('SV - SATELLITE-VIEW',90.,0.,0.)
      CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
      CALL MAPPOS (0.06, 0.94, 0.02, 0.90)
C
      CALL MAPSTI ('PE - PERIMETER',0)
      CALL MAPSTI ('GR - GRID',0)
      CALL MAPSTC ('OU - OUTLINE DATASET','PS')
      CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
      CALL MAPDRW
C
      CALL CPSETI ('SET - DO SET-CALL FLAG',0)
      CALL CPSETR ('DPS - LINE LABEL SIZE',0.02)
      CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',1.)
      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',16)
      CALL CPSETC ('HLT - TURN OFF HIGH/LOW LABELS',' ')
      CALL CPSETC ('ILT - TURN OFF INFORMATIONAL LABEL',' ')
C
      CALL CPSETR ('XC1 - X COORDINATE AT I = 1',-180.)
      CALL CPSETR ('XCM - X COORDINATE AT I = M', 180.)
      CALL CPSETR ('YC1 - Y COORDINATE AT J = 1', -90.)
      CALL CPSETR ('YCN - Y COORDINATE AT J = N',  90.)
      CALL CPSETI ('MAP - MAPPING FLAG',1)
      CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C  Reverse the indices, since Conpack wants longitude as
C  the first dimension.
C
      DO 450 I=1,NUMXOUT
        DO 460 J=1,NUMYOUT
          ZDAT(J,I) = FF_ANA(I,J)
  460     CONTINUE
  450 CONTINUE
C
      CALL CPRECT (ZDAT,NUMYOUT,NUMYOUT,NUMXOUT,RWRK,IRWRK,IWRK,IIWRK)       
      CALL ARINAM (IAMA,IAMASZ)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C  Plot the picture title.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.50,0.95,':F26:Analytic function',
     +            0.025,0.,0.)
C
      CALL FRAME
C
C  Plot the results from Natgrid without periodic points.
C
      CALL MAPPOS (0.06, 0.94, 0.02, 0.90)
      CALL MAPDRW
C
C  Reverse the indices, since Conpack wants longitude as
C  the first dimension.
C
      DO 451 I=1,NUMXOUT
        DO 461 J=1,NUMYOUT
          ZDAT(J,I) = FFN(I,J)
  461     CONTINUE
  451 CONTINUE
C
      CALL CPRECT (ZDAT,NUMYOUT,NUMYOUT,NUMXOUT,RWRK,IRWRK,IWRK,IIWRK)       
      CALL ARINAM (IAMA,IAMASZ)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C  Plot the picture title.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.50,0.95,':F26:Natgrid without periodic points', 
     +            0.025,0.,0.)
C
      CALL FRAME
C
C  Plot the results from Natgrid with periodic points.
C
      CALL MAPPOS (0.06, 0.94, 0.02, 0.90)
      CALL MAPDRW
C
C  Reverse the indices, since Conpack wants longitude as
C  the first dimension.
C
      DO 452 I=1,NUMXOUT
        DO 462 J=1,NUMYOUT
          ZDAT(J,I) = FF(I,J)
  462     CONTINUE
  452 CONTINUE
C
      CALL CPRECT (ZDAT,NUMYOUT,NUMYOUT,NUMXOUT,RWRK,IRWRK,IWRK,IIWRK)       
      CALL ARINAM (IAMA,IAMASZ)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C  Plot the picture title.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.50,0.95,':F26:Natgrid with periodic points', 
     +            0.025,0.,0.)
C
      CALL FRAME
C
C  Plot the results from Cssgrid.
C
      CALL MAPPOS (0.06, 0.94, 0.02, 0.90)
      CALL MAPDRW
C
C  Reverse the indices, since Conpack wants longitude as
C  the first dimension.
C
      DO 453 I=1,NUMXOUT
        DO 463 J=1,NUMYOUT
          ZDAT(J,I) = FFS(I,J)
  463     CONTINUE
  453 CONTINUE
C
      CALL CPRECT (ZDAT,NUMYOUT,NUMYOUT,NUMXOUT,RWRK,IRWRK,IWRK,IIWRK)       
      CALL ARINAM (IAMA,IAMASZ)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C  Plot the picture title.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.50,0.95,':F26:Cssgrid',0.025,0.,0.)
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C 
      STOP
C
      END
      SUBROUTINE GENSS(N,RLAT,RLON)
C
C  Generate random positions on a sphere in latitudes and longitudes
C  (latitude values between -90. and 90. and longitude values
C  between -180. and 180.
C
C  First select a random longitude, and then select a random value
C  on the Z axis -R to R (-1 to 1 in the case of the unit sphere).
C  From the random Z value, calculate the latitude.
C
      DIMENSION RLAT(N),RLON(N)
      PARAMETER (R=1., R2D=57.29578)
C
      DO 10 I=1,N
        RLON(I) = -180.+360.*CRRAND()
        RZ = R*(2.*CRRAND()-1.)
        RLAT(I) = R2D*ASIN(RZ)
   10 CONTINUE
C
      RETURN
      END
      REAL FUNCTION CRRAND()
C
C  Portable random number generator.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      SAVE    NEXTN
      DATA JSEED,IFRST/9,0/
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      CRRAND = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
      IDR=1
      DO 101 I=1,NAI
        IF (IAI(I) .LT. 0) IDR = 0
  101 CONTINUE
      IF (IDR .NE. 0) CALL CURVED (XCS,YCS,NCS)
C
      RETURN
      END
      SUBROUTINE CRGENI (MLOW,MHGH,DLOW,DHGH)
C
C  The routines CRGENI and CSGENPT are used to generate test 
C  data on the globe.  The data generated will have approximately 
C  "MLOW" lows and "MHGH" highs within each of the twenty areas 
C  defined by an inscribed icosahedron, a minimum value of 
C  approximately "DLOW" and a maximum value of approximately 
C  "DHGH".  CRGENI is called to initialize the process and the 
C  function CRGENPNT returns a computed functional value at the 
C  point (RLAT,RLON).
C
C  The function used is a sum of exponentials.
C  
C  This code was originally written by David Kennison at NCAR.
C
      COMMON /CSSBD1/R0,R1,R2
      COMMON /CSSCMN/ CMUL(160),NCNT,NLOW,NHGH,RLOW,RHGH,RMIN,RMAX,
     +                XCOC(160),YCOC(160),ZCOC(160)
C
C Define the vertices and faces of an icosahedron.
C
      DIMENSION XCVI(12),YCVI(12),ZCVI(12),IVOF(3,20)
      DIMENSION R0(220),R1(220),R2(220)
      DATA XCVI / .9510565162952 , -.9510565162951 ,  .4253254041760 ,
     +           -.4253254041760 ,  .4253254041760 , -.4253254041760 ,
     +            .4253254041760 , -.4253254041760 ,  .4253254041760 ,
     +           -.4253254041760 ,  .4253254041760 , -.4253254041760 /
      DATA YCVI / .0000000000000 ,  .0000000000000 ,  .8506508083520 ,
     +           -.8506508083520 ,  .2628655560596 , -.2628655560596 ,
     +           -.6881909602356 ,  .6881909602356 , -.6881909602356 ,
     +            .6881909602356 ,  .2628655560595 , -.2628655560596 /
      DATA ZCVI / .0000000000000 ,  .0000000000000 ,  .0000000000000 ,
     +            .0000000000000 ,  .8090169943749 , -.8090169943749 ,
     +            .5000000000000 , -.5000000000000 , -.5000000000000 ,
     +            .5000000000000 , -.8090169943749 ,  .8090169943749 /
      DATA IVOF / 1, 3, 5  , 1, 5, 7  , 1, 7, 9  ,
     +            1, 9,11  , 1, 3,11  , 3, 8,11  ,
     +            3, 8,10  , 3, 5,10  , 5,10,12  ,
     +            5, 7,12  , 7, 4,12  , 4, 7, 9  ,
     +            4, 6, 9  , 6, 9,11  , 6, 8,11  ,
     +            6, 2, 8  , 2, 4, 6  , 2, 4,12  ,
     +            2,10,12  , 2, 8,10  /
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
      NLOW = MAX(0,MIN(4,MLOW))
      NHGH = MAX(0,MIN(4,MHGH))
      IF (NLOW+NHGH.GE.12 .AND. NLOW.GE.6) NLOW = NLOW-1
      IF (NLOW+NHGH.GE.12 .AND. NHGH.GE.6) NHGH = NHGH-1
      NCNT = 20*(NLOW+NHGH)
C
C  Compute the center positions for the exponentials which are 
C  summed to create the function.
C
      DO 100 K=1,NCNT
        INDX = MOD(K-1,20)+1
        ILOH = (K+19)/20
        WGT1 = R0(K)
        WGT2 = R1(K)
        WGT3 = R2(K)
        WSUM = WGT1+WGT2+WGT3
        WGT1 = WGT1/WSUM
        WGT2 = WGT2/WSUM
        WGT3 = WGT3/WSUM
        XTMP = WGT1*XCVI(IVOF(1,INDX))+
     +         WGT2*XCVI(IVOF(2,INDX))+
     +         WGT3*XCVI(IVOF(3,INDX))
        YTMP = WGT1*YCVI(IVOF(1,INDX))+
     +         WGT2*YCVI(IVOF(2,INDX))+
     +         WGT3*YCVI(IVOF(3,INDX))
        ZTMP = WGT1*ZCVI(IVOF(1,INDX))+
     +         WGT2*ZCVI(IVOF(2,INDX))+
     +         WGT3*ZCVI(IVOF(3,INDX))
        TEMP = SQRT(XTMP*XTMP+YTMP*YTMP+ZTMP*ZTMP)
        XCOC(K) = XTMP/TEMP
        YCOC(K) = YTMP/TEMP
        ZCOC(K) = ZTMP/TEMP
        IF (.NOT.(ILOH .LE. NLOW)) GO TO 110
        CMUL(K) = -1.
        GO TO 120
  110   CONTINUE
        CMUL(K) = +1.
  120   CONTINUE
  100 CONTINUE
C
      RLOW = DLOW
      RHGH = DHGH
      RMIN = +1.E36
      RMAX = -1.E36
      DO 130 I=-180,175,5
        RLON = DTOR*REAL(I)
        CRLN = COS(RLON)
        SRLN = SIN(RLON)
        DO 140 J=-85,85,5
          RLAT = DTOR*REAL(J)
          CRLT = COS(RLAT)
          SRLT = SIN(RLAT)
          RVAL = 0.5*(RLOW+RHGH)
          DO 150 K=1,NCNT
            DIST = SQRT((CRLT*CRLN-XCOC(K))**2
     +                 +(CRLT*SRLN-YCOC(K))**2
     +                 +(SRLT     -ZCOC(K))**2)
            ANGL = 2.*RTOD*ASIN(DIST/2.)
            DIST = ANGL/18.
            RVAL = RVAL+.5*(RHGH-RLOW)*CMUL(K)*EXP(-DIST*DIST)
     +                              *(2.-SIN(6.283*DIST)/2.)
  150     CONTINUE
          RMIN = MIN(RMIN,RVAL)
          RMAX = MAX(RMAX,RVAL)
  140   CONTINUE
  130 CONTINUE
        RETURN
      END
      FUNCTION CRGENPNT (RLAT,RLON)
      COMMON /CSSCMN/ CMUL(160),NCNT,NLOW,NHGH,RLOW,RHGH,RMIN,RMAX,
     +                XCOC(160),YCOC(160),ZCOC(160)
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
      DATA PI   /  3.1415926/
C
      TLAT = DTOR*RLAT
      TLON = DTOR*RLON
      CRLT=COS(TLAT)
      CRLN=COS(TLON)
      SRLT=SIN(TLAT)
      SRLN=SIN(TLON)
      RVAL=.5*(RLOW+RHGH)
      DO 100 K=1,NCNT
        DIST = SQRT((CRLT*CRLN-XCOC(K))**2
     +             +(CRLT*SRLN-YCOC(K))**2
     +             +(SRLT     -ZCOC(K))**2)
        ANGL = 2.*RTOD*ASIN(DIST/2.)
        DIST = ANGL/18.
        RVAL = RVAL+.5*(RHGH-RLOW)*CMUL(K)*EXP(-DIST*DIST)
     +                          *(2.-SIN(6.283*DIST)/2.)
  100 CONTINUE
      CRGENPNT = RLOW+(RHGH-RLOW)*(RVAL-RMIN)/(RMAX-RMIN)
C
      RETURN
      END
      BLOCKDATA CSSCCBD
C
      COMMON /CSSBD1/R0,R1,R2
      DIMENSION R0(220),R1(220),R2(220)
C
      DATA R0/
     + .968,.067,.478,.910,.352,.933,.654,.021,.512,.202,.940,.204,
     + .379,.793,.288,.267,.357,.128,.703,.737,.803,.915,.511,.762,
     + .456,.471,.300,.613,.073,.498,.220,.041,.565,.698,.951,.917,
     + .630,.605,.938,.143,.807,.878,.347,.186,.671,.635,.453,.028,
     + .763,.157,.765,.566,.072,.276,.328,.528,.747,.627,.141,.821,
     + .126,.360,.862,.690,.058,.813,.607,.689,.419,.545,.831,.226,
     + .422,.178,.412,.093,.813,.866,.121,.576,.023,.886,.142,.095,
     + .162,.470,.623,.910,.097,.764,.730,.223,.124,.593,.913,.183,
     + .406,.520,.871,.825,.065,.703,.051,.488,.881,.463,.581,.694,
     + .329,.702,.270,.352,.587,.412,.446,.750,.882,.069,.659,.979,
     + .833,.390,.202,.957,.982,.115,.140,.389,.635,.011,.213,.701,
     + .714,.264,.188,.594,.727,.769,.288,.056,.471,.558,.408,.058,
     + .970,.854,.808,.851,.923,.467,.830,.756,.857,.032,.713,.839,
     + .147,.852,.228,.783,.863,.441,.483,.577,.705,.671,.171,.432,
     + .441,.459,.489,.911,.017,.897,.969,.987,.751,.777,.838,.674,
     + .244,.669,.430,.101,.701,.143,.940,.848,.995,.168,.631,.858,
     + .608,.114,.435,.313,.785,.606,.746,.226,.065,.234,.137,.082,
     + .131,.106,.069,.882,.883,.907,.556,.127,.576,.986,.228,.276,
     + .128,.168,.124,.123/
C
      DATA R1/
     + .023,.003,.880,.088,.237,.017,.170,.368,.123,.239,.250,.006,
     + .146,.806,.134,.722,.791,.361,.998,.920,.529,.122,.043,.864,
     + .877,.025,.808,.746,.442,.065,.400,.464,.068,.280,.552,.305,
     + .297,.722,.673,.420,.961,.923,.426,.107,.729,.560,.829,.520,
     + .921,.827,.440,.451,.950,.483,.315,.827,.508,.123,.573,.949,
     + .188,.973,.414,.256,.253,.966,.561,.550,.689,.234,.970,.650,
     + .157,.396,.757,.885,.956,.587,.405,.877,.414,.845,.328,.363,
     + .328,.643,.190,.836,.766,.763,.786,.954,.737,.199,.211,.990,
     + .165,.772,.540,.854,.006,.510,.504,.163,.906,.261,.048,.862,
     + .848,.454,.740,.262,.299,.068,.625,.627,.711,.815,.464,.477,
     + .579,.249,.431,.315,.449,.642,.305,.614,.414,.845,.468,.420,
     + .355,.972,.582,.261,.234,.631,.123,.082,.084,.863,.343,.383,
     + .930,.968,.011,.641,.784,.474,.118,.362,.723,.549,.678,.172,
     + .191,.983,.786,.605,.828,.254,.024,.183,.226,.607,.444,.460,
     + .237,.567,.541,.322,.430,.885,.705,.361,.853,.715,.002,.637,
     + .190,.119,.999,.913,.668,.677,.085,.859,.660,.871,.464,.488,
     + .124,.489,.671,.350,.095,.115,.810,.333,.683,.351,.654,.113,
     + .236,.359,.473,.089,.075,.475,.726,.264,.594,.725,.177,.263,
     + .402,.262,.122,.062/
C  
      DATA R2/
     + .337,.417,.503,.020,.769,.158,.133,.005,.517,.606,.094,.591,
     + .081,.820,.855,.675,.545,.033,.938,.947,.294,.060,.009,.427,
     + .646,.559,.684,.721,.781,.291,.892,.118,.708,.395,.138,.476,
     + .552,.270,.481,.069,.877,.575,.660,.957,.395,.516,.633,.939,
     + .548,.570,.886,.843,.630,.895,.270,.276,.455,.953,.998,.236,
     + .244,.889,.354,.952,.284,.492,.428,.837,.762,.909,.906,.639,
     + .484,.566,.596,.879,.082,.229,.818,.631,.799,.704,.473,.430,
     + .600,.743,.706,.055,.696,.704,.291,.940,.593,.645,.892,.877,
     + .137,.321,.714,.899,.230,.620,.538,.714,.186,.134,.593,.268,
     + .364,.411,.899,.163,.116,.372,.593,.716,.115,.298,.770,.812,
     + .002,.061,.752,.595,.706,.645,.472,.843,.965,.186,.742,.195,
     + .806,.280,.910,.992,.414,.503,.260,.778,.915,.159,.941,.030,
     + .531,.533,.746,.647,.832,.516,.458,.834,.577,.211,.429,.283,
     + .855,.901,.126,.821,.087,.868,.016,.893,.148,.926,.885,.562,
     + .429,.145,.340,.343,.304,.281,.374,.835,.814,.120,.482,.646,
     + .636,.940,.479,.213,.151,.908,.497,.006,.809,.623,.827,.895,
     + .490,.843,.788,.638,.769,.673,.200,.198,.817,.540,.541,.121,
     + .821,.915,.956,.635,.035,.438,.280,.671,.377,.760,.884,.528,
     + .668,.381,.534,.477/
C
      END  
