
      PROGRAM WMEX14
C
C  An example of using the tools in the weather map library to
C  create a color weather map.  
C
C  By default, the plot has a white background, to produce a plot
C  with a black background, set IBTYPE=1 below.
C
      PARAMETER (IBTYPE=0)
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=26,  IWKID=1)
C
      INTEGER WMGTLN
C
C  The MERGCM common block is used elsewhere in this code, but
C  can be used for scratch space here in the main driver.
C
      COMMON /MERGCM/ XTMP(9999),YTMP(9999),NDUM
C
C  Set up all of the coordinate data for the fronts, temperature regions,
C  cities, etc.
C
      PARAMETER (ICDIM=6000)
      DIMENSION USX(ICDIM),USY(ICDIM)
C
C  Weather front arrays.
C
C   NUMF    - the number of fronts.
C   NUMFP   - the maximum number of control points for any front.
C   LIMITF  - the defined number of control points for each region.
C   FRNSUX  - X user coordinates for fronts.
C   FRNSUY  - Y user coordinates for fronts.
C
      PARAMETER(NUMF=4,NUMFP=6)
      DIMENSION LIMITF(NUMFP),
     +          FRNSUX(NUMFP,NUMF),FRNSUY(NUMFP,NUMF),
     +          FRNSWX(NUMFP),FRNSWY(NUMFP)
C
C  Temperature and weather region arrays.
C
C   NUMR    - the number of regions
C   NUMRP   - the maximum number of control points for any region.
C   LIMITS  - the defined number of control points for each region.
C   IRTYPE  - the type of region.
C   RGNSUX  - X user coordinates for regions.
C   RGNSUY  - Y user coordinates for regions.
C
      PARAMETER(NUMR=14,NUMRP=35)
      CHARACTER*13 IRTYPE(NUMRP)
      DIMENSION LIMITS(NUMRP),RGNSUX(NUMRP,NUMR),RGNSUY(NUMRP,NUMR),
     +          RGNSWX(NUMRP),RGNSWY(NUMRP)
C
C  Regional weather labels.
C
C   NUMWL   - the number of labels.
C   CNDSUX  - X user coordinates for labels.
C   CNDSUY  - Y user coordinates for labels.
C   ICNDSL  - Labels.
C
      PARAMETER(NUMWL=5)
      DIMENSION CNDSUX(NUMWL),CNDSUY(NUMWL)
      CHARACTER*6 ICNDSL(NUMWL)
C
C  LOW/HIGH arrays.
C
C   NUML    - the number of lows and highs to plot.
C   FRNSUX  - X user coordinates for symbols.
C   FRNSUY  - Y user coordinates for symbols.
C   LOWHI   - Array of Low/High flags (0=low; 1=hi).
C
      PARAMETER(NUML=5)
      DIMENSION RLOHUX(NUML),RLOHUY(NUML),LOWHI(NUML)
C
C  City names, locations, daily hi/low labels.
C
C   NUMC    - the number of cities.
C   ICITYS  - City names.
C   IDLYTS  - Daily hi/low labels for cities.
C   CITYUX  - X user coordinates for city locations.
C   CITYUY  - Y user coordinates for city locations.
C   TEMPUX  - X user coordinates for daily hi/low locations.
C   TEMPUY  - Y user coordinates for daily hi/low locations.
C
      PARAMETER(NUMC=14)
      DIMENSION CITYUX(NUMC),CITYUY(NUMC),TEMPUX(NUMC),TEMPUY(NUMC)
      CHARACTER*13 ICITYS(NUMC)
      CHARACTER* 7 IDLYTS(NUMC)
C
C  Front data.
C
C  Front from Calif. to N. Dakota.
      DATA LIMITF(1)/4/
      DATA (FRNSUX(II,1),II=1,NUMFP)/ 
     +    41.0,   42.5,   44.5,   49.0,   00.0,   00.0 /
      DATA (FRNSUY(II,1),II=1,NUMFP)/ 
     +  -121.5, -111.0, -104.0, -098.0,   00.0,   00.0 /
C
C  Front from N. Dakota to Virginia.
      DATA LIMITF(2)/6/
      DATA (FRNSUX(II,2),II=1,NUMFP)/ 
     +    49.0,   47.0,   45.0,   42.0,   38.8,   38.5 /
      DATA (FRNSUY(II,2),II=1,NUMFP)/ 
     +  -098.0, -096.0, -095.0, -093.0, -088.3, -080.5 /
C
C  Front from Virginia out into the Atlantic.
      DATA LIMITF(3)/3/
      DATA (FRNSUX(II,3),II=1,NUMFP)/ 
     +    38.5,   39.0,   37.5,   00.0,   00.0,   00.0 /
      DATA (FRNSUY(II,3),II=1,NUMFP)/ 
     +  -080.5, -077.0, -071.0,   00.0,   00.0,   00.0 /
C
C  Front from Virginia into Canada.
      DATA LIMITF(4)/3/
      DATA (FRNSUX(II,4),II=1,NUMFP)/ 
     +    38.5,   43.0,   49.0,   00.0,   00.0,   00.0 /
      DATA (FRNSUY(II,4),II=1,NUMFP)/ 
     +  -080.5, -077.5, -077.0,   00.0,   00.0,   00.0 /
C
C  Region data.
C
C  60s in Calif. (must be plotted before the 70s region in Calif.)
      DATA LIMITS(1),IRTYPE(1)/14,'INDEX6'/
      DATA (RGNSUX(II,1),II=1,NUMRP)/ 
     +    32.0,   34.5,   35.5,   37.0,   40.0,   42.8,   46.0, 
     +    49.0,   50.0,   49.0,   45.0,   40.0,   35.0,   32.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,1),II=1,NUMRP)/ 
     +  -120.0, -120.0, -120.1, -121.0, -122.8, -123.4, -122.5, 
     +  -122.0, -124.0, -127.0, -126.0, -125.0, -123.0, -120.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  90s in the south and west.
      DATA LIMITS(2),IRTYPE(2)/32,'INDEX3'/
      DATA (RGNSUX(II,2),II=1,NUMRP)/ 
     +    32.7,   34.5,   38.0,   42.0,   45.0,   47.5,   46.0,   
     +    43.0,   39.5,   38.3,   40.0,   37.5,   35.5,   32.7,   
     +    36.0,   41.0,   42.0,   44.5,   47.0,   48.0,   46.0,   
     +    42.0,   39.0,   36.0,   37.3,   33.0,   37.5,   29.0,   
     +    24.0,   20.0,   29.0,   32.7,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,2),II=1,NUMRP)/ 
     +  -116.0, -117.0, -120.5, -122.7, -122.0, -118.0, -115.3, 
     +  -113.0, -107.5, -109.0, -115.0, -114.0, -111.5, -109.0, 
     +  -106.0, -105.0, -104.0, -107.0, -109.7, -104.0, -098.0, 
     +  -095.5, -096.0, -093.0, -088.5, -083.0, -076.0, -075.0, 
     +  -080.0, -097.0, -115.0, -116.0,   00.0,   00.0,   00.0 /
C
C  70s in Calif. and NW.
      DATA LIMITS(3),IRTYPE(3)/25,'INDEX5'/
      DATA (RGNSUX(II,3),II=1,NUMRP)/ 
     +    35.0,   36.0,   38.0,   40.0,   43.0,   46.5,   48.5, 
     +    47.0,   44.0,   44.5,   46.0,   47.0,   49.0,   53.0, 
     +    48.0,   46.0,   42.8,   40.0,   37.0,   35.5,   35.0, 
     +    33.0,   28.0,   30.0,   32.7,   35.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,3),II=1,NUMRP)/ 
     +  -118.7, -120.0, -121.5, -122.7, -123.1, -121.8, -118.0, 
     +  -114.5, -112.0, -110.5, -112.0, -113.2, -114.0, -122.0, 
     +  -123.5, -123.0, -123.5, -123.0, -121.5, -120.5, -120.9, 
     +  -122.0, -120.0, -116.0, -116.5, -118.7,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  70s in Gt. Lakes area
      DATA LIMITS(4),IRTYPE(4)/7,'INDEX5'/
      DATA (RGNSUX(II,4),II=1,NUMRP)/ 
     +    49.0,   45.5,   46.0,   50.0,   53.0,   51.5,   49.0, 
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,4),II=1,NUMRP)/ 
     +  -096.0, -090.5, -084.0, -082.0, -089.0, -095.0, -096.0, 
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  70s in New England.
      DATA LIMITS(5),IRTYPE(5)/7,'INDEX5'/
      DATA (RGNSUX(II,5),II=1,NUMRP)/ 
     +    45.0,   43.2,   41.5,   43.0,   47.0,   49.0,   45.0, 
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,5),II=1,NUMRP)/ 
     +  -073.3, -072.5, -070.5, -066.0, -065.0, -071.0, -073.3, 
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  100s in Calif./Arizona (Must be plotted after 90s in southwest)
      DATA LIMITS(6),IRTYPE(6)/8,'INDEX2'/
      DATA (RGNSUX(II,6),II=1,NUMRP)/ 
     +    32.7,   35.0,   36.0,   35.3,   33.0,   31.3,   30.0, 
     +    32.7,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,6),II=1,NUMRP)/ 
     +  -115.0, -116.0, -115.3, -114.0, -111.0, -111.0, -113.0, 
     +  -115.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C 100s in Okla./Texas (must be plotted after 90s in Okla./Texas)
      DATA LIMITS(7),IRTYPE(7)/5,'INDEX2'/
      DATA (RGNSUX(II,7),II=1,NUMRP)/ 
     +    35.0,   33.0,   31.5,   33.0,   35.0,   00.0,   00.0,   
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,7),II=1,NUMRP)/ 
     +  -097.5, -096.5, -097.5, -098.5, -097.5,   00.0,  000.0, 
     +  -080.0, -097.0, -115.0, -116.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  T-storms in the southwest.
      DATA LIMITS(8),IRTYPE(8)/14,'THUNDERSTORMS'/
      DATA (RGNSUX(II,8),II=1,NUMRP)/
     +    33.5,   35.5,   37.2,   38.5,   39.5,   39.5,   38.0,
     +    36.0,   34.5,   33.0,   30.0,   30.0,   31.0,   33.5,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,8),II=1,NUMRP)/
     +  -110.2, -111.5, -113.0, -111.5, -108.5, -106.3, -105.5,
     +  -106.0, -107.5, -108.0, -108.3, -109.6, -110.2, -110.2,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  T-storms in N. Minn.
      DATA LIMITS(9),IRTYPE(9)/8,'THUNDERSTORMS'/
      DATA (RGNSUX(II,9),II=1,NUMRP)/
     +    49.0,   50.5,   49.0,   47.0,   45.0,   45.0,   47.0,
     +    49.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,9),II=1,NUMRP)/
     +  -097.0, -094.0, -090.0, -089.3, -091.5, -093.5, -096.0,
     +  -097.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  T-storms in California.
      DATA LIMITS(10),IRTYPE(10)/5,'THUNDERSTORMS'/
      DATA (RGNSUX(II,10),II=1,NUMRP)/
     +    39.0,   38.0,   35.3,   37.0,   39.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,10),II=1,NUMRP)/
     +  -120.0, -118.0, -116.5, -119.0, -120.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  Showers in Montana.
      DATA LIMITS(11),IRTYPE(11)/8,'SHOWERS'/
      DATA (RGNSUX(II,11),II=1,NUMRP)/
     +    50.0,   49.0,   48.0,   47.0,   46.7,   48.0,   50.0,
     +    50.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,11),II=1,NUMRP)/
     +  -105.0, -105.0, -106.0, -108.0, -110.7, -110.0, -106.5,
     +  -105.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  T-storms in southeast
      DATA LIMITS(12),IRTYPE(12)/12,'THUNDERSTORMS'/
      DATA (RGNSUX(II,12),II=1,NUMRP)/
     +    36.7,   35.0,   32.0,   31.0,   31.6,   32.2,   32.8,
     +    32.2,   32.2,   34.0,   36.0,   36.7,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,12),II=1,NUMRP)/
     +  -076.0, -078.0, -081.5, -084.0, -088.0, -089.5, -088.0,
     +  -085.0, -083.0, -080.4, -078.0, -076.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  T-storms from Missouri to New York.
      DATA LIMITS(13),IRTYPE(13)/20,'THUNDERSTORMS'/
      DATA (RGNSUX(II,13),II=1,NUMRP)/
     +    35.0,   36.5,   36.5,   37.7,   38.5,   41.0,   45.0,
     +    47.0,   49.0,   48.5,   47.0,   44.0,   42.8,   41.5,
     +    40.0,   38.0,   36.0,   34.5,   34.5,   35.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,13),II=1,NUMRP)/
     +  -094.0, -092.0, -089.0, -084.0, -079.0, -077.0, -076.0,
     +  -076.0, -075.7, -074.5, -073.3, -071.5, -070.3, -070.5,
     +  -074.0, -077.5, -081.0, -087.0, -092.0, -094.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  Showers in extreme northeast
      DATA LIMITS(14),IRTYPE(14)/8,'SHOWERS'/
      DATA (RGNSUX(II,14),II=1,NUMRP)/
     +    49.5,   49.0,   47.0,   44.0,   44.0,   47.0,   48.5,
     +    49.5,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
      DATA (RGNSUY(II,14),II=1,NUMRP)/
     +  -073.8, -071.0, -069.5, -069.0, -071.0, -073.0, -074.0,
     +  -073.8,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0,
     +    00.0,   00.0,   00.0,   00.0,   00.0,   00.0,   00.0 /
C
C  LOWs and HIs.
C
      DATA (RLOHUX(II),II=1,NUML) /  49.0,  38.5,  47.5,  44.0,  26.5 /
      DATA (RLOHUY(II),II=1,NUML) /-099.0,-080.5,-127.0,-089.0,-088.0 /       
      DATA ( LOWHI(II),II=1,NUML) / 0, 0, 1, 1, 1 /
C
C  Labels of regional conditions.
C
      DATA (CNDSUX(II),II=1,NUMWL) /  45.5,  41.5,  31.5,  33.0,  45.5 /
      DATA (CNDSUY(II),II=1,NUMWL) /-113.0,-099.0,-095.5,-113.2,-082.0 /       
      DATA (ICNDSL(II),II=1,NUMWL) / 
     +  'BREEZY','HOT','STEAMY','HUMID','NICE'/
C
C  Data on city locations and daily temperature labels.
C
      DATA (ICITYS(II),II=1,NUMC)/
     +           'NCAR',       'Seattle', 'San Francisco',                
     +    'Los Angeles',      'Billings',       'El Paso',
     +        'Houston',   'Kansas City',   'Minneapolis',
     +        'Chicago',       'Detroit',       'Atlanta',
     +          'Miami',      'New York'                    /
      DATA (CITYUX(II),II=1,NUMC)/ 
     +    40.0,   47.6,   37.8,   34.1,   45.8,   31.8,   29.8, 
     +    39.1,   45.0,   41.9,   42.3,   33.8,   25.8,   40.8 /
      DATA (CITYUY(II),II=1,NUMC)/ 
     +  -105.0, -122.3, -122.4, -118.3, -108.5, -106.5, -095.3, 
     +  -094.6, -093.8, -087.6, -083.1, -084.4, -080.2, -074.0 /
      DATA (TEMPUX(II),II=1,NUMC)/ 
     +    38.8,   46.9,   38.6,   35.0,   46.0,   32.2,   28.8, 
     +    37.7,   46.0,   40.7,   43.7,   32.4,   27.2,   40.5 /
      DATA (TEMPUY(II),II=1,NUMC)/ 
     +  -103.2, -119.6, -120.3, -116.0, -105.6, -104.2, -097.4, 
     +  -092.4, -091.2, -085.2, -082.8, -084.6, -081.3, -071.5 /
      DATA (IDLYTS(II),II=1,NUMC)/
     + '92/58', '80/58', '68/54', '80/64', '91/58', '96/70', '98/76', 
     + '88/70', '84/67', '84/66', '84/63', '90/72', '92/80', '83/70' /        
C
C  Set up color indices to use for temperature regions.
C
      DATA IC60S,IC70S,IC80S,IC90S,IC100S/6,5,4,3,2/
C
      DATA IWBACK,IBBACK/0,1/
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
C
C  Set line caps to "butt" and fill the entire page for background (for 
C  color PostScript output only).
C
      CALL NGSETI('WO',IWKID)
      CALL NGSETI('FU',1)
      CALL NGSETI('CA',0)
C
      CALL GACWK (IWKID)
C
      IF (IBTYPE .EQ. IWBACK) THEN
C
C  White background.
        CALL GSCR(IWKID, 0, 1.00, 1.00, 1.00)
        CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
      ELSE
C
C  Black background.
        CALL GSCR(IWKID, 0, 0.00, 0.00, 0.00)
        CALL GSCR(IWKID, 1, 1.00, 1.00, 1.00)
      ENDIF
C
C  Color for 100 degree temperature regions.
      CALL GSCR(IWKID, 2, 1.00, 0.25, 0.00)
C
C  Color for 90 degree temperature regions.
      CALL GSCR(IWKID, 3, 1.00, 0.50, 0.00)
C
C  Color for 80 degree temperature regions.
      CALL GSCR(IWKID, 4, 1.00, 0.75, 0.00)
C
C  Color for 70 degree temperature regions.
      CALL GSCR(IWKID, 5, 1.00, 1.00, 0.00)
C
C  Color for 60 degree temperature regions.
      CALL GSCR(IWKID, 6, 0.25, 1.00, 0.50)
C
C  Continental shadow and background for low and hi symbols.
      CALL GSCR(IWKID, 7, 0.60, 0.60, 0.60)
C
C  Color for cold front symbols.
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL GSCR(IWKID, 8, 0.00, 0.00, 0.00)
      ELSE
        CALL GSCR(IWKID, 8, 0.20, 0.20, 1.00)
      ENDIF
C
C  Color for warm front symbols.
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL GSCR(IWKID, 9, 0.00, 0.00, 0.00)
      ELSE
        CALL GSCR(IWKID, 9, 1.00, 0.00, 0.00)
      ENDIF
C
C  Color for temperature labels.
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL GSCR(IWKID, 10, 1.00, 1.00, 1.00)
      ELSE
        CALL GSCR(IWKID, 10, 0.00, 1.00, 1.00)
      ENDIF
C
C  Regional weather patterns.
      CALL GSCR(IWKID, 11, 0.00, 0.00, 0.00)
C
C  Color for Highs
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL GSCR(IWKID, 12, 0.00, 0.00, 0.00)
      ELSE
        CALL GSCR(IWKID, 12, 0.20, 0.20, 1.00)
      ENDIF
C
C  Dot color for marking cities.
      CALL GSCR(IWKID, 13, 0.00, 0.00, 1.00)
C
C  Get world coordinates for the U.S. continental boundary and store
C  the U.S. state map in flash buffer 1.  This takes some execution
C  time.
C
      CALL GOPWK(9,8,3)
      CALL WMTINT(1,ICDIM,USX,USY,NO)
C
      CALL GSFAIS(1)
      CALL GSFACI(7)
C
C  Get world coordinate extents, and plot a shaded offset of the
C  continental U.S. boundary.
C     
      CALL TSTEXT(XEXT,YEXT)
      DO 70 I=1,NO
        XTMP(I) = USX(I)-.01*XEXT
        YTMP(I) = USY(I)-.012*YEXT
   70 CONTINUE
      CALL GFA(NO,XTMP,YTMP)
      CALL GSFACI(4)
      CALL GFA(NO,USX,USY)
C
C-------------------------------
C  Plot temperature regions.   |
C-------------------------------
C
      CALL WMGETI('COL',ICOLD)
      DO 50 I=1,NUMR
        INDXMX = LIMITS(I)
        DO 60 J=1,INDXMX
          CALL MAPTRN(RGNSUX(J,I),RGNSUY(J,I),RGNSWX(J),RGNSWY(J))
   60   CONTINUE
        IF (I .GE. 8) THEN
          CALL WMSETI('COL',11)
        ENDIF
        CALL WMDRRG(LIMITS(I),RGNSWX,RGNSWY,IRTYPE(I),NO,USX,USY)
   50 CONTINUE
      CALL WMSETI('COL',ICOLD)
C
C------------------
C   U.S. Map      |
C------------------
C
      CALL GFLAS3(1)
C
C----------------------------
C  Plot the weather fronts. |
C----------------------------
C
      DO 10 I=1,LIMITF(1)
        CALL MAPTRN(FRNSUX(I,1),FRNSUY(I,1),FRNSWX(I),FRNSWY(I))
   10 CONTINUE
      CALL WMSETC('FRO','COLD')
      CALL WMSETR('END',.040)
      CALL WMSETI('WFC',9)
      CALL WMSETI('CFC',8)
      CALL WMDRFT(LIMITF(1),FRNSWX,FRNSWY)
      CALL WMDFLT
C
C   Convert to world coordinates.
      DO 20 I=1,LIMITF(2)
        CALL MAPTRN(FRNSUX(I,2),FRNSUY(I,2),FRNSWX(I),FRNSWY(I))
   20 CONTINUE
C
C   Define the type and direction of each symbol.
      CALL WMSETI('PAI', 1)
      CALL WMSETI('STY',-2)
      CALL WMSETI('PAI', 2)
      CALL WMSETI('STY',-2)
      CALL WMSETI('PAI', 3)
      CALL WMSETI('STY', 1)
      CALL WMSETI('PAI', 4)
      CALL WMSETI('STY',-2)
      CALL WMSETI('PAI', 5)
      CALL WMSETI('STY', 1)
C
C   Define spacings.
      CALL WMSETR('BEG',.03)
      CALL WMSETR('END',.035)
      CALL WMSETR('BET',.04)
      CALL WMSETI('WFC',9)
      CALL WMSETI('CFC',8)
C
C   Draw front.
      CALL WMDRFT(LIMITF(2),FRNSWX,FRNSWY)
C
C   Reset parameters to default values.
      CALL WMDFLT
C
      DO 30 I=1,LIMITF(3)
        CALL MAPTRN(FRNSUX(I,3),FRNSUY(I,3),FRNSWX(I),FRNSWY(I))
   30 CONTINUE
      CALL WMSETC('FRO','STA')
      CALL WMSETR('BEG',.040)
      CALL WMSETI('REV',1)
      CALL WMSETI('WFC',9)
      CALL WMSETI('CFC',8)
      CALL WMDRFT(LIMITF(3),FRNSWX,FRNSWY)
      CALL WMDFLT
C
      DO 40 I=1,LIMITF(4)
        CALL MAPTRN(FRNSUX(I,4),FRNSUY(I,4),FRNSWX(I),FRNSWY(I))
   40 CONTINUE
      CALL WMSETC('FRO','COLD')
      CALL WMSETR('BEG',.040)
      CALL WMSETR('BET',.030)
      CALL WMSETI('WFC',9)
      CALL WMSETI('CFC',8)
      CALL WMDRFT(LIMITF(4),FRNSWX,FRNSWY)
      CALL WMDFLT
C
C----------------
C  LOs and HIs  |
C----------------
C
      CALL GSCLIP(0)
      IF (IBTYPE .EQ. IBBACK) THEN
        CALL WMSETI('LOS - shadow for low symbols',0)
        CALL WMSETI('LOB - character background for low symbols',1)
        CALL WMSETI('LOF - character color for low symbols',9)
        CALL WMSETI('HIS - shadow for high symbols',7)
        CALL WMSETI('HIB - character background for high symbols',1)
        CALL WMSETI('HIF - character color for high symbols',12)
        CALL WMSETI('HIC - character color circumscribed circle',0)
      ENDIF
      DO 80 I=1,NUML
        CALL MAPTRN(RLOHUX(I),RLOHUY(I),XO,YO)
        IF (LOWHI(I) .EQ. 0) THEN
          CALL WMLABS(XO,YO,'LOW')
        ELSE
          CALL WMLABS(XO,YO,'HI')
        ENDIF
   80 CONTINUE
C
C-------------------------------
C  Regional condition labels.  |
C-------------------------------
C  
      IF (IBTYPE .EQ. IBBACK) THEN
        CALL WMSETI('RC1',0)
        CALL WMSETI('RC2',1)
        CALL WMSETI('RC3',0)
        CALL WMSETI('RC4',0)
      ENDIF
      DO 90 I=1,NUMWL
        CALL MAPTRN(CNDSUX(I),CNDSUY(I),XO,YO)
        LL = WMGTLN(ICNDSL(I),LEN(ICNDSL(I)),0)
        CALL WMLABW(XO,YO,ICNDSL(I)(1:LL))
   90 CONTINUE
C  
C-----------------------------
C  Cities and temperatures.  |
C-----------------------------
C
C   NUMC    - the number of cities.
C   ICITYS  - City names.
C   IDLYTS  - Daily hi/low labels for cities.
C   CITYUX  - X user coordinates for city locations.
C   CITYUY  - Y user coordinates for city locations.
C   TEMPUX  - X user coordinates for daily hi/low locations.
C   TEMPUY  - Y user coordinates for daily hi/low locations.

      CALL WMSETI('DTC',13)
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL WMSETI('DBC- dot background color',0)
        CALL WMSETI('RFC- foreground color for labels',1)
        CALL WMSETI('CBC- background color for city labels',0)
      ELSE
        CALL WMSETI('DBC- dot background color',1)
        CALL WMSETI('RFC- foreground color for labels',0)
        CALL WMSETI('CBC- background color for city labels',1)
      ENDIF
      DO 110 I=1,NUMC
        CALL MAPTRN(CITYUX(I),CITYUY(I),XO,YO)
        CALL WMLABS(XO,YO,'D')
        CALL MAPTRN(TEMPUX(I),TEMPUY(I),XO,YO)
        LL = WMGTLN(ICITYS(I),LEN(ICITYS(I)),0)
        MM = WMGTLN(IDLYTS(I),LEN(IDLYTS(I)),0)
        CALL WMLABC(XO,YO,ICITYS(I)(1:LL),IDLYTS(I)(1:MM))
  110 CONTINUE
      CALL WMDFLT
C
C---------------------------------
C  Regional temperature labels.  |
C---------------------------------
C
      CALL WMSETI('RFC - foreground color for labels',10)
      CALL WMSETI('ASC - arrow shadow color',-1)
      CALL GSLWSC(2.)
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL WMSETI('AWC - arrow color',1)
        CALL WMSETI('AOC - arrow outline color',-1)
        CALL WMSETI('ROS - character outline color',1)
        CALL WMSETI('RLS - shadow color',1)
      ELSE
        CALL WMSETI('AWC - arrow color',10)
        CALL WMSETI('AOC - arrow outline color',0)
        CALL WMSETI('ROS - character outline color',-1)
        CALL WMSETI('RLS - shadow color',-1)
      ENDIF
      CALL GSLWSC(1.)
C
C   S. Ariz.
      CALL MAPTRN(32.,-112.,XO,YO)
      CALL WMLABT(XO,YO,'100s',2)
C   S. Calif.
      CALL WMGETR('ARD',AANGO)
      CALL WMGETR('ARL',ARLNO)
      CALL WMSETR('ARD',65.)
      CALL WMSETR('ARL',1.2)
      CALL MAPTRN(34.9,-120.,XO,YO)
      CALL WMLABS(XO,YO,'ARROW')
      CALL WMSETR('ARD',AANGO)
      CALL WMSETR('ARL',ARLNO)
      CALL MAPTRN(32.8,-116.9,XO,YO)
      CALL WMLABT(XO,YO,'70s',1)
C   Oregon Pacific coast.
      CALL MAPTRN(43.,-123.9,XO,YO)
      CALL WMLABT(XO,YO,'60s',11)
C   Idaho
      CALL MAPTRN(48.25,-114.75,XO,YO)
      CALL WMLABT(XO,YO,'70s',8)
C   Gt. Lakes
      CALL MAPTRN(47.7,-87.,XO,YO)
      CALL WMLABT(XO,YO,'70s',8)
C   N. Carolina
      CALL MAPTRN(35.0,-78.,XO,YO)
      CALL WMLABT(XO,YO,'90s',6)
C   Maine
      CALL MAPTRN(46.5,-68.5,XO,YO)
      CALL WMLABT(XO,YO,'70s',9)
C   Texas
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL WMSETI('RLS - shadow color',1)
        CALL WMSETI('ROS - character outline color',-1)
      ELSE
        CALL WMSETI('RLS - shadow color',0)
        CALL WMSETI('ROS - character outline color',0)
      ENDIF
C
      CALL MAPTRN(31.25,-100.50,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'90s',0)
C   S. of Okla.
      CALL MAPTRN(34.3,-97.5,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'100s',11)
C   N. Ariz./New Mexico
      CALL MAPTRN(36.0,-109.0,XO,YO)
      CALL WMSETI('RBS',IC80S)
      CALL WMLABT(XO,YO,'80s',0)
C   Oregon
      CALL MAPTRN(43.7,-120.5,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'90s',0)
C   Utah
      CALL MAPTRN(40.0,-110.6,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'90s',0)
C   N. Montana.
      CALL MAPTRN(48.0,-111.,XO,YO)
      CALL WMSETI('RBS',IC80S)
      CALL WMLABT(XO,YO,'80s',0)
C   S. Dakota
      CALL MAPTRN(44.5,-100.0,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'90s',0)
C   Iowa/Ill.
      CALL MAPTRN(41.5,-89.6,XO,YO)
      CALL WMSETI('RBS',IC80S)
      CALL WMLABT(XO,YO,'80s',0)
C   Miss.
      CALL MAPTRN(33.4,-89.6,XO,YO)
      CALL WMSETI('RBS',IC90S)
      CALL WMLABT(XO,YO,'90s',0)
C   Tenn.
      CALL MAPTRN(35.7,-83.,XO,YO)
      CALL WMSETI('RBS',IC80S)
      CALL WMLABT(XO,YO,'80s',0)
C   New York
      CALL MAPTRN(42.7,-75.0,XO,YO)
      CALL WMSETI('RBS',IC80S)
      CALL WMLABT(XO,YO,'80s',0)
C
C-----------------
C   Main title.  |
C-----------------
C
      CALL MAPTRN(53.,-98.0,XO,YO)
      CALL WMSETI('RFC',1)
      CALL WMSETI('RBS',-1)
      CALL WMSETI('ROS',-1)
      CALL WMSETI('RLS',-1)
      CALL WMLABT(XO,YO,'July 18, 1994',0)
C--------------
C   Legends.  |
C--------------
C
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
      IF (IBTYPE .EQ. IWBACK) THEN
        CALL WMSETI('COL',1)
      ELSE
        CALL WMSETI('COL',5)
      ENDIF
      CALL WMSETI('WFC',9)
      CALL WMSETI('CFC',8)
      CALL WMLGND(.05,0.16,1,6,1)
      CALL WMLGND(.45,0.15,3,0,0)
      CALL WMLGND(.90,0.15,2,0,0)
      CALL GSELNT(NTRO)
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (9)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
C
      END
      SUBROUTINE WMTINT(IBNUM,N,USX,USY,NO)
C
C  Does some initialization.  Stores a map of the U.S. state outlines
C  in Flash buffer number IBNUM, and returns world coordinate values
C  for the boundary of the U.S. in (USX(I),USY(I),I=1,NO).  USX and
C  USY are dimensioned for N and NO is returned as the actual number
C  of coordinates in the dataset.  N should be at least 6000.
C
C  This routine also initializes the EZMAP parameters.
C
C  Define space for the area map that is used to obtain the
C  coordinates for the boundary of the continental U.S.  Set up some 
C  arrays to be used by areas in obtaining the U.S. continental outline.
C  The merged polygons from Areas are stored in common MERGCM.
C  RTPTAR is a subroutine used by Areas to process the areas.
C
      DIMENSION USX(N),USY(N)
C
      COMMON /MEODCM/IEODF
      PARAMETER (LAMA=100000)
      DIMENSION IAMA(LAMA)
      DIMENSION XCRA(10000),YCRA(10000),IAAI(10),IAGI(10)
      COMMON /MERGCM/ XCMP(9999),YCMP(9999),NCMP
      SAVE   /MERGCM/
      EXTERNAL RTPTAR
C
C  Set up the parameters for drawing the U.S. state outlines.
C
C   Position the plot.
C     CALL MAPPOS(0.05, 0.95, 0.05, 0.95)
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
      CALL GFLAS1(IBNUM)
      IEODF = 1
      CALL MAPLOT
C
C Initialize the area map, and put selected outlines in the area map.
C (A version of MAPEOD is supplied which causes anything not part of 
C the outer boundary of the U.S. to be omitted).
C
      CALL ARINAM (IAMA,LAMA)
      CALL MAPBLA (IAMA)
      NCMP=0
C
C Scan the area map using RTPTAR to fill all the polygons representing 
C the U.S.  Merge the polygons into a single polygon in MERGCM.
C
      CALL ARSCAM (IAMA,XCRA,YCRA,10000,IAAI,IAGI,10,RTPTAR)
C
C Convert to world coordinates.
C
        DO 101 I=1,NCMP
          USX(I)=CFUX(XCMP(I))
          USY(I)=CFUY(YCMP(I))
  101   CONTINUE
      IEODF = 0
      CALL MAPLOT
      CALL GFLAS2
      NO = NCMP
C
      RETURN
      END
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
      COMMON /MEODCM/IEODF
C
C  This version of MAPEOD omits all parts of the 'US' outline dataset
C  which are strictly internal, saving only those which are part of
C  the external boundary.  (The "border" has area identifier "223";
C  we omit anything that is not on the "border".)
C
      IF (IEODF .EQ. 1) THEN
        IF (IDLS.NE.223 .AND. IDRS.NE.223) NPTS=0
      ELSE
        RETURN
      ENDiF
C
      END
      SUBROUTINE RTPTAR (XCRA,YCRA,NCRA,IAAI,IAGI,NOFG)
C
C  Calls MERGPO to merge all such polygons into one polygon.
C
      DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C  Find the area identifier of the area relative to group 1.
C
      IAG1=-1
C
      DO 101 I=1,NOFG
        IF (IAGI(I).EQ.1) IAG1=IAAI(I)
  101 CONTINUE
C
C  If the index of the area relative to group 1 is positive and not 223,
C  fill it.
C
      IF (IAG1.GT.0 .AND. IAG1.NE.223) THEN
        CALL MERGPO (XCRA,YCRA,NCRA)
      END IF
C
      RETURN
      END
      SUBROUTINE MERGPO (XCRA,YCRA,NCRA)
C
      DIMENSION XCRA(*),YCRA(*)
C
C  This routine merges polygons into a single polygon.
C
C  Merge polygons are formed in the common block MERGCM:
C
      COMMON /MERGCM/ XCMP(9999),YCMP(9999),NCMP
      SAVE   /MERGCM/
C
C  Copy the coordinates of the latest polygon into the merge polygon
C  coordinate arrays and, if the polygon is not the first of the group,
C  repeat the first point of the first polygon.  (Actually, the code
C  below does something a little more complicated: if necessary, it
C  interpolates points to ensure that the connecting lines between
C  polygons consist of horizontal and/or vertical steps; this tends
C  to prevent problems caused by deficiencies in the fill algorithms
C  on some devices.)
C
C  The following statement is a quick-and-dirty patch to leave out all
C  the off-shore islands, since they seem to cause some problems.
C
      IF (NCRA.LT.100) RETURN
C
      NTMP=NCMP
C
      IF (NTMP+NCRA+4.LE.9999) THEN
        IF (NCMP.NE.0) THEN
          IF (XCMP(NTMP).NE.XCRA(1).AND.YCMP(NTMP).NE.YCRA(1)) THEN
            IF (YCMP(NTMP).LT.YCRA(1)) THEN
              NTMP=NTMP+1
              XCMP(NTMP)=XCRA(1)
              YCMP(NTMP)=YCMP(NTMP-1)
            ELSE
              NTMP=NTMP+1
              XCMP(NTMP)=XCMP(NTMP-1)
              YCMP(NTMP)=YCRA(1)
            END IF
          END IF
          NTMP=NTMP+1
          XCMP(NTMP)=XCRA(1)
          YCMP(NTMP)=YCRA(1)
        END IF
        DO 102 ICRA=1,NCRA
          XCMP(NTMP+ICRA)=XCRA(ICRA)
          YCMP(NTMP+ICRA)=YCRA(ICRA)
  102   CONTINUE
        NTMP=NTMP+NCRA
        IF (NCMP.NE.0) THEN
          IF (XCMP(NTMP).NE.XCMP(1).AND.YCMP(NTMP).NE.YCMP(1)) THEN
            IF (YCMP(NTMP).LT.YCMP(1)) THEN
              NTMP=NTMP+1
              XCMP(NTMP)=XCMP(1)
              YCMP(NTMP)=YCMP(NTMP-1)
            ELSE
              NTMP=NTMP+1
              XCMP(NTMP)=XCMP(NTMP-1)
              YCMP(NTMP)=YCMP(1)
            END IF
          END IF
          NTMP=NTMP+1
          XCMP(NTMP)=XCMP(1)
          YCMP(NTMP)=YCMP(1)
        END IF
      ELSE
        NTMP=10000
      END IF
C
      NCMP=NTMP
C
      RETURN
      END
      SUBROUTINE TSTEXT(XEXT,YEXT)
C
C  Calculate the world coordinate extents.
C
      REAL WINDOW(4),VIEWPT(4)
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WINDOW,VIEWPT)
      XEXT = WINDOW(2)-WINDOW(1)
      YEXT = WINDOW(4)-WINDOW(3)
C
      RETURN
      END
