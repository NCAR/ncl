
      PROGRAM MPEX14
C
C This program demonstrates how to use EZMAP for various EASE grids.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Declare stuff required to do color fill.
C
        PARAMETER (LAMA=2000000,NCRA=LAMA/10,NGPS=5)
C
        DIMENSION IAMA(LAMA),XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare arrays to use in calls to MAPSET.
C
        DIMENSION PLM1(2),PLM2(2),PLM3(2),PLM4(2)
C
C Set up some equivalences to simplify the passing of limits to MAPSET.
C
        EQUIVALENCE (BLLA,PLM1(1)),(BLLO,PLM2(1))
        EQUIVALENCE (TRLA,PLM3(1)),(TRLO,PLM4(1))
C
        EQUIVALENCE (CLLA,PLM1(1)),(CLLO,PLM1(2))
        EQUIVALENCE (CRLA,PLM2(1)),(CRLO,PLM2(2))
        EQUIVALENCE (BCLA,PLM3(1)),(BCLO,PLM3(2))
        EQUIVALENCE (TCLA,PLM4(1)),(TCLO,PLM4(2))
C
C Declare arrays in which to put labels for the plots.
C
        CHARACTER*14 PLAB(6)
C
C Declare the routines that are used to color the continental map
C backgrounds (one for each of the databases one can use).  COLRCO
C is used with the old continental outlines and COLRE2 with outlines
C from the database "Earth..2".
C
        EXTERNAL COLRCO
        EXTERNAL COLRE2
C
C Choose which continental map background to use.  Set IBKG = 0 for
C the old continental outlines or = 1 for the new outlines from the
C database "Earth..2".
C
        DATA IBKG / 0 /
C       DATA IBKG / 1 /
C
C Define labels to be used on the plots.
C
        DATA PLAB / 'EASE GRID "ML"','EASE GRID "MH"',
     +              'EASE GRID "NL"','EASE GRID "NH"',
     +              'EASE GRID "SL"','EASE GRID "SH"' /
C
C Value of "pi".
C
        DATA PI / 3.14159265358979 /
C
C Conversion constants.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Radius of the earth, in kilometers.
C
        DATA REKM / 6371.22800000000 /
C
C Forward mapping functions, Northern hemisphere EASE grid (R and S as
C functions of latitude and longitude).
C
        RNOR(XLAT,XLON)=RZRO+2.*(REKM/DCKM)*SIN(XLON)*SIN(PI/4.-XLAT/2.)
        SNOR(XLAT,XLON)=SZRO+2.*(REKM/DCKM)*COS(XLON)*SIN(PI/4.-XLAT/2.)
C
C Forward mapping functions, Southern hemisphere EASE grid (R and S as
C functions of latitude and longitude).
C
        RSOU(XLAT,XLON)=RZRO+2.*(REKM/DCKM)*SIN(XLON)*COS(PI/4.-XLAT/2.)
        SSOU(XLAT,XLON)=SZRO-2.*(REKM/DCKM)*COS(XLON)*COS(PI/4.-XLAT/2.)
C
C Forward mapping functions, global EASE grid (R and S as functions of
C latitude and longitude).
C
        RGLO(XLAT,XLON)=RZRO+(REKM/DCKM)*XLON*(SQRT(3.)/2.)
        SGLO(XLAT,XLON)=SZRO-(REKM/DCKM)*SIN(XLAT)/(SQRT(3.)/2.)
C
C Inverse mapping functions, Northern hemisphere EASE grid (latitude and
C longitude as functions of R and S).
C
        RLAN(RVAL,SVAL)=PI/2.-2.*ASIN(MAX(-1.,MIN(+1.,.5*DCKM*
     +                                      SQRT((RVAL-RZRO)**2+
     +                                           (SVAL-SZRO)**2)/REKM)))
        RLON(RVAL,SVAL)=ATAN2(RVAL-RZRO,SVAL-SZRO)
C
C Inverse mapping functions, Southern hemisphere EASE grid (latitude and
C longitude as functions of R and S).
C
        RLAS(RVAL,SVAL)=PI/2.-2.*ACOS(MAX(-1.,MIN(+1.,.5*DCKM*
     +                                SQRT((RVAL-RZRO)**2+
     +                                     (SVAL-SZRO)**2)/REKM)))
        RLOS(RVAL,SVAL)=ATAN2(RVAL-RZRO,SZRO-SVAL)
C
C Inverse mapping functions, global EASE grid (latitude and longitude as
C functions of R and S).
C
        RLAG(RVAL,SVAL)=ASIN(-(DCKM/REKM)*(SVAL-SZRO)*(SQRT(3.)/2.))
        RLOG(RVAL,SVAL)=      (DCKM/REKM)*(RVAL-RZRO)/(SQRT(3.)/2.)
C
C Open GKS.
C
        CALL GOPKS (IERRF,0)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn clipping off.
C
        CALL GSCLIP (0)
C
C Define some colors to use (0 = white, the background color; 1 =
C black, the foreground color; 2 is used for oceans; 3 is used for
C land; 4 is used for the grid; and 5 is used for land borders).
C
        CALL GSCR (IWKID,0,1.,1.,1.)
        CALL GSCR (IWKID,1,0.,0.,0.)
        CALL GSCR (IWKID,2,.5,.5,1.)
        CALL GSCR (IWKID,3,.5,1.,.5)
        CALL GSCR (IWKID,4,0.,.6,.6)
        CALL GSCR (IWKID,5,1.,.5,.5)
C
C Tell EZMAP to use a little smaller portion of the plotter frame.
C
        CALL MAPPOS (.1,.9,.5,.9)
C
C Tell PLOTCHAR to use a filled font and to outline the characters.
C
C       CALL PCSETI ('FN',25)
C       CALL PCSETI ('OF',1)
C
C Loop through six possible cases.
C
        DO 103 ICSE=1,6
C
C Set the diameter of a cell, the width and height of the grid, and the
C position of the center of the projection on it.
C
C Case 1: "ML" grid:
C
          IF (ICSE.EQ.1) THEN
C
            PRINT * , ' '
            PRINT * , 'ML GRID:'
            PRINT * , ' '
C
            DCKM=25.06752500000
            WDTH=1383.0
            HGHT= 586.0
            RZRO= 691.0
            SZRO= 292.5
C
            CALL MPSETI ('EL',0)
C
C Case 2: "MH" grid:
C
          ELSE IF (ICSE.EQ.2) THEN
C
            PRINT * , ' '
            PRINT * , 'MH GRID:'
            PRINT * , ' '
C
            DCKM=12.53376250000
            WDTH=2766.0
            HGHT=1171.0
            RZRO=1382.0
            SZRO= 585.0
C
            CALL MPSETI ('EL',0)
C
C Case 3: "NL" grid:
C
          ELSE IF (ICSE.EQ.3) THEN
C
            PRINT * , ' '
            PRINT * , 'NL GRID:'
            PRINT * , ' '
C
            DCKM=25.06752500000
            WDTH= 721.0
            HGHT= 721.0
            RZRO= 360.0
            SZRO= 360.0
C
            CALL MPSETI ('EL',1)
C
C Case 4: "NH" grid:
C
          ELSE IF (ICSE.EQ.4) THEN
C
            PRINT * , ' '
            PRINT * , 'NH GRID:'
            PRINT * , ' '
C
            DCKM=12.53376250000
            WDTH=1441.0
            HGHT=1441.0
            RZRO= 720.0
            SZRO= 720.0
C
            CALL MPSETI ('EL',1)
C
C Case 5: "SL" grid:
C
          ELSE IF (ICSE.EQ.5) THEN
C
            PRINT * , ' '
            PRINT * , 'SL GRID:'
            PRINT * , ' '
C
            DCKM=25.06752500000
            WDTH= 721.0
            HGHT= 721.0
            RZRO= 360.0
            SZRO= 360.0
C
            CALL MPSETI ('EL',1)
C
C Case 6: "SH" grid:
C
          ELSE IF (ICSE.EQ.6) THEN
C
            PRINT * , ' '
            PRINT * , 'SH GRID:'
            PRINT * , ' '
C
            DCKM=12.53376250000
            WDTH=1441.0
            HGHT=1441.0
            RZRO= 720.0
            SZRO= 720.0
C
            CALL MPSETI ('EL',1)
C
          END IF
C
C For cases 1 and 2, compute and print the lat/lon coordinates of the
C corner points of the grid and initialize the cylindrical equal-area
C projection using them.
C
          IF (ICSE.EQ.1.OR.ICSE.EQ.2) THEN
C
            TLLA=RTOD*RLAG(    -.5,    -.5)
            TLLO=RTOD*RLOG(    -.5,    -.5)
            TRLA=RTOD*RLAG(WDTH-.5,    -.5)
            TRLO=RTOD*RLOG(WDTH-.5,    -.5)
            BLLA=RTOD*RLAG(    -.5,HGHT-.5)
            BLLO=RTOD*RLOG(    -.5,HGHT-.5)
            BRLA=RTOD*RLAG(WDTH-.5,HGHT-.5)
            BRLO=RTOD*RLOG(WDTH-.5,HGHT-.5)
C
            PRINT * , 'TOP LEFT LATITUDE:     ',TLLA
            PRINT * , 'TOP LEFT LONGITUDE:    ',TLLO
            PRINT * , 'TOP RIGHT LATITUDE:    ',TRLA
            PRINT * , 'TOP RIGHT LONGITUDE:   ',TRLO
            PRINT * , 'BOTTOM LEFT LATITUDE:  ',BLLA
            PRINT * , 'BOTTOM LEFT LONGITUDE: ',BLLO
            PRINT * , 'BOTTOM RIGHT LATITUDE: ',BRLA
            PRINT * , 'BOTTOM RIGHT LONGITUDE:',BRLO
C
C Initialize EZMAP to use the projection 'EA' and tell it what portion
C of the projection to use.
C
            CALL MAPROJ ('EA',0.,(TLLO+TRLO)/2.,0.)
C
            CALL MAPSET ('CO',PLM1,PLM2,PLM3,PLM4)
C
C For cases 3 and 4, compute and print the lat/lon coordinates of the
C center points of the edges of the grid and initialize the Lambert
C equal-area projection using them, centering it at the North Pole.
C
          ELSE IF (ICSE.EQ.3.OR.ICSE.EQ.4) THEN
C
            TCLA=RTOD*RLAN(.5*WDTH-.5,       -.5)
            TCLO=RTOD*RLON(.5*WDTH-.5,       -.5)
            CLLA=RTOD*RLAN(       -.5,.5*HGHT-.5)
            CLLO=RTOD*RLON(       -.5,.5*HGHT-.5)
            CRLA=RTOD*RLAN(   WDTH-.5,.5*HGHT-.5)
            CRLO=RTOD*RLON(   WDTH-.5,.5*HGHT-.5)
            BCLA=RTOD*RLAN(.5*WDTH-.5,   HGHT-.5)
            BCLO=RTOD*RLON(.5*WDTH-.5,   HGHT-.5)
C
            PRINT * , 'TOP CENTER LATITUDE:    ',TCLA
            PRINT * , 'TOP CENTER LONGITUDE:   ',TCLO
            PRINT * , 'CENTER LEFT LATITUDE:   ',CLLA
            PRINT * , 'CENTER LEFT LONGITUDE:  ',CLLO
            PRINT * , 'CENTER RIGHT LATITUDE:  ',CRLA
            PRINT * , 'CENTER RIGHT LONGITUDE: ',CRLO
            PRINT * , 'BOTTOM CENTER LATITUDE: ',BCLA
            PRINT * , 'BOTTOM CENTER LONGITUDE:',BCLO
C
C Initialize EZMAP to use the projection 'LE', centered at the North
C Pole, and tell it what portion of the projection to use.
C
            CALL MAPROJ ('LE',+90.,0.,0.)
C
            CALL MAPSET ('PO',PLM1,PLM2,PLM3,PLM4)
C
C For cases 5 and 6, compute and print the lat/lon coordinates of the
C center points of the edges of the grid and initialize the Lambert
C equal-area projection using them, centering it at the South Pole.
C
          ELSE
C
            TCLA=RTOD*RLAS(.5*WDTH-.5,       -.5)
            TCLO=RTOD*RLOS(.5*WDTH-.5,       -.5)
            CLLA=RTOD*RLAS(       -.5,.5*HGHT-.5)
            CLLO=RTOD*RLOS(       -.5,.5*HGHT-.5)
            CRLA=RTOD*RLAS(   WDTH-.5,.5*HGHT-.5)
            CRLO=RTOD*RLOS(   WDTH-.5,.5*HGHT-.5)
            BCLA=RTOD*RLAS(.5*WDTH-.5,   HGHT-.5)
            BCLO=RTOD*RLOS(.5*WDTH-.5,   HGHT-.5)
C
            PRINT * , 'TOP CENTER LATITUDE:    ',TCLA
            PRINT * , 'TOP CENTER LONGITUDE:   ',TCLO
            PRINT * , 'CENTER LEFT LATITUDE:   ',CLLA
            PRINT * , 'CENTER LEFT LONGITUDE:  ',CLLO
            PRINT * , 'CENTER RIGHT LATITUDE:  ',CRLA
            PRINT * , 'CENTER RIGHT LONGITUDE: ',CRLO
            PRINT * , 'BOTTOM CENTER LATITUDE: ',BCLA
            PRINT * , 'BOTTOM CENTER LONGITUDE:',BCLO
C
C Initialize EZMAP to use the projection 'LE', centered at the South
C Pole, and tell it what portion of the projection to use.
C
            CALL MAPROJ ('LE',-90.,0.,0.)
C
            CALL MAPSET ('PO',PLM1,PLM2,PLM3,PLM4)
C
          END IF
C
C Write appropriate labels at the top of the frame.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),PLAB(ICSE),.015,0.,0.)
          CALL PLCHHQ (CFUX(.5),CFUY(.948),'(Only every 20th grid line i
     +s shown.)',
     +                                                       .015,0.,0.)
C
C Draw a color-filled map.
C
          CALL ARINAM (IAMA,LAMA)
C
          IF (IBKG.EQ.0) THEN
            CALL MDPBLA (IAMA)
            CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLRCO)
            CALL GSPLCI (5)
            CALL MDPLOT
          ELSE
            CALL MDLNAM ('Earth..2',2,IAMA)
            CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLRE2)
            CALL GSPLCI (5)
            CALL MDLNDR ('Earth..2',2)
          END IF
C
          CALL GSPLCI (1)
C
C         CALL MDPGRD
C
C Overlay a grid.
C
          CALL GSPLCI (4)
C
C Horizontal lines.
C
          SMIN=-.5
          SMAX=HGHT-.5
C
          VMIN=-(DCKM/REKM)*(SMIN-SZRO)
          VMAX=-(DCKM/REKM)*(SMAX-SZRO)
C
          IF (ICSE.EQ.1.OR.ICSE.EQ.2) THEN
            VMIN=2.*VMIN/SQRT(3.)
            VMAX=2.*VMAX/SQRT(3.)
          END IF
C

          DO 101 I=0,INT(WDTH),20
C
            RVAL=REAL(I)-.5
            UVAL=(DCKM/REKM)*(RVAL-RZRO)
C
            IF (ICSE.EQ.1.OR.ICSE.EQ.2) THEN
              UVAL=2.*UVAL/SQRT(3.)
            END IF
C
            CALL LINE (UVAL,VMIN,UVAL,VMAX)
C
  101     CONTINUE
C
C Vertical lines.
C
          RMIN=-.5
          RMAX=WDTH-.5
C
          UMIN=(DCKM/REKM)*(RMIN-RZRO)
          UMAX=(DCKM/REKM)*(RMAX-RZRO)
C
          IF (ICSE.EQ.1.OR.ICSE.EQ.2) THEN
            UMIN=2.*UMIN/SQRT(3.)
            UMAX=2.*UMAX/SQRT(3.)
          END IF
C
          DO 102 J=0,INT(HGHT),20
C
            SVAL=REAL(J)-.5
            VVAL=-(DCKM/REKM)*(SVAL-SZRO)
C
            IF (ICSE.EQ.1.OR.ICSE.EQ.2) THEN
              VVAL=2.*VVAL/SQRT(3.)
            END IF
C
            CALL LINE (UMIN,VVAL,UMAX,VVAL)
C
  102     CONTINUE
C
          CALL GSPLCI (1)
C
C Annotate the plot.
C
          CALL GSFACI (1)
C
          CALL GETSET (XVPL,XVPR,YVPB,YVPT,ULFT,URGT,VBOT,VTOP,LNLG)
C
          CALL DRWAWL (XVPL,YVPT+.02,XVPR,YVPT+.02,'r',.015)
          CALL DRWAWL (XVPL-.02,YVPT,XVPL-.02,YVPB,'s',.015)
          CALL DRWAWL (XVPL,YVPB-.02,XVPR,YVPB-.02,'u',.015)
          CALL DRWAWL (XVPR+.02,YVPB,XVPR+.02,YVPT,'v',.015)
C
          IF (ICSE.EQ.1) THEN
            CALL DRWAWL (XVPL,.5*(YVPB+YVPT),
     +                   XVPR,.5*(YVPB+YVPT),':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),YVPB,
     +                   .5*(XVPL+XVPR),YVPT,':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(sqrt(3)/2)
     +(R/C):G:k:R: and s = s:B:0:N:-(2/sqrt(3))(R/C)sin(:G:u:R:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 1382.5 (right), s from -.5 (top) to 585.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = (2/sqrt(3))(C/
     +R)(r-r:B:0:N:) and :G:u:R: = sin:S:-1:N:((sqrt(3)/2)(C/R)(s-s:B:0:
     +N:)).  :G:k:R: runs from ',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),'-180:S:o:N: (left) to 18
     +0:S:o:N: (right), :G:u:R: from -86.7168:S:o:N:(bottom) to 86.7168:
     +S:o:N: (top).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Cylindrical Equ
     +al-area projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = :G:k:R: and v = (4/3
     +)sin(:G:u:R:), which means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252), ':G:k:R: = u and :G:u:R:
     + = sin:S:-1:N:((3/4)v)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208),'r = r:B:0:N:+(sqrt(3)/2)
     +(R/C)u and s = s:B:0:N:-(sqrt(3)/2)(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186),'u=(2/sqrt(3))(C/R)(r-r:B
     +:0:N:) and v=-(2/sqrt(3))(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 25.067525 km (nomina
     +l cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 691.0, and s:
     +B:0:N: = 292.5.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          ELSE IF (ICSE.EQ.2) THEN
            CALL DRWAWL (XVPL,.5*(YVPB+YVPT),
     +                   XVPR,.5*(YVPB+YVPT),':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),YVPB,
     +                   .5*(XVPL+XVPR),YVPT,':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(sqrt(3)/2)
     +(R/C):G:k:R: and s = s:B:0:N:-(2/sqrt(3))(R/C)sin(:G:u:R:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 2765.5 (right), s from -.5 (top) to 1170.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = (2/sqrt(3))(C/
     +R)(r-r:B:0:N:) and :G:u:R: = sin:S:-1:N:((sqrt(3)/2)(C/R)(s-s:B:0:
     +N:)).  :G:k:R: runs from',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),'-179.935:S:o:N: (left) t
     +o 180.065:S:o:N: (right), :G:u:R: from -85.9533:S:o:N:(bottom) to
     +85.9533:S:o:N: (top).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Cylindrical Equ
     +al-area projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = :G:k:R: and v = (4/3
     +)sin(:G:u:R:), which means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252), ':G:k:R: = u and :G:u:R:
     + = sin:S:-1:N:((3/4)v)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208),'r = r:B:0:N:+(sqrt(3)/2)
     +(R/C)u and s = s:B:0:N:-(sqrt(3)/2)(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186),'u=(2/sqrt(3))(C/R)(r-r:B
     +:0:N:) and v=-(2/sqrt(3))(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 12.5337625 km (nomin
     +al cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 1382.0, and s
     +:B:0:N: = 585.0.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          ELSE IF (ICSE.EQ.3) THEN
            CALL DRWCWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),.5*(XVPR-XVPL),
     +                   -90.,-50.,-40.,270.,':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),    YVPB      ,
     +                   .5*(XVPL+XVPR),.5*(YVPB+YVPT),':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(2R/C)sin(:
     +G:k:R:)sin(:G:p:R:/4-:G:u:R:/2) and s = s:B:0:N:+(2R/C)cos(:G:k:R:
     +)sin(:G:p:R:/4-:G:u:R:/2).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 720.5 (right), s from -.5 (top) to 720.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = tan:S:-1:N:((r
     +-r:B:0:N:)/(s-s:B:0:N:)) and :G:u:R: = :G:p:R:/2-2sin:S:-1:N:((C/2
     +R)sqrt((r-r:B:0:N:):S:2:N:+(s-s:B:0:N:):S:2:N:))',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),':G:u:R: runs from -.3383
     +61:S:o:N: at any edge center to 90:S:o:N: at the grid center.',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Lambert Equal-a
     +rea projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = 2sin(:G:k:R:)sin(:G:
     +p:R:/4-:G:u:R:/2) and v = -2cos(:G:k:R:)sin(:G:p:R:/4-:G:u:R:/2),
     +which means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252),':G:k:R: = tan:S:-1:N:(-u
     +/v) and :G:u:R: = :G:p:R:/2-2*sin:S:-1:N:(sqrt(u:S:2:N:+v:S:2:N:)/
     +4)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208), 'r = r:B:0:N:+(R/C)u and
     + s = s:B:0:N:-(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186), 'u=(C/R)(r-r:B:0:N:) and
     + v=-(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 25.067525 km (nomina
     +l cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 360.0, and s:
     +B:0:N: = 360.0.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          ELSE IF (ICSE.EQ.4) THEN
            CALL DRWCWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),.5*(XVPR-XVPL),
     +                   -90.,-50.,-40.,270.,':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),    YVPB      ,
     +                   .5*(XVPL+XVPR),.5*(YVPB+YVPT),':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(2R/C)sin(:
     +G:k:R:)sin(:G:p:R:/4-:G:u:R:/2) and s = s:B:0:N:+(2R/C)cos(:G:k:R:
     +)sin(:G:p:R:/4-:G:u:R:/2).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 1440.5 (right), s from -.5 (top) to 1440.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = tan:S:-1:N:((r
     +-r:B:0:N:)/(s-s:B:0:N:)) and :G:u:R: = :G:p:R:/2-2sin:S:-1:N:((C/2
     +R)sqrt((r-r:B:0:N:):S:2:N:+(s-s:B:0:N:):S:2:N:))',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),':G:u:R: runs from -.3383
     +61:S:o:N: at any edge center to 90:S:o:N: at the grid center.',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Lambert Equal-a
     +rea projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = 2sin(:G:k:R:)sin(:G:
     +p:R:/4-:G:u:R:/2) and v = -2cos(:G:k:R:)sin(:G:p:R:/4-:G:u:R:/2),
     +which means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252),':G:k:R: = tan:S:-1:N:(-u
     +/v) and :G:u:R: = :G:p:R:/2-2*sin:S:-1:N:(sqrt(u:S:2:N:+v:S:2:N:)/
     +4)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208), 'r = r:B:0:N:+(R/C)u and
     + s = s:B:0:N:-(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186), 'u=(C/R)(r-r:B:0:N:) and
     + v=-(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 12.5337625 km (nomin
     +al cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 720.0, and s:
     +B:0:N: = 720.0.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          ELSE IF (ICSE.EQ.5) THEN
            CALL DRWCWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),.5*(XVPR-XVPL),
     +                   90.,50.,40.,-270.,':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),
     +                   .5*(XVPL+XVPR),         YVPT ,':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(2R/C)sin(:
     +G:k:R:)cos(:G:p:R:/4-:G:u:R:/2) and s = s:B:0:N:-(2R/C)cos(:G:k:R:
     +)cos(:G:p:R:/4-:G:u:R:/2).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 720.5 (right), s from -.5 (top) to 720.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = tan:S:-1:N:(-(
     +r-r:B:0:N:)/(s-s:B:0:N:)) and :G:u:R: = :G:p:R:/2-2cos:S:-1:N:((C/
     +2R)sqrt((r-r:B:0:N:):S:2:N:+(s-s:B:0:N:):S:2:N:))',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),':G:u:R: runs from -90:S:
     +o:N: at the grid center to +.338361:S:o:N: at any edge center.',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Lambert Equal-a
     +rea projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = 2sin(:G:k:R:)cos(:G:
     +p:R:/4-:G:u:R:/2) and v = 2cos(:G:k:R:)cos(:G:p:R:/4-:G:u:R:/2), w
     +hich means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252),':G:k:R: = tan:S:-1:N:(u/
     +v) and :G:u:R: = :G:p:R:/2-2*cos:S:-1:N:(sqrt(u:S:2:N:+v:S:2:N:)/4
     +)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208), 'r = r:B:0:N:+(R/C)u and
     + s = s:B:0:N:-(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186), 'u=(C/R)(r-r:B:0:N:) and
     + v=-(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 25.067525 km (nomina
     +l cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 360.0, and s:
     +B:0:N: = 360.0.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          ELSE IF (ICSE.EQ.6) THEN
            CALL DRWCWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),.5*(XVPR-XVPL),
     +                   90.,50.,40.,-270.,':G:k:R:',.015)
            CALL DRWAWL (.5*(XVPL+XVPR),.5*(YVPB+YVPT),
     +                   .5*(XVPL+XVPR),         YVPT ,':G:u:R:',.015)
            CALL PLCHHQ (CFUX(.500),CFUY(.450),'r = r:B:0:N:+(2R/C)sin(:
     +G:k:R:)cos(:G:p:R:/4-:G:u:R:/2) and s = s:B:0:N:-(2R/C)cos(:G:k:R:
     +)cos(:G:p:R:/4-:G:u:R:/2).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.428),'r runs from -.5 (left) t
     +o 1440.5 (right), s from -.5 (top) to 1440.5 (bottom).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.384), ':G:k:R: and :G:u:R: are
     + longitude and latitude, in radians.  Note that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.362),':G:k:R: = tan:S:-1:N:(-(
     +r-r:B:0:N:)/(s-s:B:0:N:)) and :G:u:R: = :G:p:R:/2-2cos:S:-1:N:((C/
     +2R)sqrt((r-r:B:0:N:):S:2:N:+(s-s:B:0:N:):S:2:N:))',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.340),':G:u:R: runs from -90:S:
     +o:N: at the grid center to .258454:S:o:N: at any edge center.',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.296),'EZMAP''s Lambert Equal-a
     +rea projection maps :G:k:R: and :G:u:R: to u and v, where',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.274),'u = 2sin(:G:k:R:)cos(:G:
     +p:R:/4-:G:u:R:/2) and v = 2cos(:G:k:R:)cos(:G:p:R:/4-:G:u:R:/2), w
     +hich means that',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.252),':G:k:R: = tan:S:-1:N:(u/
     +v) and :G:u:R: = :G:p:R:/2-2*cos:S:-1:N:(sqrt(u:S:2:N:+v:S:2:N:)/4
     +)',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.208), 'r = r:B:0:N:+(R/C)u and
     + s = s:B:0:N:-(R/C)v, so',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.186), 'u=(C/R)(r-r:B:0:N:) and
     + v=-(C/R)(s-s:B:0:N:).',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.142), 'R = 6371.228 km (radius
     + of Earth),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.120),'C = 12.5337625 km (nomin
     +al cell diameter),',
     +                                                       .011,0.,0.)
            CALL PLCHHQ (CFUX(.500),CFUY(.098),'r:B:0:N: = 720.0, and s:
     +B:0:N: = 720.0.',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC','|')
            CALL PLCHHQ (CFUX(.500),CFUY(.025),'For more information, se
     +e "http://nsidc.org/data/ease"',
     +                                                       .011,0.,0.)
            CALL PCSETC ('FC',':')
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
C End of case loop.
C
  103   CONTINUE
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END


      SUBROUTINE COLRCO (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MDPACI(IAI1).EQ.1) THEN
C           LAND
            CALL GSFACI (2)
          ELSE
C           LAND
            CALL GSFACI (3)
          END IF
          CALL GFA    (NCRA-1,XCRA,YCRA)
        END IF
        RETURN
      END


      SUBROUTINE COLRE2 (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
        IAI1=-1
        DO 101 IGRP=1,NGPS
          IF (IAGI(IGRP).EQ.1) IAI1=IAAI(IGRP)
  101   CONTINUE
        IF (IAI1.GT.0) THEN
          IF (MDIPAN(IAI1,'Water').NE.0) THEN
C           OCEAN
            CALL GSFACI (2)
          ELSE
C           LAND
            CALL GSFACI (3)
          END IF
          CALL GFA    (NCRA-1,XCRA,YCRA)
        END IF
        RETURN
      END


      SUBROUTINE DRWAWL (XBEG,YBEG,XEND,YEND,CHLB,CHSZ)
C
        CHARACTER*(*) CHLB
C
C This routine draws an arrow from the position (XBEG,YBEG) to the
C position (XEND,YEND) and labels it with the single character from
C CHLB, using a width of CHSZ.  The position and size are given in
C the fractional coordinate system.
C
C Double the line width.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (2.)
C
C Compute the X and Y differences along the arrow.
C
        XDIF=XEND-XBEG
        YDIF=YEND-YBEG
C
C Compute direction cosines for the arrow.
C
        ALEN=SQRT(XDIF*XDIF+YDIF*YDIF)
C
        XDCS=XDIF/ALEN
        YDCS=YDIF/ALEN
C
C Draw the body of the arrow, leaving a gap for the character label.
C
        CALL PLOTIF (XBEG,YBEG,0)
        CALL PLOTIF (XBEG+.25*XDIF-CHSZ*XDCS,YBEG+.25*YDIF-CHSZ*YDCS,1)
        CALL PLOTIF (XBEG+.25*XDIF+CHSZ*XDCS,YBEG+.25*YDIF+CHSZ*YDCS,0)
        CALL PLOTIF (XEND,YEND,1)
C
C Draw the arrowhead, making it the same size as the character label.
C
        CALL PLOTIF (XEND-CHSZ*XDCS-.5*CHSZ*YDCS,
     +               YEND-CHSZ*YDCS+.5*CHSZ*XDCS,0)
        CALL PLOTIF (XEND,YEND,1)
C
        CALL PLOTIF (XEND-CHSZ*XDCS+.5*CHSZ*YDCS,
     +               YEND-CHSZ*YDCS-.5*CHSZ*XDCS,0)
        CALL PLOTIF (XEND,YEND,1)
C
C Insert the label.
C
        CALL PCSETI ('CE',1)
        CALL PLCHHQ (CFUX(XBEG+.25*XDIF),CFUY(YBEG+.25*YDIF),CHLB,CHSZ,
     +                                                           0.,0.)
        CALL PCSETI ('CE',0)
C
C Reset the line width.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (1.)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DRWCWL (XCEN,YCEN,RADI,ANG1,ANG2,ANG3,ANG4,CHLB,CHSZ)
C
        CHARACTER*(*) CHLB
C
C This routine draws a circular arc centered at (XCEN,YCEN) and having
C radius RADI.  The arc begins at ANG1 and stops at ANG2, starts again
C at ANG3 and stops at ANG4.  The single character from CHLB is written
C in the gap between ANG2 and ANG3, using a character width of CHSZ.
C An arrow is placed at ANG4.  Positions and size are given in the
C fractional coordinate system, angles are given in degrees.
C
C Conversion constants.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Double the line width.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (2.)
C
C Draw the first arc.
C
        CALL PLOTIF (XCEN+RADI*COS(DTOR*ANG1),
     +               YCEN+RADI*SIN(DTOR*ANG1),0)
C
        NSTP=MAX(0,INT(ABS(ANG2-ANG1)/5)-1)
C
        DO 101 I=1,NSTP
          ANGI=(ANG1*REAL(NSTP-I+1)+ANG2*REAL(I))/REAL(NSTP+1)
          CALL PLOTIF (XCEN+RADI*COS(DTOR*ANGI),
     +                 YCEN+RADI*SIN(DTOR*ANGI),1)
  101   CONTINUE
C
        CALL PLOTIF (XCEN+RADI*COS(DTOR*ANG2),
     +               YCEN+RADI*SIN(DTOR*ANG2),1)
C
C Draw the second arc.
C
        CALL PLOTIF (XCEN+RADI*COS(DTOR*ANG3),
     +               YCEN+RADI*SIN(DTOR*ANG3),0)
C
        NSTP=MAX(0,INT(ABS(ANG4-ANG3)/5)-1)
C
        DO 102 I=1,NSTP
          ANGI=(ANG3*REAL(NSTP-I+1)+ANG4*REAL(I))/REAL(NSTP+1)
          CALL PLOTIF (XCEN+RADI*COS(DTOR*ANGI),
     +                 YCEN+RADI*SIN(DTOR*ANGI),1)
  102   CONTINUE
C
        CALL PLOTIF (XCEN+RADI*COS(DTOR*ANG4),
     +               YCEN+RADI*SIN(DTOR*ANG4),1)
C
C Save the final position of the second arc.
C
        XEND=XCEN+RADI*COS(DTOR*ANG4)
        YEND=YCEN+RADI*SIN(DTOR*ANG4)
C
C Compute direction cosines for the arrowhead.
C
        XDCS=COS(DTOR*(ANG4+SIGN(90.,ANG4-ANG3)))
        YDCS=SIN(DTOR*(ANG4+SIGN(90.,ANG4-ANG3)))
C
C Draw the arrowhead, making it the same size as the character label.
C
        CALL PLOTIF (XEND-CHSZ*XDCS-.5*CHSZ*YDCS,
     +               YEND-CHSZ*YDCS+.5*CHSZ*XDCS,0)
        CALL PLOTIF (XEND,YEND,1)
C
        CALL PLOTIF (XEND-CHSZ*XDCS+.5*CHSZ*YDCS,
     +               YEND-CHSZ*YDCS-.5*CHSZ*XDCS,0)
        CALL PLOTIF (XEND,YEND,1)
C
C Insert the label.
C
        CALL PCSETI ('CE',1)
C
        CALL PLCHHQ (CFUX(XCEN+RADI*COS(.5*DTOR*(ANG2+ANG3))),
     +               CFUY(YCEN+RADI*SIN(.5*DTOR*(ANG2+ANG3))),
     +                                                  CHLB,CHSZ,0.,0.)
C
        CALL PCSETI ('CE',0)
C
C Reset the line width.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (1.)
C
C Done.
C
        RETURN
C
      END
