
        PROGRAM CPEX11
C
C This program demonstrates the use of the new internal parameter 'PIT',
C which was installed in May of 1994.
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
C Declare the data array and the real and integer workspace arrays.
C
        DIMENSION ZDAT(37,19),RWRK(10000),IWRK(10000)
C
C Define the constant pi/180, used to convert angles from degrees to
C radians.
C
        DATA DTOR / .017453292519943 /
C
C Define a global array of data using a simple function of longitude
C (which is a linear function of the first subscript of the data array)
C and latitude (which is a linear function of the second subscript).
C
        DO 102 I=1,37
          RLON=-180.+10.*REAL(I-1)
          DO 101 J=1,19
            RLAT=-90.+10.*REAL(J-1)
            ZDAT(I,J)=SIN(DTOR*RLON)*COS(DTOR*RLAT)
  101     CONTINUE
  102   CONTINUE
C
C Salt in a few special values.
C
        ZDAT( 1, 8)=1.E36
        ZDAT( 2, 8)=1.E36
        ZDAT( 3, 8)=1.E36
        ZDAT(35, 8)=1.E36
        ZDAT(36, 8)=1.E36
        ZDAT(37, 8)=1.E36
        ZDAT(19, 9)=1.E36
        ZDAT(16,10)=1.E36
        ZDAT(17,10)=1.E36
        ZDAT(18,10)=1.E36
        ZDAT(19,10)=1.E36
        ZDAT(20,10)=1.E36
        ZDAT(21,10)=1.E36
        ZDAT(22,10)=1.E36
        ZDAT(19,11)=1.E36
        ZDAT( 1,12)=1.E36
        ZDAT( 2,12)=1.E36
        ZDAT( 3,12)=1.E36
        ZDAT(35,12)=1.E36
        ZDAT(36,12)=1.E36
        ZDAT(37,12)=1.E36
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Tell EZMAP what part of the plotter frame to use.
C
        CALL MAPPOS (.05,.95,.01,.91)
C
C Tell EZMAP what projection to use.  This projection maps the entire
C globe to the interior of a circle of radius 2.  Distortion near the
C outer edge of the circle is very great; mapped points from the data
C grid become so widely separated that contour lines wind up crossing
C each other.
C
        CALL MAPROJ ('LE',75.,99.,0.)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Tell CONPACK to map its output through CPMPXY, using that value which
C causes the EZMAP routine MAPTRA to be called.
C
        CALL CPSETI ('MAP - MAPPING FLAG',1)
C
C Tell CONPACK what value EZMAP returns for the out-of-range value.
C
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Tell CONPACK not to do a SET call (because EZMAP has already done it).
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK what the special value is.
C
        CALL CPSETR ('SPV - SPECIAL VALUE',1.E36)
C
C Tell CONPACK what values are to be associated with the extreme values
C of each of the subscripts of the data array.
C
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',-180.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',+180.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1', -90.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N', +90.)
C
C Tell CONPACK to draw the mapped boundary of the data grid.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
C Tell CONPACK to draw the mapped boundary of the area filled with
C special values.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
C Tell CONPACK to draw a limb line (separating areas that are visible
C under the projection from area that are not visible).
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-3)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
C Tell PLOTCHAR not to interpret colons as function-code signal
C characters.
C
        CALL PCSETC ('FC',CHAR(0))
C
C Tell PLOTCHAR to use one of the filled fonts.
C
        CALL PCSETI ('FN',25)
C
C Tell CONPACK what the dimensions of the data array and the workspace
C arrays are and make it initialize itself.
C
        CALL CPRECT (ZDAT,37,37,19,RWRK,10000,IWRK,10000)
C
C Draw the mapped contour lines, the mapped edges of the grid, the
C mapped edges of the special value area, and the visible/invisible
C boundary of the mapping.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Put a label at the top of the first frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +  'Using certain mappings can cause a problem like this:  Grid',
     +                                                       .018,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.939),
     +  'points are spread so far apart that contour lines cross.',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Set the value of the parameters 'PIC' and 'PIE' to interpolate points
C on all contour lines and all edge lines.
C
        CALL CPSETI ('PIC - POINT INTERPOLATION ON CONTOURS   ',7)
        CALL CPSETI ('PIE - POINT INTERPOLATION ON OTHER EDGES',7)
C
C Again draw the mapped contour lines, the mapped edges of the grid,
C the mapped edges of the special value area, and the visible/invisible
C boundary of the mapping.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Label the second frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +  'Using ''PIC'' = ''PIE'' = 7 solves the problem expensively by',
     +                                                       .018,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.939),
     +  'interpolating seven points on every segment before mapping.',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Turn off the interpolation requested by 'PIC' and 'PIE'.
C
        CALL CPSETI ('PIC - POINT INTERPOLATION ON CONTOURS   ',0)
        CALL CPSETI ('PIE - POINT INTERPOLATION ON OTHER EDGES',0)
C
C Set the value of the parameter 'PIT' to .05 to turn on interpolation
C of points only in problem areas (where the X/Y distance between points
C in user space exceeds 5/100ths of the horizontal/vertical dimension
C of the window.
C
        CALL CPSETR ('PIT - POINT INTERPOLATION THRESHOLD',.05)
C
C Again draw the mapped contour lines, the mapped edges of the grid,
C the mapped edges of the special value area, and the visible/invisible
C boundary of the mapping.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Label the third frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +  'Using ''PIT'' = .05 solves the problem less expensively (and',
     +                                                       .018,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.939),
     +  'more reliably) by interpolating points only as needed.',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Now turn smoothing on, using a value that says to smooth before
C mapping.
C
        CALL CPSETR ('T2D - TENSION ON 2D SMOOTHER',-2.5)
C
C Again draw the mapped contour lines, the mapped edges of the grid,
C the mapped edges of the special value area, and the visible/invisible
C boundary of the mapping.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Label the fourth frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +  'With ''PIT'' on, you can still smooth: Here, ''T2D'' = -2.5,',
     +                                                       .018,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.939),
     +  'which causes the smoothing to be done before the mapping.',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Now turn smoothing on, using a value that says to smooth after
C mapping.
C
        CALL CPSETR ('T2D - TENSION ON 2D SMOOTHER',2.5)
C
C Again draw the mapped contour lines, the mapped edges of the grid,
C the mapped edges of the special value area, and the visible/invisible
C boundary of the mapping.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Label the fifth frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +  'With ''PIT'' on, you can still smooth: Here, ''T2D'' = +2.5,',
     +                                                       .018,0.,0.)
        CALL PLCHHQ (CFUX(.5),CFUY(.939),
     +  'which causes the smoothing to be done after the mapping.',
     +                                                       .018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
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
