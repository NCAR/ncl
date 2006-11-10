
        PROGRAM PPEX01
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
C Declare arrays in which to define the clip polygon and the subject
C polygon.
C
        DIMENSION XCCP(5),YCCP(5),XCSP(11),YCSP(11)
C
C Declare the required work arrays.
C
        PARAMETER (NWRK=999)
C
        DIMENSION RWRK(NWRK),IWRK(NWRK)
C 
C The EQUIVALENCE line is commented out below. If memory storage is an
C issue for you, *and* RWRK is not a DOUBLE PRECISION variable, then you
C can uncomment this line.
C
C        EQUIVALENCE (RWRK(1),IWRK(1))
C
C Tell the compiler that the fill and merge routines for polygons and
C the fill routine for trapezoids are EXTERNALs, not REALs.
C
        EXTERNAL FILLPO,MERGPO,FILLTR
C
C Merge polygons are formed in the common block MERGCM:
C
        COMMON /MERGCM/ XCMP(999),YCMP(999),NCMP
        SAVE   /MERGCM/
C
C Define the clip polygon to be a small square.
C
        DATA NCCP / 5 /
C
        DATA XCCP( 1),YCCP( 1) / -5. , -5. /
        DATA XCCP( 2),YCCP( 2) /  5. , -5. /
        DATA XCCP( 3),YCCP( 3) /  5. ,  5. /
        DATA XCCP( 4),YCCP( 4) / -5. ,  5. /
        DATA XCCP( 5),YCCP( 5) / -5. , -5. /
C
C Define the subject polygon to be a diamond with a hole in it.
C
        DATA NCSP / 11 /
C
        DATA XCSP( 1),YCSP( 1) /  0. ,  9. /
        DATA XCSP( 2),YCSP( 2) /  0. ,  6. /
        DATA XCSP( 3),YCSP( 3) /  6. ,  0. /
        DATA XCSP( 4),YCSP( 4) /  0. , -6. /
        DATA XCSP( 5),YCSP( 5) / -6. ,  0. /
        DATA XCSP( 6),YCSP( 6) /  0. ,  6. /
        DATA XCSP( 7),YCSP( 7) /  0. ,  9. /
        DATA XCSP( 8),YCSP( 8) / -9. ,  0. /
        DATA XCSP( 9),YCSP( 9) /  0. , -9. /
        DATA XCSP(10),YCSP(10) /  9. ,  0. /
        DATA XCSP(11),YCSP(11) /  0. ,  9. /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Enable solid fill instead of the default hollow fill.
C
        CALL GSFAIS (1)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Tell GKS to use a doubled line width.
C
        CALL GSLWSC (2.)
C
C Define some colors to use for various purposes.
C
        CALL GSCR   (IWKID,0,0.,0.,0.)
        CALL GSCR   (IWKID,1,1.,1.,1.)
        CALL GSCR   (IWKID,2,1.,0.,0.)
        CALL GSCR   (IWKID,3,0.,1.,0.)
        CALL GSCR   (IWKID,4,0.,0.,1.)
        CALL GSCR   (IWKID,5,1.,0.,1.)
C
C Define the mapping from the user system to the fractional system.
C
        CALL SET (.05,.95,.05,.95,-10.,10.,-10.,10.,1)
C
C Tell PLOTCHAR to use filled characters from font 25.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
C
C Fill the polygons representing the difference of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 1',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Difference polygon formed using routines PPDIPO and FILLPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The difference S-C is filled in
     + white.',
     +                                                      .0148,0.,0.)
C
        CALL PPDIPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPDIPO RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Fill the polygons representing the intersection of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 2',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Intersection polygon formed using routines PPINPO and FILLPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The intersection S:F34:G:F:C is
     + filled in white.',
     +                                                      .0148,0.,0.)
C
        CALL PPINPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPINPO RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Fill the polygons representing the union of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 3',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Union polygon formed using routines PPUNPO and FILLPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The union S:F34:H:F:C is filled
     + in white.  Using FILLPO results in three errors; compare with fra
     +me 6.',
     +                                                      .0148,0.,0.)
C
        CALL PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPUNPO RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Merge the polygons representing the difference of the clip polygon
C and the subject polygon and fill the result.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 4',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Difference polygon formed using routines PPDIPO and MERGPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The difference S-C is filled in
     + white.',
     +                                                      .0148,0.,0.)
C
        NCMP=0
C
        CALL PPDIPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,MERGPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPDIPO RETURNS IERR = ',IERR
C
        IF (NCMP.EQ.0) THEN
          PRINT * , 'MERGE POLYGON IS NULL'
        ELSE IF (NCMP.EQ.1000) THEN
          PRINT * , 'MERGE POLYGON WAS TOO BIG TO HANDLE'
        ELSE
          CALL GFA (NCMP-1,XCMP,YCMP)
        END IF
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Merge the polygons representing the intersection of the clip polygon
C and the subject polygon and fill the result.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 5',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Intersection polygon formed using routines PPINPO and MERGPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The intersection S:F34:G:F:C is
     + filled in white.',
     +                                                      .0148,0.,0.)
C
        NCMP=0
C
        CALL PPINPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,MERGPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPINPO RETURNS IERR = ',IERR
C
        IF (NCMP.EQ.0) THEN
          PRINT * , 'MERGE POLYGON IS NULL'
        ELSE IF (NCMP.EQ.1000) THEN
          PRINT * , 'MERGE POLYGON WAS TOO BIG TO HANDLE'
        ELSE
          CALL GFA (NCMP-1,XCMP,YCMP)
        END IF
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Merge the polygons representing the union of the clip polygon
C and the subject polygon and fill the result.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 6',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Union polygon formed using routines PPUNPO and MERGPO.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green and the clip polygon C in red.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The union S:F34:H:F:C is filled
     + in white; using MERGPO fixes the errors seen on frame 3.',
     +                                                      .0148,0.,0.)
C
        NCMP=0
C
        CALL PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,MERGPO,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPUNPO RETURNS IERR = ',IERR
C
        IF (NCMP.EQ.0) THEN
          PRINT * , 'MERGE POLYGON IS NULL'
        ELSE IF (NCMP.EQ.1000) THEN
          PRINT * , 'MERGE POLYGON WAS TOO BIG TO HANDLE'
        ELSE
          CALL GFA (NCMP-1,XCMP,YCMP)
        END IF
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Fill the trapezoids representing the difference of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 7',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Difference polygon formed using routines PPDITR and FILLTR.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green, the clip polygon C in red, trapezoid edges in blue.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The trapezoids comprising the d
     +ifference S-C are filled in white.',
     +                                                      .0148,0.,0.)
C
        CALL GSPLCI (4)
        CALL PPDITR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLTR,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPDITR RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Fill the trapezoids representing the intersection of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 8',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Intersection polygon formed using routines PPINTR and FILLTR.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green, the clip polygon C in red, trapezoid edges in blue.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The trapezoids comprising the i
     +ntersection S:F34:G:F:C are filled in white.',
     +                                                      .0148,0.,0.)
C
        CALL GSPLCI (4)
        CALL PPINTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLTR,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPINTR RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Fill the trapezoids representing the union of the clip polygon
C and the subject polygon.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 9',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Union polygon formed using routines PPUNTR and FILLTR.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The subject polygon S is outlin
     +ed in green, the clip polygon C in red, trapezoid edges in blue.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04),'The trapezoids comprising the u
     +nion S:F34:H:F:C are filled in white.',
     +                                                      .0148,0.,0.)
C
        CALL GSPLCI (4)
        CALL PPUNTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                  RWRK,IWRK,NWRK,FILLTR,IERR)
C
        IF (IERR.NE.0) PRINT * , 'PPUNTR RETURNS IERR = ',IERR
C
        CALL GSPLCI (3)
        CALL GPL    (NCSP,XCSP,YCSP)
        CALL GSPLCI (2)
        CALL GPL    (NCCP,XCCP,YCCP)
        CALL GSPLCI (1)
C
        CALL FRAME
C
C Do one more test, of polyline clipping.
C
        CALL TSTCLP
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



      SUBROUTINE TSTCLP
C
C Define the size of the polyline this subroutine is to generate.
C
        PARAMETER (NCRA=100)
C
C The parameter LWRK specifies the length of the workspace to be used
C by the polyline package.
C
        PARAMETER (LWRK=100)
C
C Define some arrays in which to put the coordinates of lines.
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C The array RWRK is a workspace array required by the polyline clipper.
C
        DIMENSION RWRK(LWRK)
C
C The routine DRAWEM is called by the polyline clipping routine.
C
        EXTERNAL DRAWEM
C
C Initialize the polyline color index and the line width scale factor.
C
        CALL GSPLCI (1)
        CALL GSLWSC (1.)
C
C Call SET in such a way that we can use fractional coordinates.
C
        CALL SET (0.,1.,0.,1.,-.15,1.15,-.15,1.15,1)
C
C Create a random polyline.
C
        DO 101 J=1,NCRA
          XCRA(J)=PPFRAN()
          YCRA(J)=PPFRAN()
  101   CONTINUE
C
C Draw the polyline twice, once directly and once using the clipper, a
C different color, and a greater line width.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.96),
     +               'POLYPACK EXAMPLE - FRAME 10',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.93),
     +  'Demonstrating the use of PPPLCL for clipping polylines.',
     +                                                      .0175,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.07),'The clipping rectangle is outli
     +ned in magenta.',
     +                                                      .0148,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.04), 'The polyline is first drawn in
     + white and then again in red using PPPLCL.',
     +                                                      .0148,0.,0.)
C
        FRCT=.25
        XWDL=FRCT
        XWDR=1.-FRCT
        YWDB=FRCT
        YWDT=1.-FRCT
C
        CALL GSPLCI (1)
        CALL GSLWSC (2.)
        CALL CURVE  (XCRA,YCRA,NCRA)
C
        CALL GSPLCI (2)
        CALL GSLWSC (4.)
        CALL PPPLCL (XWDL,XWDR,YWDB,YWDT,XCRA,YCRA,NCRA,RWRK,LWRK,
     +                                                DRAWEM,IERR)
C
        IF (IERR.NE.0) THEN
          PRINT *, 'ERROR IN PPPLCL, IERR = ', IERR
        ENDIF
C
        CALL GSPLCI (5)
        CALL GSLWSC (2.)
        CALL LINE   (XWDL,YWDB,XWDR,YWDB)
        CALL LINE   (XWDR,YWDB,XWDR,YWDT)
        CALL LINE   (XWDR,YWDT,XWDL,YWDT)
        CALL LINE   (XWDL,YWDT,XWDL,YWDB)
C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE FILLPO (XCRA,YCRA,NCRA)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C This routine processes polygons generated by the routines PPDIPO,
C PPINPO, and PPUNPO.
C
C Fill the polygon.
C
        CALL GFA (NCRA-1,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE MERGPO (XCRA,YCRA,NCRA)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C This routine merges the polygons generated by one of the routines
C PPDIPO, PPINPO, and PPUNPO into a single polygon with holes.
C
C Merge polygons are formed in the common block MERGCM:
C
        COMMON /MERGCM/ XCMP(999),YCMP(999),NCMP
        SAVE   /MERGCM/
C
C Copy the coordinates of the latest polygon into the merge polygon
C coordinate arrays and, if the polygon is not the first of the group,
C repeat the first point of the first polygon.  (Actually, the code
C below does something a little more complicated: if necessary, it
C interpolates points to ensure that the connecting lines between
C polygons consist of horizontal and/or vertical steps; this tends
C to prevent problems caused by deficiencies in the fill algorithms
C on some devices.)
C
        NTMP=NCMP
C
        IF (NTMP+NCRA+4.LE.999) THEN
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
          DO 101 ICRA=1,NCRA
            XCMP(NTMP+ICRA)=XCRA(ICRA)
            YCMP(NTMP+ICRA)=YCRA(ICRA)
  101     CONTINUE
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
          NTMP=1000
        END IF
C
        NCMP=NTMP
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE FILLTR (XCBL,XCBR,YCOB,DXLE,DXRE,YCOT)
C
        DIMENSION XCRA(5),YCRA(5)
C
C This routine fills trapezoids generated by the routines PPDITR,
C PPINTR, and PPUNTR.
C
C If the trapezoid is not degenerate, fill it and outline it.
C
        IF (YCOT.GT.YCOB) THEN
          XCRA(1)=XCBL
          YCRA(1)=YCOB
          XCRA(2)=XCBR
          YCRA(2)=YCOB
          XCRA(3)=XCBR+DXRE*(YCOT-YCOB)
          YCRA(3)=YCOT
          XCRA(4)=XCBL+DXLE*(YCOT-YCOB)
          YCRA(4)=YCOT
          XCRA(5)=XCBL
          YCRA(5)=YCOB
          CALL GFA (4,XCRA,YCRA)
          CALL GPL (5,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END



      FUNCTION PPFRAN()
        DOUBLE PRECISION X
        SAVE X
        DATA X / 2.718281828459045D0 /
        X=MOD(9821.D0*X+.211327D0,1.D0)
        PPFRAN=REAL(X)
        RETURN
      END



      SUBROUTINE DRAWEM (XCRA,YCRA,NCRA)
C
C Draw the polyline fragment defined by the arguments.
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
        CALL CURVE (XCRA,YCRA,NCRA)
C
        RETURN
C
      END
