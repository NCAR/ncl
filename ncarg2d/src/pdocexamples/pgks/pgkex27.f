
      PROGRAM PGKEX27
C
C  This program illustrates apeending to metafiles, both to
C  metafiles that have been suspended in the middle of a picture
C  as well as to previously created full metafiles.  The logic
C  of the code is:
C
C         o  Open and activate an initial metafile and establish
C            values for the primitive attributes for that file.
C         o  Draw a line and a filled area in the first metafile
C            and suspend output.
C         o  Open a second metafile, establish values for the
C            primitive attributes in that metafile.
C         o  Draw a picture in the second metafile and deactivate
C            it and close it.
C         o  Reopen the first metafile and complete drawing the
C            first picture.
C         o  Close the first metafile.
C         o  Reopen the second metafile and draw another picture.
C         o  Close the second metafile.
C
C  Two metafiles are created.  The first, gmeta1, has one picture;
C  the second, gmeta2, has two pictures.
C
C
C  Number of colors in the color tables.
C
      PARAMETER    (NCOLS=5)
C
C  Arrays for storing the attribute settings, and the color tables. 
C
      INTEGER      IC1(14), IC2(14)
      REAL         RC1( 7), RC2( 7)
      REAL         CTAB1(3,NCOLS), CTAB2(3,NCOLS)
C
C  Dafine the color tables for the two metafiles.
C
      DATA CTAB1/
     +           1., 1., 1.,
     +           0., 0., 0.,
     +           1., 0., 0.,
     +           0., 1., 0.,
     +           0., 0., 1.
     +         /
      DATA CTAB2/
     +           0., 0., 0.,
     +           1., 1., 1.,
     +           0., 1., 1.,
     +           1., 0., 1.,
     +           1., 1., 0.
     +         /
C
C  Open GKS.
C
      CALL GOPKS(6,0)
C
C  Open a metafile named "gmeta1" and begin a picture, using
C  the color table in CTAB1.
C
      CALL NGSETC('ME','gmeta1')
      CALL GOPWK(1,2,1)
      CALL GACWK(1)
      DO 10 I=1,NCOLS
        CALL GSCR(1,I-1,CTAB1(1,I),CTAB1(2,I),CTAB1(3,I))
   10 CONTINUE
C
C  Establish values for the GKS attributes for gmeta1.
C
      CALL GSLN(1)
      CALL GSPLCI(1)
      CALL GSLWSC(5.)
      CALL GSFAIS(1)
      CALL GSFACI(4)
      CALL GSMK(2)
      CALL GSPMCI(2)
      CALL GSMKSC(5.)
      CALL GSTXCI(3)
      CALL GSCHH(0.05)
      CALL GSTXFP(12,2)
      CALL GSTXAL(2,3)
C
C  Draw a line and a filled area.
C
      CALL DRPRIM(' ',0)
C
C  Save the current attribute settings and suspend drawing in gmeta1.
C
      CALL NGSRAT(2,IC1,RC1)
      CALL GDAWK(1)
      CALL NGMFTC(1)
C
C  Open and activate a metafile named gmeta2.
C
      CALL NGSETC('ME','gmeta2')
      CALL GOPWK(1,2,1)
      CALL GACWK(1)
      DO 20 I=1,NCOLS
        CALL GSCR(1,I-1,CTAB2(1,I),CTAB2(2,I),CTAB2(3,I))
   20 CONTINUE
C
C  Draw a picture and close the workstation.
C
      CALL GSLN(2)
      CALL GSPLCI(4)
      CALL GSLWSC(1.)
      CALL GSFAIS(3)
      CALL GSFASI(5)
      CALL GSFACI(1)
      CALL GSMK(4)
      CALL GSPMCI(2)
      CALL GSMKSC(10.)
      CALL GSTXCI(3)
      CALL GSCHH(0.03)
      CALL GSTXFP(13,2)
      CALL GSTXAL(2,3)
      CALL DRPRIM(' ',0)
      CALL DRPRIM('gmeta2 - picture 1',1)
      CALL FRAME
C
C  Save the attriburtes of the second metafile.
C
      CALL NGSRAT(2,IC2,RC2)
C
C  Close the workstation for gmeta2.
C
      CALL GDAWK(1)
      CALL GCLWK(1)
C
C  Reopen gmeta1 and add to the first picture.
C
      CALL NGREOP(1, 2, 1, 'gmeta1', 2, IC1, RC1, NCOLS, 0, CTAB1)
      CALL GACWK(1)
C
      CALL DRPRIM('gmeta1',1)
      CALL FRAME
C
C  Deactivate and close the first metafile.
C
      CALL GDAWK(1)
      CALL GCLWK(1)
C
C  Reopen and add a second picture to gmeta2.
C
      CALL NGREOP(1, 2, 1, 'gmeta2', 2, IC2, RC2, NCOLS, 0, CTAB2)
      CALL GACWK(1)
      CALL DRPRIM(' ',0)
      CALL DRPRIM('gmeta2 - picture 2',1)
      CALL FRAME
C
C  Close things down.
C
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE DRPRIM(STR,IOPT)
C
C  Draws output primitives.
C
C   GPL and GFA if IOPT=0;
C   GPM and GTX if IOPT=1.
C
      CHARACTER*(*) STR
      REAL XX(2), YY(2), BX(5), BY(5), TMX(4), TMY(4)
      DATA XX/0.15, 0.45/
      DATA YY/0.7, 0.7/
      DATA BX/0.6, 0.8, 0.8, 0.6, 0.6/
      DATA BY/0.4, 0.4, 0.8, 0.8, 0.4/
      DATA TX/0.30/
      DATA TY/0.50/
      DATA TMX/0.2, 0.4, 0.6, 0.8/
      DATA TMY/0.2, 0.2, 0.2, 0.2/
C
      IF (IOPT .EQ. 0) THEN
        CALL GPL(2,XX,YY)
        CALL GFA(5,BX,BY)
      ELSE IF (IOPT .EQ. 1) THEN
        CALL GPM(4,TMX,TMY)
        CALL GTX(TX,TY,STR)
        CALL NGDOTS(TX,TY,1,0.01,1)
      ENDIF
C
      RETURN
      END
