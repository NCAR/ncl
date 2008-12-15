
      PROGRAM MPEX15
C
C This is a test program for the routines MAPNGR, MAPNGD, and MAPNGQ,
C which allow one to display a transformed PNG as an Ezmap background.
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
C Define flags that control the drawing of grids and outlines.
C
        DATA IGRD,IOUT / 2,4 /
C
C Define conversion constants.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Open GKS.
C
        CALL GOPKS (IERRF,0)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping.
C
        CALL GSCLIP (0)
C
C Define basic colors.
C
        CALL GSCR (IWKID,0,1.,1.,1.)
        CALL GSCR (IWKID,1,0.,0.,0.)
        CALL GSCR (IWKID,2,1.,0.,0.)
        CALL GSCR (IWKID,3,0.,.8,0.)
C
C Define 129 gray-scale colors.
C
        DO 101 I=4,132
          P=REAL(I-4)/128.
          CALL GSCR (IWKID,I,P,P,P)
  101   CONTINUE
C
C Turn error recovery on.
C
        CALL ENTSR (ITMP,1)
C
C Tell PLOTCHAR to use a filled font and to outline the characters.
C
        CALL PCSETI ('FN',25)
        CALL PCSETI ('OF',1)
C
C Tell EZMAP what grid spacing to use.
C
        CALL MPSETI ('GR',IGRD)
C
C Read a PNG, downloaded from Wikimapia, showing most of Europe, and
C an associated PNGI and initilize information that geo-references the
C PNG so that it can be displayed as a background on a different
C projection of the globe.
C
        CALL MAPNGR ('Europe.png'//CHAR(0),40000)
C
C If an error occurred, print an error message.
C
        IF (NERRO(ITMP).NE.0) THEN
C
          CALL EPRIN
          CALL ERROF
          STOP
C
C Otherwise, produce the example plot.
C
        ELSE
C
C Initialize EZMAP to produce a satellite view of Europe looking north
C from a position somewhere over the Mediterranean.
C
          CALL MAPROJ ('SV', 25., 18., 10.)
          DFCE=2.
          CALL MPSETR ('SA',DFCE)
          CALL MPSETR ('S1',.6*RTOD*ASIN(1./DFCE))
          CALL MPSETR ('S2',90.)
          CALL MAPSET ('AN',8.,8.,8.,8.)
C
          CALL MAPINT
C
C Draw the projected PNG of the European map.
C
          CALL MAPNGD (4194304,0,4,129)
C
          IF (IGRD.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (2.)
            CALL GSPLCI (2)
            CALL MDPGRD
            CALL MDPLBL
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (1.)
            CALL GSPLCI (1)
          END IF
C
C If the "outline" flag is set, overlay geopolitical boundary lines
C from "Earth..4" on the map.
C
          IF (IOUT.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (2.)
            CALL GSPLCI (3)
            CALL MDLNDR ('Earth..4',IOUT)
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (1.)
            CALL GSPLCI (1)
          END IF
C
C Supply a title for the frame.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +               'USING A PROJECTED PNG AS A BACKGROUND',.018,0.,0.)
C
          CALL FRAME
C
          CALL MAPNGQ
C
        END IF
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
