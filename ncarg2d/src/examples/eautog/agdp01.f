
      PROGRAM AGDP01
C
C This is a demo program showing how to make AUTOGRAPH use DASHPACK,
C rather than DASHCHAR, to draw dashed lines, and also how to use a
C new feature of DASHPACK - the capability of drawing lines according
C to a pattern that specifies the use of interspersed symbols.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.  Use one
C of the following:
C
C       PARAMETER (IERF=6,LUNI=2,IWTY=1 ,IWID=1)  !  NCGM
C       PARAMETER (IERF=6,LUNI=2,IWTY=8 ,IWID=1)  !  X Windows
C       PARAMETER (IERF=6,LUNI=2,IWTY=20,IWID=1)  !  PostScript
C       PARAMETER (IERF=6,LUNI=2,IWTY=11,IWID=1)  !  PDF, Portrait
C       PARAMETER (IERF=6,LUNI=2,IWTY=12,IWID=1)  !  PDF, Landscape
C
        PARAMETER (IERF=6,LUNI=2,IWTYPE=1,IWTY=IWTYPE,IWID=1)
C
C Dimension arrays in which to define X and Y data for five curves.
C
        DIMENSION XCRA(101),YCRA(101,5)
C
C Declare some needed character variables.
C
        CHARACTER*12 DPT1,DPT2,DPT3,DPT4,DPT5
C
C Open GKS.
C
        CALL GOPKS (IERF,0)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Double the line width.
C
        CALL GSLWSC (2.)
C
C Define the data for the curves.
C
        DO 102 I=1,101
          XCRA(I)=REAL(I-1)/100.
          DO 101 J=1,5
            YCRA(I,J)=.02+.05*REAL(5-J)+.78*XCRA(I)**J
     +                                         +.5*(.25-(XCRA(I)-.5)**2)
  101     CONTINUE
  102   CONTINUE
C
C Tell AUTOGRAPH exactly what minimum and maximum values to use on the
C axes.
C
        CALL AGSETR ('X/MIN.',0.)
        CALL AGSETR ('X/MAX.',1.)
        CALL AGSETR ('Y/MIN.',0.)
        CALL AGSETR ('Y/MAX.',1.)
C
C Turn on windowing of curves drawn by AUTOGRAPH.
C
        CALL AGSETI ('WINDOW.',1)
C
C Set DASHPACK's "single character flag", so that strings of characters
C will more closely follow the path of the curve along which they are
C written.
C
        CALL DPSETI ('SCF - SINGLE CHARACTER FLAG',1)
C
C Tell AUTOGRAPH to use characters of somewhat greater than the default
C width in dash patterns.
C
        CALL AGSETR ('DASH/CHARACTER.',.018)
C
C Define five different character dash patterns using selected symbols
C and draw the curves.
C
        CALL AGSETI ('DASH/SELECTOR.',5)
        CALL AGSETI ('DASH/LENGTH.',12)
C
        CALL DPDPWS ('$$0$$0$$0$$0',
     +               '--+--+--+--+',DPT1)
        CALL DPDPWS ('$$1$$1$$1$$1',
     +               '--+--+--+--+',DPT2)
        CALL DPDPWS ('$$2$$2$$2$$2',
     +               '--+--+--+--+',DPT3)
        CALL DPDPWS ('$$3$$3$$3$$3',
     +               '--+--+--+--+',DPT4)
        CALL DPDPWS ('$$4$$4$$4$$4',
     +               '--+--+--+--+',DPT5)
C
        CALL AGSETC ('DASH/PATTERN/ 1.',DPT1)
        CALL AGSETC ('DASH/PATTERN/ 2.',DPT2)
        CALL AGSETC ('DASH/PATTERN/ 3.',DPT3)
        CALL AGSETC ('DASH/PATTERN/ 4.',DPT4)
        CALL AGSETC ('DASH/PATTERN/ 5.',DPT5)
C
        CALL EZMXY  (XCRA,YCRA,101,5,101,'DASH PATTERNS WITH SYMBOLS')
C
C Redefine the character dash patterns and draw the curves again.
C
        CALL AGSETI ('DASH/SELECTOR.',5)
        CALL AGSETI ('DASH/LENGTH.',12)
C
        CALL DPDPWS ('$dot$0$dot$0',
     +               '-----+-----+',DPT1)
        CALL DPDPWS ('$$$$$6$$$$$6',
     +               '-----+-----+',DPT2)
        CALL DPDPWS ('$$$$$7$$$$$7',
     +               '-----+-----+',DPT3)
        CALL DPDPWS ('$$$$$8$$$$$8',
     +               '-----+-----+',DPT4)
        CALL DPDPWS ('$$$$$9$$$$$9',
     +               '-----+-----+',DPT5)
C
        CALL AGSETC ('DASH/PATTERN/ 1.',DPT1)
        CALL AGSETC ('DASH/PATTERN/ 2.',DPT2)
        CALL AGSETC ('DASH/PATTERN/ 3.',DPT3)
        CALL AGSETC ('DASH/PATTERN/ 4.',DPT4)
        CALL AGSETC ('DASH/PATTERN/ 5.',DPT5)
C
        CALL EZMXY  (XCRA,YCRA,101,5,101,'DASH PATTERNS WITH SYMBOLS')
C
C Close GKS.
C
        CALL GDAWK (IWID)
        CALL GCLWK (IWID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END


      SUBROUTINE DASHDB (IPAT)
        CALL DPSETI ('DPS - DASH PATTERN SELECTOR',-16)
        CALL DPSETI ('DPT - DASH PATTERN (INTEGER)',IPAT)
        RETURN
      END


      SUBROUTINE DASHDC (IPAT,JCRT,JSIZ)
        CHARACTER*(*) IPAT
        CALL DPSETI ('DPS - DASH PATTERN SELECTOR',0)
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',IPAT)
        CALL DPSETR ('WOG - WIDTH OF GAP',CPFX(JCRT))
        CALL DPSETR ('WOS - WIDTH OF SOLID',CPFX(JCRT))
        IF (JSIZ.EQ.0) THEN
          ISIZ=8
        ELSE IF (JSIZ.EQ.1) THEN
          ISIZ=12
        ELSE IF (JSIZ.EQ.2) THEN
          ISIZ=16
        ELSE IF (JSIZ.EQ.3) THEN
          ISIZ=24
        ELSE
          ISIZ=JSIZ
        END IF
        CALL DPSETR ('WOC - WIDTH OF CHARACTER',CPFX(ISIZ))
        RETURN
      END


      SUBROUTINE CURVED (XCRA,YCRA,NCRA)
        DIMENSION XCRA(NCRA),YCRA(NCRA)
        CALL DPCURV (XCRA,YCRA,NCRA)
        RETURN
      END


      SUBROUTINE FRSTD (XCOP,YCOP)
        CALL DPFRST (XCOP,YCOP)
        RETURN
      END


      SUBROUTINE VECTD (XCOP,YCOP)
        CALL DPVECT (XCOP,YCOP)
        RETURN
      END


      SUBROUTINE LASTD
        CALL DPLAST
        RETURN
      END
