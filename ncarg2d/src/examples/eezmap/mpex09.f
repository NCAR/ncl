
        PROGRAM MPEX09
C
C The program MPEX09 produces a set of plots showing the numbers
C of all the segments in a chosen EZMAP outline dataset.  Certain
C variables must be set in data statements at the beginning of
C the program.  In each of the seven places marked off by rows of
C dashes, un-comment the first card to do outline dataset 'CO',
C the second to do 'US', the third to do 'PO', and the fourth to
C do 'PS'.
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
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Define variables required.
C
      CHARACTER*2 OUTD
      CHARACTER*2 PROJ
      CHARACTER*2 LMTS
      DIMENSION PL1(2),PL2(2),PL3(2),PL4(2)
      CHARACTER*38 PLBL
C
C Select the outline dataset.
C
C-----------------------
      DATA OUTD / 'CO' /
C     DATA OUTD / 'US' /
C     DATA OUTD / 'PO' /
C     DATA OUTD / 'PS' /
C-----------------------
C
C Select the projection type.
C
C-----------------------
      DATA PROJ / 'ME' /
C     DATA PROJ / 'LC' /
C     DATA PROJ / 'ME' /
C     DATA PROJ / 'ME' /
C-----------------------
C
C Select the appropriate values of PLAT, PLON, and ROTA.
C
C------------------------------------------
      DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C     DATA PLAT,PLON,ROTA / 30.,-100.,45. /
C     DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C     DATA PLAT,PLON,ROTA /  0.,   0., 0. /
C------------------------------------------
C
C Select the parameter saying how the map limits are chosen.
C
C-----------------------
      DATA LMTS / 'MA' /
C     DATA LMTS / 'CO' /
C     DATA LMTS / 'MA' /
C     DATA LMTS / 'MA' /
C-----------------------
C
C Select the values to be put in the limits arrays.
C
C---------------------------------------------------------------
      DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) / 22.6,-120.,46.9,-64.2 /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C     DATA PL1(1),PL2(1),PL3(1),PL4(1) /   0.,   0.,  0.,   0. /
C---------------------------------------------------------------
C
C---------------------------------------------------------------
      DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C     DATA PL1(2),PL2(2),PL3(2),PL4(2) /   0.,   0.,  0.,   0. /
C---------------------------------------------------------------
C
C Select values determining how the whole map is to be carved
C up into little maps.  ILIM is the number of divisions of the
C horizontal axis, JLIM the number of divisions of the vertical
C axis.  (If all the labels are put on a single map, the result
C is confusing.)
C
C---------------------------
      DATA ILIM,JLIM / 2,2 /
C     DATA ILIM,JLIM / 3,2 /
C     DATA ILIM,JLIM / 2,2 /
C     DATA ILIM,JLIM / 6,6 /
C---------------------------
C
C Define the plot label.
C
      DATA PLBL / 'SEGMENT NUMBERS FOR OUTLINE DATASET XX' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Finish the plot label.
C
      PLBL(37:38)=OUTD
C
C Set the character size; 6 is about the smallest usable value.
C
      ICSZ=6
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU',OUTD)
C
C Set the projection-type parameters.
C
      CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C Set the limits parameters.
C
      CALL MAPSET (LMTS,PL1,PL2,PL3,PL4)
C
C Initialize EZMAP.
C
      CALL MAPINT
C
C Retrieve the values with which MAPINT called SET.
C
      CALL GETSET (FLEM,FREM,FBEM,FTEM,ULEM,UREM,VBEM,VTEM,LNLG)
      ILEM=KFPX(FLEM)
      IREM=KFPX(FREM)
      IBEM=KFPY(FBEM)
      ITEM=KFPY(FTEM)
C
C Now, plot a set of maps which are subsets of the whole map.
C
      DO 102 I=1,ILIM
        DO 101 J=1,JLIM
          CALL MAPSET ('LI',
     +                 ULEM+(UREM-ULEM)*REAL(I-1)/REAL(ILIM),
     +                 ULEM+(UREM-ULEM)*REAL(I  )/REAL(ILIM),
     +                 VBEM+(VTEM-VBEM)*REAL(J-1)/REAL(JLIM),
     +                 VBEM+(VTEM-VBEM)*REAL(J  )/REAL(JLIM))
C
C Re-initialize EZMAP with the new limits.
C
          CALL MAPINT
C
C Retrieve the values with which MAPINT called SET.
C
          CALL GETSET (FLEW,FREW,FBEW,FTEW,
     +                 ULEW,UREW,VBEW,VTEW,LNLG)
          ILEW=KFPX(FLEW)
          IREW=KFPX(FREW)
          IBEW=KFPY(FBEW)
          ITEW=KFPY(FTEW)
C
C Compute quantities required by MAPEOD and MOVEIT to position
C labels.
C
          DELU=3.5*(REAL(ICSZ)/REAL(IREW-ILEW))*(UREW-ULEW)
          DELV=2.0*(REAL(ICSZ)/REAL(ITEW-IBEW))*(VTEW-VBEW)
          NSAV=0
C
C Draw the outlines.
C
          CALL MAPLOT
C
C Put a label at the top of the plot.
C
          CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
          CALL PWRIT (.5,.975,PLBL,38,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
          CALL BNDARY
C
C Advance the frame.
C
          CALL FRAME
C
  101   CONTINUE
  102 CONTINUE
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
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
C This version of MAPEOD marks each segment of the map with its
C segment number.  The resulting map may be used to set up other
C versions of MAPEOD which plot selected segments.
C
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Define local variables to hold the character-string form of the
C segment number.
C
      CHARACTER*4 CSG1,CSG2
C
C Find out where on the map the center of the segment is.
C
      MPTS=NPTS/2+1
      CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCEN,VCEN)
C
C If the center is visible, label it.
C
      IF (UCEN.GE.ULEW.AND.UCEN.LE.UREW.AND.
     +    VCEN.GE.VBEW.AND.VCEN.LE.VTEW) THEN
C
C Generate a character string representing the value of the
C segment number.
C
        WRITE (CSG2,1001) NSEG
        NFCH=4-INT(ALOG10(REAL(NSEG)+.5))
        NCHS=5-NFCH
        CSG1=CSG2(NFCH:4)
C
C Find out where the two points on either side of the center are.
C
        MPTS=MAX(NPTS/2,1)
        CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCM1,VCM1)
C
        MPTS=MIN(NPTS/2+2,NPTS)
        CALL MAPTRN (PNTS(2*MPTS-1),PNTS(2*MPTS),UCP1,VCP1)
C
C Compute the preferred position of the label, with one corner
C of its enclosing box at the center of the segment.
C
        ULAB=UCEN-SIGN(.1428*DELU*REAL(NCHS),UCM1+UCP1-2*UCEN)
        VLAB=VCEN-SIGN(.3333*DELV,VCM1+VCP1-2*VCEN)
C
C Move the label as necessary to avoid its being on top of any
C previous label.
C
        CALL MOVEIT (ULAB,VLAB)
C
C Write out the character string and connect it to the segment
C with a straight line.
C
        CALL LINE (UCEN,VCEN,
     +             ULAB-SIGN(.1428*DELU*REAL(NCHS),ULAB-UCEN),
     +             VLAB-SIGN(.3333*DELV,VLAB-VCEN))
C
        CALL PWRIT (ULAB,VLAB,CSG1,NCHS,ICSZ,0,0)
C
      END IF
C
C Done.
C
      RETURN
C
C Format.
C
 1001 FORMAT (I4)
C
      END
      SUBROUTINE MOVEIT (ULAB,VLAB)
C
C The object of this routine is to avoid putting segment labels
C on top of each other.  (ULAB,VLAB), on entry, is the most
C desirable position for a segment label.  MOVEIT modifies this
C position as necessary to make sure the label will not be on top
C of any previous label.
C
C The common block LIMITS communicates values between TESTIT and
C the routines MAPEOD and MOVEIT.
C
C ULEW, UREW, VBEW, and VTEW specify the u/v limits of the window
C in which the map was drawn.  DELU and DELV are the minimum
C allowable distances between segment-label centers in the u
C and v directions, respectively.  NSAV is the number of label
C positions saved in the arrays USAV and VSAV.
C
      COMMON /LIMITS/ ICSZ,ULEW,UREW,VBEW,VTEW,DELU,DELV,NSAV
C
C Previous label positions are saved in the arrays USAV and VSAV.
C
      DIMENSION USAV(1000),VSAV(1000)
C
C USAV and VSAV must maintain their values between calls.
C
      SAVE USAV,VSAV
C
C Zero the variables which control the generation of a spiral.
C
      IGN1=0
      IGN2=2
C
C Check for overlap at the current position.
C
  101 DO 102 I=1,NSAV
        IF (ABS(ULAB-USAV(I)).LT.DELU.AND.
     +      ABS(VLAB-VSAV(I)).LT.DELV) GO TO 103
  102 CONTINUE
C
C No overlap.  Save the new label position and return to caller.
C
      NSAV=NSAV+1
      IF (NSAV.GT.1000) STOP 1
      USAV(NSAV)=ULAB
      VSAV(NSAV)=VLAB
C
      RETURN
C
C Overlap.  Try a new point.  The points tried form a spiral.
C
  103 IGN1=IGN1+1
      IF (IGN1.LE.IGN2/2) THEN
        ULAB=ULAB+SIGN(DELU,-.5+REAL(MOD(IGN2/2,2)))
      ELSE
        VLAB=VLAB+SIGN(DELV,-.5+REAL(MOD(IGN2/2,2)))
      END IF
      IF (IGN1.EQ.IGN2) THEN
        IGN1=0
        IGN2=IGN2+2
      END IF
      IF (ULAB.LT.ULEW.OR.ULAB.GT.UREW.OR.
     +    VLAB.LT.VBEW.OR.VLAB.GT.VTEW) GO TO 103
      GO TO 101
C
      END
