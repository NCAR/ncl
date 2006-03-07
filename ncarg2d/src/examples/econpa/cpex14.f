
        PROGRAM CPEX14
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
C This program attempts to present a solution to a problem sent to us
C by a user: Imagine that we have three arrays, ICLR, XP, and YP, each
C of which is dimensioned 129x97.  For each index pair "I,J" on a 129x97
C grid, ICLR(I,J) is a color index associated with the point having user
C coordinates XP(I,J) and YP(I,J); the color indices presumably imply a
C mapping of a physical quantity into a palette of colors.  We want to
C use the color indices to paint a picture of the grid in user space; we
C do this in two different ways: 1) by using the GKS routine GFA to fill
C areas; and 2) by using the GKS routine GCA to fill a cell array.  The
C first of these is straightforward, but the second is not; to do it, we
C have to program the inverse of the mapping from grid coordinates to
C user coordinates that is defined by the contents of the arrays XP and
C YP and this is difficult to do efficiently, because it is difficult,
C given a point in user space, to determine which of the mapped grid
C boxes contains that point.  The inverse mapping is done by the routine
C CPMPXY, below.
C
C The key to doing the inverse mapping efficiently is to use an index
C array, called INDX.  The following PARAMETER statement specifies two
C of the dimensions of this array, MIND and NIND (the third dimension
C is a 4).  Using larger values of MIND and/or NIND results in faster
C execution and a larger INDX array, while using smaller values results
C in slower execution and a smaller INDX array.  Note that, if this
C PARAMETER statement is changed, another one in the mapping routine
C CPMPXY must be changed to match it.
C
        PARAMETER (MIND=25,NIND=25)
C
C The parameters MCRA and NCRA declare the dimensions of the cell array,
C ICRA, to be used.  Using larger values of MCRA and NCRA give a better
C picture, but increase execution time and metafile size.  Using smaller
C values give smaller execution times and smaller metafiles.  It is up
C to the user to determine whether the picture given by a particular
C pair of values is acceptable or not.
C
C       PARAMETER (MCRA=800,NCRA=640)
        PARAMETER (MCRA=400,NCRA=320)
C       PARAMETER (MCRA=200,NCRA=160)
C
C The parameters LRWK and LIWK define the lengths of the workspace
C arrays to be passed to CONPACK.
C
        PARAMETER (LRWK=2000,LIWK=1000)
C
C Declare stuff that has to be passed to the mapping routine CPMPXY:
C this includes the arrays XP and YP; XPMN, XPMX, YPMN, and YPMX, in
C which are placed the min/max values of XP/YP; IBEG, IEND, JBEG, and
C JEND, which specify the grid box in which CPMPXY determined that the
C last point to be inverse-transformed lay; the index array INDX; and
C a flag, ISOD, saying whether to use single or double precision
C arithmetic.
C
        COMMON /TESTCM/ XP(129,97),YP(129,97),XPMN,XPMX,YPMN,YPMX,
     +                  IBEG,IEND,JBEG,JEND,INDX(MIND,NIND,4),ISOD
C
C The array ICLR holds the color indices described above.  The array
C RCLR, which is the same size and occupies the same space, but is of
C type REAL, will be used to produce a fourth frame, showing contours.
C
        DIMENSION ICLR(129,97),RCLR(129,97)
        EQUIVALENCE (ICLR,RCLR)
C
C The array ICRA is the cell array.
C
        DIMENSION ICRA(MCRA,NCRA)
C
C The arrays XTMP and YTMP are used to hold the coordinates defining
C polygons in calls to GFA, below.
C
        DIMENSION XTMP(4),YTMP(4)
C
C The arrays RWRK and IWRK are workspace arrays for CONPACK.
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C INITIALIZE GKS AND SET INTERNAL PARAMETERS IN UTILITIES. -------------
C
C Open GKS.
C
        CALL GOPKS (IERRF,ISZDM)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn clipping by GKS off.
C
        CALL GSCLIP (0)
C
C Define some basic colors to use (0 = black, for the background;
C 1 = white, for the foreground; and 2 = light red).
C
        CALL GSCR (IWKID,0,0.,0.,0.)
        CALL GSCR (IWKID,1,1.,1.,1.)
        CALL GSCR (IWKID,2,1.,.4,.4)
C
C Tell PLOTCHAR to use font number 25 (a filled font) and to outline
C each character.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
C
C Tell PLOTCHAR to tell the Bezier package to reproduce the curves
C outlining the characters with a little less fidelity.  This cuts
C down on the size of the metafile.
C
        CALL PCSETR ('FB - FIDELITY OF BEZIER CURVES',.00015)
C
C READ INPUT FILE. -----------------------------------------------------
C
C Open an input file created by running the user's program, with some
C added WRITE statements, on the Cray.
C
        PRINT * , ' '
        PRINT * , 'READING INPUT DATA FILE'
C
        OPEN (UNIT=29,FILE='cpex14.dat',STATUS='OLD',FORM='FORMATTED')
C
C Read argument values and call SET to duplicate the user's mapping of
C user coordinates into the plotter frame.
C
        READ (29,'(4F10.0)') XVPL,XVPR,YVPB,YVPT
        READ (29,'(4F10.0)') XWDL,XWDR,YWDB,YWDT
        READ (29,'(I10)') LNLG
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
        PRINT * , '  VIEWPORT LEFT EDGE:  ',XVPL
        PRINT * , '  VIEWPORT RIGHT EDGE: ',XVPR
        PRINT * , '  VIEWPORT BOTTOM EDGE:',YVPB
        PRINT * , '  VIEWPORT TOP EDGE:   ',YVPT
        PRINT * , '  WINDOW LEFT EDGE:    ',XWDL
        PRINT * , '  WINDOW RIGHT EDGE:   ',XWDR
        PRINT * , '  WINDOW BOTTOM EDGE:  ',YWDB
        PRINT * , '  WINDOW TOP EDGE:     ',YWDT
C
C Read color-index information and do the calls necessary to duplicate
C the user's palette of colors.  To avoid redefining color indices 1 and
C 2, we define color indices 11-138, rather than 1-128.
C
        DO 101 I=1,128
          READ (29,'(I10,3F10.0)') ICNT,REDV,GRNV,BLUV
          CALL GSCR (IWKID,ICNT+10,REDV,GRNV,BLUV)
  101   CONTINUE
C
C Read the arrays XP, YP, and ICLR, as described above.
C
        READ (29,'(8F10.0)') XP
        READ (29,'(8F10.0)') YP
        READ (29,'(8I10)') ICLR
C
C Close the input unit.
C
        CLOSE (UNIT=29)
C
C INITIALIZE STUFF NEEDED BY CPMPXY. -----------------------------------
C
C First, find the minimum and maximum values in the XP and YP arrays.
C
        XPMN=XP(1,1)
        XPMX=XP(1,1)
        YPMN=YP(1,1)
        YPMX=YP(1,1)
C
        DO 103 IP=1,129
          DO 102 JP=1,97
            XPMN=MIN(XPMN,XP(IP,JP))
            XPMX=MAX(XPMX,XP(IP,JP))
            YPMN=MIN(YPMN,YP(IP,JP))
            YPMX=MAX(YPMX,YP(IP,JP))
  102     CONTINUE
  103   CONTINUE
C
        PRINT * , '  MINIMUM X COORDINATE:',XPMN
        PRINT * , '  MAXIMUM X COORDINATE:',XPMX
        PRINT * , '  MINIMUM Y COORDINATE:',YPMN
        PRINT * , '  MAXIMUM Y COORDINATE:',YPMX
C
C Initialize the indices that CPMPXY uses to search for the box that a
C point to be inverse-transformed lies in.
C
        IBEG=1
        IEND=129
        JBEG=1
        JEND=97
C
C Fill the array INDX.  Each combination of indices "IN,JN", where IN
C is between 1 and MIND and JN is between 1 and NIND, inclusive, is
C associated with a particular rectangular portion of the user system,
C (call it R(IN,JN) - itself a subrectangle of the rectangle bounded
C by the lines X=XPMN, X=XPMX, Y=YPMN, and Y=YPMX); INDX(IN,JN,1) and
C INDX(IN,JN,2) specify a range of values of IP and INDX(IN,JN,3) and
C INDX(IN,JN,4) specify a range of values of JP such that, if a point
C is in R(IN,JN), then it is either in the union of the mapped grid
C boxes for values of IP and JP in those ranges or it is not in the
C mapped grid at all.  Thus, to find the mapped grid box containing a
C particular point, we first find out what R(IN,JN) it's in and then
C examine all the grid boxes specified by the entries in INDX(IN,JN,.).
C
C The initial values are such as to specify an illegal portion of the
C grid.  (The specified ranges of IP and JP are in the wrong order.)
C
        DO 105 IN=1,MIND
          DO 104 JN=1,NIND
            INDX(IN,JN,1)=129
            INDX(IN,JN,2)=1
            INDX(IN,JN,3)=97
            INDX(IN,JN,4)=1
  104     CONTINUE
  105   CONTINUE
C
C Examine each mapped grid box and use its bounding box to update the
C contents of INDX.
C
        DO 109 IP=1,128
          DO 108 JP=1,96
            XMIN=MIN(XP(IP,JP),XP(IP,JP+1),XP(IP+1,JP),XP(IP+1,JP+1))
            XMAX=MAX(XP(IP,JP),XP(IP,JP+1),XP(IP+1,JP),XP(IP+1,JP+1))
            YMIN=MIN(YP(IP,JP),YP(IP,JP+1),YP(IP+1,JP),YP(IP+1,JP+1))
            YMAX=MAX(YP(IP,JP),YP(IP,JP+1),YP(IP+1,JP),YP(IP+1,JP+1))
            INMN=MAX(1,MIN(MIND,1+INT(((XMIN-XPMN)/
     +                                 (XPMX-XPMN))*REAL(MIND))))
            INMX=MAX(1,MIN(MIND,1+INT(((XMAX-XPMN)/
     +                                 (XPMX-XPMN))*REAL(MIND))))
            JNMN=MAX(1,MIN(NIND,1+INT(((YMIN-YPMN)/
     +                                 (YPMX-YPMN))*REAL(NIND))))
            JNMX=MAX(1,MIN(NIND,1+INT(((YMAX-YPMN)/
     +                                 (YPMX-YPMN))*REAL(NIND))))
            DO 107 IN=INMN,INMX
              DO 106 JN=JNMN,JNMX
                INDX(IN,JN,1)=MIN(INDX(IN,JN,1),IP)
                INDX(IN,JN,2)=MAX(INDX(IN,JN,2),IP+1)
                INDX(IN,JN,3)=MIN(INDX(IN,JN,3),JP)
                INDX(IN,JN,4)=MAX(INDX(IN,JN,4),JP+1)
  106         CONTINUE
  107       CONTINUE
  108     CONTINUE
  109   CONTINUE
C
C Set the flag ISOD, which says whether to use single precision or
C double precision arithmetic.  We do this by calling a subroutine to
C generate and return the three real numbers 1, 1+1E-10, and 1+2E-10.
C If all three of those numbers appear to be different, we can use
C single precision arithmetic and we set ISOD = 0; otherwise, we should
C use double precision arithmetic, so we set ISOD = 1.  (The subroutine
C call is necessary to fool some compilers into storing TST1, TST2, and
C TST3; otherwise, real precision may be used on machines on which
C double precision is actually necessary.)
C
      PRINT * , ' '
C
      CALL GETTRN (TST1,TST2,TST3)
C
      IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
        ISOD=0
        PRINT * , 'USING SINGLE PRECISION ARITHMETIC'
      ELSE
        ISOD=1
        PRINT * , 'USING DOUBLE PRECISION ARITHMETIC'
      END IF
C
C TEST CPMPXY TO SEE IF IT'S WORKING ALL RIGHT. ------------------------
C
C First, just do a single point and print out all values involved.
C
        PRINT * , ' '
        PRINT * , 'SIMPLE TEST:'
        XGRD=13.56
        YGRD=17.29
C
        CALL CPMPXY ( 3,XGRD,YGRD,XUSR,YUSR)
        PRINT * , '  FORWARD TRANSFORM:'
        PRINT * , '    XGRD = ',XGRD
        PRINT * , '    YGRD = ',YGRD
        PRINT * , '    XUSR = ',XUSR
        PRINT * , '    YUSR = ',YUSR
C
        CALL CPMPXY (-3,XUSR,YUSR,XGRD,YGRD)
        PRINT * , '  REVERSE TRANSFORM:'
        PRINT * , '    XUSR = ',XUSR
        PRINT * , '    YUSR = ',YUSR
        PRINT * , '    XGRD = ',XGRD
        PRINT * , '    YGRD = ',YGRD
C
C Next, do a bunch of points inside a single grid box and see what
C the largest error looks like.
C
        PRINT * , ' '
        PRINT * , 'MORE COMPLICATED TEST:'
C
        EAVG=0.
        EMAX=0.
C
        DO 111 I=1,101
          XGRD=13.+REAL(I-1)/100.
          DO 110 J=1,101
            YGRD=17.+REAL(J-1)/100.
            CALL CPMPXY ( 3,XGRD,YGRD,XUSR,YUSR)
            CALL CPMPXY (-3,XUSR,YUSR,XGBK,YGBK)
            EROR=SQRT((XGBK-XGRD)**2+(YGBK-YGRD)**2)
            EAVG=EAVG+EROR
            EMAX=MAX(EMAX,EROR)
  110     CONTINUE
  111   CONTINUE
C
        EAVG=EAVG/10201.
C
        PRINT * , '  EAVG = ',EAVG
        PRINT * , '  EMAX = ',EMAX
C
C FRAME 1:  DRAW THE TRANSFORMED GRID. ---------------------------------
C
C Label the frame.
C
        PRINT * , ' '
        PRINT * , 'GENERATING FRAME 1, SHOWING THE MAPPED GRID'
C
        CALL PLCHHQ (CFUX(.50),CFUY(.98),'ARBITRARILY TRANSFORMED GRID',
     +                                                       .015,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.950),'X and Y coordinate arrays, ea
     +ch dimensioned 129x97, specify the shape of this irregular grid.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.926),'In the next two frames, we wi
     +ll show two different ways to fill this grid with color.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.062),'For more information, see the
     + comments in the code for this example.',
     +                                                       .012,0.,0.)
C
C Draw the transformed vertical lines of the grid.
C
        DO 113 I=1,129
          CALL FRSTPT (XP(I,1),YP(I,1))
          DO 112 J=2,97
            CALL VECTOR (XP(I,J),YP(I,J))
  112     CONTINUE
  113   CONTINUE
C
C Draw the transformed horizontal lines of the grid.
C
        DO 115 J=1,97
          CALL FRSTPT (XP(1,J),YP(1,J))
          DO 114 I=2,129
            CALL VECTOR (XP(I,J),YP(I,J))
  114     CONTINUE
  115   CONTINUE
C
C Dump out anything that's left in the buffer.
C
        CALL PLOTIF (0.,0.,2)
C
C Advance the frame.
C
        CALL FRAME
C
C FRAME 2:  CALL GFA TO DRAW A PICTURE OF THE GRID. --------------------
C
C Label the frame.
C
        PRINT * , ' '
        PRINT * , 'GENERATING FRAME 2, USING FILL AREAS'
C
        CALL PLCHHQ (CFUX(.50),CFUY(.98),
     +               'FILLED AREAS EXACTLY COVERING GRID',
     +                                         .015,0.,0.)
C
        CALL PLCHHQ (CFUX(.50),CFUY(.950),'Here we use the routine GFA t
     +o fill the grid, using color indices from a 129x97 array.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.926),'Note that the total filled ar
     +ea (outlined in red) is exactly the area occupied by the grid.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.086),'This frame''s metafile contai
     +ns 375,840 bytes; a cell-array version (next) contains 306,720 byt
     +es.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.062),'For more information, see the
     + comments in the code for this example.',
     +                                                       .012,0.,0.)
C
C Do the GFA calls.  Note that, because the color specified by ICLR(I,J)
C is thought of as being associated with the point (XP(I,J),YP(I,J)), we
C fill boxes centered at the grid points (except along the edges), which
C is a little different from filling the grid boxes themselves.  Note
C also that we add 10 to each color index, because that's how they were
C defined (in order to avoid redefining color indices 1 and 2).
C
        DO 117 I=1,129
          DO 116 J=1,97
            CALL CPMPXY (3,MAX(1.,MIN(129.,REAL(I)-.5)),
     +                     MAX(1.,MIN( 97.,REAL(J)-.5)),XTMP(1),YTMP(1))
            CALL CPMPXY (3,MAX(1.,MIN(129.,REAL(I)+.5)),
     +                     MAX(1.,MIN( 97.,REAL(J)-.5)),XTMP(2),YTMP(2))
            CALL CPMPXY (3,MAX(1.,MIN(129.,REAL(I)+.5)),
     +                     MAX(1.,MIN( 97.,REAL(J)+.5)),XTMP(3),YTMP(3))
            CALL CPMPXY (3,MAX(1.,MIN(129.,REAL(I)-.5)),
     +                     MAX(1.,MIN( 97.,REAL(J)+.5)),XTMP(4),YTMP(4))
            CALL GSFACI (ICLR(I,J)+10)
            CALL GFA    (4,XTMP,YTMP)
  116     CONTINUE
  117   CONTINUE
C
C In red, outline the mapped grid.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (2.)
        CALL GSPLCI (2)
        CALL FRSTPT (XP(1,1),YP(1,1))
C
        DO 118 I=2,129
          CALL VECTOR (XP(I,1),YP(I,1))
  118   CONTINUE
C
        DO 119 J=2,97
          CALL VECTOR (XP(129,J),YP(129,J))
  119   CONTINUE
C
        DO 120 I=128,1,-1
          CALL VECTOR (XP(I,97),YP(I,97))
  120   CONTINUE
C
        DO 121 J=97,1,-1
          CALL VECTOR (XP(1,J),YP(1,J))
  121   CONTINUE
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (1.)
        CALL GSPLCI (1)
C
C Advance the frame.
C
        CALL FRAME
C
C FRAME 3:  USE GCA TO DRAW A PICTURE OF THE GRID. ---------------------
C
C Label the frame.
C
        PRINT * , ' '
        PRINT * , 'GENERATING FRAME 3, USING A CELL ARRAY'
C
        CALL PLCHHQ (CFUX(.50),CFUY(.98),
     +               'CELL ARRAY FILLING BOUNDING BOX OF GRID',
     +                                              .015,0.,0.)
C
        CALL PLCHHQ (CFUX(.50),CFUY(.950),'Here we use the routine GCA t
     +o fill the bounding box of the grid (the area outlined in red).',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.926),'To do this, we need a routine
     + that does the inverse mapping, from user coordinates to grid coor
     +dinates.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.902),'Such a routine must be constr
     +ucted carefully in order to run in less than geologic time.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.110),'Note that the cell array used
     + here could have been built using the CONPACK routine CPCICA.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.086),'This frame''s metafile contai
     +ns 306,720 bytes; the fill-area version (last) contained 375,840 b
     +ytes.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.062),'For more information, see the
     + comments in the code for this example.',
     +                                                       .012,0.,0.)
C
C We generate a cell array covering the area occupied by the bounding
C box of the transformed grid and dump it out with a single call to GCA.
C Note that we could do this using the CONPACK routine CPCICA; we do it
C this way instead for the sake of clarity and because our data is
C already an integer array of color indices.
C
C At each box in a rectangular grid of boxes, we must figure out where
C that box is relative to the transformed data grid, determine a color
C index for the box, and stuff it into the cell array.
C
        DO 123 I=1,MCRA
          IBEG=1
          IEND=129
          JBEG=1
          JEND=97
          XUSR=XPMN+(XPMX-XPMN)*((REAL(I)-.5)/REAL(MCRA))
          DO 122 J=1,NCRA
            YUSR=YPMN+(YPMX-YPMN)*((REAL(J)-.5)/REAL(NCRA))
            CALL CPMPXY (-3,XUSR,YUSR,XDAT,YDAT)
            IF (XDAT.GT..5.AND.XDAT.LT.129.5.AND.
     +          YDAT.GT..5.AND.YDAT.LT. 97.5) THEN
              ICRA(I,J)=ICLR(NINT(XDAT),NINT(YDAT))+10
            ELSE
              ICRA(I,J)=0
            END IF
  122     CONTINUE
  123   CONTINUE
C
C Dump out the cell array.
C
        CALL GCA (XPMN,YPMN,XPMX,YPMX,MCRA,NCRA,1,1,MCRA,NCRA,ICRA)
C
C In red, outline the bounding box.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (2.)
        CALL GSPLCI (2)
        CALL FRSTPT (XPMN,YPMN)
        CALL VECTOR (XPMX,YPMN)
        CALL VECTOR (XPMX,YPMX)
        CALL VECTOR (XPMN,YPMX)
        CALL VECTOR (XPMN,YPMN)
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (1.)
        CALL GSPLCI (1)
C
C Advance the frame.
C
        CALL FRAME
C
C FRAME 4:  USE CONPACK TO PUT CONTOURS OVER THE CELL ARRAY. -----------
C
C Label the frame.
C
        PRINT * , ' '
        PRINT * , 'GENERATING FRAME 4, A CELL ARRAY PLUS CONTOURS'
C
        CALL PLCHHQ (CFUX(.50),CFUY(.98),
     +               'CELL ARRAY IN BOUNDING BOX, PLUS CONTOURS',
     +                                                .015,0.,0.)
C
        CALL PLCHHQ (CFUX(.50),CFUY(.950),'This frame is just like the l
     +ast one, except that we use CONPACK to contour the data field.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.926),'The contours are drawn in bla
     +ck over the filled area produced by the call to GCA.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.902),'What is contoured is just the
     + field of color indices, and the label values reflect this.',
     +                                                       .012,0.,0.)
        CALL PLCHHQ (CFUX(.50),CFUY(.062),'For more information, see the
     + comments in the code for this example.',
     +                                                       .012,0.,0.)
C
C Dump out the cell array again.
C
        CALL GCA (XPMN,YPMN,XPMX,YPMX,MCRA,NCRA,1,1,MCRA,NCRA,ICRA)
C
C In red, outline the bounding box.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (2.)
        CALL GSPLCI (2)
        CALL FRSTPT (XPMN,YPMN)
        CALL VECTOR (XPMX,YPMN)
        CALL VECTOR (XPMX,YPMX)
        CALL VECTOR (XPMN,YPMX)
        CALL VECTOR (XPMN,YPMN)
        CALL PLOTIF (0.,0.,2)
        CALL GSLWSC (1.)
        CALL GSPLCI (1)
C
C Move the integer data from ICLR to the real array RCLR (for delivery
C to CONPACK).  Note that the real data overlay the integer data in
C memory, so that ICLR becomes undefined.
C
        DO 125 I=1,129
          DO 124 J=1,97
            RCLR(I,J)=ICLR(I,J)
  124     CONTINUE
  125   CONTINUE
C
C Tell CONPACK not to do its own set call.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK which mapping to use and what the out-of-range value
C is for that mapping.
C
        CALL CPSETI ('MAP - MAPPING FLAG',3)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Tell CONPACK to use DASHPACK.
C
        CALL CPSETI ('DPU - DASH PATTERN USE FLAG',-3)
C
C Tell CONPACK to use twenty-five contour levels: 5, 10, 15, ... 125.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',25)
C
        DO 126 I=1,25
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
          CALL CPSETI ('CLV - CONTOUR LEVEL',5*I)
          IF (MOD(I,2).EQ.0) THEN
            CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',3)
            CALL CPSETI ('CLL - CONTOUR LEVEL USE FLAG',2)
          ELSE
            CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
            CALL CPSETI ('CLL - CONTOUR LEVEL USE FLAG',0)
          END IF
          CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',0)
  126   CONTINUE
C
C Initialize CONPACK, telling it where the data array and the two work
C arrays are.
C
        CALL CPRECT (RCLR,129,129,97,RWRK,LRWK,IWRK,LIWK)
C
C Draw the contour lines; the calls to PCSETI make the labels black.
C
        CALL PCSETI ('CC',0)
        CALL PCSETI ('OC',0)
        CALL CPCLDR (RCLR,RWRK,IWRK)
        CALL PCSETI ('CC',-1)
        CALL PCSETI ('OC',-1)
C
C Advance the frame.
C
        CALL FRAME
C
C DONE.  CLOSE GKS AND QUIT. -------------------------------------------
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
        PRINT * , ' '
C
C Done.
C
        STOP
C
      END


      SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOUT,YOUT)
C
C This version of CPMPXY implements what has sometimes, in the past,
C been called a "parameterized distortion".  If IMAP has the value 3,
C then, for given input values of XINP and YINP, where XINP lies in
C the interval (1.,129.) and YINP lies in the interval (1.,97.),
C CPMPXY generates output values of XOUT and YOUT, representing the
C transformed position of a point on a 129x97 grid of data.
C
C The arrays XP and YP are passed in from the main program.  Each
C (XP(I,J),YP(I,J)) represents a position associated with the grid
C point (I,J).  XPMN, XPMX, YPMN, and YPMX are the computed minimum
C and maximum values in XP and YP.  The variables IBEG, IEND, JBEG,
C and JEND keep track of what box the last point inverse-transformed
C turned out to be in.  The array INDX is used to determine quickly
C approximately what part of the mapped grid contains a given point.
C ISOD is a flag that says whether to use single-precision or
C double-precision arithmetic.  The calling program is responsible
C for initializing all the values in this array properly.
C
        PARAMETER (MIND=25,NIND=25)
C
        COMMON /TESTCM/ XP(129,97),YP(129,97),XPMN,XPMX,YPMN,YPMX,
     +                  IBEG,IEND,JBEG,JEND,INDX(MIND,NIND,4),ISOD
C
C Declare some required double precision variables.
C
        DOUBLE PRECISION DDIR,DDOS,DEPS,DFOS,DLST,DS,DS1,DS2,DT,
     +                   DX1,DX2,DX3,DXTM,DY1,DY2,DY3,DYTM
C
C Set the value of "epsilons" used to test for convergence of a
C Newton iteration and/or binary search loop below.
C
        DATA SEPS,DEPS / 1.E-8 , 1.D-8 /
C
C The following arithmetic statement functions have a negative value
C if and only if the point (X3,Y3) is to the left of the line from
C (X1,Y1) to (X2,Y2).
C
        SDIR(X1,Y1,X2,Y2,X3,Y3)=(X1-X3)*(Y3-Y2)-(X2-X3)*(Y3-Y1)
C
        DDIR(DX1,DY1,DX2,DY2,DX3,DY3)=(DX1-DX3)*(DY3-DY2)-
     +                                (DX2-DX3)*(DY3-DY1)
C
C Do it.  If IMAP = 0, the caller wants information about the mapping
C specified by INT(XINP).  Returning YINP = 3 says that both forward
C and inverse mappings are implemented, while returning YINP = 0 says
C that neither is.
C
        IF (IMAP.EQ.0) THEN
C
          IF (INT(XINP).EQ.3) THEN
            YINP=3.
          ELSE
            YINP=0.
          END IF
C
C If IMAP = 3, a forward "parameterized distortion" mapping is
C requested.  The input value XINP is expected to be in the range
C [1.,129.] and the input value YINP is expected to be in the range
C [1.,97.].  If XINP and YINP are integers I and J, respectively,
C then XOUT and YOUT should be XP(I,J) and YP(I,J), respectively;
C if XINP and YINP are not integers, linear interpolation is done
C to get the appropriate values of XOUT and YOUT.
C
        ELSE IF (IMAP.EQ.3) THEN
C
C IGRD and JGRD are the indices of the lower left corner of the grid
C box in which the input point falls.
C
          IGRD=MAX(1,MIN(128,INT(XINP)))
          JGRD=MAX(1,MIN( 96,INT(YINP)))
C
C We use either single precision or double precision arithmetic, as
C appropriate.
C
          IF (ISOD.EQ.0) THEN
C
            XOUT= XP(IGRD  ,JGRD  )+
     +           (XP(IGRD+1,JGRD  )-XP(IGRD  ,JGRD  ))*
     +                               (XINP-REAL(IGRD))+
     +           (XP(IGRD  ,JGRD+1)-XP(IGRD  ,JGRD  ))*
     +                               (YINP-REAL(JGRD))+
     +           (XP(IGRD+1,JGRD+1)-XP(IGRD  ,JGRD+1)-
     +            XP(IGRD+1,JGRD  )+XP(IGRD  ,JGRD  ))*
     +                               (XINP-REAL(IGRD))*
     +                               (YINP-REAL(JGRD))
C
            YOUT= YP(IGRD  ,JGRD  )+
     +           (YP(IGRD+1,JGRD  )-YP(IGRD  ,JGRD  ))*
     +                               (XINP-REAL(IGRD))+
     +           (YP(IGRD  ,JGRD+1)-YP(IGRD  ,JGRD  ))*
     +                               (YINP-REAL(JGRD))+
     +           (YP(IGRD+1,JGRD+1)-YP(IGRD  ,JGRD+1)-
     +            YP(IGRD+1,JGRD  )+YP(IGRD  ,JGRD  ))*
     +                               (XINP-REAL(IGRD))*
     +                               (YINP-REAL(JGRD))
C
          ELSE
C
            XOUT=REAL(DBLE(XP(IGRD  ,JGRD  ))+
     +                DBLE(XP(IGRD+1,JGRD  )-XP(IGRD  ,JGRD  ))*
     +                                    DBLE(XINP-REAL(IGRD))+
     +                DBLE(XP(IGRD  ,JGRD+1)-XP(IGRD  ,JGRD  ))*
     +                                    DBLE(YINP-REAL(JGRD))+
     +                DBLE(XP(IGRD+1,JGRD+1)-XP(IGRD  ,JGRD+1)-
     +                     XP(IGRD+1,JGRD  )+XP(IGRD  ,JGRD  ))*
     +                                    DBLE(XINP-REAL(IGRD))*
     +                                    DBLE(YINP-REAL(JGRD)))
C
            YOUT=REAL(DBLE(YP(IGRD  ,JGRD  ))+
     +                DBLE(YP(IGRD+1,JGRD  )-YP(IGRD  ,JGRD  ))*
     +                                    DBLE(XINP-REAL(IGRD))+
     +                DBLE(YP(IGRD  ,JGRD+1)-YP(IGRD  ,JGRD  ))*
     +                                    DBLE(YINP-REAL(JGRD))+
     +                DBLE(YP(IGRD+1,JGRD+1)-YP(IGRD  ,JGRD+1)-
     +                     YP(IGRD+1,JGRD  )+YP(IGRD  ,JGRD  ))*
     +                                    DBLE(XINP-REAL(IGRD))*
     +                                    DBLE(YINP-REAL(JGRD)))
C
          END IF
C
C If IMAP = -3, the inverse of the "parameterized distortion" is
C requested.  This is somewhat difficult.  We first have to find out
C what mapped grid box (XINP,YINP) is in.
C
        ELSE IF (IMAP.EQ.-3) THEN
C
C Jump if (XINP,YINP) is in the same mapped grid box as the last point
C was.
C
          IF (IEND.EQ.IBEG+1.AND.JEND.EQ.JBEG+1) THEN
            IF (ISINPO(XINP,YINP,XP,YP,129,
     +                 IBEG,IEND,JBEG,JEND,ISOD).NE.0) GO TO 102
          END IF
C
C It isn't; retrieve starting values of IBEG, IEND, JBEG, and JEND from
C the index array INDX.
C
          IN=MAX(1,MIN(MIND,1+INT(((XINP-XPMN)/
     +                             (XPMX-XPMN))*REAL(MIND))))
          JN=MAX(1,MIN(NIND,1+INT(((YINP-YPMN)/
     +                             (YPMX-YPMN))*REAL(NIND))))
C
          IBEG=INDX(IN,JN,1)
          IEND=INDX(IN,JN,2)
          JBEG=INDX(IN,JN,3)
          JEND=INDX(IN,JN,4)
C
C If the indices imply an illegal portion of the mapped grid, return
C out-of-range values.
C
          IF (IEND.LE.IBEG.OR.JEND.LE.JBEG) THEN
            XOUT=1.E12
            YOUT=1.E12
            RETURN
          END IF
C
C If the point is outside the specified portion of the mapped grid,
C return out-of-range values.
C
          IF (ISINPO(XINP,YINP,XP,YP,129,
     +               IBEG,IEND,JBEG,JEND,ISOD).EQ.0) THEN
            XOUT=1.E12
            YOUT=1.E12
            RETURN
          END IF
C
C Zoom in until we find a single mapped grid box containing (XINP,YINP).
C
  101     IF (IEND.NE.IBEG+1.OR.JEND.NE.JBEG+1) THEN
            IF (IEND-IBEG.GT.JEND-JBEG) THEN
              ITRY=(IBEG+IEND)/2
              IF (ISINPO(XINP,YINP,XP,YP,129,
     +                   IBEG,ITRY,JBEG,JEND,ISOD).EQ.0) THEN
                IBEG=ITRY
              ELSE
                IEND=ITRY
              END IF
            ELSE
              JTRY=(JBEG+JEND)/2
              IF (ISINPO(XINP,YINP,XP,YP,129,
     +                   IBEG,IEND,JBEG,JTRY,ISOD).EQ.0) THEN
                JBEG=JTRY
              ELSE
                JEND=JTRY
              END IF
            END IF
            GO TO 101
          END IF
C
C Compute fractional coordinates within the grid box.  This is done with
C an iterative procedure, rather than an algebraic solution, because of
C the surprising number of cases that the latter solution gives rise to,
C many of them seemingly pathological.  We first attempt to use Newton's
C rule to come up with a solution and, if that doesn't work, we use a
C binary search technique.
C
  102     IF (ISOD.EQ.0) THEN
C
            SS=.5
C
            NTRY=0
C
  103       NTRY=NTRY+1
            IF (NTRY.GT.10) GO TO 104
            SFOS=SDIR((1.-SS)*XP(IBEG,JBEG+1)+SS*XP(IBEG+1,JBEG+1),
     +                (1.-SS)*YP(IBEG,JBEG+1)+SS*YP(IBEG+1,JBEG+1),
     +                (1.-SS)*XP(IBEG,JBEG  )+SS*XP(IBEG+1,JBEG  ),
     +                (1.-SS)*YP(IBEG,JBEG  )+SS*YP(IBEG+1,JBEG  ),
     +                XINP                                        ,
     +                YINP                                        )
            SDOS=2.*((XP(IBEG+1,JBEG+1)-XP(IBEG  ,JBEG+1))*
     +               (YP(IBEG  ,JBEG  )-YP(IBEG+1,JBEG  ))-
     +               (XP(IBEG+1,JBEG  )-XP(IBEG  ,JBEG  ))*
     +               (YP(IBEG  ,JBEG+1)-YP(IBEG+1,JBEG+1)))*SS+
     +               (XP(IBEG  ,JBEG+1)-XINP             )*
     +               (YP(IBEG  ,JBEG  )-YP(IBEG+1,JBEG  ))+
     +               (XP(IBEG+1,JBEG+1)-XP(IBEG  ,JBEG+1))*
     +               (YINP             -YP(IBEG  ,JBEG  ))-
     +               (XP(IBEG  ,JBEG  )-XINP             )*
     +               (YP(IBEG  ,JBEG+1)-YP(IBEG+1,JBEG+1))-
     +               (XP(IBEG+1,JBEG  )-XP(IBEG  ,JBEG  ))*
     +               (YINP             -YP(IBEG  ,JBEG+1))
            IF (SDOS.EQ.0.) GO TO 104
            SLST=SS
            SS=SS-SFOS/SDOS
            IF (ABS(SS-SLST).LT.SEPS) THEN
              IF (SS.LT.-SEPS.OR.SS.GT.1.+SEPS) GO TO 104
              GO TO 106
            END IF
            GO TO 103
C
  104       SS1=0.
            SS2=1.
C
  105       SS=.5*(SS1+SS2)
C
            IF (SDIR((1.-SS)*XP(IBEG,JBEG+1)+SS*XP(IBEG+1,JBEG+1),
     +               (1.-SS)*YP(IBEG,JBEG+1)+SS*YP(IBEG+1,JBEG+1),
     +               (1.-SS)*XP(IBEG,JBEG  )+SS*XP(IBEG+1,JBEG  ),
     +               (1.-SS)*YP(IBEG,JBEG  )+SS*YP(IBEG+1,JBEG  ),
     +               XINP                                        ,
     +               YINP                                        )
     +                                                      .LT.0.) THEN
              SS1=SS
            ELSE
              SS2=SS
            END IF
C
            IF (ABS(SS2-SS1).GT.SEPS) GO TO 105
C
  106       SXTM=(1.-SS)*(XP(IBEG  ,JBEG+1)-XP(IBEG  ,JBEG))+
     +               SS *(XP(IBEG+1,JBEG+1)-XP(IBEG+1,JBEG))
C
            SYTM=(1.-SS)*(YP(IBEG  ,JBEG+1)-YP(IBEG  ,JBEG))+
     +               SS *(YP(IBEG+1,JBEG+1)-YP(IBEG+1,JBEG))
C
            IF (ABS(SXTM).GT.ABS(SYTM)) THEN
              ST=(XINP-(1.-SS)*XP(IBEG,JBEG)-SS*XP(IBEG+1,JBEG))/SXTM
            ELSE
              ST=(YINP-(1.-SS)*YP(IBEG,JBEG)-SS*YP(IBEG+1,JBEG))/SYTM
            END IF
C
          ELSE
C
            DS=.5D0
C
            NTRY=0
C
  203       NTRY=NTRY+1
            IF (NTRY.GT.10) GO TO 204
            DFOS=DDIR((1.D0-DS)*DBLE(XP(IBEG  ,JBEG+1))+
     +                      DS *DBLE(XP(IBEG+1,JBEG+1)),
     +                (1.D0-DS)*DBLE(YP(IBEG  ,JBEG+1))+
     +                      DS *DBLE(YP(IBEG+1,JBEG+1)),
     +                (1.D0-DS)*DBLE(XP(IBEG  ,JBEG  ))+
     +                      DS *DBLE(XP(IBEG+1,JBEG  )),
     +                (1.D0-DS)*DBLE(YP(IBEG  ,JBEG  ))+
     +                      DS *DBLE(YP(IBEG+1,JBEG  )),
     +                          DBLE(XINP)             ,
     +                          DBLE(YINP)             )
            DDOS=2.D0*(DBLE(XP(IBEG+1,JBEG+1)-XP(IBEG  ,JBEG+1))*
     +                 DBLE(YP(IBEG  ,JBEG  )-YP(IBEG+1,JBEG  ))-
     +                 DBLE(XP(IBEG+1,JBEG  )-XP(IBEG  ,JBEG  ))*
     +                 DBLE(YP(IBEG  ,JBEG+1)-YP(IBEG+1,JBEG+1)))*DS+
     +                 DBLE(XP(IBEG  ,JBEG+1)-XINP             )*
     +                 DBLE(YP(IBEG  ,JBEG  )-YP(IBEG+1,JBEG  ))+
     +                 DBLE(XP(IBEG+1,JBEG+1)-XP(IBEG  ,JBEG+1))*
     +                 DBLE(YINP             -YP(IBEG  ,JBEG  ))-
     +                 DBLE(XP(IBEG  ,JBEG  )-XINP             )*
     +                 DBLE(YP(IBEG  ,JBEG+1)-YP(IBEG+1,JBEG+1))-
     +                 DBLE(XP(IBEG+1,JBEG  )-XP(IBEG  ,JBEG  ))*
     +                 DBLE(YINP             -YP(IBEG  ,JBEG+1))
            IF (DDOS.EQ.0.D0) GO TO 204
            DLST=DS
            DS=DS-DFOS/DDOS
            IF (ABS(DS-DLST).LT.DEPS) THEN
              IF (DS.LT.-DEPS.OR.DS.GT.1.D0+DEPS) GO TO 204
              GO TO 206
            END IF
            GO TO 203
C
  204       DS1=0.
            DS2=1.
C
  205       DS=.5D0*(DS1+DS2)
C
            IF (DDIR((1.D0-DS)*DBLE(XP(IBEG  ,JBEG+1))+
     +                     DS *DBLE(XP(IBEG+1,JBEG+1)),
     +               (1.D0-DS)*DBLE(YP(IBEG  ,JBEG+1))+
     +                     DS *DBLE(YP(IBEG+1,JBEG+1)),
     +               (1.D0-DS)*DBLE(XP(IBEG  ,JBEG  ))+
     +                     DS *DBLE(XP(IBEG+1,JBEG  )),
     +               (1.D0-DS)*DBLE(YP(IBEG  ,JBEG  ))+
     +                     DS *DBLE(YP(IBEG+1,JBEG  )),
     +                         DBLE(XINP)             ,
     +                         DBLE(YINP)             ).LT.0.D0) THEN
              DS1=DS
            ELSE
              DS2=DS
            END IF
C
            IF (ABS(DS2-DS1).GT.DEPS) GO TO 205
C
  206       DXTM=(1.D0-DS)*DBLE(XP(IBEG  ,JBEG+1)-XP(IBEG  ,JBEG))+
     +                 DS *DBLE(XP(IBEG+1,JBEG+1)-XP(IBEG+1,JBEG))
C
            DYTM=(1.D0-DS)*DBLE(YP(IBEG  ,JBEG+1)-YP(IBEG  ,JBEG))+
     +                 DS *DBLE(YP(IBEG+1,JBEG+1)-YP(IBEG+1,JBEG))
C
            IF (ABS(DXTM).GT.ABS(DYTM)) THEN
              DT=(DBLE(XINP)-(1.D0-DS)*DBLE(XP(IBEG,JBEG))-
     +                             DS* DBLE(XP(IBEG+1,JBEG)))/DXTM
            ELSE
              DT=(DBLE(YINP)-(1.D0-DS)*DBLE(YP(IBEG,JBEG))-
     +                             DS* DBLE(YP(IBEG+1,JBEG)))/DYTM
            END IF
C
            SS=REAL(DS)
            ST=REAL(DT)
C
          END IF
C
C Output values of X and Y are the fractional values added to the
C indices of the lower left corner of the grid box.
C
          XOUT=REAL(IBEG)+MAX(0.,MIN(1.,SS))
          YOUT=REAL(JBEG)+MAX(0.,MIN(1.,ST))
C
C If IMAP is anything else, log an error.  If error recovery is on,
C control will return to caller; otherwise, SETER will clobber the run.
C
        ELSE
C
          CALL SETER ('CPMPXY - UNKNOWN MAPPING',5,1)
C
        END IF
C
C Done.
C
        RETURN
C
      END


      FUNCTION ISINPO(XPNT,YPNT,XCRA,YCRA,IDIM,IBEG,IEND,JBEG,JEND,ISOD)
C
        DIMENSION XCRA(IDIM,*),YCRA(IDIM,*)
C
C Declare variables which are needed when double-precision arithmetic
C is executed.
C
        DOUBLE PRECISION DACH,DDIF,DLST,DNXT
C
C The value of this function is intended to be greater than zero if and
C only if the point (XPNT,YPNT) is inside the polygon defined by the
C points (XCRA(I,J),YCRA(I,J)), for "I,J" = "IBEG,JBEG", "IBEG+1,JBEG",
C ... "IEND,JBEG", "IEND,JBEG+1", ... "IEND,JEND", "IEND-1,JEND", ...
C "IBEG,JEND", "IBEG,JEND-1", ... "IBEG,JBEG".  If the function value
C is zero, the point is outside the polygon; if it is negative, some
C error has occurred.
C
C IDIM is the first dimension of each of the arrays XCRA and YCRA in
C the calling routine.
C
C IBEG, IEND, JBEG, and JEND are as described above.
C
C ISOD is set to zero if single-precision arithmetic is to be used;
C otherwise, double-precision arithmetic will be.
C
C We compute the total angular change described by a ray emanating from
C the point (XPNT,YPNT) and passing through a point that traces out the
C polygon.
C
        IF (ISOD.EQ.0) THEN
C
          ANCH=0.
C
          INXT=IBEG
          JNXT=JBEG
C
          IF (XCRA(IBEG,JBEG).EQ.XPNT.AND.YCRA(IBEG,JBEG).EQ.YPNT) THEN
            ISINPO=1
            RETURN
          ELSE
            ANXT=ATAN2(YCRA(IBEG,JBEG)-YPNT,XCRA(IBEG,JBEG)-XPNT)
          END IF
C
          DO 101 IPNT=2,1+IEND-IBEG+JEND-JBEG+IEND-IBEG+JEND-JBEG
            IF      (IPNT.LE.1+IEND-IBEG) THEN
              INXT=INXT+1
            ELSE IF (IPNT.LE.1+IEND-IBEG+JEND-JBEG) THEN
              JNXT=JNXT+1
            ELSE IF (IPNT.LE.1+IEND-IBEG+JEND-JBEG+IEND-IBEG) THEN
              INXT=INXT-1
            ELSE
              JNXT=JNXT-1
            END IF
            ALST=ANXT
            IF (XCRA(INXT,JNXT).EQ.XPNT.AND.YCRA(INXT,JNXT).EQ.YPNT)THEN
              ISINPO=1
              RETURN
            ELSE
              ANXT=ATAN2(YCRA(INXT,JNXT)-YPNT,XCRA(INXT,JNXT)-XPNT)
            END IF
            ADIF=ANXT-ALST
            IF (ABS(ADIF).GT.3.14159265358979)
     +                             ADIF=ADIF-SIGN(6.28318530717958,ADIF)
            ANCH=ANCH+ADIF
  101     CONTINUE
C
C If the point is outside the polygon, the total angular change should
C be exactly zero, while if the point is inside the polygon, the total
C angular change should be exactly plus or minus two pi.  We just test
C for the absolute value of the change being less than pi.
C
          IF (ABS(ANCH).LT.3.14159265358979) THEN
            ISINPO=0
          ELSE
            ISINPO=1
          END IF
C
        ELSE
C
          DACH=0.D0
C
          INXT=IBEG
          JNXT=JBEG
C
          IF (XCRA(IBEG,JBEG).EQ.XPNT.AND.YCRA(IBEG,JBEG).EQ.YPNT) THEN
            ISINPO=1
            RETURN
          ELSE
            DNXT=ATAN2(DBLE(YCRA(IBEG,JBEG)-YPNT),
     +                 DBLE(XCRA(IBEG,JBEG)-XPNT))
          END IF
C
          DO 102 IPNT=2,1+IEND-IBEG+JEND-JBEG+IEND-IBEG+JEND-JBEG
            IF      (IPNT.LE.1+IEND-IBEG) THEN
              INXT=INXT+1
            ELSE IF (IPNT.LE.1+IEND-IBEG+JEND-JBEG) THEN
              JNXT=JNXT+1
            ELSE IF (IPNT.LE.1+IEND-IBEG+JEND-JBEG+IEND-IBEG) THEN
              INXT=INXT-1
            ELSE
              JNXT=JNXT-1
            END IF
            DLST=DNXT
            IF (XCRA(INXT,JNXT).EQ.XPNT.AND.YCRA(INXT,JNXT).EQ.YPNT)THEN
              ISINPO=1
              RETURN
            ELSE
              DNXT=ATAN2(DBLE(YCRA(INXT,JNXT)-YPNT),
     +                   DBLE(XCRA(INXT,JNXT)-XPNT))
            END IF
            DDIF=DNXT-DLST
            IF (ABS(DDIF).GT.3.14159265358979D0)
     +                           DDIF=DDIF-SIGN(6.28318530717958D0,DDIF)
            DACH=DACH+DDIF
  102     CONTINUE
C
C If the point is outside the polygon, the total angular change should
C be exactly zero, while if the point is inside the polygon, the total
C angular change should be exactly plus or minus two pi.  We just test
C for the absolute value of the change being less than or equal to pi.
C
          IF (ABS(DACH).LT.3.14159265358979D0) THEN
            ISINPO=0
          ELSE
            ISINPO=1
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE GETTRN (TST1,TST2,TST3)
C
C This subroutine generates and returns the three real numbers 1,
C 1+1E-10, and 1+2E-10.  On a machine with 64-bit arithmetic, all
C three of these numbers will be different from each other, while,
C on machines with 32-bit arithmetic, two or more of them will be
C equal.  Thus, we can use these numbers to determine whether we
C should use single precision or double precision arithmetic.
C
        TST1=1.
        TST2=TST1+1.E-10
        TST3=TST2+1.E-10
C
        RETURN
C
      END
