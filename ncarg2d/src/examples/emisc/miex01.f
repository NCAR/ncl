
      PROGRAM MIEX01
C
C  Demonstrate the usage of the logo subroutines NGLOGO and NGEZLOGO.
C
C  Define the error file, the Fortran unit number, the workstation type,
C  and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C  Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Set up a color table.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,0.,0.,1.)
      CALL GSCR(IWKID,3,1.,0.,0.)
C
C  Main title.
C
      CALL PLCHHQ(0.5,0.90,":F26:Logos",0.05,0.,0.)
C
C  Change the Plotchar function code control character from
C  its default value of a colon, since we want to produce a colon
C  in calls to PLCHHQ.
C
      CALL PCSETC('FC','#')
C
C  Use Helvetica for the label font.
C
      CALL PCSETC('FN','HELVETICA')
C
C  Put out examples of the five logo types.
C
C   Type 1 - an NCAR logo that will be full-color for
C            PostScript output and single color otherwise.
C
      PSIZE = 0.04
      PXPOS = 0.23
      XLPOS = 0.67
      YLPOS = 0.75
      SIZEL = 0.10
      YINC  = 0.14
      CALL PLCHHQ(PXPOS, YLPOS, "Type 1:", PSIZE, 0.,-1.)
      CALL NGLOGO(IWKID, XLPOS, YLPOS, SIZEL, 1, 1, 1)
C
C   Type 2 - the UCAR star logo in red.
C
      YLPOS = YLPOS-YINC
      CALL PLCHHQ(PXPOS, YLPOS, "Type 2:", PSIZE, 0.,-1.)
      CALL NGLOGO(IWKID, XLPOS, YLPOS, SIZEL, 2, 3, 1)
C
C   Type 3 - the text string "NCAR" in Bell Gothic font.
C
      YLPOS = YLPOS-YINC
      CALL PLCHHQ(PXPOS, YLPOS, "Type 3:", PSIZE, 0.,-1.)
      CALL NGLOGO(IWKID, XLPOS, YLPOS, 0.6*SIZEL, 3, 1, 1)
C
C   Type 4 - the text string "UCAR" in Bell Gothic font.
C
      YLPOS = YLPOS-YINC
      CALL PLCHHQ(PXPOS, YLPOS, "Type 4:", PSIZE, 0.,-1.)
      CALL NGLOGO(IWKID, XLPOS, YLPOS, 0.6*SIZEL, 4, 1, 1)
C
C   Type 5 - the UCAR star logo in blue with the text "UCAR" in red.
C
      YLPOS = YLPOS-YINC
      CALL PLCHHQ(PXPOS, YLPOS, "Type 5:", PSIZE, 0.,-1.)
      CALL NGLOGO(IWKID, XLPOS-0.1, YLPOS, SIZEL, 5, 2, 3)
C
C  Put an NCAR logo at the lower right using NGEZLOGO.
C
      CALL NGEZLOGO()
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      STOP
      END
