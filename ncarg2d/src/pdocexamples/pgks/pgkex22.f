
      PROGRAM PGKEX22
C
C  Illustrate setting nominal linewidth.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1)
C
      DIMENSION X(2),Y(2),XP(4),YP(3)
      DIMENSION WSCALE(5),WIDTHN(3)
      CHARACTER*3 LLAB
C
C  Strategic horizontal and vertical positions.
C  
      DATA XP/ 0.12, 0.35, 0.60, 0.85/
      DATA YP/ 0.80, 0.73, .63/
C
C  Length of displayed lines.
C
      DATA XLEN/.18/
C
C  Linewidth scale factors.
C
      DATA WSCALE/ 0.5, 1.0, 2.0, 4.0, 8.0/
C
C  Nominal linewidths.
C
      DATA WIDTHN/ 0.5, 1.0, 4.0/
C
C  Open GKS.
C
      CALL GOPKS (IERRF, IDUM)
C
C  Open and activate a color PostScript workstation.
C
      CALL GOPWK (IWKID, 2, NGPSWK('PS','PORT','COLOR'))
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
C
C  All changes apply to workstation with id IWKID.
C
      CALL NGSETI('Workstation',IWKID)
C
C  Set line caps to "butt".
C
      CALL NGSETI('Caps',0)
C
      DO 10 NWIDTH=1,3
        X(1) = XP(NWIDTH+1)-0.5*XLEN
        X(2) = XP(NWIDTH+1)+0.5*XLEN
C
C  Set nominal linewidth.
C
        CALL NGSETR('Nominal linewidth',WIDTHN(NWIDTH))
        DO 20 ISCL=1,5
C
C  Set linewidth scale factor.
C
          CALL GSLWSC(WSCALE(ISCL))
          Y(1) = YP(3) - 0.1*REAL(ISCL-1)
          Y(2) = Y(1)
          CALL GPL(2,X,Y)
   20   CONTINUE
   10 CONTINUE
C
C  Label the plot.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',1)
      CALL PLCHHQ(XP(3),YP(1)-0.010,'Nominal linewidths',.030,0.,0.)
      CALL PLCHHQ(XP(2),YP(2)+0.002,'0.5',.023,0.,0.)
      CALL PLCHHQ(XP(3),YP(2)+0.002,'1.0 (default)',.023,0.,0.)
      CALL PLCHHQ(XP(4),YP(2)+0.002,'4.0',.023,0.,0.)
      CALL PLCHHQ(XP(1),YP(1),'Linewidth',.019,0.,0.)
      CALL PLCHHQ(XP(1),0.5*(YP(1)+YP(2)),'scale',.019,0.,0.)
      CALL PLCHHQ(XP(1),YP(2),'factors',.019,0.,0.)
      SF = .25
      DO 30 I=1,5
        SF = 2.*SF
        WRITE(LLAB,'(F3.1)') SF
        CALL PLCHHQ(XP(1),YP(3)-0.1*REAL(I-1),LLAB,.023,0.,0.)
   30 CONTINUE
C
      CALL NGSETR('Nominal linewidth',1.)
      CALL GSPLCI(2)
      CALL GSLWSC(8.)
      X(1) = .03
      X(2) = .97
      Y(1) = YP(2)-0.04
      Y(2) = Y(1)
      CALL GPL(2,X,Y)
      X(1) = XP(2)-0.70*XLEN
      X(2) = X(1)
      Y(1) = YP(1)+0.035
      Y(2) = YP(3)-0.435
      CALL GPL(2,X,Y)
C 
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
