C*PL*ERROR* Comment line too long
c---------------------------------------------------------------------
      SUBROUTINE DTRVSPH77(INTL,IGRIDA,NLONA,NLATA,UA,VA,IGRIDB,NLONB,
     +                    NLATB,UB,VB,JWAVE,WORK,LWORK,WSAVE,LSAVE,
     +                    DWORK,LDWORK,IER)

      INTEGER INTL,IGRIDA(2),NLONA,NLATA,IGRIDB(2),NLONB,NLATB,JWAVE,IER
      INTEGER LSAVE,LSVMIN,LWORK,LWKMIN
C input
      DOUBLE PRECISION UA(NLONA,NLATA),VA(NLONA,NLATA)
C output
      DOUBLE PRECISION UB(NLONB,NLATB),VB(NLONB,NLATB)
      DATA IENTRY/0/
      SAVE IENTRY


      DOUBLE PRECISION WORK(LWORK),WSAVE(LSAVE)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IENTRY = IENTRY + 1
c c c write (*,'(//,15(''-''),1x,'' sub trvsph77: ientry='',i3
c c c*             ,15(''-'') )')   ientry

c c c print *,' sub trvsph77: debug1: ', nlona,nlata,nlonb,nlatb,jwave

c c c write (*,'('' sub trvsph77: jwave ='', i3)') jwave
c c c write (*,'('' sub trvsph77: igrida='',2i3)') (igrida(i),i=1,2)
c c c write (*,'('' sub trvsph77: igridb='',2i3)') (igridb(i),i=1,2)
c c c write (*,'('' sub trvsph77: nlona,nlata='',2i3)') nlona,nlata
c c c write (*,'('' sub trvsph77: nlonb,nlatb='',2i3)') nlonb,nlatb
c c c write (*,'('' sub trvsph77:  ua(grida)='',i3,10f7.1)')
c c c*                          (j,(ua(i,j),i=1,10),j=1,nlata,nlata-1)

c call the f77 version with the full argument list

      IVECA = 0
      IVECB = 0
      CALL DTRVSPH(INTL,IGRIDA,NLONA,NLATA,IVECA,UA,VA,IGRIDB,NLONB,
     +            NLATB,IVECB,UB,VB,WSAVE,LSAVE,LSVMIN,WORK,LWORK,
     +            LWKMIN,DWORK,LDWORK,LER)

c c c write (*,'('' sub trvsph77:  ub(gridb)='',i3,10f7.1)')
c c c*                          (j,(ub(i,j),i=1,10),j=1,nlatb,nlatb-1)
      IF (LER.NE.0) THEN
          PRINT *,' sub trvsph: ler=',LER,LSVMIN,LWKMIN
c c c     stop
      END IF

c c c print *,'sub trvsph77: lsvmin, lwkmin=', lsvmin, lwkmin

      IF (IABS(JWAVE).NE.0) THEN

c truncate the data at a specified truncation

          CALL DTRCWAV(IGRIDB,NLATB,NLONB,UB,WSAVE,LSAVE,WORK,LWORK,
     +                DWORK,LDWORK,KER,JWAVE)
          CALL DTRCWAV(IGRIDB,NLATB,NLONB,VB,WSAVE,LSAVE,WORK,LWORK,
     +                DWORK,LDWORK,KER,JWAVE)
c c c     print *,'sub trcwav: ker=',ker
c c c     print *,'sub trcwav: nlatb,nlonb,jwave=',nlatb,nlonb,jwave

      END IF

      RETURN
      END
c-----------------------------------------------------------------
      SUBROUTINE DF2FOSHV(UOFF,VOFF,ILON,JLAT,UREG,VREG,JLAT1,WORK,
     +                   LWORK,WSAVE,LSAVE,IOFF,IER)
      DOUBLE PRECISION WORK
      DOUBLE PRECISION WSAVE

c NCL: f2foshv (ureg,vreg,uoff,voff)

c vector: fixed (regular) grid to fixed-offset (equal spaced)

c nomenclature:
c .   goff   - output grid. Data are in a fixed-offset grid
c .            with no wraparound or cyclic pts.
c .            eg: a 5 degree grid ==> goff(72,36)
c .            where ilon=72 and jlat=36.
c .   greg   - input grid. data are on a fixed grid
c .            The output grid will have pole pts.
c .            eg: for a 5 deg grid  greg(72,36).


C input
      INTEGER ILON,JLAT,JLAT1
      DOUBLE PRECISION UOFF(ILON,JLAT),VOFF(ILON,JLAT)
      DOUBLE PRECISION UREG(ILON,JLAT1),VREG(ILON,JLAT1)
C output
      INTEGER IER

      IER = 0
      IF (ILON.LT.4 .OR. JLAT.LT.3) IER = 10
      IF ((JLAT1-JLAT).NE.1) IER = IER + 100
      IF (IER.NE.0) RETURN

      CALL DVSHIFTI(IOFF,ILON,JLAT,LSAVE,WSAVE,IER)
      CALL DVSHIFTE(IOFF,ILON,JLAT,UOFF,VOFF,UREG,VREG,WSAVE,LSAVE,
     +     WORK,LWORK,IER)

      RETURN
      END
