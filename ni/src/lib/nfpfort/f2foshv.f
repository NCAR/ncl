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
