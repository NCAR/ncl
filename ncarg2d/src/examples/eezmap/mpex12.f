
        PROGRAM MPEX12
C
C This example produces no graphical output.  Instead, it produces
C print output demonstrating the use of some of the EZMAPB routines
C allowing one to retrieve information from the a map database.
C
C Declare the types of a couple of EZMAPB functions and a temporary
C character variable to be used below.
C
        CHARACTER*64 MPNAME
        CHARACTER*128 MPFNME,CTMP
C
C Read name information from the EZMAP database.
C
        CALL MPLNRI ('Earth..1')
C
C Do a simple check of some information-returning routines.  First, find
C the area in the database with a particular short name.
C
        WRITE (*,'('' '')')
        WRITE (*,'(''Test routines that return area information:'')')
        WRITE (*,'('' '')')
        WRITE (*,'(''  Search map database for Madeline Island.'')')
C
        DO 101 I=1,10000
          IF (MPIATY(I).NE.0) THEN
            CTMP=MPNAME(I)
            IF (CTMP(1:15).EQ.'Madeline Island') THEN
              IAID=I
              WRITE (*,'('' '')')
              WRITE (*,'(''  Madeline Island has area identifier IAID ='
     +',I4)') IAID
              GO TO 102
            END IF
          ELSE
            WRITE (*,'('' '')')
            WRITE (*,'(''  Madeline Island not found in database.'')')
            GO TO 103
          END IF
  101   CONTINUE
C
C Search failure.
C
        WRITE (*,'('' '')')
        WRITE (*,'(''  Search failure - too many names in database.'')')
        GO TO 103
C
C Write out simple information for the area.
C
  102   WRITE (*,'(''  Its area type is MPIATY(IAID) =          '',I5)')
     +                                                      MPIATY(IAID)
        WRITE (*,'(''  Its suggested color is MPISCI(IAID) =    '',I5)')
     +                                                      MPISCI(IAID)
        WRITE (*,'(''  Its parent identifier is MPIPAR(IAID) =  '',I5)')
     +                                                      MPIPAR(IAID)
        CTMP=MPNAME(IAID)
        WRITE (*,'(''  Its short name is MPNAME(IAID) = '',A)')
     +                                            CTMP(1:MPILNB(CTMP))
C
        WRITE (*,'('' '')')
        WRITE (*,'(''  Short name of smallest containing area at various
     + display levels:'')')
        WRITE (*,'('' '')')
C
        CTMP=MPNAME(MPIOSA(IAID,5))
        WRITE (*,'(''    MPNAME(MPIOSA(IAID,5)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOSA(IAID,4))
        WRITE (*,'(''    MPNAME(MPIOSA(IAID,4)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOSA(IAID,3))
        WRITE (*,'(''    MPNAME(MPIOSA(IAID,3)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOSA(IAID,2))
        WRITE (*,'(''    MPNAME(MPIOSA(IAID,2)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOSA(IAID,1))
        WRITE (*,'(''    MPNAME(MPIOSA(IAID,1)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
C
        WRITE (*,'('' '')')
        WRITE (*, '(''  Short name of largest containing area at various
     + display levels:'')')
        WRITE (*,'('' '')')
C
        CTMP=MPNAME(MPIOLA(IAID,5))
        WRITE (*,'(''    MPNAME(MPIOLA(IAID,5)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOLA(IAID,4))
        WRITE (*,'(''    MPNAME(MPIOLA(IAID,4)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOLA(IAID,3))
        WRITE (*,'(''    MPNAME(MPIOLA(IAID,3)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOLA(IAID,2))
        WRITE (*,'(''    MPNAME(MPIOLA(IAID,2)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPNAME(MPIOLA(IAID,1))
        WRITE (*,'(''    MPNAME(MPIOLA(IAID,1)) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
C
        WRITE (*,'('' '')')
        WRITE (*,'(''  Suggested color at various display levels:'')')
        WRITE (*,'('' '')')
C
        WRITE (*,'(''    MPISCI(MPIOSA(IAID,5)) = '',I1)')
     +                                            MPISCI(MPIOSA(IAID,5))
        WRITE (*,'(''    MPISCI(MPIOSA(IAID,4)) = '',I1)')
     +                                            MPISCI(MPIOSA(IAID,4))
        WRITE (*,'(''    MPISCI(MPIOSA(IAID,3)) = '',I1)')
     +                                            MPISCI(MPIOSA(IAID,3))
        WRITE (*,'(''    MPISCI(MPIOSA(IAID,2)) = '',I1)')
     +                                            MPISCI(MPIOSA(IAID,2))
        WRITE (*,'(''    MPISCI(MPIOSA(IAID,1)) = '',I1)')
     +                                            MPISCI(MPIOSA(IAID,1))
C
        WRITE (*,'('' '')')
        WRITE (*,'(''  Full name at various display levels:'')')
        WRITE (*,'('' '')')
C
        CTMP=MPFNME(MPIOSA(IAID,5),5)
        WRITE (*,'(''    MPFNME(MPIOSA(IAID,5),5) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPFNME(MPIOSA(IAID,4),4)
        WRITE (*,'(''    MPFNME(MPIOSA(IAID,4),4) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPFNME(MPIOSA(IAID,3),3)
        WRITE (*,'(''    MPFNME(MPIOSA(IAID,3),3) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPFNME(MPIOSA(IAID,2),2)
        WRITE (*,'(''    MPFNME(MPIOSA(IAID,2),2) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
        CTMP=MPFNME(MPIOSA(IAID,1),1)
        WRITE (*,'(''    MPFNME(MPIOSA(IAID,1),1) = '',A)')
     +                                              CTMP(1:MPILNB(CTMP))
C
        WRITE (*,'('' '')')
        WRITE (*,'(''  Full name from a specified level down:'')')
        WRITE (*,'('' '')')
C
        CTMP=MPFNME(IAID,5)
        LCTM=MPILNB(CTMP)
        WRITE (*,'(''    MPFNME(IAID,5) = '',A)') CTMP( 1:MIN( 58,LCTM))
        IF (LCTM.GT.58)
     +  WRITE (*,'(''                     '',A)') CTMP(59:MIN(116,LCTM))
        CTMP=MPFNME(IAID,4)
        LCTM=MPILNB(CTMP)
        WRITE (*,'(''    MPFNME(IAID,4) = '',A)') CTMP( 1:MIN( 58,LCTM))
        IF (LCTM.GT.58)
     +  WRITE (*,'(''                     '',A)') CTMP(59:MIN(116,LCTM))
        CTMP=MPFNME(IAID,3)
        LCTM=MPILNB(CTMP)
        WRITE (*,'(''    MPFNME(IAID,3) = '',A)') CTMP( 1:MIN( 58,LCTM))
        IF (LCTM.GT.58)
     +  WRITE (*,'(''                     '',A)') CTMP(59:MIN(116,LCTM))
        CTMP=MPFNME(IAID,2)
        LCTM=MPILNB(CTMP)
        WRITE (*,'(''    MPFNME(IAID,2) = '',A)') CTMP( 1:MIN( 58,LCTM))
        IF (LCTM.GT.58)
     +  WRITE (*,'(''                     '',A)') CTMP(59:MIN(116,LCTM))
        CTMP=MPFNME(IAID,1)
        LCTM=MPILNB(CTMP)
        WRITE (*,'(''    MPFNME(IAID,1) = '',A)') CTMP( 1:MIN( 58,LCTM))
        IF (LCTM.GT.58)
     +  WRITE (*,'(''                     '',A)') CTMP(59:MIN(116,LCTM))
C
C Done.
C
  103   STOP
C
      END
