
      PROGRAM AREX03
C
C This example shows the effect of the new internal parameter 'RC' on
C the process of reconciling conflicting area-identifier information.
C It puts into an area map five rows of decagons; each of the rows is
C identified with a particular edge group and a particular element of
C 'RC'.  Each of the decagons is drawn using 10 edge segments having
C different area identifiers; the area identifier associated with each
C edge is shown and the area identifier selected for the whole decagon
C is shown in its center.
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
C Define the length of the area map array.
C
        PARAMETER (LAMA=50000)
C
C Declare the area map array for ARSCAM.
C
        DIMENSION IAMA(LAMA)
C
C Declare arrays in which to define edge segments for AREDAM.
C
        DIMENSION XEDG(11),YEDG(11)
C
C Declare an array in which to put area identifiers for each of the
C ten edges of each of the five decagons in each row of decagons on
C each of two frames.
C
        DIMENSION IAID(10,5,2)
C
C Declare external the user routine to process a subarea, so the
C compiler won't mistake its name for that of a variable.
C
        EXTERNAL URTPSA
C
C Declare scratch arrays for ARSCAM to use in calls to URTPSA.
C
        DIMENSION XCRA(1000),YCRA(1000),IAAI(9),IAGI(9)
C
C Declare a character temporary to use in writing area ids.
C
        CHARACTER*2 CTMP
C
C Declare a character variable in which to put the main title.
C
        CHARACTER*38 TITL
C
C Define the area identifiers to be used for each of the ten edges
C of each of the five decagons in each row of decagons.
C
        DATA IAID / -1 , -1 , -2 , -3 , -4 , -5 , -6 , -7 , -8 , -9 ,
     +               0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     +               1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,
     +               2 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,
     +               1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  1 ,  2 ,
     +               1 ,  1 ,  1 ,  1 ,  1 ,  2 ,  2 ,  2 ,  2 ,  2 ,
     +              -1 ,  1 ,  1 ,  1 ,  1 ,  2 ,  2 ,  2 ,  2 ,  2 ,
     +               0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  1 ,
     +               0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  2 ,  2 ,  1 ,
     +               0 ,  0 ,  0 ,  0 , -3 , -2 , -1 ,  2 ,  2 ,  1 /
C
C Define the title with an "n" to be replaced by a frame number.
C
        DATA TITL / 'USING VARIOUS VALUES OF ''RC'' - FRAME n' /
C
C Open GKS.
C
        CALL GOPKS (IERRF,ISZDM)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Set up a convenient mapping from the user coordinate system to the
C fractional coordinate system.
C
        CALL SET (.15,.95,.02,.82,.5,5.5,.5,5.5,1)
C
C Tell PLOTCHAR to use font number 25 and to put outlines on all filled
C characters.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
C
C Set up an outer loop on frame number.
C
        DO 105 IFRA=1,2
C
C Put some labels at the top of the plot.
C
        TITL(38:38)=CHAR(ICHAR('0')+IFRA)
        CALL PLCHHQ (CFUX(.5),CFUY(.954),TITL,.02,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.918), 'The specified element of ''RC
     +'' is used for each decagon in a particular horizontal row.',
     +                                                       .012,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.894), 'The same set of area identifi
     +ers is used for each decagon in a particular vertical column.',
     +                                                       .012,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.870),'Each of the small numbers in a
     + decagon is an area identifier for one edge of the decagon.',
     +                                                       .012,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.846),'(The larger the character size
     + used for the number, the more recently it was seen by AREAS.)',
     +                                                       .012,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.822),'The largest number (in the cen
     +ter of the decagon) is a reconciled area identifier for the decago
     +n.',
     +                                                       .012,0.,0.)
C
C Set the values of the elements of the 'RC' parameter array so that
C each reconciliation method will be used for exactly one edge group.
C
        CALL ARSETI ('RC(1) - RECONCILIATION METHOD FOR GROUP 1',-2)
        CALL ARSETI ('RC(2) - RECONCILIATION METHOD FOR GROUP 2',-1)
        CALL ARSETI ('RC(3) - RECONCILIATION METHOD FOR GROUP 3', 0)
        CALL ARSETI ('RC(4) - RECONCILIATION METHOD FOR GROUP 4', 1)
        CALL ARSETI ('RC(5) - RECONCILIATION METHOD FOR GROUP 5', 2)
C
C Change the function-code signal character for PLOTCHAR so we can
C use colons in some labels.
C
        CALL PCSETC ('FC - FUNCTION-CODE CHARACTER','|')
C
C Put labels on each of the rows of decagons.
C
        CALL PLCHHQ (CFUX(.05),4.75,'RC(1): -2',.015,0.,-1.)
        CALL PLCHHQ (CFUX(.05),3.75,'RC(2): -1',.015,0.,-1.)
        CALL PLCHHQ (CFUX(.05),2.75,'RC(3):  0',.015,0.,-1.)
        CALL PLCHHQ (CFUX(.05),1.75,'RC(4): +1',.015,0.,-1.)
        CALL PLCHHQ (CFUX(.05),0.75,'RC(5): +2',.015,0.,-1.)
C
        CALL PLCHHQ (CFUX(.05),4.5,'Method: If any are negative, use -1;
     + else, use most-frequently-occurring value, possibly zero.',
     +                                                      .012,0.,-1.)
C
        CALL PLCHHQ (CFUX(.05),3.5,'Method: If any are negative, use -1;
     + if all are zero, use zero; else, use most-frequently-occurring no
     +n-zero.',
     +                                                      .012,0.,-1.)
C
        CALL PLCHHQ (CFUX(.05),2.5,'Method: If any are negative, use -1;
     + if all are zero, use zero; else, use most-recently-seen non-zero.
     +',
     +                                                      .012,0.,-1.)
C
        CALL PLCHHQ (CFUX(.05),1.5,'Method: Ignore zeroes; treat negativ
     +es as -1s; use most-frequently-occurring value in resulting set.',
     +                                                      .012,0.,-1.)
C
        CALL PLCHHQ (CFUX(.05),0.5, 'Method: Do not ignore zeroes; treat
     + negatives as -1s; use most-frequently-occurring value in resultin
     +g set.',
     +                                                      .012,0.,-1.)
C
C Change the function-code signal character for PLOTCHAR back to the
C default.
C
        CALL PCSETC ('FC - FUNCTION-CODE CHARACTER',':')
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put a collection of decagons into the area map.  Each horizontal row
C of decagons is identified with a particular edge group and therefore
C with a particular element of 'RC'.
C
        DO 104 IGID=1,5
          YCEN=REAL(6-IGID)
          DO 103 IDEC=1,5
            XCEN=REAL(IDEC)
            DO 101 IANG=1,10
              ANG1=.017453292519943*REAL(MOD(36*IANG-36,360))
              ANG2=.017453292519943*REAL(MOD(36*IANG-18,360))
              XEDG(IANG)=XCEN+.4*COS(ANG1)
              YEDG(IANG)=YCEN+.4*SIN(ANG1)
              WRITE (CTMP,'(I2)') IAID(IANG,IDEC,IFRA)
              CALL PLCHHQ (XCEN+.27*COS(ANG2),YCEN+.27*SIN(ANG2),CTMP,
     +                                    .006+.0009*REAL(IANG),0.,0.)
  101       CONTINUE
            XEDG(11)=XEDG(1)
            YEDG(11)=YEDG(1)
            CALL GSLWSC (2.)
            CALL GPL    (11,XEDG,YEDG)
            CALL GSLWSC (1.)
            DO 102 IANG=1,10
              CALL AREDAM (IAMA,XEDG(IANG),YEDG(IANG),2,IGID,
     +                                IAID(IANG,IDEC,IFRA),0)
  102       CONTINUE
  103     CONTINUE
  104   CONTINUE
C
C Scan the area map, extracting all subareas and delivering each to
C a processing routine that will just draw the edge of the area and
C write its area identifier, relative to the group IGID, in its
C center.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IAAI,IAGI,9,URTPSA)
C
C Advance the frame.
C
        CALL FRAME
C
C End of frame loop.
C
  105   CONTINUE
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



      SUBROUTINE URTPSA (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare a character temporary to use in writing area identifiers.
C
        CHARACTER*2 CTMP
C
C If the number of groups reported is not equal to five, log an error
C and return.
C
        IF (NGPS.NE.5) THEN
          PRINT * , 'IN URTPSA, NGPS NOT EQUAL TO 5'
          RETURN
        END IF
C
C Otherwise, if the number of coordinates is eleven, it's one of the
C decagons, so plot the appropriate area identifier in the middle of it.
C
        IF (NCRA.EQ.11) THEN
          XCEN=0.
          YCEN=0.
          DO 101 I=1,NCRA-1
            XCEN=XCEN+XCRA(I)
            YCEN=YCEN+YCRA(I)
  101     CONTINUE
          XCEN=XCEN/REAL(NCRA-1)
          YCEN=YCEN/REAL(NCRA-1)
          IGID=MAX(1,MIN(5,1+INT((.82-YCEN)/.16)))
          IAID=-2
          DO 102 I=1,NGPS
            IF (IAGI(I).EQ.IGID) IAID=IAAI(I)
  102     CONTINUE
          IF (IAID.NE.-2) THEN
            WRITE (CTMP,'(I2)') IAID
            CALL PLCHHQ (XCEN,YCEN,CTMP,.02,0.,0.)
          ELSE
            PRINT * , 'IN URTPSA, DESIRED AREA IDENTIFIER NOT FOUND'
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
