C
C
C       $Id: coeff1.f,v 1.2 1997-05-05 21:45:41 boote Exp $
C
C PACKAGE CUBSPL         NOTE--DOCUMENTATION FOR INDIVIDUAL ROUTINES
C                              FOLLOWS THE GENERAL PACKAGE INFORMATION
C
C LATEST REVISION        JANUARY 1985
C
C PURPOSE                TO PERFORM ONE AND TWO-DIMENSIONAL CUBIC SPLINE
C                        INTERPOLATION WITH CHOICE OF BOUNDARY
C                        CONDITIONS.  THE FUNCTION AND SELECTED
C                        DERIVATIVES MAY BE EVALUATED AT ANY POINT WHERE
C                        INTERPOLATION IS REQUIRED.  IN THE
C                        TWO-DIMENSIONAL CASE, THE GIVEN DATA POINTS
C                        MUST BE ON A RECTANGULAR GRID, WHICH NEED NOT
C                        BE EQUALLY SPACED.  THE PACKAGE CUBSPL CONTAINS
C                        SEVEN ROUTINES.
C
C                        SUBROUTINE COEFF1
C                          COMPUTES THE COEFFICIENTS FOR ONE-DIMENSIONAL
C                          CUBIC SPLINE INTERPOLATION USING ONE OF
C                          THE FOLLOWING BOUNDARY CONDITIONS AT
C                          EACH END OF THE RANGE.
C                            . SECOND DERIVATIVE GIVEN AT BOUNDARY.
C                            . FIRST DERIVATIVE GIVEN AT BOUNDARY.
C                            . PERIODIC BOUNDARY CONDITION.
C                            . FIRST DERIVATIVE DETERMINED BY FITTING A
C                              CUBIC TO THE FOUR POINTS NEAREST TO THE
C                              BOUNDARY.
C
C                        SUBROUTINE TERP1
C                          USING THE COEFFICIENTS COMPUTED BY COEFF1,
C                          THIS ROUTINE EVALUATES THE FUNCTION AND/OR
C                          FIRST AND SECOND DERIVATIVES AT ANY POINT
C                          WHERE INTERPOLATION IS REQUIRED.
C
C                        SUBROUTINE COEFF2
C                          COMPUTES THE COEFFICIENTS FOR TWO-DIMENSIONAL
C                          BICUBIC SPLINE INTERPOLATION WITH THE SAME
C                          CHOICE OF BOUNDARY CONDITIONS AS FOR COEFF1.
C
C                        FUNCTION TERP2
C                          USING THE COEFFICIENTS PRODUCED BY COEFF2,
C                          THIS SUBROUTINE EVALUATES THE FUNCTION OR A
C                          SELECTED DERIVATIVE AT ANY POINT WHERE
C                          TWO-DIMENSIONAL INTERPOLATION IS REQUIRED.
C
C                        SUBROUTINE TRIP
C                          A SIMPLE PERIODIC, TRIDIAGONAL LINEAR
C                          EQUATION SOLVER USED BY COEFF1.
C
C                        SUBROUTINE SEARCH
C                          PERFORMS A BINARY SEARCH IN A ONE-DIMENSIONAL
C                          FLOATING POINT TABLE ARRANGED IN ASCENDING
C                          ORDER.  THIS ROUTINE IS CALLED BY TERP1 AND
C                          TERP2.
C
C                        SUBROUTINE INTERP
C                          GIVEN COEFFICIENTS PROVIDED BY COEFF1 AND THE
C                          POSITION OF THE INTERPOLATION POINT IN THE
C                          INDEPENDENT VARIABLE TABLE, THIS ROUTINE
C                          PERFORMS ONE-DIMENSIONAL INTERPOLATION FOR
C                          THE FUNCTION VALUE, FIRST AND SECOND
C                          DERIVATIVE, AS DESIRED.  THIS ROUTINE IS
C                          CALLED BY TERP1 AND TERP2.
C
C USAGE                  FOR ONE-DIMENSIONAL CUBIC SPLINE INTERPOLATION,
C                        THE USER FIRST CALLS COEFF1 BY
C
C                          CALL COEFF1 (N,X,F,W,IOP,INT,WK)
C
C                        THIS SUBROUTINE RETURNS THE COEFFICIENTS
C                        NEEDED FOR THE SUBSEQUENT INTERPOLATION
C                        IN THE ARRAY W.  THE USER THEN CALLS
C                        SUBROUTINE TERP1 BY
C
C                          CALL TERP1 (N,X,F,W,Y,INT,TAB,ITAB)
C
C                        IN ORDER TO COMPUTE THE VALUE OF THE
C                        FUNCTION AND/OR ITS DERIVATIVES.  THE USER
C                        SPECIFIES Y, THE VALUE OF THE INDEPENDENT
C                        VARIABLE WHERE THE INTERPOLATION IS TO BE
C                        PERFORMED.  THE INTERPOLATED VALUES ARE
C                        RETURNED IN TAB.  THE PARAMETERS
C                        N,X,F,W, AND INT MUST NOT BE CHANGED
C                        BETWEEN THE SUBROUTINE CALLS.
C
C                        FOR TWO-DIMENSIONAL CUBIC SPLINE INTERPOLATION
C                        THE USER FIRST CALLS COEFF2 BY
C
C                          CALL COEFF2 (NX,X,NY,Y,F,FXX,FYY,FXXYY,
C                                       IDM,IBD,WK)
C
C                        THIS SUBROUTINE RETURNS THE COEFFICIENTS
C                        NEEDED FOR THE SUBSEQUENT INTERPOLATION IN
C                        THE ARRAYS FXX, FYY, FXXYY.  THE USER THEN
C                        CALLS THE ROUTINE TERP2 BY
C
C                          R = TERP2 (XB,YB,NX,X,NY,Y,F,FXX,FYY,FXXYY,
C                                     IDM,IXD,IYD)
C
C                        IN ORDER TO PERFORM THE INTERPOLATION AT THE
C                        POINT SPECIFIED BY THE VALUES OF XB AND YB.
C                        DEPENDING ON THE VALUES INPUT IN IXD AND IYD,
C                        THE ROUTINE RETURNS AN INTERPOLATED VALUE
C                        FOR THE FUNCTION OR ONE OF ITS PARTIAL
C                        DERIVATIVES.  THE PARAMETERS NX,X,NY,Y,F,FXX,
C                        FYY,FXXYY, AND IDM MUST NOT BE CHANGED
C                        BETWEEN THE CALLS TO COEFF2 AND TERP2.
C
C SPECIAL CONDITIONS     TABLES OF INDEPENDENT VARIABLES MUST BE
C                        ARRANGED IN ASCENDING ORDER.  FOR
C                        TWO-DIMENSIONAL INTERPOLATION, THE DATA POINTS
C                        MUST LIE ON A RECTANGULAR MESH.
C
C I/O                    NONE
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       NONE
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                THIS PACKAGE IS BASED ON THE ROUTINES
C                            LA E 102A, SPL1D1
C                            LA E 103A, SPL1D2
C                            LA E 104A, SPL2D1
C                            LA E 105A, SPL2D2
C                        OF THE LOS ALAMOS CUBIC SPLINE PACKAGE WRITTEN
C                        BY THOMAS J. JORDAN AND BERTHA FAGAN, 1968.
C                        THE ROUTINES HAVE BEEN STREAMLINED AND
C                        STANDARDIZED.  THE ALGORITHM FOR
C                        TWO-DIMENSIONAL INTERPOLATION IS CONSIDERABLY
C                        MODIFIED.
C
C ALGORITHM              FOR ONE-DIMENSIONAL INTERPOLATION, THE CUBIC
C                        SPLINE IS EXPRESSED IN TERMS OF THE FUNCTION
C                        VALUES AND SECOND DERIVATIVES AT THE DATA
C                        POINTS.  THE SECOND DERIVATIVES ARE
C                        DETERMINED FROM A TRIDIAGONAL LINEAR SYSTEM
C                        WHICH DESCRIBES THE CONTINUITY OF THE FIRST
C                        DERIVATIVE AND INCORPORATES THE GIVEN
C                        BOUNDARY CONDITIONS.  COEFF1  SETS UP THIS
C                        SYSTEM AND CALLS SUBROUTINE TRIP TO SOLVE IT.
C
C                        THE CUBIC SEGMENT BETWEEN TWO ADJACENT
C                        TABULAR POINTS IS CONSTRUCTED FROM THE
C                        FUNCTION VALUES AND SECOND DERIVATIVES AT
C                        THESE POINTS.  THESE PROVIDE THE FOUR
C                        CONSTANTS NEEDED TO DEFINE THE CUBIC
C                        UNIQUELY.  FROM THIS CUBIC, VALUES OF THE
C                        FUNCTION AND ITS FIRST AND SECOND
C                        DERIVATIVES ARE READILY DETERMINED AT ANY
C                        INTERMEDIATE POINT.  ONE-DIMENSIONAL
C                        INTERPOLATION IS PERFORMED BY THE ROUTINE
C                        TERP1.  FOR TWO-DIMENSIONAL INTERPOLATION,
C                        THE BICUBIC SPLINE IS DESCRIBED IN TERMS OF
C                        VALUES OF F,FXX,FYY, AND FXXYY  AT EACH
C                        POINT ON THE GIVEN TWO-DIMENSIONAL
C                        RECTANGULAR GRID OF DATA POINTS.  HERE F
C                        IS THE FUNCTION VALUE,
C
C                          FXX = (D/DX)**2*F
C
C                        AND SO ON.  THE COEFFICIENTS ARE DETERMINED
C                        BY COEFF2, WHICH USES SUCCESSIVE
C                        APPLICATIONS OF COEFF1.
C
C                        1.  THE ARRAY FXX IS DETERMINED BY APPLYING
C                            COEFF1 TO F ALONG EACH LINE IN THE
C                            X-DIRECTION.
C
C                        2.  THE ARRAY FYY IS DETERMINED BY APPLYING
C                            COEFF1 TO F ALONG EACH LINE IN THE
C                            Y-DIRECTION.
C
C                        3.  FXXYY IS DETERMINED ON THE CONSTANT Y
C                            BOUNDARIES BY APPLYING COEFF1 TO FYY
C                            ALONG THESE BOUNDARIES.
C
C                        4.  THE REMAINDER OF THE ARRAY FXXYY IS
C                            DETERMINED BY APPLYING COEFF1 TO FXX
C                            ALONG EACH LINE IN THE Y-DIRECTION.
C
C                        THE BICUBIC WITHIN ANY RECTANGULAR ELEMENT
C                        OF THE GRID IS CONSTRUCTED FROM THE VALUES
C                        OF F,FXX,FYY,FXXYY AT THE FOUR CORNERS.
C                        THESE PROVIDE THE 16 CONSTANTS NECESSARY
C                        TO DEFINE THE BICUBIC UNIQUELY.  TO FIND
C                        THE VALUE OF F CORRESPONDING TO A POINT
C                        (XB,YB) WITHIN THE ELEMENTARY RECTANGLE,
C                        (X(I),Y(J)),(X(I+1),Y(J)),(X(I),Y(J+1)),
C                        (X(I+1),Y(J+1)), FIVE ONE DIMENSIONAL
C                        INTERPOLATIONS ARE PERFORMED.
C
C                        1.  F AT (XB,Y(J)) AND (XB,Y(J+1)) ARE
C                            FOUND BY INTERPOLATING F IN THE
C                            X-DIRECTION USING FXX. (TWO INTERPOLATIONS)
C
C                        2.  FYY AT (XB,Y(J)) AND (XB,Y(J+1)) ARE
C                            FOUND BY INTERPOLATING FYY IN THE
C                            X-DIRECTION USING FXXYY. (TWO
C                            INTERPOLATIONS.)
C
C                        3.  FINALLY F AT (XB,YB) IS FOUND BY
C                        INTERPOLATING BETWEEN F(XB,Y(J)) AND
C                        F(XB,Y(J+1)) IN THE Y-DIRECTION USING
C                        VALUES OF FYY(XB,Y(J)) AND FYY(XB,Y(J+1))
C                        OBTAINED ABOVE. (ONE INTERPOLATION).
C
C                        TWO-DIMENSIONAL INTERPOLATION IS PERFORMED
C                        IN TERP2.
C
C REFERENCES             FOR GREATER DETAIL, SEE J.L.WALSH,
C                        J.H.AHLBERG, E.N.NILSEN, BEST APPROXIMATION
C                        PROPERTIES OF THE SPLINE FIT, JOURNAL OF
C                        MATHEMATICS AND MECHANICS, VOL.II(1962),
C                        225-234.
C
C                        T.L. JORDAN, SMOOTHING AND MULTIVARIABLE
C                        INTERPOLATION WITH SPLINES, LOS ALAMOS
C                        REPORT, LA-3137, 1965.
C
C ACCURACY               NEAR MACHINE ACCURACY WAS OBTAINED WHEN
C                        INTERPOLATING A CUBIC IN ONE DIMENSION
C                        OR A BICUBIC IN TWO DIMENSIONS.
C
C PORTABILITY            FULLY PORTABLE WITH RESPECT TO FORTRAN 66.
C***********************************************************************
C
C SUBROUTINE COEFF1 (N,X,F,W,IOP,INT,WK)
C
C DIMENSION OF           X(N),F(INT*(N-1)+1),W(INT*(N-1)+1),IOP(2),
C ARGUMENTS              WK(3*N+1)
C
C PURPOSE                SUBROUTINE COEFF1 COMPUTES THE COEFFICIENTS
C                        FOR ONE-DIMENSIONAL CUBIC SPLINE
C                        INTERPOLATION USING ONE OF THE FOLLOWING
C                        BOUNDARY CONDITIONS AT EACH END OF THE
C                        RANGE
C
C                        .  SECOND DERIVATIVES GIVEN AT BOUNDARY
C                        .  FIRST DERIVATIVE GIVEN AT BOUNDARY
C                        .  PERIODIC BOUNDARY CONDITIONS
C                        .  FIRST DERIVATIVE CALCULATED BY FITTING A
C                           CUBIC TO THE FOUR POINTS NEAREST TO THE
C                           BOUNDARY
C
C                        NOTE THAT TERP1 MUST BE CALLED TO PERFORM
C                        THE INTERPOLATION.
C
C USAGE                  CALL COEFF1 (N,X,F,W,IOP,INT,WK)
C
C ARGUMENTS
C
C ON INPUT               N
C                          THE NUMBER OF DATA POINTS.  N MUST BE AT
C                          LEAST 4.
C
C                        X
C                          TABLE OF N INDEPENDENT VARIABLE VALUES IN
C                          ASCENDING ORDER.  DIMENSION OF X IN CALLING
C                          PROGRAM MUST BE AT LEAST N.
C
C                        F
C                          TABLE OF N CORRESPONDING DEPENDENT VARIABLE
C                          VALUES.  THE VALUES ARE SEPARATED BY INTERVAL
C                          INT.  THIS IS USUALLY UNITY FOR
C                          ONE-DIMENSIONAL INTERPOLATION.  OTHER VALUES
C                          ARE USEFUL WHEN COEFF1 IS INCORPORATED IN A
C                          TWO-DIMENSIONAL INTERPOLATION SCHEME (SEE
C                          COEFF2).  DIMENSION OF F IN THE CALLING
C                          PROGRAM MUST BE AT LEAST (INT*(N-1)+1).
C
C                        IOP
C                          TWO ELEMENT INTEGER ARRAY DEFINING BOUNDARY
C                          CONDITIONS AT X(1) AND X(N) ACCORDING TO THE
C                          FOLLOWING CODE.
C
C                          FOR IOP(1)
C                          = 1  SECOND DERIVATIVE GIVEN AT X(1).  PLACE
C                               VALUE OF SECOND DERIVATIVE IN W(1)
C                               BEFORE CALL TO COEFF1.
C                          = 2  FIRST DERIVATIVE GIVEN AT X(1).  PLACE
C                               VALUE OF FIRST DERIVATIVE IN W(1) BEFORE
C                               CALL.
C                          = 3  PERIODIC BOUNDARY CONDITION.  X(1) AND
C                               X(N) ARE EQUIVALENT POINTS.  F(1) AND
C                               F(INT*(N-1)+1) ARE EQUAL.
C                          = 4  THE FIRST DERIVATIVE AT X(1) IS
C                               CALCULATED BY FITTING A CUBIC TO POINTS
C                               X(1) THROUGH X(4).
C                          SIMILARLY, IOP(2) DEFINES THE BOUNDARY
C                          CONDITION AT X(N).  WHEN IOP(2) = 1 (OR 2),
C                          THE VALUE OF THE SECOND (OR FIRST) DERIVATIVE
C                          MUST BE PLACED IN W(INT*(N-1)+1).  NOTE THAT
C                          IF IOP(1) = 3, CONSISTENCY DEMANDS THAT
C                          IOP(2) = 3 ALSO.
C
C                        INT
C                          SPACING IN F AND W TABLES.  FOR
C                          ONE-DIMENSIONAL INTERPOLATION THIS WILL
C                          USUALLY BE UNITY.
C
C                        WK
C                          WORK AREA OF DIMENSION AT LEAST (3*N+1).
C
C ON OUTPUT              W
C                          TABLE OF SECOND DERIVATIVES CORRESPONDING TO
C                          GIVEN X AND F VALUES.  THE SEPARATION OF
C                          TABULAR ENTRIES IS INT (SEE ABOVE).
C                          DIMENSION OF W IN THE CALLING PROGRAM MUST BE
C                          AT LEAST (INT*(N-1)+1).
C
C                          THE ARRAYS X, F, W ARE USED AS INPUT FOR THE
C                          ROUTINE TERP1, WHICH PERFORMS INTERPOLATION
C                          AT A GIVEN VALUE OF THE INDEPENDENT VARIABLE.
C
C TIMING                 THE TIMING IS LINEARLY PROPORTIONAL TO N, THE
C                        NUMBER OF DATA POINTS.
C***********************************************************************
C
C     SUBROUTINE TERP1 (N,X,F,W,Y,INT,TAB,ITAB)
C
C
C DIMENSION OF           X(N),F(INT*(N-1)+1),W(INT*(N-1)+1),TAB(3),
C ARGUMENTS              ITAB(3)
C
C PURPOSE                USING THE COEFFICIENTS COMPUTED BY COEFF1,
C                        THIS ROUTINE EVALUATES THE FUNCTION AND/OR
C                        FIRST AND SECOND DERIVATIVES AT ANY POINT.
C
C USAGE                  CALL TERP1 (N,X,F,W,Y,INT,TAB,ITAB)
C
C ARGUMENTS
C
C ON INPUT               N
C                          THE NUMBER OF DATA POINTS.  N MUST BE AT
C                          LEAST 4.
C
C                        X
C                          TABLE OF N INDEPENDENT VARIABLE VALUES IN
C                          ASCENDING ORDER.  DIMENSION OF X IN THE
C                          CALLING PROGRAM MUST BE AT LEAST N.
C
C                        F
C                          TABLE OF N CORRESPONDING DEPENDENT VARIABLE
C                          VALUES SEPARATED BY INTERVAL INT, USUALLY
C                          UNITY FOR ONE-DIMENSIONAL INTERPOLATION.
C                          DIMENSION OF F IN THE CALLING PROGRAM MUST BE
C                          AT LEAST (INT*(N-1)+1).
C
C                        W
C                          TABLE OF SECOND DERIVATIVES COMPUTED BY
C                          COEFF1.  THE SEPARATION OF TABULAR ENTRIES IS
C                          INT.  DIMENSION OF W IN THE CALLING PROGRAM
C                          MUST BE AT LEAST (INT*(N-1)+1).
C
C                        Y
C                          VALUE OF THE INDEPENDENT VARIABLE AT WHICH
C                          INTERPOLATION IS REQUIRED.  IF Y LIES OUTSIDE
C                          THE RANGE OF THE TABLE, EXTRAPOLATION TAKES
C                          PLACE.
C
C                        INT
C                          SPACING OF TABULAR ENTRIES IN F AND W ARRAYS.
C                          THIS IS USUALLY UNITY FOR ONE-DIMENSIONAL
C                          INTERPOLATION.
C
C                        ITAB
C                          THREE ELEMENT INTEGER ARRAY DEFINING
C                          INTERPOLATION TO BE PERFORMED AT Y.
C                            IF ITAB(1) = 1, THE FUNCTION VALUE IS
C                                            RETURNED IN TAB(1).
C                            IF ITAB(2) = 1, THE FIRST DERIVATIVE IS
C                                            RETURNED IN TAB(2).
C                            IF ITAB(3) = 1, THE SECOND DERIVATIVE IS
C                                            RETURNED IN TAB(3).
C                          IF ITAB(I) = 0 FOR I = 1, 2 OR 3, THE
C                          CORRESPONDING FUNCTION VALUE OR DERIVATIVE IS
C                          NOT COMPUTED AND TAB(I) IS NOT REFERENCED.
C
C ON OUTPUT              TAB
C                          THREE ELEMENT ARRAY IN WHICH INTERPOLATED
C                          FUNCTION VALUE, FIRST AND SECOND DERIVATIVES
C                          ARE RETURNED AS DICTATED BY ITAB (SEE ABOVE).
C
C TIMING                 THIS PROCEDURE IS FAST.  THE MAXIMUM TIME FOR
C                        THE BINARY SEARCH IS PROPORTIONAL TO ALOG(N).
C                        THE TIME FOR FUNCTION AND DERIVATIVE EVALUATION
C                        IS INDEPENDENT OF N.
C***********************************************************************
C
C     SUBROUTINE COEFF2 (NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,IBD,WK)
C
C
C DIMENSION OF           X(NX),Y(NY),F(IDM,NY),FXX(IDM,NY),FYY(IDM,NY),
C ARGUMENTS              FXXYY(IDM,NY),IBD(4),WK(3*MAX0(NX,NY)+1)
C                        (IDM MUST BE .GE. NX)
C
C PURPOSE                SUBROUTINE COEFF2 COMPUTES THE COEFFICIENTS
C                        FOR TWO-DIMENSIONAL BICUBIC SPLINE
C                        INTERPOLATION WITH THE SAME CHOICE OF
C                        BOUNDARY CONDITIONS AS FOR COEFF1.  TERP2
C                        IS CALLED TO PERFORM INTERPOLATION.
C
C USAGE                  CALL COEFF2 (NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,IBD,
C                                     WK)
C
C ARGUMENTS
C
C ON INPUT               NX
C                          NUMBER OF GRID POINTS IN THE X-DIRECTION.  NX
C                          MUST BE AT LEAST 4.
C
C                        X
C                          TABLE OF NX VALUES OF THE FIRST INDEPENDENT
C                          VARIABLE ARRANGED IN ASCENDING ORDER.
C                          DIMENSION OF X IN THE CALLING PROGRAM MUST BE
C                          AT LEAST NX.
C
C                        NY
C                          NUMBER OF GRID POINTS IN THE Y-DIRECTION.  NY
C                          MUST BE AT LEAST 4.
C
C                        Y
C                          TABLE OF NY VALUES OF THE SECOND INDEPENDENT
C                          VARIABLE ARRANGED IN ASCENDING ORDER.
C                          DIMENSION OF Y IN THE CALLING PROGRAM MUST BE
C                          AT LEAST NY.
C
C                        F
C                          TWO DIMENSIONAL ARRAY OF FUNCTION VALUES AT
C                          THE GRID POINTS DEFINED BY THE ARRAYS X AND
C                          Y.  DIMENSION OF F IN THE CALLING PROGRAM IS
C                          (IDM, NYY) WHERE
C                              IDM .GE. NX
C                              NYY .GE. NY
C
C                        IDM
C                          FIRST DIMENSION IN THE CALLING PROGRAM OF
C                          ARRAYS F, FXX, FYY, FXXYY.  IDM MUST BE AT
C                          LEAST NX.
C
C                        IBD
C                          FOUR ELEMENT INTEGER ARRAY DEFINING BOUNDARY
C                          CONDITIONS ACCORDING TO THE FOLLOWING CODE.
C                          FOR IBD(1)
C                          = 1  THE SECOND DERIVATIVE OF F WITH RESPECT
C                               TO X IS GIVEN AT (X(1),Y(J)) FOR
C                               J = 1,NY,1.  VALUES OF THIS SECOND
C                               DERIVATIVE MUST BE PLACED IN FXX(1,J)
C                               FOR J = 1,NY,1, BEFORE CALLING COEFF2.
C                          = 2  THE FIRST DERIVATIVE OF F WITH RESPECT
C                               TO X IS GIVEN AT (X(1),Y(J)) FOR
C                               J = 1,NY,1.  VALUES OF THE DERIVATIVE
C                               MUST BE PLACED IN FXX(1,J) FOR
C                               J = 1,NY,1 BEFORE CALLING COEFF2.
C                          = 3  PERIODIC BOUNDARY CONDITION IN THE
C                               X-DIRECTION.  (X(1),Y(J)) AND
C                               AND (X(NX),Y(J)) ARE EQUIVALENT POINTS
C                               FOR J = 1,NY,1.  F(1,J) AND F(NX,J) ARE
C                               EQUAL.
C                          = 4  THE FIRST DERIVATIVE OF F WITH RESPECT
C                               TO X AT (X(1),Y(J)) IS COMPUTED BY
C                               FITTING A CUBIC TO F(1,J) THROUGH F(4,J)
C                               FOR J = 1,NY,1.
C
C                          SIMILARLY, IBD(2) DEFINES THE BOUNDARY
C                          CONDITION AT (X(NX),Y(J)) FOR J = 1,NY,1.
C                          WHEN IBD(2) = 1 (OR 2) THE VALUES OF THE
C                          SECOND (OR FIRST) DERIVATIVE OF F WITH
C                          RESPECT TO X ARE PLACED IN FXX(NX,J) FOR
C                          J = 1,NY,1.
C                            NOTE THAT IF IBD(1) = 3, CONSISTENCY
C                            REQUIRES THAT IBD(2) = 3 ALSO.
C                          FOR IBD(3)
C                          = 1  THE SECOND DERIVATIVE OF F WITH RESPECT
C                               TO Y IS GIVEN AT (X(I),Y(1)).  PLACE
C                               VALUES OF THE DERIVATIVE IN FYY(I,1) FOR
C                               I = 1,NX,1 BEFORE CALLING COEFF2.
C                          = 2  THE FIRST DERIVATIVE OF F WITH RESPECT
C                               TO Y IS GIVEN AT (X(I),Y(1)).  VALUES OF
C                               THIS DERIVATIVE MUST BE PLACED IN
C                               FYY(I,1) FOR I = 1,NX,1 BEFORE CALLING
C                               COEFF2.
C                          = 3  PERIODIC BOUNDARY CONDITION IN THE
C                               Y-DIRECTION.  (X(I),Y(1)) AND
C                               (X(I),Y(NY)) ARE EQUIVALENT POINTS.
C                               F(I,1) AND F(I,NY) ARE EQUAL.
C                          = 4  THE FIRST DERIVATIVE OF F WITH RESPECT
C                               TO Y AT (X(I),Y(1)) IS COMPUTED BY
C                               FITTING A CUBIC TO F(I,1) THROUGH F(I,4)
C                               FOR I = 1,NX,1.
C
C                          SIMILARY, IBD(4) DEFINES THE BOUNDARY
C                          CONDITION AT (X(I),Y(NY)) FOR I = 1,NX,1 AND
C                          GIVEN DERIVATIVE VALUES ARE PLACED IN
C                          FYY(I,NY).
C                            NOTE THAT CONSISTENCY DEMANDS THAT IF
C                            IBD(3) = 3, THEN IBD(4) = 3 ALSO.
C
C                        WK
C                          WORK AREA OF DIMENSION AT LEAST
C                          (3*MAX0(NX,NY)+1)
C
C ON OUTPUT              FXX
C                          ARRAY OF SECOND DERIVATIVES OF F WITH RESPECT
C                          TO X COMPUTED BY COEFF2.  FXX(I,J) IS
C                          DERIVATIVE AT (X(I),Y(J)).  AS FOR F,
C                          DIMENSION OF FXX IN THE CALLING PROGRAM IS
C                          (IDM,NYY).
C
C                        FYY
C                          ARRAY OF SECOND DERIVATIVES OF F WITH RESPECT
C                          TO Y COMPUTED BY COEFF2.  DIMENSION OF FYY IN
C                          THE CALLING PROGRAM IS (IDM,NYY).
C
C                        FXXYY
C                          ARRAY OF FOURTH DERIVATIVES
C                          (D/DX)**2*(D/DY)**2*F, COMPUTED BY COEFF2.
C                          DIMENSION OF FXXYY IN THE CALLING PROGRAM IS
C                          (IDM,NYY).
C
C                        THE ARRAYS X, Y, F, FXX, FYY, FXXYY ARE USED AS
C                        INPUT FOR THE ROUTINE TERP2 WHICH PERFORMS
C                        INTERPOLATION AT REQUIRED VALUES OF THE TWO
C                        INDEPENDENT VARIABLES.
C
C TIMING                 THE TIMING IS PROPORTIONAL TO NX*NY.
C***********************************************************************
C
C     FUNCTION TERP2 (XB,YB,NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,IXD,IYD)
C
C
C DIMENSION OF           X(NX),Y(NY),F(IDM,NY),FXX(IDM,NY),FYY(IDM,NY),
C ARGUMENTS              FXXYY(IDM,NY))
C                        (IDM MUST BE .GE. NX)
C
C PURPOSE                USING THE COEFFICIENTS PRODUCED BY COEFF2,
C                        THIS ROUTINE EVALUATES THE FUNCTION ON A
C                        SELECTED DERIVATIVE OF ANY POINT WHERE
C                        TWO-DIMENSIONAL INTERPOLATION IS REQUIRED.
C
C USAGE                  R = TERP2 (XB,YB,NX,X,NY,Y,F,FXX,FYY,FXXYY,IDM,
C                                   IXD,IYD)
C
C ARGUMENTS
C
C ON INPUT               XB, YB
C                          VALUES OF THE INDEPENDENT VARIABLES, X AND Y,
C                          AT WHICH INTERPOLATION IS REQUIRED.
C
C                        NX
C                          NUMBER OF GRID POINTS IN THE X-DIRECTION.  NX
C                          MUST BE AT LEAST 4.
C
C                        X
C                          TABLE OF NX VALUES OF THE INDEPENDENT
C                          VARIABLE, X, ARRANGED IN ASCENDING ORDER.
C                          DIMENSION OF X IN THE CALLING PROGRAM MUST BE
C                          AT LEAST NX.
C
C                        NY
C                          NUMBER OF GRID POINTS IN THE Y-DIRECTION.  NY
C                          MUST BE AT LEAST 4.
C
C                        Y
C                          TABLE OF NY VALUES OF THE INDEPENDENT
C                          VARIABLE, Y, ARRANGED IN ASCENDING ORDER.
C                          DIMENSION OF Y IN THE CALLING PROGRAM MUST BE
C                          AT LEAST NY.
C
C                        F
C                          TWO-DIMENSIONAL ARRAY OF FUNCTION VALUES AT
C                          GRID POINTS DEFINED BY THE ARRAYS X AND Y.
C                          DIMENSION OF F IN THE CALLING PROGRAM IS
C                          (IDM,NYY), WHERE
C                              IDM .GE. NX
C                              NYY .GE. NY
C
C                        FXX
C                          ARRAY OF SECOND DERIVATIVES OF F WITH RESPECT
C                          TO X COMPUTED BY COEFF2.  DIMENSION OF FXX IN
C                          THE CALLING PROGRAM IS (IDM,NYY).  SEE UNDER
C                          F ABOVE.
C
C                        FYY
C                          ARRAY OF SECOND DERIVATIVES OF F WITH RESPECT
C                          TO Y COMPUTED BY COEFF2.  DIMENSION OF FYY IN
C                          THE CALLING PROGRAM IS (IDM,NYY).
C
C                        FXXYY
C                          ARRAY OF FOURTH DERIVATIVES,
C                          (D/DX)**2*(D/DY)**2*F, COMPUTED BY COEFF2.
C                          DIMENSION OF FXXYY IN THE CALLING PROGRAM IS
C                          (IDM,NYY).
C
C                        IDM
C                          FIRST DIMENSION IN THE CALLING PROGRAM OF
C                          ARRAYS F, FXX, FYY AND FXXYY,
C                              IDM .GE. NX
C
C                        IXD, IYD
C                          DEFINE DERIVATIVE TO BE RETURNED BY THE
C                          FUNCTION TERP2.  IXD, IYD MAY EACH TAKE THE
C                          THE VALUES 0, 1, 2.  THE DERIVATIVE RETURNED
C                          IS (D/DX)**IXD*(D/DY)**IYD*F.
C                            NOTE THAT IF IXD = IYD = 0, THE FUNCTION
C                            VALUE ITSELF IS RETURNED.
C
C TIMING                 THIS PROCEDURE IS FAST.  THE MAXIMUM
C                        TIME FOR THE BINARY SEARCH IS PROPORTIONAL TO
C                        ALOG(NX*NY).  THE TIME FOR FUNCTION EVALUATION
C                        IS INDEPENDENT OF N.
C***********************************************************************
      SUBROUTINE COEFF1 (N,X,F,W,IOP,INT,WK)
      DIMENSION       X(4)       ,F(4)       ,W(4)       ,IOP(2)     ,
     1                WK(N,4)
      SAVE
C
C ARITHMETIC STATENENT FUNCTION USED TO LOCATE ENTRIES IN F AND W ARRAYS
C
      II(INDEX)=(INDEX-1)*INT+1
C
C
C
C
C
C
C
C START TO SET UP TRIDIAGONAL SYSTEM
C
      J0 = 1
      DO 101 I=2,N
         JM = J0
         J0 = J0+INT
         WK(I,1) = X(I)-X(I-1)
         WK(I,2) = (F(J0)-F(JM))/WK(I,1)
         WK(I,3) = WK(I,1)/6.
         WK(I,1) = WK(I,1)/3.
  101 CONTINUE
      NN = N
      MK = IOP(1)
      ML = IOP(2)
C
C APPLY BOUNDARY CONDITIONS AT BOUNDARY 1
C
      GO TO (102,103,104,105),MK
C
C SECOND DERIVATIVE GIVEN AT BOUNDARY 1
C
  102 CONTINUE
      WK(2,2) = WK(3,2)-WK(2,2)-WK(2,3)*W(1)
      WK(2,3) = 0.
      WK(2,1) = WK(2,1)+WK(3,1)
      I1 = 2
      NN = NN-1
      GO TO 106
C
C FIRST DERIVATIVE GIVEN AT BOUNDARY 1
C
  103 CONTINUE
      WK(1,2) = WK(2,2)-W(1)
      WK(2,2) = WK(3,2)-WK(2,2)
      WK(1,3) = 0.
      WK(1,1) = WK(2,1)
      WK(2,1) = WK(2,1)+WK(3,1)
      I1 = 1
      GO TO 106
C
C PERIODIC BOUNDARY CONDITION
C
  104 CONTINUE
      Y2 = WK(2,2)
      B2 = WK(2,1)
      WK(2,2) = WK(3,2)-WK(2,2)
      WK(2,1) = WK(3,1)+WK(2,1)
      I1 = 2
      NN = NN-1
      GO TO 106
C
C FIRST DERIVATIVE AT BOUNDARY 1 FROM 4 POINT INTERPOLATION.
C
  105 CONTINUE
      A12 = X(1)-X(2)
      A13 = X(1)-X(3)
      A14 = X(1)-X(4)
      A23 = X(2)-X(3)
      A24 = X(2)-X(4)
      A34 = X(3)-X(4)
      J1 = 1
      J2 = J1+INT
      J3 = J2+INT
      J4 = J3+INT
      W(1)    = (1./A12+1./A13+1./A14)*F(J1)-
     1          A13*A14/(A12*A23*A24)*F(J2)+A12*A14/(A13*A23*A34)*F(J3)-
     2          A12*A13/(A14*A24*A34)*F(J4)
      GO TO 103
C COMPUTE TRIDIAGONAL ARRAYS
  106 CONTINUE
      I2 = N-2
      DO 107 I=3,I2
         WK(I,2) = WK(I+1,2)-WK(I,2)
         WK(I,1) = WK(I+1,1)+WK(I,1)
  107 CONTINUE
C
C APPLY BOUNDARY CONDITIONS AT BOUNDARY 2.
C
      IN = II(N)
      GO TO (108,109,110,111),ML
C
C SECOND DERIVATIVE GIVEN AT BOUNDARY 2.
C
  108 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)-WK(N,3)*W(IN)
      WK(N,3) = 0.
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      NN = NN-1
      GO TO 112
C
C FIRST DERIVATIVE GIVEN AT BOUNDARY 2.
C
  109 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)
      WK(N,2) = -WK(N,2)+W(IN)
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      WK(1,4) = 0.
      GO TO 112
C
C PERIODIC BOUNDARY CONDITION
C
  110 CONTINUE
      WK(N-1,2) = WK(N,2)-WK(N-1,2)
      WK(N,2) = Y2-WK(N,2)
      WK(N-1,1) = WK(N-1,1)+WK(N,1)
      WK(N,1) = WK(N,1)+B2
      WK(1,4) = WK(2,3)
      GO TO 112
C
C FIRST DERIVATIVE AT BOUNDARY 2 FROM 4 POINT INTERPOLATION.
C
  111 CONTINUE
      A12 = X(N)-X(N-1)
      A13 = X(N)-X(N-2)
      A14 = X(N)-X(N-3)
      A23 = X(N-1)-X(N-2)
      A24 = X(N-1)-X(N-3)
      A34 = X(N-2)-X(N-3)
      J1 = IN
      J2 = J1-INT
      J3 = J2-INT
      J4 = J3-INT
      W(IN)   = (1./A12+1./A13+1./A14)*F(J1)-
     1          A13*A14/(A12*A23*A24)*F(J2)+A12*A14/(A13*A23*A34)*F(J3)-
     2          A12*A13/(A14*A24*A34)*F(J4)
      GO TO 109
  112 CONTINUE
      IW1 = II(I1)
      CALL TRIP (NN,WK(I1,3),WK(I1,1),WK(I1+1,3),WK(I1,2),W(IW1),INT)
      GO TO (114,114,113,114),MK
  113 CONTINUE
      W(1) = W(IN)
  114 CONTINUE
      RETURN
      END


