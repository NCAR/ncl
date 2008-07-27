C
C	$Id: conran.f,v 1.5 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONRAN (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C    SUBROUTINE CONRAN(XD,YD,ZD,NDP,WK,IWK,SCRARR)
C    Standard and smooth versions of CONRAN
C
C          DIMENSION OF  XD(NDP),YD(NDP),ZD(NDP),WK(13*NDP)
C             ARGUMENTS  IWK((27+NCP)*NDP),SCRARR(RESOLUTION**2)
C                        where NCP = 4 and RESOLUTION = 40 by
C                        default.
C
C              OVERVIEW  CONRAN  performs  contouring  of  irregularly
C                        distributed  data.   It is the standard and
C                        smooth members of the CONRAN family. This
C                        version  will  plot contours; smooth them using
C                        splines  under  tension (if the package DASHSMTH
C                        is loaded); plot a perimeter or grid; title the
C                        plot; print a message giving the contour intervals
C                        below the map; plot the input data on the map;
C                        and label the contour lines.
C
C               PURPOSE  CONRAN  plots  contour  lines  using  random,
C                        sparse  or  irregular data sets.  The data is
C                        triangulated and then contoured.   Contouring
C                        is  performed using interpolation of the tri-
C                        angulated data.  There are two methods of
C                        interpolation:  C1 surfaces and linear.
C
C                 USAGE  CALL CONRAN(XD,YD,ZD,NDP,WK,IWK,SCRARR)
C                        An option setting routine  can  also  be  in-
C                        voked,  see  writeup  below.   FRAME  must be
C                        called by the user.
C
C                        If different colors (or intensities) are to be
C                        used for normal intensity, low intensity or
C                        text output, then the values in common block
C                        RANINT should be changed:
C
C                        IRANMJ  Color index for normal (major) intensity
C                                lines.
C                        IRANMN  Color index for low intensity lines
C                        IRANTX  Color index for text (labels)
C
C
C             ARGUMENTS
C
C              ON INPUT  XD
C                            Array of dimension NDP containing the  X-
C                            coordinates of the data points.
C
C                        YD
C                            Array of dimension NDP containing the  Y-
C                            coordinates of the data points.
C
C                        ZD
C                            Array of  dimension  NDP  containing  the
C                            data values at the points.
C
C                        NDP
C                            Number of  data  points  (must  be  4  or
C                            greater) to be contoured.
C
C                        WK
C                            Real work array  of  dimension  at  least
C                            13*NDP
C
C                        IWK
C                            Integer work array.  When using C1 surfaces
C                            the array must be at least IWK((27+NCP)*NDP).
C                            When using linear interpolation the array
C                            must be at least IWK((27+4)*NDP).
C
C                        SCRARR
C                            Real work array  of  dimension  at  least
C                            (RESOLUTION**2)   where   RESOLUTION   is
C                            described in the SSZ option below.  RESO-
C                            LUTION is 40 by default.
C
C             ON OUTPUT  All arguments  remain  unchanged  except  the
C                        scratch arrays IWK, WK, and SCRARR which have
C                        been written into.  If making  multiple  runs
C                        on  the same triangulation IWK and WK must be
C                        saved and returned to the next invocation  of
C                        CONRAN.
C
C          ENTRY POINTS  CONRAN, CONDET, CONINT, CONCAL, CONLOC, CONTNG,
C                        CONDRW, CONCLS, CONSTP, CONBDN, CONBDNX,
C                        CONTLK, CONPDV, CONOP1, CONOP2, CONOP3, CONOP4,
C                        CONXCH, CONREO, CONCOM, CONCLD, CONPMM,
C                        CONGEN, CONLOD, CONECD, CONOUT, CONOT2,
C                        CONSLD, CONLCM, CONLIN, CONDSD, CONSSD
C
C         COMMON BLOCKS  CONRA1, CONRA2, CONRA3, CONRA4, CONRA5, CONRA6,
C                        CONR18, CONR19, CONRA9, CONR10, CONR20, CONR12,
C                        CONR13, CONR14, CONR15, CONR16, CONR17, RANINT
C                        INTPR from the DASH package
C
C                   I/O  Plots the contour map and, via  the ERPRT77
C                        package,  outputs  messages  to  the message
C                        output  unit; at  NCAR  this  unit  is  the
C                        printer.  The option values are all listed on
C                        standard ERPRT77 output unit; at NCAR this
C                        unit is the printer.
C
C             PRECISION  Single
C
C      REQUIRED LIBRARY  Standard version: DASHCHAR, which at NCAR is
C              ROUTINES  loaded by default.
C                        Smooth version: DASHSMTH which must be
C                        requested at NCAR.
C                        Both versions require CONCOM, CONTERP, GRIDAL
C                        the ERPRT77 package, and the SPPS.
C
C    REQUIRED GKS LEVEL  0A
C
C              LANGUAGE  FORTRAN77
C
C               HISTORY
C
C             ALGORITHM  The sparse data is triangulated and a virtual
C                        grid  is  laid  over  the  triangulated area.
C                        Each virtual grid point receives an  interpo-
C                        lated  value.   The  grid is scanned once for
C                        each contour level and all contours  at  that
C                        level are plotted.
C                        There are two methods of interpolation. The
C                        first is a smooth data interpolation
C                        scheme based  on  Lawson's  C1
C                        surface  interpolation  algorithm,  which has
C                        been refined  by  Hirosha  Akima.   Parts  of
C                        Akima's  algorithm  are used in this package.
C                        See the "REFERENCE" section below.
C                        The second is a linear interpolation scheme.
C                        When data is sparse it is usually better to
C                        use the C1 interpolation.  If you have dense
C                        data (over 100 points) then the linear
C                        interpolation will give the better results.
C
C           PORTABILITY  ANSI FORTRAN
C
C
C             OPERATION  CALL CONRAN (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C                        FRAME must be called by the user.
C
C                        CONRAN has many options, each of which may
C                        be changed by calling one of the four
C                        subroutines CONOP1, CONOP2, CONOP3, or
C                        CONOP4.  The number of arguments to each
C                        CONOP routine is the same as the final
C                        suffix character in the routine's name.
C
C                        The CONOP routines are called before CONRAN
C                        is called, and values set by these calls
C                        continue to be in effect until they are
C                        changed by another call to a CONOP routine.
C
C                        All the CONOP routines have as their first
C                        argument a character string to identify the
C                        option being changed.  This is the only
C                        argument to CONOP1.  CONOP2 has an integer
C                        second argument.  CONOP3 has a real array (or
C                        constant) as its second argument and an
C                        integer (usually the dimension of the
C                        array) as its third argument.  CONOP4 has a
C                        character string as its second argument and
C                        integers for the third and fourth arguments.
C
C                        Only the first two characters on each side of
C                        the equal sign are scanned.  Therefore only 2
C                        characters  for  each  option are required on
C                        input to CONOP (i.e.  'SCA=PRI'  and  'SC=PR'
C                        are equivalent.)
C
C                        Remember, there must be at least 4 data points.
C                        This is equal to  the  default  number of
C                        data points to be used for estimation of par-
C                        tial derivatives at  each  data  point.
C                        The estimated partial derivatives  are
C                        used  for the construction of the interpolat-
C                        ing polynomial's coefficients.
C
C                        Listed below are options which can enhance
C                        your plot.  An example of an appropriate
C                        CONOP call is given for each option.  A
C                        complete list of default settings follows
C                        the last option.
C
C               OPTIONS
C
C                   CHL  This flag determines how  the  high  and  low
C                        contour values are set.  These contour values
C                        may be set by the program or by the user.  If
C                        CHL=OFF,  the program examines the user's in-
C                        put data and determines both the high and low
C                        values.  If CHL=ON, the user must specify the
C                        desired high (HI) and low (FLO) values.
C                        The default is CHL=OFF.
C
C                        If program set:   CALL CONOP3('CHL=OFF',0.,0)
C
C                        If user set:  CALL CONOP3('CHL=ON',ARRAY,2)
C                                where ARRAY(1)=HI, ARRAY(2)=FLO
C
C                        Note: The values supplied for contour  incre-
C                        ment  and contour high and low values assumes
C                        the unscaled data values.  See the SDC  flag,
C                        below.
C
C                        Example:  CALL   CONOP3('CHL=ON',ARRAY,2)
C                                  where  ARRAY(1)=5020. (the desired
C                                  high contour value) and ARRAY(2)=
C                                  2000 (the desired low contour value).
C                                  These are floating point numbers.
C
C                   CIL  This flag determines how the  contour  incre-
C                        ment  (CINC) is set.  The increment is either
C                        calculated by the program (CIL=OFF) using the
C                        range  of high and low values from the user's
C                        input data, or set by the user (CIL=ON). The
C                        default is CIL=OFF.
C
C                        If program set:   CALL CONOP3('CIL=OFF',0.,0)
C
C                        If user set:      CALL CONOP3('CIL=ON',CINC,1)
C
C                        Note: By default,  the  program  will examine
C                        the user's input data and determine the contour
C                        interval (CINC)  at some appropriate range between
C                        the level of high and low values supplied, usually
C                        generating between 15 and 20 contour levels.
C
C                        Example:          CALL CONOP3('CIL=ON',15.,1)
C                                          where  15.  represents  the
C                                          contour  increment  desired
C                                          by the user.
C
C                   CON  This flag determines how the  contour  levels
C                        are  set.   If  CON=ON, the user must specify
C                        the array of contour values and the number of
C                        contour levels.  A  maximum of 30 contour (NCL)
C                        levels is permitted.  If CON=OFF,  default
C                        values  are  used.  In this case, the program
C                        will calculate the values for the  array  and
C                        NCL using input data.  The default is OFF.
C
C                        If program set:   CALL CONOP3('CON=OFF',0.,0)
C
C                        If user set:   CALL CONOP3('CON=ON',ARRAY,NCL)
C
C                        Note: The array (ARRAY) contains the  contour
C                        levels  (floating  point only) and NCL is the
C                        number of levels.  The maximum number of con-
C                        tour levels allowed is 30.  When assigning
C                        the array of contour values, the values must
C                        be ordered from smallest to largest.
C
C                        Example:
C                         DATA RLIST(1),...,RLIST(5)/1.,2.,3.,10.,12./
C
C                                  CALL CONOP3('CON=ON',RLIST,5) where
C                                  'RLIST' contains the user specified
C                                  contour levels, and 5  is  the
C                                  number  of  user  specified contour
C                                  levels (NCL).
C
C                        Warning on contour options:
C                        It is illegal to use the CON option when
C                        either  CIL  or  CHL  are activated.  If
C                        this is done, the option call that detected
C                        the error will not be executed.
C
C                   DAS  This  flag  determines  which  contours   are
C                        represented  by  dashed lines.  The user sets
C                        the dashed line pattern.  The user may speci-
C                        fy  that  dashed  lines  be used for contours
C                        whose  value  is  less  than,  equal  to,  or
C                        greater than the dash pattern breakpoint (see
C                        the DBP option below), which is zero by
C                        default.  If DAS=OFF (the default value), all
C                        solid lines are used.
C
C                        All solid lines: CALL CONOP4('DAS=OFF',' ',0,0)
C
C                        If greater:      CALL CONOP4('DAS=GTR',PAT,0,0)
C
C                        If equal:        CALL CONOP4('DAS=EQU',PAT,0,0)
C
C                        If less:         CALL CONOP4('DAS=LSS',PAT,0,0)
C
C                        If all same:     CALL CONOP4('DAS=ALL',PAT,0,0)
C
C                        Note: Pat must be a ten character
C                        string with a dollar sign ($) for solid and a
C                        single quote (') for blank.  Recall that in
C                        FORTRAN 77, in a quoted string a single quote
C                        is represented by two single quotes ('').
C
C                        Example:
C                          CALL CONOP4('DAS=GTR','$$$$$''$$$$',0,0)
C
C                   DBP  This flag determines  how  the  dash  pattern
C                        break point (BP) is set.  If DBP=ON, BP  must
C                        be set by the  user  by  specifying  BP.   If
C                        DBP=OFF  the  program  will set BP to the
C                        default value which is zero.
C
C                        If program set:   CALL CONOP3('DBP=OFF',0.,0)
C
C                        If user set:      CALL CONOP3('DBP=ON',BP,1)
C
C                        Note: BP is a floating point number where the
C                        break  for  GTR and LSS contour dash patterns
C                        are defined.  BP is assumed to be given rela-
C                        tive to the untransformed contours.
C
C                        Example:          CALL CONOP3('DBP=ON',5.,1)
C                                          where 5. is the user speci-
C                                          fied break point.
C
C                   DEF  Reset flags to  default  values.   Activating
C                        this  option  sets  all  flags to the default
C                        value.  DEF has no 'ON' or 'OFF' states.
C
C                        To activate:   CALL CONOP1('DEF')
C
C                   EXT  Flag to set extrapolation. Normally all
C                        CONRAN versions will  only plot the boundaries
C                        of the convex hull defined by the user's data.
C                        To have the contours fill the rectangular
C                        area of the frame, set the EXT switch ON.
C                        The default is OFF.
C
C                        To turn on:    CALL CONOP1('EXT=ON')
C
C                        To turn off:   CALL CONOP1('EXT=OFF')
C
C                   FMT  Flag for the format of the plotted input data
C                        values.   If  FMT=OFF, the default values for
C                        FT, L, and IF are used.  The  default  values
C                        are:
C
C                        FT = '(G10.3)'
C                        L  = 7  characters including the parentheses
C                        IF = 10 characters printed in the output
C                             field by the format
C
C                        If FMT=ON, the user must specify  values  for
C                        FT,  L,  and  IF.   All user specified values
C                        must be given in the correct format.
C
C                        If program set:  CALL CONOP4('FMT=OFF',' ',0,0)
C
C                        If user set:  CALL CONOP4('FMT=ON',FT,L,IF)
C
C                        Note: FT is a character string containing the
C                        format.   The  format  must  be  enclosed  in
C                        parentheses.  Any  format, up to 10 characters
C                        which is  allowed  at  your installation will be
C                        accepted.  L is the number of  characters  in
C                        FT.  IF is the length of the field created by
C                        the format.
C
C                        Example:  CALL CONOP4('FMT=ON','(G30.2)',7,30)
C
C                        Warning: CONRAN will not  test  for  a  valid
C                        format.  The format is only allowed to be
C                        10 characters long.
C
C                   GRI  Flag to display the grid. GRI is OFF by default.
C
C                        To turn on:    CALL CONOP1('GRI=ON')
C
C                        To turn off:   CALL CONOP1('GRI=OFF')
C
C                        Note: If GRI is ON, the virtual grid will
C                        be superimposed over the contour plot.
C                        The X and Y tick intervals will be displayed
C                        under the map only if PER=ON.  (see PER)
C
C                   INT  Flag to determine the intensities of the con-
C                        tour  lines  and other parts of the plot.  If
C                        INT=OFF,  all intensities are set to the default
C                        values.  If INT=ALL, all intensities are set
C                        to the given value, IVAL.  If INT is set to
C                        one of the other possible options (MAJ, MIN,
C                        LAB or DAT), the intensity level for that
C                        option is set to the given value, IVAL.
C
C                        If program set:   CALL CONOP2('INT=OFF',0)
C
C                        All the same:     CALL  CONOP2('INT=ALL',IVAL)
C
C                        Major lines:      CALL CONOP2('INT=MAJ',IVAL)
C
C                        Minor lines:      CALL CONOP2('INT=MIN',IVAL)
C
C                        Title and message:
C                                          CALL  CONOP2('INT=LAB',IVAL)
C
C                        Data values:      CALL  CONOP2('INT=DAT',IVAL)
C
C                        Note: 'INT=DAT' relates to the plotted data
C                        values and the plotted maximums and minimums.
C
C                        Note: IVAL is the intensity desired.  IVAL
C                        values range from 0 to 255 or the character
C                        strings 'LO' and 'HI'.  The default is 'HI'
C                        except for INT=MIN which is set to 'LO'.
C
C                        Example:           CALL CONOP2('INT=ALL',110)
C
C                   ITP  Set the interpolation scheme.
C                        There are two schemes--C1 surfaces and linear.
C                        The C1 method takes longer but will give the
C                        best results when the data is sparse (less
C                        than 100 points).  The linear method will
C                        produce a better plot when there is a dense
C                        data set.  The default is C1 surface.
C
C                        For C1 surface   CALL CONOP1('ITP=C1')
C
C                        For linear       CALL CONOP1('ITP=LIN')
C
C                   LAB  This flag can be set to either label the con-
C                        tours (LAB=ON) or not (LAB=OFF).  The default
C                        value is LAB=ON.
C
C                        To turn on:    CALL CONOP1('LAB=ON')
C
C                        To turn off:   CALL CONOP1('LAB=OFF')
C
C                   LOT  Flag to list options on the printer.  The de-
C                        fault  value  is  set  to OFF, and no options
C                        will be displayed.
C
C                        To turn on:    CALL CONOP1('LOT=ON')
C
C                        To turn off:   CALL CONOP1('LOT=OFF')
C
C                        Note: If  users  want  to  print  the  option
C                        values, they should turn this option ON.  The
C                        option values will be sent  to  the  standard
C                        output  unit  as  defined by the support
C                        routine I1MACH.
C
C                   LSZ  This flag  determines  the  label  size.   If
C                        LSZ=OFF,  the  default ISZLSZ value will be
C                        used.  If LSZ=ON,  the  user  should  specify
C                        ISZLSZ.   The  default value is 9 plotter
C                        address units.
C
C                        If program set:   CALL CONOP2('LSZ=OFF',0)
C
C                        If user set:   CALL CONOP2('LSZ=ON',ISZLSZ)
C
C                        Note: ISZLSZ  is  the  requested  character
C                        size in plotter address units.
C
C                        Example:          CALL CONOP2('LSZ=ON',4)
C                                          where 4 is the user desired
C                                          integer plotter address
C                                          units.
C
C                   MES  Flag to plot a message. The default is ON.
C
C                        To turn on:    CALL CONOP1('MES=ON')
C
C                        To turn off:   CALL CONOP1('MES=OFF')
C
C                        Note: If MES=ON, a message is  printed   below
C                        the  plot giving contour intervals and execu-
C                        tion time in seconds.  If PER or GRI is  ON,
C                        the  message  also  contains the X and Y tick
C                        intervals.
C
C                   NCP  Flag to indicate the number of data points
C                        used for the partial derivative
C                        estimation.   If NCP=OFF, NUM is set to
C                        4, which is the default  value.   If  NCP=ON,
C                        the  user  must  specify  NUM greater than or
C                        equal to 2.
C
C                        If program set:   CALL CONOP2('NCP=OFF',0)
C
C                        If user set:      CALL CONOP2('NCP=ON',NUM)
C
C                        Note: NUM = number of data  points  used  for
C                        estimation.   Changing this value effects the
C                        contours produced and the size of input array
C                        IWK.
C
C                        Example:          CALL CONOP2('NCP=ON',3)
C
C                   PDV  Flag to plot the input data values.  The
C                        default value is PDV=OFF.
C
C                        To turn on:    CALL CONOP1('PDV=ON')
C
C                        To turn off:   CALL CONOP1('PDV=OFF')
C
C                        Note: If PDV=ON, the input  data  values  are
C                        plotted  relative  to  their  location on the
C                        contour map.  If you only wish to see the
C                        locations  and  not the values, set PDV=ON and
C                        change FMT to produce an asterisk (*) such as
C                        (I1).
C
C                   PER  Flag to set the perimeter.  The default value
C                        is  PER=ON,  which  causes  a perimeter to be
C                        drawn around the contour plot.
C
C                        To turn on:    CALL CONOP1('PER=ON')
C
C                        To turn off:   CALL CONOP1('PER=OFF')
C
C                        Note: If MES is ON, the X and Y tick intervals
C                        will  be given.  These are the intervals in user
C                        coordinates that each tick mark represents.
C
C                   PMM  Flag to plot relative minimums and maximums.
C                        This flag is OFF by default.
C
C                        To turn off:      CALL CONOP1('PMM=OFF')
C
C                        To turn on:       CALL CONOP1('PMM=ON')
C
C                   PSL  Flag which sets the plot shield option.
C                        The outline of the shield will be drawn on
C                        the same frame as the contour plot.
C                        By default this option is OFF.
C                        (see SLD option).
C
C                        Draw the shield:  CALL CONOP1('PSL=ON')
C
C                        Don't draw it:    CALL CONOP1('PSL=OFF')
C
C                   REP  Flag indicating the use of the same data in
C                        a new execution.  The default value is OFF.
C
C                        To turn on:    CALL CONOP1('REP=ON')
C
C                        To turn off:   CALL CONOP1('REP=OFF')
C
C                        Note: If REP=ON, the same X-Y data and triangu-
C                        lation  are  to  be  used but it is assumed
C                        the user has changed contour values or resolution
C                        for this run.  Scratch arrays WK and IWK must
C                        remain unchanged.
C
C                   SCA  Flag for scaling of the plot on a frame.
C                        This flag is ON by default.
C
C                        User scaling:     CALL CONOP1('SCA=OFF')
C
C                        Program scaling:  CALL CONOP1('SCA=ON')
C
C                        Prior window:     CALL CONOP1('SCA=PRI')
C
C                        Note:  With  SCA=OFF,  plotting  instructions
C                        will be issued using the user's input coordi-
C                        nates, unless they are transformed via FX and
C                        FY  transformations.   Users will find an
C                        extended discussion in the "INTERFACING WITH
C                        OTHER GRAPHICS ROUTINES" section below.  The SCA
C                        option  assumes  that all input data falls into
C                        the current window setting.  With SCA=ON, the
C                        entry point will establish a viewport so that
C                        the user's plot will fit into the  center  90
C                        percent of the frame.  When SCA=PRI, the
C                        program  maps the user's plot instructions  into
C                        the  portion of the frame defined by the
C                        current normalization transformation.  SCA=OFF
C                        should be used to interface with EZMAP.
C
C                   SDC  Flag to determine how to scale  the  data  on
C                        the contours.  If SDC=OFF, the floating point
C                        value is given by scale.  If SDC=ON, the user
C                        may specify SCALE.  The default value for SCALE
C                        is 1.
C
C                        If program set:   CALL CONOP3('SDC=OFF',0.,0)
C
C                        If user set:      CALL CONOP3('SDC=ON',SCALE,1)
C
C                        Note: The data plotted on contour  lines  and
C                        the  data  plotted  for relative minimums and
C                        maximums will be scaled by the floating point
C                        value  given  by SCALE.  Typical SCALE values
C                        are 10., 100., 1000., etc.  The original data
C                        values are multiplied by SCALE.  SCALE must be
C                        a floating point number and is displayed in the
C                        message  (see  MES).
C
C                        Example:          CALL CONOP2('SDC=ON',100.,1)
C
C                   SLD  Activate or deactivate the shielding option.
C                        When this option is activated,  only those
C                        contours within the shield are drawn. The shield
C                        is a polygon specified by the user which must
C                        be given in the same coordinate range as the
C                        the data. It must define only one polygon.
C
C                        To activate the shield:
C                               CALL CONOP3('SLD=ON',ARRAY,ICSD)
C
C                        To deactivate the shield:
C                               CALL CONOP3('SLD=OFF',0.,0)
C
C                        Note:  ARRAY is a real array ICSD elements long.
C                        The first ICSD/2 elements are X coordinates and
C                        the second ICSD/2 elements are Y coordinates.
C                        ICSD is the length of entire array, the
C                        number of (X + Y) shield coords. The polygon
C                        must be closed, that is the first and last
C                        points describing it must be the same.
C
C                        Example:       DIMENSION SHLD
C                                       DATA SHLD/ 7.,10.,10.,7.,7.,
C                                      1           7.,7.,10.,10.,7./
C                                       CALL CONOP3 (6HSLD=ON,SHLD,10)
C
C
C                   SML  Flag to determine the  size  of  minimum  and
C                        maximum contour labels.  If SML=OFF, the
C                        ISZSML default value of  15 is  used.
C                        If SML=ON, the user must specify ISZSML.
C
C                        If program set:   CALL CONOP2('SML=OFF',0)
C
C                        If user set:  CALL CONOP2('SML=ON',ISZSML)
C
C                        Note: ISZSML is an integer number which  is
C                        the size of labels in plotter address units
C                        as defined in the SPPS entry WTSTR.
C
C                        Example:      CALL CONOP2('SML=ON',12)
C
C                   SPD  Flag for the size of the plotted  input  data
C                        values.  If SPD=OFF, the value of ISZSPD is
C                        8, which is the default.  If SPD=ON, the user
C                        must specify ISZSPD.
C
C                        If program set:   CALL CONOP2('SPD=OFF',0)
C
C                        If user set:   CALL CONOP2('SPD=ON',ISZSPD)
C
C                        Note: ISZSPD is an integer number giving the
C                        size to plot the data values in plotter address
C                        units as defined in the SPPS entry WTSTR.    .
C
C                        Example:          CALL CONOP2('SPD=ON',6)
C
C                   SSZ  Flag to determine the resolution  (number  of
C                        steps  in  each  direction).   If SSZ=ON, the
C                        user sets ISTEP, or, if SSZ=OFF, the  program
C                        will  automatically  set ISTEP at the default
C                        value of 40.
C
C                        If program set:   CALL CONOP2('SSZ=OFF',0)
C
C                        If user set:      CALL CONOP2('SSZ=ON',ISTEP)
C
C                        Note: ISTEP is an integer specifying the density
C                        of the virtual grid. In most cases, the default
C                        value of 40 produces pleasing contours.   For
C                        coarser   but  quicker  contours,  lower  the
C                        value.  For smoother contours at
C                        the expense of taking a longer  time,  raise
C                        the  value.   Note:   For  step sizes greater
C                        than 200 in CONRAN, the arrays  PV  in  common
C                        CONRA1  and ITLOC in common CONRA9,  must be
C                        expanded to about 10 more than ISTEP.
C                        See CONRA1 and CONRA9 comments below for more
C                        information.
C
C                        Example:          CALL    CONOP2('SSZ=ON',25)
C                                          This  ISTEP value will pro-
C                                          duce a coarse contour.
C
C                   STL  Flag to determine  the  size  of  the  title.
C                        ISZSTL  may be set by the user (STL=ON), or
C                        the program will set it to the  default  size
C                        of 16 plotter address units (STL=OFF).
C
C                        If program set:   CALL CONOP2('STL=OFF',0)
C
C                        If user set:  CALL  CONOP2('STL=ON',ISZSTL)
C
C                        Note: When 30 or 40 characters are  used  for
C                        the title, the default size of 16 plotter
C                        address units works well.  For longer titles,
C                        a smaller title size is required.
C
C                        Example:          CALL CONOP2('STL=ON',13)
C
C                   TEN  Flag to determine the tension factor  applied
C                        when  smoothing  contour lines.  The user may
C                        set TENS or allow  the  program  to  set  the
C                        value.   If  user set, TENS must have a value
C                        greater than zero and less than or  equal  to
C                        30.  The default value is 2.5.
C
C                        If program set:   CALL CONOP3('TEN=OFF',0.,0)
C
C                        If user set:      CALL CONOP3('TEN=ON',TENS,1)
C
C                        Note: TENS is not available in the standard
C                        version of CONRAN.
C                        Smoothing of contour  lines  is  accomplished
C                        with  splines  under  tension.  To adjust the
C                        amount of smoothing applied, adjust the  ten-
C                        sion  factor.   Setting TENS very large
C                        (i.e. 30.), effectively shuts off smoothing.
C
C                        Example:          CALL CONOP3('TEN=ON',14.,1)
C
C                   TFR  Flag to advance the frame before triangulation.
C                        The default value is TFR=ON, which means that
C                        the contours and the triangles will be plotted
C                        on separate frames.
C
C                        If program set:   CALL CONOP1('TFR=ON')
C
C                        To turn off:      CALL CONOP1('TFR=OFF')
C
C                        Note: Triangles are plotted  after  the  con-
C                        touring  is completed.  To see the triangles
C                        over  the  contours,  turn this switch off.
C
C                   TLE  Flag to place a title at the top of the plot.
C                        If  TLE=ON,  the user must specify CHARS and
C                        INUM.  CHARS  is the character string containing
C                        the title.  INUM is the number of characters
C                        in CHARS.  The default value is  OFF.
C
C                        To turn on: CALL CONOP4('TLE=ON',CHARS,INUM,0)
C
C                        To turn off:   CALL CONOP4('TLE=OFF',' ',0,0)
C
C                        Note: If longer than 64-character titles are
C                        desired, the character variable ISTRNG found
C                        in CONR18 must be increased appropriately.
C
C                        Example: CALL CONOP4('TLE=ON','VECTOR REVIEW'
C                                       ,13,0)
C
C                   TOP  Flag to plot only the triangles.
C
C                        To turn off:   CALL CONOP1('TOP=OFF')
C
C                        To turn on:    CALL CONOP1('TOP=ON')
C
C                        Note: The user may wish to overlay the trian-
C                        gles on some other plot.  'TOP=ON' will
C                        allow  that.   This  option  when   activated
C                        (TOP=ON),  will  set TRI=ON, and TFR=OFF.  If
C                        the user wants TFR=ON, it should be set after
C                        TOP is set.  If the user sets TOP=OFF it will
C                        set TRI=OFF and TFR=ON. If the user wants TRI
C                        or  TFR  different, set them after the
C                        TOP call.
C
C                   TRI  Flag to plot the triangulation.  The default is
C                        OFF and therefore the triangles are not drawn.
C
C                        To turn on:    CALL CONOP1('TRI=ON')
C
C                        To turn off:   CALL CONOP1('TRI=OFF')
C
C                        Note: Plotting the triangles will indicate to
C                        the  user where good and bad points of inter-
C                        polation are occurring in  the  contour  map.
C                        Equilateral  triangles are optimal for inter-
C                        polation.  Quality degrades as triangles
C                        approach  a  long and narrow shape.  The convex
C                        hull of the  triangulation  is  also  a  poor
C                        point of interpolation.
C
C        OPTION DEFAULT  Below are  listed  the  default
C                VALUES  values  for  the various options given above.
C                        Unless the user  specifies  otherwise,  these
C                        values will be used in execution of the vari-
C                        ous options.
C
C                           CHL=OFF  LOT=OFF  SLD=OFF  TRI=OFF
C                           CIL=OFF  LSZ=OFF  SML=OFF
C                           CON=OFF  MES=ON   SPD=OFF
C                           DAS=OFF  NCP=OFF  SPT=OFF
C                           DBP=OFF  PDV=OFF  SSZ=OFF
C                           EXT=OFF  PER=ON   STL=OFF
C                           FMT=OFF  PMM=OFF  TEN=OFF
C                           GRI=OFF  REP=OFF  TFR=ON
C                           ITP=C1   SCA=ON   TLE=OFF
C                           LAB=ON   SDC=OFF  TOP=OFF
C
C    DEFAULT VALUES FOR  The option default  values  given  above,  if
C        USER SPECIFIED  used, will set default values for the follow-
C            PARAMETERS  ing parameters:
C
C                        PARAMETER    DEFAULT
C                        ---------    -------
C
C                        ARRAY        Up to 30 contour levels allowed.
C                                     Values  are computed by the
C                                     program, based on input.
C
C                        BP           0.
C
C                        CINC         Computed by the program based on the
C                                     range of HI and LO values of the
C                                     input data.
C
C                        FLO          Computed by the program based on the
C                                     lowest unscaled input data.
C
C                        FT           (G10.3)  Parentheses must be
C                                     included.
C
C                        HI           Computed by the program based on the
C                                     highest unscaled input data.
C
C                        CHARS        No title
C
C                        IF           10 characters
C
C                        INUM         No title
C
C                        IPAT         '$$$$$$$$$$'  (This is a 10 character
C                                     string.)
C
C                        ISZLSZ       9 plotter address units
C
C                        ISZSML       15 plotter address units
C
C                        ISZSPD       8 plotter address units
C
C                        ISZSTL       16 plotter address units
C
C                        ISTEP        40
C
C                        IVAL         'HI' for all except  minor  con-
C                                     tour   lines   which  are  'LO'.
C
C                        L            7  characters  (including   both
C                                     parentheses)
C
C                        NCL          Computed by the program based on
C                                     input data.  Up to 30 contour
C                                     levels are permitted.
C
C                        NUM          4 data points
C
C                        SCALE        1. (no scaling performed)
C
C                        TENS         2.5
C
C                        ICSD         0 (no shield)
C
C         OPTIONS WHICH  The shape of the contours may be modified by
C            EFFECT THE  changing NCP  and SSZ.  NCP controls the
C              CONTOURS  number of data points to be used in the
C                        interpolation.  Increasing NCP causes more
C                        of the  surrounding  data  to influence the
C                        point of interpolation.  Some  datasets cause
C                        difficulty  when  trying  to produce meaningful
C                        contours (triangles which are long and  narrow).
C                        By modifying NCP a user can fine-tune a
C                        plot.   Increasing  ISTEP, the density of the
C                        virtual grid, will  smooth out the contour
C                        lines and pick up  more  detail  (new  contours
C                        will appear as ISTEP increases and old ones will
C                        sometimes break into more  distinct units).
C                        ISTEP is changed by the SSD option.
C
C                  NOTE  If NCP.GT.25, arrays DSQ0 and IPC0 in CONDET
C                        must  be adjusted accordingly.  Also NCPSZ in
C                        CONBDNX (25 by default), must be increased to
C                        NCP.   The  default  value of NCP, which is 4,
C                        produces pleasing  pictures  in  most  cases.
C                        However, fine-tuning of the interpolation can
C                        be obtained by increasing the  size  of  NCP,
C                        with  a corresponding linear increase in work
C                        space.
C
C                        The interpolation method used will also cause
C                        different looking contours.  The C1 method
C                        is recommended when the data is sparse.  It
C                        will smooth the data and add trends (false
C                        hills and valleys).  The linear method is
C                        recommended when data is dense (GT 50 to 100)
C                        it will not smooth the data or add trends.
C
C      INTERFACING WITH  Normally the scaling factor will be set to OFF.
C        OTHER GRAPHICS  In most cases mapping can be performed before
C              ROUTINES  calling the CONRAN entry point, thus saving the
C                        user from  modifying the file.  If reasonable
C                        results cannot be obtained, the statement
C                        functions, FX and FY, will have to be replaced.
C                        The routines having these statement functions
C                        are:
C
C                           CONDRW, CONPDV, CONTLK, CONPMS, CONGEN
C
C            REFERENCES  Akima, Hirosha
C                           A Method of Bivariate Interpolation and
C                           Smooth Surface Fitting for Irregularly
C                           Distributed Data Points.
C                           ACM Transactions on Mathematical Software
C                           vol 4, no. 2, June 1978, pages 148-159
C                        Lawson, C.L.
C                           Software for C1 Surface Interpolation
C                           JPL Publication 77-30
C                           August 15, 1977
C
C     CONRAN ERROR  ERROR  ROUTINE               MESSAGE
C       MESSAGES
C                    1      CONRAN         INPUT PARAMETER NDP LT NCP
C                    2      CONRAN         NCP GT MAX SIZE OR LT 2
C                    3      CONTNG         ALL COLINEAR DATA POINTS
C                    4      CONTNG         IDENTICAL INPUT DATA POINTS
C                                          FOUND
C                    5      CONOP          UNDEFINED OPTION
C                    6      CONCLS         CONSTANT INPUT FIELD
C                    7      CONOP          INCORRECT CONOP CALL USED
C                    8      CONOP          ILLEGAL USE OF CON OPTION
C                                          WITH CIL OR CHL OPTIONS
C                    9      CONOP          NUMBER OF CONTOUR LEVELS
C                                          EXCEEDS 30
C                    10     CONDRW         CONTOUR STORAGE EXHAUSTED
C                                          This error is trapped and
C                                          nullified by CONRAN.  It
C                                          serves to signal the user
C                                          that a contour level may not
C                                          be complete.
C                    11     CONSTP         ASPECT RATIO OF X AND Y
C                                          GREATER THAN 5 TO 1.
C                                          (This error may cause a poor
C                                          quality plot.  Usually this
C                                          can be fixed by multiplying
C                                          X or Y by a constant factor.
C                                          If this solution is
C                                          unacceptable then increasing
C                                          SSZ to a very large value
C                                          may help.  Note:  This can be
C                                          expensive.)
C
C                     The errors listed above are defined as recoverable
C                     errors should the user wish to use them in that
C                     fashion.  The documentation on the ERPRT77 package
C                     explains how to recover from an error.
C
C
C Note:  The common blocks listed include all the common used by
C        the entire CONRAN family.  Not all members will use all
C        the common variables.
C
C   CONRA1
C       CL-array of contour levels
C       NCL-number of contour levels
C       OLDZ-Z value of left neighbor to current location
C       PV-array of previous row values
C       HI-largest contour plotted
C       FLO-lowest contour plotted
C       FINC-increment level between equally spaced contours
C   CONRA2
C       REPEAT-flag to triangulate and draw or just draw
C       EXTRAP-plot data outside of convex data hull
C       PER-put perimeter around plot
C       MESS-flag to indicate message output
C       ISCALE-scaling switch
C       LOOK-plot triangles flag
C       PLDVLS-plot the data values flag
C       GRD-plot grid flag
C       CON-user set or program set contours flag
C       CINC-user or program set increment flag
C       CHILO-user or program set hi low contours
C       LABON-flag to control labeling of contours
C       PMIMX-flag to control the plotting of min's
C             and max's
C       SCALE-the scale factor for contour line values
C             and min, max plotted values
C       FRADV-advance frame before plotting triangulation
C       EXTRI-only plot triangulation
C       BPSIZ-breakpoint size for dashpatterns
C       LISTOP-list options on UNIT6 flag
C   CONRA3
C       IRED-ERPRT77 recoverable error flag
C   CONRA4
C       NCP-number of data points used at each point for
C           polynomial construction.
C       NCPSZ-max size allowed for NCP
C   CONRA5
C       NIT-flag to indicate status of search data base
C       ITIPV-last triangle interpolation occurred in
C  CONRA6
C       XST-X coordinate start point for contouring
C       YST-Y coordinate start point for contouring
C       XED-X coordinate end point for contouring
C       YED-Y coordinate end point for contouring
C       STPSZ-step size for X,Y change when contouring
C       IGRAD-number of graduations for contouring (step size)
C       IG-reset value for IGRAD
C       XRG-X range of coordinates
C       YRG-Y range of coordinates
C       BORD-percent of frame used for contour plot
C       PXST-X plotter start address for contours
C       PYST-Y plotter start address for contours
C       PXED-X plotter end address for contours
C       PYED-Y plotter end address for contours
C       ITICK-number of tick marks for grids and perimeters
C CONR18
C       TITLE-switch to indicate if title option ON or OFF
C       ISTRNG-character string containing the title
C       ICNT-character count of ISTRNG
C       ITLSIZ-size of title in PWRIT units
C CONR19
C       IHIGH-default intensity setting
C       INMAJ-contour Level intensity for major lines
C       INMIN-contour Level intensity for minor lines
C       INLAB-title and message intensity
C       INDAT-data value intensity
C       FORM-the format for plotting the data values
C       LEN-the number of characters in the format
C       IFMT-size of the format field
C       LEND-default format length
C       IFMTD-default format field size
C       ISIZEP-size of the plotted data values
C  CONRA9
C       X-array of X coordinates of contours drawn at current contour
C          level
C       Y-array of Y coordinates of contours drawn at current contour
C          level
C       NP-count in X and Y
C       MXXY-size of X and Y
C       TR-top right corner value of current cell
C       BR-bottom right corner value of current cell
C       TL-top left corner value of current cell
C       BL-bottom left corner value of current cell
C       CONV-current contour value
C       XN-X position where contour is being drawn
C       YN-Y position where contour is being drawn
C       ITLL-triangle where top left corner of current cell lies
C       IBLL-triangle of bottom left corner
C       ITRL-triangle of top right corner
C       IBRL-triangle of bottom right corner
C       XC-X coordinate of current cell
C       YC-Y coordinate of current cell
C       ITLOC-in conjunction with PV stores the triangle where PV
C             value came from
C CONR10
C       NT-number of triangles generated
C       NL-number of line segments
C       NTNL-NT+NL
C       JWIPT-pointer into IWK where where triangle point numbers
C             are stored
C       JWIWL-in IWK the location of a scratch space
C       JWIWP-in IWK the location of a scratch space
C       JWIPL-in IWK the location of end points for border line
C             segments
C       IPR-in WK the location of the partial derivatives at each
C           data point
C       ITPV-the triangle where the previous value came from
C CONR20
C       NREP-number of repetitions of dash pattern before a label
C       NCRT-number of CRT units for a dash mark or blank
C       ISIZEL-size of contour line labels
C       NDASH-array containing the negative valued contour dash
C             pattern
C       MINGAP-number of unlabeled lines between each labeled one
C       IDASH-positive valued contour dash pattern
C       ISIZEM-size of plotted minimums and maximums
C       EDASH-equal valued contour dash pattern
C       TENS-default tension setting for smoothing
C CONR12
C       IXMAX,IYMAX-maximum X and Y coordinates relative to the
C                 scratch array, SCRARR
C       XMAX,YMAX-maximum X and Y coordinates relative to users
C                 coordinate space
C CONR13
C       XVS-array of the X coordinates for shielding
C       YVS-array of the Y coordinates for shielding
C       IXVST-pointer to the users X array for shielding
C       IYVST-pointer to the users Y array for shielding
C       ICOUNT-count of the shield elements
C       SPVAL-special value used to halt contouring at the shield
C               boundary
C       SHIELD-logical flag to signal status of shielding
C       SLDPLT-logical flag to indicate status of shield plotting
C CONR14
C       LINEAR-C1 linear interpolating flag
C CONR15
C       ISTRNG-title of the plot
C CONR16
C       FORM-Format used for data
C CONR17
C       NDASH-Dash pattern used for contour lines less than BP
C       IDASH-Dash pattern used for contour lines greater than BP
C       EDASH-Dash pattern used for contour lines equal to the BP
C RANINT
C       IRANMJ-color index for normal (major) intensity lines
C       IRANMN-color index for low intensity lines
C       IRANMJ-color index for text (labels)
C
C
      DIMENSION       LNGTHS(4)  ,HOLD(4)
        CHARACTER*110   IWORK
        CHARACTER*13    ENCSCR,  ENSCRY
        CHARACTER*1     ICHAR
        CHARACTER*500   DPAT
        REAL            WIND(4),VIEW(4),NWIND(4),NVIEW(4),RECT(4)
      DIMENSION       XD(*)      ,YD(*)      ,ZD(*)      ,WK(*)      ,
     1                IWK(*)     ,SCRARR(*)
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONR18/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONR19/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR20/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
      COMMON /CONR14/LINEAR
      LOGICAL LINEAR
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
        COMMON /RANINT/ IRANMJ, IRANMN, IRANTX
        INTEGER OPLASF, OTXASF, LASF(13), OCOLI, OTEXCI
        SAVE
C
C
        DATA LNGTHS(1),LNGTHS(2),LNGTHS(3),LNGTHS(4)/13,4,21,6/
C
C ICONV CONVERT FORM 0-32767 TO 1-1024
C
        DATA ICONV/32/
C
C IABOVE AMOUNT TITLE IS PLACED ABOVE PLOT
C IBELOW, IBEL2 AMOUNT MESSAGE IS BELOW PLOT
C
        DATA IABOVE,IBELOW,IBEL2/30,-30,-45/
C
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL CONBDN
C
C  THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('NSSL','CONRAN','CONRAN','VERSION 01')
C
C  LIST THE OPTION VALUES IF REQUESTED
C
      IF (LISTOP) CALL CONOUT (2)
C
C  SET SWITCH TO MAP TRIANGLES, IN CONLOC, FOR QUICK SEARCHES
C
      NIT = 0
C
C  TEST TO SEE IF ENOUGH INPUT DATA
C
      IF (NDP.GE.NCP) GO TO    10
      CALL SETER (' CONRAN - INPUT PARAMETER NDP LESS THAN NCP',1,
     1             IREC)
      RETURN
C
   10 IF (NCPSZ.GE.NCP .AND. NCP.GE.2) GO TO    20
      CALL SETER (' CONRAN - NCP LT 2 OR GT NCPSZ',2,IREC)
C
   20 IWK(1) = NDP
      IWK(2) = NCP
      IWK(3) = 1
C
C  SET POLYLINE COLOR ASF TO INDIVIDUAL
C
        CALL GQASF(IERR,LASF)
        OPLASF = LASF(3)
        LASF(3) = 1
        OTXASF = LASF(10)
        LASF(10) = 1
        CALL GSASF(LASF)
C
C  INQUIRE CURRENT POLYLINE AND TEXT COLOR
C
        CALL GQPLCI(IERR,OCOLI)
        CALL GQTXCI(IERR,OTEXCI)
C
C  SET POLYLINE AND TEXT COLOR TO VALUE IN COMMON
C
        CALL GSPLCI(IRANMJ)
      CALL GSTXCI(IRANTX)
C
C CONSTRUCTION OF WORK SPACE POINTERS
C
C TRIANGLE POINT NUMBERS
C
      JWIPT = 16
C
C SCRATCH SPACE
C
      JWIWL = 6*NDP + 1
C
C  END POINTS OF BORDER LINE SEGMENTS AND TRIANGLE NUMBER
C
      JWIPL = 24*NDP + 1
C
C  POINT NUMBERS WHERE THE NCP DATA POINTS AROUND EACH POINT
C
      JWIPC = 27*NDP + 1
C
C  SCRATCH SPACE
C
      JWIWP = 30*NDP + 1
C
C  PARTIAL DERIVATIVES AT EACH DATA POINT
C
      IPR = 8*NDP + 1
C
C  TEST IF REPEAT (JUST NEW CONTOURS OF INTERPOLATED DATA)
C  OR NO REPEAT (TRIANGULATE AND CONTOUR)
C
      IF (REPEAT) GO TO    30
C
C TRIANGULATES THE X-Y PLANE.
C
      CALL CONTNG (NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),IWK(JWIWL),
     1             IWK(JWIWP),WK)
      IF (NERRO(ITEMP).NE.0) RETURN
C
      IWK(5) = NT
      IWK(6) = NL
      NTNL = NT+NL
C
C  SKIP IF NOT LINEAR INTERPOLATION
C
      IF (.NOT.LINEAR) GO TO 25
C
C  FIND THE COEFICENTS FOR LINER INTERPOLATION OF EACH TRIANGLE
C
      CALL CONLIN(XD,YD,ZD,NT,IWK(JWIPT),WK(IPR))
      GO TO 35
C
C
C DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.
C
 25   CALL CONDET (NDP,XD,YD,NCP,IWK(JWIPC))
C
C  ESTIMATE THE PARTIAL DERIVATIVES AT ALL DATA POINTS
C
      CALL CONINT (NDP,XD,YD,ZD,NCP,IWK(JWIPC),WK(IPR))
C
C  VERIFY DATA VALUES VALID
C
   30 NT = IWK(5)
      NL = IWK(6)
      NTNL = NT+NL
C
C  COMPUTE STEP SIZE FOR CONTOURING
C
 35   CALL CONSTP (XD,YD,NDP)
C
C  Save the current clipping state and get current SET parameters.
C
      CALL GQCLIP (IERR,ICLP,RECT)
      CALL GSCLIP (0)
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
      RX1 = VIEW(1)
      RX2 = VIEW(2)
      RY1 = VIEW(3)
      RY2 = VIEW(4)
C
      ISC = ISCALE+1
      GO TO (   40,   60,   50),ISC
C
C  CONRAN SETS SCALING FACTOR
C
   40 CALL SET(PXST,PXED,PYST,PYED,XST,XED,YST,YED,1)
      GO TO 60
C
C  CONRAN PLOTS WITHIN USERS BOUNDARIES
C
   50 CALL SET(RX1,RX2,RY1,RY2,XST,XED,YST,YED,1)
C
C  IF TRIANGULATION PLOT ONLY BRANCH
C
   60 IF (EXTRI) GO TO   390
C
C  GENERATE CONTOURS IF NONE SUPPLIED BY USER
C
      CALL CONCLS (ZD,NDP)
      IF (NERRO(ITEMP).NE.0) RETURN
C
C  REORDER THE CONTOUR LINES FOR CORRECT PATTERN DISPLAY
C
      MAJLNS = 0
      IF (LABON) CALL CONREO (MAJLNS)
C
C  MAKE SURE INTEGER COORDINATES IN 1-1024 RANGE
C
      CALL SETUSV('XF',10)
      CALL SETUSV('YF',10)
C
C  SET THE DASH PATTERNS TO DEFAULT IF THEY HAVE NOT BEEN SET
C
C
      IF (IDASH(1:1).NE.' ') GO TO        80
C
C  SET POSITIVE CONTOUR VALUE TO DEFAULT
C
        IDASH = '$$$$$$$$$$'
   80 IF (NDASH(1:1).NE.' ') GO TO       100
C
C  SET NEGATIVE CONTOUR DASH PATTERN TO DEFAULT
C
        NDASH = '$$$$$$$$$$'
  100 IF (EDASH(1:1).NE.' ') GO TO   120
C
C  SET EQUAL CONTOUR DASH PATTERN TO DEFAULT
C
        EDASH = '$$$$$$$$$$'
C
C  INITIALIZE THE CONTOURING DATA STRUCTURE
C
  120 IF (.NOT.EXTRAP) YST = YST+STPSZ
C
C  LOAD THE SCRATCH SPACE
C
      CALL CONLOD (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C  PERFORM SHIELDING IF SO REQUESTED
C
      IF (SHIELD) CALL CONSLD(SCRARR)
C
C  *******************************************************
C  *                                                     *
C  * IF THE USER NEEDS TO DIVIDE THE PROGRAM UP          *
C  * THIS IS THE BREAK POINT.  ALL SUBROUTINES CALLED    *
C  * PRIOR TO THIS MESSAGE ARE NOT USED AGAIN AND        *
C  * ALL ROUTINES AFTER THIS MESSAGE ARE NOT USED        *
C  * ANY EARLIER.  NOTE THIS ONLY REFEARS TO ENTRY POINTS*
C  * WHICH ARE PART OF THE CONRAN PACKAGE.               *
C  * ALL DATA STRUCTURES AND VARIABLES MUST BE RETAINED. *
C  *******************************************************
C
C
C  PLOT RELATIVE MINIMUMS AND MAXIMUMS IF REQUESTED
C
      IF (PMIMX) CALL CONPMM (SCRARR)
C
C
      LENDAS = NREP*10
C
C  SET THE ERROR MODE TO RECOVERY FOR THE CONTOURING STORAGE ERROR
C
      CALL ENTSR (IROLD,1)
C
C  DRAW THE CONTOURS
C
      DO   250 I=1,NCL
C
          CONV = CL(I)
          IF (CONV.GE.BPSIZ) GO TO   150
C
C  SET UP NEGATIVE CONTOUR PATTERN
C
              DO   140 J=1,10
                ICHAR = NDASH(J:J)
                  DO   130 K=1,NREP
                DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  130             CONTINUE
  140         CONTINUE
          GO TO   210
C
C  SET UP POSITIVE CONTOUR DASH PATTERN
C
  150     IF (CONV.EQ.BPSIZ) GO TO   180
              DO   170 J=1,10
                ICHAR = IDASH(J:J)
                  DO   160 K=1,NREP
                  DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  160             CONTINUE
  170         CONTINUE
          GO TO   210
C
C  SET UP EQUAL CONTOUR DASH PATTERN
C
  180         DO   200 J=1,10
                ICHAR = EDASH(J:J)
                  DO   190 K=1,NREP
                DPAT( J+( 10*(K-1) ): J+( 10*(K-1)) ) = ICHAR
  190             CONTINUE
  200         CONTINUE
C
  210     IF (I.GT.MAJLNS) GO TO   230
C
C  SET UP MAJOR LINES
C
          CALL GSPLCI (IRANMJ)
          CALL CONECD (CONV,IWORK,NCUSED)
          NCHAR = LENDAS + NCUSED
        DPAT(LENDAS+1:NCHAR) = IWORK(1:NCUSED)
          GO TO   240
C
C  SET UP MINOR LINES
C
  230     NCHAR = 10
      CALL GSPLCI (IRANMN)
C
C  PROCESS FOR ALL CONTOURS
C
  240     CALL DASHDC (DPAT(1:NCHAR),NCRT,ISIZEL)
C
C  DRAW ALL CONTOURS AT THIS LEVEL
C
          CALL CONDRW (SCRARR)
C
C  GET NEXT CONTOUR LEVEL
C
  250     CONTINUE
C
C  CONTOURING COMPLETED CHECK FOR OPTIONAL OUTPUTS ON PLOT
C
C  FIRST SET ERROR MODE BACK TO USERS VALUE
C
      CALL RETSR (IROLD)
C
C  GET PLOT BOUNDARIES FOR TITLING AND MESSAGE POSITIONING
C
      CALL GQCNTN(IER,ICN)
        CALL GQNT(ICN,IER,NWIND,NVIEW)
        XST = NWIND(1)
        XED = NWIND(2)
        YST = NWIND(3)
        YED = NWIND(4)
C
C
C  RESET POLYLINE COLOR INDEX TO MAJOR (NORMAL)
C
        CALL GSPLCI (IRANMJ)
C
C  DRAW SHIELD ON PLOT IF REQUESTED
C
      IF(SLDPLT.AND.SHIELD) CALL CONDSD
C
C  DRAW PERIMETER ARROUND PLOT IF DESIRED
C
      IF (PER) CALL PERIM (ITICK,0,ITICK,0)
C
C  DRAW GRID IF REQUESTED
C
      IF (GRD) CALL GRID (ITICK,0,ITICK,0)
C
C  PLOT THE DATA VALUES IF REQUESTED
C
      IF (.NOT.PLDVLS) GO TO   260
      CALL CONPDV (XD,YD,ZD,NDP)
C
C  OUTPUT TITLE IF REQUESTED
C
  260 IF (.NOT.TITLE) GO TO   270
        CALL GSTXCI (IRANTX)
      CALL FL2INT (XED,YED,MX,MY)
      MY = (MY/ICONV)+IABOVE
        ILAST = 64
        DO 261 I = 64,1,-1
            IF (ISTRNG(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 262
            ENDIF
  261 CONTINUE
  262 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      XC = CFUX((NVIEW(1)+NVIEW(2))/2.)
      YC = CPUY(MY)
      CALL WTSTR(XC,YC,ISTRNG(1:ILAST),ITLSIZ,0,0)
C
C
C
C  OUTPUT MESSAGE IF REQUESTED
C
  270 IF (.NOT.MESS) GO TO   390
C
        CALL GSTXCI(IRANTX)
      CALL FL2INT (XST,YST,MX,MY)
      MY = (MY/ICONV)
C
C  IF PERIMETER OR GRID PUT OUT TICK INTERVAL
C
        IMSZ = 0
      IF (.NOT.PER .AND. .NOT.GRD) GO TO   300
        IWORK(1:36) = 'X INTERVAL=              Y INTERVAL='
        WRITE(ENCSCR,'(G13.5)')XRG
        WRITE(ENSCRY,'(G13.5)')YRG
        IWORK(12:24) = ENCSCR
        IWORK(37:49) = ENSCRY
        IMSZ = 50
  300 IF (SCALE .EQ. 1.) GOTO 330
            IWORK(IMSZ:IMSZ+10) = ' SCALED BY '
            WRITE(ENCSCR,'(G13.5)')SCALE
            IWORK(IMSZ+11:IMSZ+23) = ENCSCR
            IMSZ = 73
  330 IF (IMSZ .NE. 0) THEN
        ILAST = IMSZ
        DO 291 I = IMSZ,1,-1
            IF (IWORK(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 292
            ENDIF
  291 CONTINUE
  292 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      XC = CFUX((NVIEW(1)+NVIEW(2))/2.)
      YC = CPUY(MY+IBEL2)
      CALL WTSTR(XC,YC,IWORK(1:ILAST),8,0,0)
C
        ENDIF
C
C  PRODUCE CONTOUR INFO
C
      IWORK(1:42) = 'CONTOUR FROM              TO              '
      IWORK(43:77) = 'CONTOUR INTERVAL OF                '
      HOLD(1) = FLO
      HOLD(2) = HI
      HOLD(3) = FINC
        WRITE(ENCSCR,'(G13.5)')HOLD(1)
        IWORK(13:25) = ENCSCR
        WRITE(ENCSCR,'(G13.5)')HOLD(2)
        IWORK(29:41) = ENCSCR
        WRITE(ENCSCR,'(G13.5)')HOLD(3)
        IWORK(62:74) = ENCSCR
C
C  IF IRREGULAR SPACED CONTOURS MODIFY CONTOUR INTERVAL STATEMENT
C
      IF (FINC.GE.0.) GO TO   380
      NC = 62
          IWORK(NC:NC+15) = ' IRREGULAR      '
C
        ILAST = 77
  380 DO 381 I = 77,1,-1
            IF (IWORK(I:I) .NE. ' ')THEN
                  ILAST = I + 1
                  GOTO 382
            ENDIF
  381 CONTINUE
  382 CONTINUE
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      XC = CFUX((NVIEW(1)+NVIEW(2))/2.)
      YC = CPUY(MY+IBELOW)
      CALL WTSTR(XC,YC,IWORK(1:ILAST),8,0,0)
C
C
C
C
C  PLOT TRIANGLES IF REQUESTED
C
  390 IF (LOOK) THEN
          CALL GSPLCI(IRANMN)
          CALL CONTLK (XD,YD,NDP,IWK(JWIPT))
          CALL GSPLCI(IRANMJ)
      ENDIF
C
C  Restore the original clipping state.
C
      CALL GSCLIP (ICLP)
C
C  RESTORE ORIGINAL COLOR AND POLYLINE COLOR ASPECT SOURCE FLAGS
C
      CALL GSPLCI(OCOLI)
      CALL GSTXCI(OTEXCI)
C
        LASF(10) = OTXASF
        LASF(3) = OPLASF
        CALL GSASF(LASF)
      RETURN
      END
