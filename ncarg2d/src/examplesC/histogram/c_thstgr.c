/*
 *	$Id: c_thstgr.c,v 1.1 1994-05-13 14:28:14 haley Exp $
 */
#include <math.h>
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

  
main()
{
    int idum,ierr;

    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * INVOKE DEMO DRIVER
 */
    thstgr(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

thstgr (ierror)
int *ierror;
{

/*
 *
 * PURPOSE                To provide a demonstration of the HISTGR
 *                        utility and to test each of the four IFLAG
 *                        options.
 *
 * USAGE                  CALL THSTGR (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An error flag
 *                          = 0, If the test is successful,
 *                          = 1, otherwise.
 *
 * I/O                    If the test is successful, the message
 *
 *                          HISTGR TEST SUCCESSFUL  . . .  SEE PLOT
 *                          TO VERIFY PERFORMANCE
 *
 *                        is written on unit 6.
 *
 *                        In addition, three (3) labeled frames are
 *                        produced.  To determine if the test was
 *                        successful examine these plots.
 *
 * PRECISION              Single
 *
 * REQUIRED LIBRARY       HISTGR
 * FILES
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * HISTORY                Originally written May, 1985; revised
 *                        August, 1987
 *
 * ALGORITHM              THSTGR computes data and calls HISTGR
 *                        5 times, exercising different options.
 *
 */
    int ndim,ncls,nwrk,idum;
    int i,j,iflag,nclass,npts;
/*
 *  NWRK = NDIM + 3*(NCLS+1)
 */
    char mon[37];
    float dat1[2][320], x, arr7[7], work[374];
    float class[17+1];
    float spac[2];
    int colours[15],npts2;
    float ty,tx;
    Gcolr_rep rgb[15];
/*
 *  Array DAT1 is filled with values to be used as input for HISTGR
 */
    strcpy( mon, "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC" );
    ndim=320; ncls=17; nwrk=374;
/*
 *  Define the RGB triples needed below.
 */
    rgb[0].rgb.red = 0.70; rgb[0].rgb.green = 0.70; rgb[0].rgb.blue = 0.70;
    rgb[1].rgb.red = 0.75; rgb[1].rgb.green = 0.00; rgb[1].rgb.blue = 1.00;
    rgb[2].rgb.red = 0.30; rgb[2].rgb.green = 0.10; rgb[2].rgb.blue = 1.00;
    rgb[3].rgb.red = 0.10; rgb[3].rgb.green = 0.50; rgb[3].rgb.blue = 1.00;
    rgb[4].rgb.red = 0.00; rgb[4].rgb.green = 0.70; rgb[4].rgb.blue = 1.00;
    rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 1.00; rgb[5].rgb.blue = 1.00;
    rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 1.00; rgb[6].rgb.blue = 0.70;
    rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 0.00;
    rgb[8].rgb.red = 0.70; rgb[8].rgb.green = 0.00; rgb[8].rgb.blue = 0.70;
    rgb[9].rgb.red = 1.00; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
    rgb[10].rgb.red = 1.00; rgb[10].rgb.green = 0.75; rgb[10].rgb.blue = 0.00;
    rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 0.48; rgb[11].rgb.blue = 0.00;
    rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.00; rgb[12].rgb.blue = 0.48;
    rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.00; rgb[13].rgb.blue = 0.00;
    rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 1.00; rgb[14].rgb.blue = 1.00;

    tx = .5; ty = .9765;
/*
 *  Define 15 color indices, 14 spaced throughout the color
 *  spectrum, and the last one being white.
 */
    for(i=0;i<=14;i++){
        gset_colr_rep(1,i+1,&rgb[i]);
    }

/*
 * ENTSR (from PORT library) to recover from warnings;
 */
    c_entsr(&idum, 1);
/*
 *  Frame 1:  Demonstrate the default version of HISTGR, IFLAG = 0.
 */
    iflag = 0;
    nclass = 11;
    npts = 320;

    for(i=1;i<=npts;i++){
        for(j=1;j<=2;j++){
            dat1[j-1][i-1]=0.;
        }
    }
    
    for(i=1;i<=npts;i++){
        x = (float)i;
        dat1[0][i-1] = 10. * (float)log10(0.1*(double)x+1.);
    }
/*
 *   call HSTOPL('DEF=ON') to activate all default options.);;;
 */
    c_hstopl("DE=ON");
/*
 *  Flush PLOTIT's buffers.
 */
    c_plotit(0,0,0);
/*
 *  Set text and polyline color indices to 15 (white).
 */
    gset_text_colr_ind(15);
    gset_line_colr_ind(15);

    c_plchlq (tx,ty,"DEMONSTRATION PLOT FOR DEFAULT VERSION OF HISTGR",16.,0.,0.);

    c_histgr(&dat1[0][0], ndim, npts, iflag, class, nclass, work, nwrk);
/*
 *  Frame 2:  Demonstrate the comparison of two previously determined
 *            histograms, IFLAG = 3.
 */
    iflag = 3;
    nclass = 6;
    npts2 = 6;
    for(i=1;i<=npts2;i++){
        dat1[0][i-1]=2*sin((double)(i));
        dat1[1][i-1]=2.5*cos((double)(i)/.5);
        class[i-1]=(float)(i);
    }
/*
 * first call HSTOPL('DEF=ON') to activate all default options.);
 */
    c_hstopl("DE=ON");
/*
 *  Turn on color, title, perimeter, frequency, format, character,
 *  label, and spacing options.
 */
    colours[0] = 8;
    colours[1] = 3;
    colours[2] = 14;
    colours[3] = 11;
    colours[4] = 6;
    colours[5] = 13;
    colours[6] = 14;
    colours[7] = 5;
    c_hstopi("COL=ON",3,0,colours,8);
/*
 *  Choose large, horizontal alphanumeric labels for class labels.
 */
    c_hstopc("TI=ON","OPTIONS CHANGED: PRM,TIT,CHA,LAB,FOR,COL,FQN and SPA",7,3);
    c_hstopl("PR=ON");
    c_hstopc("FQ=ON","MONTHLY PRECIPITATION",7,3);
    c_hstopc("FO=ON","(F3.0)",9,3);
    c_hstopc("CH=ON",mon,12,3);
    c_hstopc("LA=ON","COMPARE TWO DATASETS",7,3);
    spac[0] = 2.0;
    spac[1] = -1.5;
    c_hstopr("SP=ON",spac,2);
/*
 *  The second argument must be the actual dimension size of DAT1.
 */
    c_histgr(&dat1[0][0], ndim, npts2, iflag, class, nclass, work, nwrk);
/*
 *  Frame 3:  Put four plots on 1 frame by setting FRAME = OFF for the
 *            first 3 plots and FRAME = ON for the last plot.
 */
/*     Plot 1:   IFLAG = 0, automatic sorting of the input data into
 *               NCLASS = 17 bins.
 */
    iflag = 0;
    nclass = 17;
    npts = 320;
    for(i=1;i<=npts;i++){
        x = (float)i;
        dat1[0][i-1] = 10. * log10(0.1*x+1.);
    }

/* 
 *  (First call HSTOPL('DEF=ON') to activate all default options.)
 */
    c_hstopl("DE=ON");
/*
 *  Turn on horizontal bars, title, median, window, color,
 *  and spacing option
 *
 *  Choose large horizontal class labels.
 */
    c_hstopc("TI=ON","OPTS CHANGED: HOR, WIN, FRA, MED, COL, SPA, and TIT",9,3);
    c_hstopl("HO=ON");
    c_hstopl("ME=ON");
/*
 *  Plot 1 goes into the top left quadrant of the frame.
 */

/*
 *       ARR7 coordinates are XMIN, XMAX, YMIN, YMAX.
 */
    arr7[0] = 0.;
    arr7[1] = .5;
    arr7[2] = .5;
    arr7[3] = 1.;
    c_hstopr("WI=ON",arr7,4);

    colours[0] = 8;
    colours[1] = 2;
    colours[2] = 10;
    colours[3] = 4;
    colours[4] = 5;
    colours[5] = 6;
    colours[6] = 7;
    colours[7] = 8;
    c_hstopi("COL=ON",2,0,colours,8);
    spac[0] = 3.0;
    spac[1] = 0.0;
    c_hstopr("SP=ON",spac,2);
/*
 *  Turn off the frame advance.
 */
    c_hstopl("FR=OF");
    c_histgr(&dat1[0][0], ndim, npts, iflag, class, nclass, work, nwrk);
/*
 *     Plot 2:   IFLAG = 2, one set of 11 histogram classes and their
 *               associated values are plotted.
 */
    iflag = 2;
    nclass = 11;
    npts2 = 11;
    for(i=1;i<=npts2;i++){
        class[i-1]=(float)(2*i);
        dat1[0][i-1]=(float)sqrt((double)2*i);
    }
/*
 * first call HSTOPL('DEF=ON') to activate all default options.);
 */
    c_hstopl("DE=ON");
/*
 *  Turn on title, label, frequency, format, and window options.
 *
 *  Choose medium sized, horizontal labels for class labels
 *
 */
    c_hstopc("TI=ON","OPTIONS CHANGED: LAB,FQN,TIT,FOR,FRA AND WIN",11,3);
    c_hstopc("LA=ON","Y VALUES ASSIGNED TO X VALUES",11,3);
    c_hstopc("FQ=ON","SQUARE ROOT OF CLASS MID-VALUES",12,3);
    c_hstopc("FO=ON","(I3)",11,3);
/*
 *  Plot 2 goes into the top right quadrant of the frame.
 */
    arr7[0] = .5;
    arr7[1] = 1.;
    arr7[2] = .5;
    arr7[3] = 1.;
    c_hstopr("WIN=ON",arr7,4);
/*
 *  Turn off color and frame advance options.
 */
    c_hstopi("COL=OF",2,0,colours,8);
    c_hstopl("FR=OFF");
    c_histgr(&dat1[0][0], ndim, npts2, iflag, class, nclass, work, nwrk);
/*
 *     Plot 3:   IFLAG = 1, input values are sorted into a defined set of
 *               8 classes.
 */
    iflag = 1;
    nclass = 8;
    npts = 320;
    x = 0.;
    for(i=1;i<=npts;i++){
        dat1[0][i-1] = sin(x);
        x = x + .02;
    }

    class[0] = -0.6;
    for(i=1;i<=nclass;i++){
        class[i] = class[i-1] + 0.20;
    }
/*
 * First call HSTOPL("DEF=ON") to activate all default options.)
 */
    c_hstopl("DE=ON");
/*
 *  Turn on class, draw line, format, title, and label options.
 */
    c_hstopi("CL=ON",2,30,colours,8);
    c_hstopl("DR=ON");
    c_hstopc("FOR=ON","(F6.2)",9,3);
    c_hstopc("TI=ON","OPTIONS CHANGED: CLA,DRL,FOR,TIT,LAB,MID,WIN,SHA and FRA",9,3);
    c_hstopc("LA=ON","CLASS VALUES CHOSEN FROM -0.6 to 1.0",9,3);
/*
 *  Plot 3 goes into the lower left quadrant of the frame.
 */
    arr7[0] = 0.;
    arr7[1] = .5;
    arr7[2] = 0.;
    arr7[3] = .5;
    c_hstopr("WI=ON",arr7,4);
/*
 *  Turn off color, midvalues, frame advance and shading options.
 */
    c_hstopi("CO=OF",2,30,colours,8);
    c_hstopl("MI=OFF");
    c_hstopl("FR=OFF");
    c_hstopl("SH=OFF");
    c_histgr(&dat1[0][0], ndim, npts, iflag, class, nclass, work, nwrk);
/*
 *     Plot 4:   IFLAG = 0, input values are sorted into 11 equally sized
 *               bins over the range of the input values.
 */
    iflag = 0;
    nclass = 11;
    npts = 320;
    x = 0.;
    for(i=1;i<=npts;i++){
        dat1[0][i-1] = sin(x);
        x = x + .02;
    }
/*
 * First call HSTOPL("DEF=ON") to activate all default options.)
 */
    c_hstopl("DE=ON");
/*
 *  Turn on class, frequency, format, title, window, and label options.
 *  Choose medium sized vertical class value labels.
 */
    c_hstopi("CL=ON",2,90,colours,8);
    c_hstopc("FQN=ON","NUMBER OF OCCURENCES IN EACH CLASS",9,3);
    c_hstopc("FOR=ON","(F6.2)",9,3);
    c_hstopc("TI=ON","OPTIONS CHANGED: CLA,LAB,FQN,TIT,SPA,PER,FOR AND WIN",9,3);
/*
 *  Plot 4 goes into the lower right quadrant of the frame.
 */
    arr7[0] = .5;
    arr7[1] = 1.;
    arr7[2] = 0.;
    arr7[3] = .5;
    c_hstopr("WIN=ON",arr7,4);
    c_hstopc("LAB=ON","CLASS VALUES COMPUTED WITHIN HISTGR",9,3);
/*
 *  Turn off color, spacing and percent axis options.
 */
    c_hstopi("CO=OF",2,90,colours,8);
    c_hstopr("SP=OF",spac,2);
    c_hstopl("PER=OFF");
    
    c_histgr(&dat1[0][0], ndim, npts, iflag, class, nclass, work, nwrk);

    *ierror = 0;
    return(1);
}
