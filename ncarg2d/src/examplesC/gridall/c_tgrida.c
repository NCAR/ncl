/*
 *	$Id: c_tgrida.c,v 1.5 1995-06-14 13:59:34 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{
    int idum,ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws (WKID);
/*
 * INVOKE DEMO DRIVER
 */    
    tgrida(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

int tgrida(ier)
int *ier;
{
/*
 *
 * PURPOSE                To provide a simple demonstration of
 *                        all of the entry points of the GRIDAL
 *                        package.  Eight plots are produced.
 *
 * USAGE                  CALL TGRIDA (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, otherwise
 *
 * I/O                    If the test is successful, the message
 *
 *              GRIDAL TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 8
 *                        c_frames are produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        these plots.
 *
 * PRECISION              Single
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              All of the entries of the GRIDAL package
 *                        are invoked (GRID, GRIDL, PERIM, PERIML,
 *                        HALFAX, TICK4, LABMOD, and GRIDAL) to
 *                        produce plots.  The GRIDAL entry is called
 *                        ten times.  The first call is to demonstate
 *                        a full c_frame grid.  The next nine calls
 *                        create a single c_frame that contains the
 *                        nine legal IGPH grid options to show how
 *                        up to nine plots can be placed on a single
 *                        c_frame.
 */
    int igph,i;
    int knt;
    float x1,x2,y1,y2;
    float h1,h2,v1,v2;
    float xg, yg;
    char buff[3];
    Glimit win, vp;
/*
 * Define normalization transformation 1.
 */
    win.x_min = 0.;
    win.x_max = 1.;
    win.y_min = 0.;
    win.y_max = 1.;
    gset_win(1,&win);
    vp.x_min = .2;
    vp.x_max = .8;
    vp.y_min = .2;
    vp.y_max = .8;
    gset_vp(1,&vp);
/*    
 * GRID 
 */
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR GRID",16.,0.,0.);
    gsel_norm_tran(1);
    c_grid(5,2,6,3);
    c_frame();
    
/* GRIDL */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR GRIDL",16.,0.,0.);
    gsel_norm_tran(1);
    c_gridl(5,2,6,3);
    c_frame();
    
/* PERIM */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR PERIM",16.,0.,0.);
    gsel_norm_tran(1);
    c_perim(5,2,6,3);
    c_frame();
    
/* PERIML */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR PERIML",16.,0.,0.);
    gsel_norm_tran(1);
    c_periml(5,2,6,3);
    c_frame();
    
/* HALFAX */ 
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR HALFAX",16.,0.,0.);
    gsel_norm_tran(1);
    c_halfax(5,2,6,3,.3,.5,0,0);
    c_frame();
    
/* TICK4 and TICKS */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR TICK4",16.,0.,0.);
    gsel_norm_tran(1);
    c_ticks(150,150);
    c_perim(5,2,6,3);
    c_frame();
    c_tick4(12,8,12,8);

/* LABMOD */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR LABMOD",16.,0.,0.);
    gsel_norm_tran(1);
    c_labmod("(E10.2)","(F4.2)",10,4,15,15,0,0,0);
    c_halfax(2,1,10,1,0.,0.,1,1);
    c_frame();
    
/* Use LABMOD to reduce the number of digits in the grid scales */
    
    c_labmod("(F4.2)","(F4.2)",4,4,0,0,0,0,0);
    
/* GRIDAL  -  A single grid on a c_frame.*/
    
    igph = 0;
    sprintf(buff,"%d",igph);
    gsel_norm_tran(0);
    c_plchlq(.5,.85,"IGPH = ",16.,0.,1.);
    c_plchlq(.5,.85,buff,16.,0.,-1.);
    c_plchlq(.5,.9,"DEMONSTRATION PLOT FOR GRIDAL",16.,0.,0.);
    gsel_norm_tran(1);
    c_gridal(5,2,6,3,1,1,igph,.3,.13);
    c_frame();
    
/* c_gridal  -  All 9 legal grids on a single c_frame. */
    
    gsel_norm_tran(0);
    c_plchlq(.5,.98,"TEST IGPH OPTIONS OF GRIDAL",16.,0.,0.);
    knt= 0;
    for(i=0;i<=10;i++){
        if( (i==3)||(i==7)) goto finis;
        igph = i;
        sprintf(buff,"%d",igph);
        knt +=1;
        
    /* Compute the X and Y grid boundaries for the 9 plots.*/
        
        y1 = .42;
        if(knt<4) y1 = .74;
        if(knt>6) y1 = .10;
        x1 = .10;
        if((knt==2)||(knt==5)||(knt==8)) x1 = .42;
        if((knt==3)||(knt==6)||(knt==9)) x1 = .74;
        x2 = x1 + .18;
        y2 = y1 + .18;
        
    /* Specify some user coordinates.*/
        
        h1 = x1*10.;
        h2 = x2*10.;
        v1 = y1*10.;
        v2 = y2*10.;
        
    /* Locate the IGPH legend above the grid.*/
        
        xg = x1 + .13;
        yg = y2 + .03;
        gsel_norm_tran(0);
        c_plchlq(xg,yg,"IGPH = ",16.,0.,1.);
        c_plchlq(xg,yg,buff,16.,0.,-1.);
        gsel_norm_tran(1);
        c_set(x1,x2,y1,y2,h1,h2,v1,v2,1);
        c_gridal(3,3,4,2,1,1,igph,h1,v1);
      finis:  printf(" ");
    }
    
/* 
 *  Advance the frame.
 */
    c_frame();
    printf(" GRIDAL TEST EXECUTED--SEE PLOTS TO CERTIFY \n ");
    *ier = 1;
    return(1);
}
