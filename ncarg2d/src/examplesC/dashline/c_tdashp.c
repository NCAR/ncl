/*
 *	$Id: c_tdashp.c,v 1.2 1994-06-21 14:59:56 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 * INVOKE DEMO DRIVER
 */
    tdashp(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tdashp (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of DASHSUPR
 *
 * USAGE                  CALL TDASHP (IERROR)
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
 *               DASHSUPR TEST EXECUTED--SEE PLOT TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * REQUIRED ROUTINES      DASHSUPR
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              TDASHP utilizes the software DASHSUPR
 *                        routines DASHDB, DASHDC, FRSTD, VECTD,
 *                        LINED, and CURVED to draw 5 curves on 1
 *                        picture using 5 different DASHSUPR patterns.
 *                        Each curve is centered about solid axis
 *                        lines and labelled with the binary
 *                        representation of the DASHSUPR pattern used.
 *
 * X contains the abscissae and Y the ordinates of the curve to be plotted.
 */
    float x[31],y[31], org, theta, ory;
    int i, j, k,isolid;
    Gtext_align text_align;
    Gpoint text_pos;
    extern long c_ior();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * RESET initializes the model picture array.  It should be called with
 * each new c_frame.  The call should occur before a call to any other
 * DASHSUPR routine.
 */
    c_reset();
/*
 * Set a solid dash pattern,  1111111111111111 (BINARY).
 * Boolean operations (using locally-implemented support
 * routines) are used to support porting to hosts with 16
 * bit integers.
 */
    isolid = c_ior(c_ishift (32767,1), 1);

    for( k=1; k <= 5; k++ ) {
        c_dashdb (&isolid);
        org =1.07-0.195*k;
/*
 * Draw the central axis for each curve.
 */
        c_frstd (.50,org-0.03);
        c_vectd (.50,org+0.03);
        c_lastd();
        c_lined (.109,org,.891,org);
/*
 * Call DASHDC with a different dashed line and character combination
 * for each of 5 different curves.
 */
        switch(k) {
          case 1:
            c_dashdc ("$'$'$'$'$'$'$'$K = 1",10,12);
            break;
          case 2:
            c_dashdc ("$$$$$$'$'$$$$$$K = 2",10,12);
            break;
          case 3:
            c_dashdc ("$$$$'$$$$'$$$$'K = 3",10,12);
            break;
          case 4:
            c_dashdc ("$$$$$'''''$$$$$K = 4",10,12);
            break;
          case 5:
            c_dashdc ("$$$'$$$'$$$'$$$K = 5",10,12);
            break;
        }
/*
 * Compute some curve coordinates and draw the curves.
 */
        for( i = 0; i < 31; i++ ) {
            theta = (float)(i)*3.1415926535897932/15.;
            x[i] = 0.5+.4*cos(theta);
            y[i] = org+.075*sin((float)(k)*theta);
        }
        c_curved (x,y,31);
/*
 * Label the curves with the character representation of the appropriate
 * DASHCHAR pattern.  In the pattern labels, A and D should be interpreted
 * as the apostrophe and dollar sign.
 */
/*
 * Locate the string at the left end, but vertically centered.
 */
        text_align.hor = GHOR_LEFT;
        text_align.vert = GVERT_HALF;
        gset_text_align(&text_align);
/*
 * Set the character height.
 */
        gset_char_ht(.012);
        
        ory = org+.089;
        text_pos.x = .1;
        text_pos.y = ory;
        switch(k) {
          case 1:
            gtext(&text_pos,"IPAT=DADADADADADADADK=1");
            break;
          case 2:
            gtext(&text_pos,"IPAT=DDDDDDADADDDDDDK=2");
            break;
          case 3:
            gtext(&text_pos,"IPAT=DDDDADDDDADDDDAK=3");
            break;
          case 4:
            gtext(&text_pos,"IPAT=DDDDDAAAAADDDDDK=4");
            break;
          case 5:
            gtext(&text_pos,"IPAT=DDDADDDADDDADDDK=5");;
            break;
          default:
            break;
        }
    }
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    text_pos.x = .5;
    text_pos.y = .985;
    gtext (&text_pos,"DEMONSTRATION PLOT FOR DASHSUPR");
    text_pos.y = .013;
    gtext (&text_pos,"IN IPAT STRINGS, A AND D SHOULD BE INTERPRETED AS APOSTROPHE AND DOLLAR SIGN");
    c_frame();

    *ierror = 0;
    printf(" DASHSUPR TEST EXECUTED--SEE PLOT TO CERTIFY\n");
    return(1);
}


c_ishift(i,nshift)
/* shift the 4 byte integer i by nshift bits  */
/* if nshift is negative, right shift end off zero fill  */
/* if nshift is positive, left shift end around  */
/* the routine behaves properly if magnitude of nshift > 32  */
    long           i, nshift;
{
    long            jshift, nbits;
    if (nshift < 0) {
        nbits = (nshift < -32 ? 32 : -nshift);
        jshift = (i >> nbits) & (017777777777 >> (nbits - 1));
    } else {
        nbits = nshift % 32;
        jshift = (i << nbits) | ((i >> (32 - nbits))
                      & (~(037777777777 << nbits)));
    }
    return (jshift);
}

/* integer valued function to return logical OR of i and j  */
/* i and j are assumed to be 4 byte integers  */
long
c_ior(i, j)
    long    i, j;
{
    return (i | j);
}
