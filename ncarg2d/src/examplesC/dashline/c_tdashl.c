/*
 *	$Id: c_tdashl.c,v 1.2 1994-06-21 14:59:54 haley Exp $
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
    tdashl(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tdashl (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of DASHLINE
 *
 * USAGE                  CALL TDASHL (IERROR)
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
 *               DASHLINE TEST EXECUTED--SEE PLOT TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * REQUIRED ROUTINES      DASHLINE
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              TDASHL utilizes the software DASHLINE
 *                        routines DASHDB, DASHDC, FRSTD, VECTD,
 *                        LINED, and CURVED to draw 5 curves on 1
 *                        picture using 5 different DASHLINE patterns.
 *                        Each curve is centered about solid axis
 *                        lines and labelled with the binary
 *                        representation of the DASHLINE pattern used.
 *
 * X contains the abscissae and Y the ordinates of the curve to be plotted.
 */
    float x[31],y[31], org, theta, ory;
    int i, j, k,isolid,ipat[5];
    Gtext_align text_align;
    Gpoint text_pos;
    extern long c_ior();
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
/*
 * Set a solid dash pattern,  1111111111111111 (BINARY).
 * Boolean operations (using locally-implemented support
 * routines) are used to support porting to hosts with 16
 * bit integers.
 */
    isolid = c_ior(c_ishift (32767,1), 1);
/*
 * Array IPAT contains 5 different 16-BIT dash patterns.  The patterns
 * are constructed with boolean operations as shown above.
 * The binary representations of the patterns are
 *        0001110001111111
 *        1111000011110000
 *        1111110011111100
 *        1111111100000000
 *        1111111111111100
 */
    ipat[0] = c_ior (c_ishift ( 3647,1), 1);
    ipat[1] = c_ishift (30840,1);
    ipat[2] = c_ishift (32382,1);
    ipat[3] = c_ishift (32640,1);
    ipat[4] = c_ishift (32766,1);

    for( k=1; k <= 5; k++ ) {
        c_dashdb (&isolid);
        org =1.07-0.195*k;
/*
 * Draw the central axis for each curve.
 */
        c_frstd (.50,org-0.03);
        c_vectd (.50,org+0.03);
        c_lined (.109,org,.891,org);
        c_dashdb (&ipat[k-1]);
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
        ory = org+.09;
        text_pos.x = .1;
        text_pos.y = ory;
        switch(k) {
          case 1:
            gtext (&text_pos,"IPAT=0001110001111111");
            break;
          case 2:
            gtext (&text_pos,"IPAT=1111000011110000");
            break;
          case 3:
            gtext(&text_pos,"IPAT=1111110011111100");
            break;
          case 4:
            gtext(&text_pos,"IPAT=1111111100000000");
            break;
          case 5:
            gtext (&text_pos,"IPAT=1111111111111100");
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
    gtext (&text_pos,"DEMONSTRATION PLOT FOR DASHLINE");
    c_frame();

    *ierror = 0;
    printf( "DASHLINE TEST EXECUTED--SEE PLOT TO CERTIFY\n");
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
