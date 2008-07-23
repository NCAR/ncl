/*
 *      $Id: transform.c,v 1.5 2008-07-23 17:28:01 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *      $Id: transform.c,v 1.5 2008-07-23 17:28:01 haley Exp $
 */
/************************************************************************
*                                                                       *
*                            Copyright (C)  1992                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      File:           transform.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed Nov 18 16:51:56 MST 1992
 *
 *      Description:    Maintain the coordinate transformation system.
 */

#include <stdio.h>
#include "transform.h"
#include "common.h"


/* 
 *      compute the transformation from the 2D coordinate space "from" to
 *      the 2D coordinate space "to"
 */
static  Transform2D     compute_transform2d(from, to)
        CoordSpace      *from;
        CoordSpace      *to;
{
        double  a2, b2, x2, y2, x, y;
        Transform2D     transform;
        /*
         *      solve the linear systems:
         *
         *              A1*X + B1 = X1
         *              A2*X + B2 = X2
         *      and
         *              A1*Y + B1 = Y1
         *              A2*Y + B2 = Y2
         */

        x2 = to->urx - to->llx;
        x = from->urx - from->llx;
        a2 = x2 / x;
        b2 = to->urx - (a2 * from->urx);
        transform.x_scale = a2;
        transform.x_trans = b2;
        
        y2 = to->ury - to->lly;
        y = from->ury - from->lly;
        a2 = y2 / y;
        b2 = to->ury - (a2 * from->ury);
        transform.y_scale = a2;
        transform.y_trans = b2;

        
        return(transform);
}

/*
 *      compute the transform C = A * B
 *      i.e. C is the transform that takes the results of B and jams
 *      them through A.
 */
static  Transform2D     multiply_transform2D(A,B)
        Transform2D     *A;
        Transform2D     *B;
{
        Transform2D     transform;

        transform.x_scale = A->x_scale * B->x_scale;
        transform.x_trans = (A->x_scale * B->x_trans) + A->x_trans;

        transform.y_scale = A->y_scale * B->y_scale;
        transform.y_trans = (A->y_scale * B->y_trans) + A->y_trans;

        return(transform);
        
}

/*
 * Function:    TransformSetScreenSpace
 *
 * Description:
 *                      Inform the transformation module of the screen space. 
 *                      The information is stored in tsystem
 *
 * In Args:
 *      llx,lly         : lower left corner
 *      urx,ury         : upper right corner
 *
 * Out Args:
 *      *tsystem        : stores the screen space
 *
 * Return Values:
 *
 * Side Effects:
 */
void    TransformSetScreenSpace(tsystem, llx, lly, urx, ury)
        TransSystem     *tsystem;
        double          llx;
        double          lly;
        double          urx;
        double          ury;
{
        tsystem->screen.llx = llx;
        tsystem->screen.lly = lly;
        tsystem->screen.urx = urx;
        tsystem->screen.ury = ury;
}

/*
 * Function:    TransformSetWindow
 *
 * Description:
 *                      Inform the transformation module of the window space. 
 *                      The information is stored in tsystem
 *
 * In Args:
 *      llx,lly         : lower left corner
 *      urx,ury         : upper right corner
 *
 * Out Args:
 *      *tsystem        : stores the window space
 *
 * Return Values:
 *
 * Side Effects:
 */
void    TransformSetWindow(tsystem, llx, lly, urx, ury)
        TransSystem     *tsystem;
        double          llx;
        double          lly;
        double          urx;
        double          ury;
{
        tsystem->window.llx = llx;
        tsystem->window.lly = lly;
        tsystem->window.urx = urx;
        tsystem->window.ury = ury;
}

/*
 * Function:    TransformSetViewport
 *
 * Description:
 *                      Inform the transformation module of the viewport space. 
 *                      The information is stored in tsystem
 *
 * In Args:
 *      llx,lly         : lower left corner
 *      urx,ury         : upper right corner
 *
 * Out Args:
 *      *tsystem        : stores the viewport space
 *
 * Return Values:
 *
 * Side Effects:
 */
void    TransformSetViewport(tsystem, llx, lly, urx, ury)
        TransSystem     *tsystem;
        double          llx;
        double          lly;
        double          urx;
        double          ury;
{
        tsystem->viewport.llx = llx;
        tsystem->viewport.lly = lly;
        tsystem->viewport.urx = urx;
        tsystem->viewport.ury = ury;
}

/*
 * Function:    TransformSetNDScreenSpace
 *
 * Description:
 *                      Inform the transformation module of the nd screen 
 *                      space. The information is stored in tsystem
 *
 * In Args:
 *      llx,lly         : lower left corner
 *      urx,ury         : upper right corner
 *
 * Out Args:
 *      *tsystem        : stores the screen space
 *
 * Return Values:
 *
 * Side Effects:
 */
void    TransformSetNDScreenSpace(tsystem, llx, lly, urx, ury)
        TransSystem     *tsystem;
        double          llx;
        double          lly;
        double          urx;
        double          ury;
{
        tsystem->nd_screen.llx = llx;
        tsystem->nd_screen.lly = lly;
        tsystem->nd_screen.urx = urx;
        tsystem->nd_screen.ury = ury;
}


/*
 * Function:    TransformGetTransform
 *
 * Description: Compute the transform necessary to go from window space,
 *              to viewport space, to nd screen space, to screen space.
 *
 * In Args:
 *      *tsystem        : The transformation system established with calls
 *                      to TransformSet*.
 *
 * Out Args:
 *
 * Return Values:
 *      return          : composited transformation matrix.
 *
 * Side Effects:
 */
Transform2D     TransformGetTransform(tsystem)
        TransSystem     *tsystem;
{

        Transform2D     win2view;
        Transform2D     nd_screen2screen;

        Transform2D     transform;

        win2view = compute_transform2d(&tsystem->window, &tsystem->viewport);

        nd_screen2screen = compute_transform2d(
                        &tsystem->nd_screen, &tsystem->screen
        );

        transform = multiply_transform2D(&nd_screen2screen, &win2view);

        return(transform);
}

/*
 *      Compute the lartest square that will fit in the 
 *      center of the given coord pairs
 */     
CoordSpace      ComputeLargestSquare(llx, lly, urx, ury)
        double  llx;
        double  lly;
        double  urx;
        double  ury;
{
        double  width, height;
        double  diff;
        CoordSpace      cs;

        width = urx - llx;
        width = ABS(width) + 1;

        height = ury - lly;
        height = ABS(height) + 1;

        if (width > height) {
                cs.lly = lly;
                cs.ury = ury;

                diff = width - height;
                if (llx < urx) {
                        cs.llx = llx + (diff/2);
                        cs.urx = urx - (diff/2);
                }
                else {
                        cs.llx = llx - (diff/2);
                        cs.urx = urx + (diff/2);
                }
        }
        else {
                cs.llx = llx;
                cs.urx = urx;

                diff = height - width;
                if (lly < ury) {
                        cs.lly = lly + (diff/2);
                        cs.ury = ury - (diff/2);
                }
                else {
                        cs.lly = lly - (diff/2);
                        cs.ury = ury + (diff/2);
                }
        }

        return(cs);
}
