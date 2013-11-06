/*
 *      $Id: ctxt.c,v 1.7 2008-07-23 17:28:00 haley Exp $
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
 *      File:           ctxt.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri May 10 08:40:06 MDT 1991
 *
 *      Description:    This file contains the definition of the clear text
 *                      device driver.
 */
#include <stdio.h>
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include "gksc.h"
#include "ctxt.h"
#include "gks.h"

/*
 * cvs test comment
 */

extern void gerr_hand (Gint, Gint, const char *);

static  void    print_points(points, num)
        CTXTPoint       *points;
        unsigned        num;
{
        int     i;

        for (i=0; i<num; i++) {
                (void) printf(" (%f, %f)\n", points[i].x, points[i].y);
        }
}

/*ARGSUSED*/
int ctxt_OpenWorkstation(gksc)
        GKSC    *gksc;
{
        _NGCesc *cesc;

        (void) printf("OpenWorkstation\n");
        while( (cesc = _NGGetCEscInit()) ){
                gerr_hand(182,11,NULL);
        }
        return(0);
}

/*ARGSUSED*/
int ctxt_ActivateWorkstation(gksc)
        GKSC    *gksc;
{
        (void) printf("ActivateWorkstation\n");
        return(0);
}

/*ARGSUSED*/
int ctxt_DeactivateWorkstation(gksc)
        GKSC    *gksc;
{
        (void) printf("DeactivateWorkstation\n");
        return(0);
}



/*ARGSUSED*/
int ctxt_CloseWorkstation(gksc)
        GKSC    *gksc;
{
        (void) printf("CloseWorkstation\n");
        return(0);
}

/*ARGSUSED*/
int ctxt_ClearWorkstation(gksc)
        GKSC    *gksc;
{
        (void) printf("ClearWorkstation\n");
        return(0);
}


/*ARGSUSED*/
int ctxt_Polyline(gksc)
        GKSC    *gksc;
{
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;

        (void) printf("Polyline\n");
        print_points(pptr, gksc->p.num);
        return(0);
}

/*ARGSUSED*/
int ctxt_Polymarker(gksc)
        GKSC    *gksc;
{
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;

        (void) printf("Polymarker\n");
        print_points(pptr, gksc->p.num);
        return(0);
}

/*ARGSUSED*/
int ctxt_Text(gksc)
        GKSC    *gksc;
{
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;
        char            *sptr = (char *) gksc->s.list;

        (void) printf("Text\n");
        print_points(pptr, gksc->p.num);
        (void) printf(" text    -> %s\n", sptr);
        return(0);
}

/*ARGSUSED*/
int ctxt_FillArea(gksc)
        GKSC    *gksc;
{
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;

        (void) printf("FillArea\n");
        print_points(pptr, gksc->p.num);
        return(0);
}


/*ARGSUSED*/
int ctxt_Cellarray(gksc)
        GKSC    *gksc;
{
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;
        int             *iptr = (int *) gksc->i.list;
        int             *xptr = (int *) gksc->x.list;

        int             nx = iptr[0];   /* number of cols       */
        int             ny = iptr[1];   /* number of rows       */

        CTXTPoint       *Pptr = &pptr[0];
        CTXTPoint       *Qptr = &pptr[1];
        CTXTPoint       *Rptr = &pptr[2];

        int     index;
        int     i,j;

        (void) printf("Cellarray\n");
        (void) printf(" P corner        -> (%f, %f)\n", Pptr->x, Pptr->y);
        (void) printf(" Q corner        -> (%f, %f)\n", Qptr->x, Qptr->y);
        (void) printf(" R corner        -> (%f, %f)\n", Rptr->x, Rptr->y);
        (void) printf(" number of cols  -> %d\n", nx);
        (void) printf(" number of rows  -> %d\n", ny);

        for (i=0, index = 0; i<ny; i++) {
                (void) printf(" ");
                for (j=0; j<nx; j++, index++) {
                        (void) printf("%4d", xptr[index]);
                }
                (void) printf("\n");
        }

        return(0);
}

/*ARGSUSED*/
int ctxt_SetLinetype(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int             line_type = iptr[0];

        (void) printf("SetLineType\n");

        (void) printf(" line type       -> ");

        switch (line_type) {
        
        case    SOLID_LINE:
                (void) printf("solid\n");
                break;
        case    DASHED_LINE:
                (void) printf("dashed\n");
                break;
        case    DOTTED_LINE:
                (void) printf("dotted\n");
                break;
        case    DASH_DOT_LINE:
                (void) printf("dashed-dotted\n");
                break;
        default:
                (void) printf("unsuppored line type\n");
        }

        return(0);
}

/*ARGSUSED*/
int ctxt_SetLineWidthScaleFactor(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           line_width = fptr[0];

        (void) printf("SetLineWidthScaleFactor\n");
        (void) printf(" line width      -> %f\n", line_width);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetPolylineColorIndex(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;

        int     index   = xptr[0];

        (void) printf("SetPolylineColorIndex\n");
        (void) printf(" color index     -> %d\n", index);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetMarkerType(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int             marker_type = iptr[0];

        (void) printf("SetMarkerType\n");

        (void) printf(" marker type     -> ");

        switch (marker_type) {
        
        case    DOT_MARKER:
                (void) printf("dot\n");
                break;
        case    PLUS_MARKER:
                (void) printf("plus\n");
                break;
        case    STAR_MARKER:
                (void) printf("star\n");
                break;
        case    CIRCLE_MARKER:
                (void) printf("star\n");
                break;
        case    X_MARKER:
                (void) printf("X\n");
                break;
        default:
                (void) printf("unsuppored marker type\n");
        }
        return(0);
}


/*ARGSUSED*/
int ctxt_SetMarkerSizeScaleFactor(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           marker_size = fptr[0];

        (void) printf("SetMarkerSizeScaleFactor\n");
        (void) printf(" marker size     -> %f\n", marker_size);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetPolymarkerColorIndex(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;

        int     index   = xptr[0];

        (void) printf("SetPolymarkerColorIndex\n");
        (void) printf(" color index     -> %d\n", index);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetTextFontAndPrecision(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int     font            = iptr[0];
        int     precision       = iptr[1];

        (void) printf("SetTextFontAndPrecision\n");
        (void) printf(" text font       -> %d\n", font);
        (void) printf(" precision       -> ");

        switch (precision) {
        
        case    STRING_PREC:
                (void) printf("string\n");
                break;
        case    CHAR_PREC:
                (void) printf("char\n");
                break;
        case    STROKE_PREC:
                (void) printf("stroke\n");
                break;
        default:
                (void) printf("unsuppored text precision\n");
        }
        return(0);
}

/*ARGSUSED*/
int ctxt_SetCharacterExpansionFactor(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           exp_factor = fptr[0];

        (void) printf("SetCharacterExpansionFactor\n");
        (void) printf(" expansion factor        -> %f\n", exp_factor);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetCharacterSpacing(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           spacing = fptr[0];

        (void) printf("SetCharacterSpacing\n");
        (void) printf(" spacing -> %f\n", spacing);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetTextColorIndex(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;

        int     index   = xptr[0];

        (void) printf("SetTextColorIndex\n");
        (void) printf(" color index     -> %d\n", index);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetCharacterHeightAndUpVector(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           x_up = fptr[0];
        float           x_base = fptr[1];
        float           y_up = fptr[2];
        float           y_base = fptr[3];

        (void) printf("SetCharacterHeightAndUpVector\n");
        (void) printf(" up vector       -> (%f, %f)\n", x_up, y_up);
        (void) printf(" base vector     -> (%f, %f)\n", x_base, y_base);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetTextPath(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int     path    = iptr[0];

        (void) printf("SetTextPath\n");
        (void) printf(" path    -> ");

        switch (path) {
        
        case    RIGHT_TEXT_PATH:
                (void) printf("right\n");
                break;
        case    LEFT_TEXT_PATH:
                (void) printf("left\n");
                break;
        case    UP_TEXT_PATH:
                (void) printf("up\n");
                break;
        case    DOWN_TEXT_PATH:
                (void) printf("down\n");
                break;
        default:
                (void) printf("unsuppored text path\n");
        }
        return(0);
}


/*ARGSUSED*/
int ctxt_SetTextAlignment(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int     horiz_align     = iptr[0];
        int     vert_align      = iptr[1];

        (void) printf("SetTextAlignment\n");
        (void) printf(" horizontal      -> ");

        switch (horiz_align) {
        
        case    NORMAL_ALIGNMENT_HORIZ:
                (void) printf("normal\n");
                break;
        case    LEFT_ALIGNMENT_HORIZ:
                (void) printf("left\n");
                break;
        case    CENTER_ALIGNMENT_HORIZ:
                (void) printf("center\n");
                break;
        case    RIGHT_ALIGNMENT_HORIZ:
                (void) printf("right\n");
                break;
        default:
                (void) printf("unsuppored text alignment\n");
        }

        (void) printf(" vertical        -> ");
        switch (vert_align) {
        
        case    NORMAL_ALIGNMENT_VERT:
                (void) printf("normal\n");
                break;
        case    TOP_ALIGNMENT_VERT:
                (void) printf("top\n");
                break;
        case    CAP_ALIGNMENT_VERT:
                (void) printf("cap\n");
                break;
        case    HALF_ALIGNMENT_VERT:
                (void) printf("half\n");
                break;
        case    BASE_ALIGNMENT_VERT:
                (void) printf("base\n");
                break;
        case    BOTTOM_ALIGNMENT_VERT:
                (void) printf("bottom\n");
                break;
        default:
                (void) printf("unsuppored text alignment\n");
        }
        return(0);
}

/*ARGSUSED*/
int ctxt_SetFillAreaInteriorStyle(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int     fill_style      = iptr[0];

        (void) printf("SetFillAreaInteriorStyle\n");
        (void) printf(" style   -> ");

        switch (fill_style) {
        
        case    HOLLOW_FILL:
                (void) printf("hollow\n");
                break;
        case    SOLID_FILL:
        case    SOLID_TEXT_FILL:
                (void) printf("top\n");
                break;
        case    PATTERN_FILL:
                (void) printf("cap\n");
                break;
        case    HATCH_FILL:
                (void) printf("half\n");
                break;
        default:
                (void) printf("unsuppored fill style\n");
        }
        return(0);
}


/*ARGSUSED*/
int ctxt_SetFillAreaStyleIndex(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;

        int     hatch_index     = iptr[0];

        (void) printf("SetFillAreaStyleIndex (hatch index)\n");
        (void) printf(" hatch style     -> ");

        switch (hatch_index) {
        
        case    HORIZONTAL_HATCH:
                (void) printf("horizontal\n");
                break;
        case    VERTICAL_HATCH:
                (void) printf("vertical\n");
                break;
        case    POSITIVE_HATCH:
                (void) printf("positive\n");
                break;
        case    NEGATIVE_HATCH:
                (void) printf("negative\n");
                break;
        case    HORIZ_VERT_HATCH:
                (void) printf("horizontal - vertical\n");
                break;
        case    POS_NEG_HATCH:
                (void) printf("possitive - negative\n");
                break;
        default:
                (void) printf("unsuppored hatch style\n");
        }
        return(0);
}

/*ARGSUSED*/
int ctxt_SetFillAreaColorIndex(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;

        int     index   = xptr[0];

        (void) printf("SetFillAreaColorIndex\n");
        (void) printf(" color index     -> %d\n", index);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetColorRepresentation(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;
        CTXTColor       *rgbptr = (CTXTColor *) gksc->rgb.list;

        int             index   = xptr[0];
        float           r =  rgbptr[0].r;
        float           g =  rgbptr[0].g;
        float           b =  rgbptr[0].b;
        

        (void) printf("SetColorRepresentation\n");
        (void) printf(" color index     -> %d\n", index);
        (void) printf(" color value     -> (%f, %f, %f)\n", r,g,b);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetClipIndicator(gksc)
        GKSC    *gksc;
{
        int             *iptr = (int *) gksc->i.list;
        CTXTPoint       *pptr = (CTXTPoint *) gksc->p.list;

        int             clip_flag = iptr[0];
        CTXTPoint       *llptr = &pptr[0];
        CTXTPoint       *urptr = &pptr[1];

        (void) printf("SetClipIndicator\n");
        if (clip_flag) (void) printf("  clip indicator  -> on\n");
        else (void) printf("    clip indicator  -> off\n");

        (void) printf(" lower left      -> (%f %f)\n", llptr->x, llptr->y);
        (void) printf(" upper right     -> (%f %f)\n", urptr->x, urptr->y);
        return(0);
}


/*ARGSUSED*/
int ctxt_SetWindow(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           llx = fptr[0];
        float           urx = fptr[1];
        float           lly = fptr[2];
        float           ury = fptr[3];

        (void) printf("SetWindow\n");
        (void) printf(" lower left corner       -> (%f %f)\n", llx, lly);
        (void) printf(" upper right corner      -> (%f %f)\n", urx, ury);
        return(0);
}

/*ARGSUSED*/
int ctxt_SetViewport(gksc)
        GKSC    *gksc;
{
        float           *fptr = (float *) gksc->f.list;

        float           llx = fptr[0];
        float           urx = fptr[1];
        float           lly = fptr[2];
        float           ury = fptr[3];

        (void) printf("SetViewport\n");
        (void) printf(" lower left corner       -> (%f %f)\n", llx, lly);
        (void) printf(" upper right corner      -> (%f %f)\n", urx, ury);
        return(0);
}

/*ARGSUSED*/
int ctxt_GetColorRepresentation(gksc)
        GKSC    *gksc;
{
        int             *xptr = (int *) gksc->x.list;

        int             index   = xptr[0];

        (void) printf("GetColorRepresentation\n");
        (void) printf(" index   -> %d\n", index);
        return(0);
}


/*ARGSUSED*/
int ctxt_Esc(gksc)
        GKSC    *gksc;
{
        char    *sptr = (char *) gksc->s.list;
        int     *iptr = (int *) gksc->i.list;

        int     flag = iptr[0];

        (void) printf("Escape\n");
        (void) printf(" flag    -> %d\n", flag);
        (void) printf(" data    -> %s\n", sptr);
        return(0);
}

/*ARGSUSED*/
int ctxt_UpdateWorkstation(gksc)
        GKSC    *gksc;
{
        (void) printf("UpdateWorkstation\n");
        return(0);
}
