/*
 *      $Id: gksc.c,v 1.11 2008-07-23 17:28:00 haley Exp $
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
 *      File:           gksc.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed May  1 17:49:30 MDT 1991
 *
 *      Description:    This file manages the GKSC structure. gksc's are 
 *                      used to shuttle GKS commands to a particular device
 *                      driver's support routine.  Each gksc contains pointers
 *                      to the support routines for whichever instance of 
 *                      of a particular driver it is associated with. For 
 *                      example, a gksc which was created for an X window
 *                      will contain all the necessary funtions for 
 *                      displaying GKS commands in that particular X window. 
 *                      A gksc will have functions which are common to
 *                      all X window drivers as well as data which is 
 *                      private to a single instance of an X driver invocation.
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <ncarg/c.h>
#include "gks_device.h"
#include "gksc.h"
#include "gks.h"

/*
 *      The following entries define a table which maps references to 
 *      a GKSC structure into an integer value and back again.
 */
static  unsigned long indexMask = 0;    /* mask of used table entries   */
static  int     numEntries = 0;         /* number GKSC's in table       */
#define TABLESIZE (sizeof(indexMask) * CHAR_BIT)
static  GKSC    *gkscTable[TABLESIZE];  /* the table                    */

/*
 * insert a GKSC pointer into the table. Return its index or a -1 if
 * the table is full
 */
static int      insert_table(GKSC *gksc) 
{
        int     index;

        if (numEntries >= TABLESIZE) {
                return(-1);
        }

        /*
         * find a free entry in the table
         */
        /*SUPPRESS570*/
        for(index = 0; index<TABLESIZE  && ((indexMask >> index) & 1); index++);

        gkscTable[index] = gksc;

        indexMask |= (1 << index); 
        numEntries++;

        return(index);
}

/*
 *      Look up a gksc. If it exists return it, else return NULL
 */
static GKSC     *table_lookup(int index)
{
        if (!(indexMask & (1 << index))) {
                return((GKSC *) NULL);     /* invalid index     */
        }
        
        return(gkscTable[index]);
}

/*
 *      Delete a reference from the GKSC table. Return a -1 if the 
 *      reference is not found, else return 0.
 */
static int      remove_table(int index)
{

        if (!(indexMask & (1 << index))) {
                return(-1);     /* invalid index        */
        }

        indexMask &= (~(1 << index));
        numEntries--;

        return(0);

}

/*
 *      CreateGKSC
 *      [exported]
 * 
 *      Create an instance of a gksc for a particular driver. 
 * on entry
 *      dev_name        : name of device driver
 * on exit
 *      return          : pointer to a GKSC for the driver 'name', else
 *                        NULL => error
 */
GKSC    *CreateGKSC(dev_name)
        char    *dev_name;
{
        GKSdev  *gks_dev;
        GKSC    *gksc;

        /*
         * get the common functions for the device named 'dev_name'
         */
        if ((gks_dev = GKS_GetDevByName(dev_name)) == (GKSdev *) NULL) {
                return((GKSC *) NULL);
        }

        if ((gksc = (GKSC *) malloc (sizeof (GKSC))) == (GKSC *) NULL) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", sizeof(GKSC));
                return((GKSC *) NULL);
        }

        if ((gksc->gd = insert_table(gksc)) < 0) {
                ESprintf(ERR_DEV_NUM, "Too many open devices");
                return((GKSC *) NULL);
        }

        gksc->opcode = 0;

        gksc->p.size = gks_dev->sizeof_point;
        gksc->p.list = (GKSC_Ptr) malloc (INITIAL_MAL * gksc->p.size);
        if (! gksc->p.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->p.size);
                return ((GKSC *) NULL);
        }
        gksc->p.list_size = INITIAL_MAL;
        gksc->p.convert = gks_dev->conv_points;
        gksc->p.num = 0;

        gksc->s.size = gks_dev->sizeof_string;
        gksc->s.list = (GKSC_Ptr) malloc (INITIAL_MAL * gksc->s.size);
        if (! gksc->s.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->s.size);
                return ((GKSC *) NULL);
        }
        gksc->s.list_size = INITIAL_MAL;
        gksc->s.convert = gks_dev->conv_string;
        gksc->s.num = 0;

        gksc->i.size = gks_dev->sizeof_int;
        gksc->i.list = (GKSC_Ptr) malloc (INITIAL_MAL * gksc->i.size);
        if (! gksc->i.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->i.size);
                return ((GKSC *) NULL);
        }
        gksc->i.list_size = INITIAL_MAL;
        gksc->i.convert = gks_dev->conv_ints;
        gksc->i.num = 0;

        gksc->f.size = gks_dev->sizeof_float;
        gksc->f.list = (GKSC_Ptr) malloc (INITIAL_MAL * gksc->f.size);
        if (! gksc->f.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->f.size);
                return ((GKSC *) NULL);
        }
        gksc->f.list_size = INITIAL_MAL;
        gksc->f.convert = gks_dev->conv_floats;
        gksc->f.num = 0;

        gksc->x.size = gks_dev->sizeof_index;
        gksc->x.list = (GKSC_Ptr) malloc (INITIAL_MAL * gksc->x.size);
        if (! gksc->x.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->x.size);
                return ((GKSC *) NULL);
        }
        gksc->x.list_size = INITIAL_MAL;
        gksc->x.convert = gks_dev->conv_indexes;
        gksc->x.num = 0;

        gksc->rgb.size = gks_dev->sizeof_rgb;
        gksc->rgb.list = (GKSC_Ptr)malloc(INITIAL_MAL * gksc->rgb.size);
        if (! gksc->rgb.list) {
                ESprintf(ERR_DEV_MEMORY, "malloc(%d)", 
                        INITIAL_MAL * gksc->rgb.size);
                return ((GKSC *) NULL);
        }
        gksc->rgb.list_size = INITIAL_MAL;
        gksc->rgb.convert = gks_dev->conv_rgbs;
        gksc->rgb.num = 0;

        gksc->native = NULL;
        gksc->ddp = NULL;

        gksc->exec_gksc = gks_dev->exec_gksc;
        gksc->operations = gks_dev->operations;

        return(gksc);
}
        
static  GKSC_Ptr        gks_mem_manage(comp_data, size)
        ComplexData     *comp_data;
        int             size;
{
        GKSC_Ptr        ptr;

        if (comp_data->list) free ((char *) comp_data->list);

        ptr = (GKSC_Ptr) malloc ((unsigned) size * comp_data->size);

        if (! ptr) {
                /*
                 * try and restore what was freed
                 */
                ptr = (GKSC_Ptr) malloc(comp_data->list_size * comp_data->size);
                comp_data->list = ptr;
                return((GKSC_Ptr) NULL);
        }
        comp_data->list_size = (unsigned) size;
        comp_data->list = ptr;
        return(ptr);
}

/*
 *      WriteToGKSC
 *      [exported]
 *      
 *      Convert raw GKS data provided by provided by the Fortran 
 *      interface, ggkwdr, into the format specified by the gksc. The 
 *      converted data is then written into the gksc. The result of this
 *      call is a gksc which can execute the requested GKS command provided
 *      all the GKS data is available. This procedure may be called 
 *      multiple times for a single GKS command. After the first invocation,
 *      subsequent calls will concatenate the raw data to the data already
 *      in the gksc. The gksc maintains a record of how much data it contains.
 *      To begin a new command it is necessary to clear the gksc with the 
 *      ClearGKSC() function.
 *
 *      This code assumes that there is sufficient memory in each gksc data
 *      field for at least 5 items. It makes this assumption in order to
 *      avoid having to check memory space for every single element since
 *      most elements have very little data (less the 5 items per data type).
 *
 * on entry
 *      gksc            : a gksc created by CreateGKSC().
 *      gks_opcode      : opcode for GKS instruction to format data for.
 *
 *      The remaining arguments are as describe in 'fort_c.c' 
 * 
 * on exit
 *      gksc            : contains formated data for command  specified
 *                        by 'gks_opcode'
 */

int     WriteToGKSC
#if     NeedFuncProto
(
        GKSC    *gksc,
        int     gks_opcode,
        int     total_i,
        int     num_i_sent,
        int     *ints,
        int     total_x,
        int     num_x_sent,
        int     *indexes,
        int     total_f,
        int     num_f_sent,
        float   *fxs,
        float   *fys,
        int     total_c,
        int     num_c_sent,
        int     *chars
)
#else
(gksc,gks_opcode,total_i,num_i_sent,ints,total_x,num_x_sent,indexes,
total_f,num_f_sent,fxs,fys,total_c,num_c_sent,chars)
        GKSC    *gksc;
        int     gks_opcode;

        int     total_i,
                num_i_sent, 
                *ints;

        int     total_x,
                num_x_sent, 
                *indexes;

        int     total_f,
                num_f_sent;
        float   *fxs, *fys;

        int     total_c,
                num_c_sent,
                *chars;
#endif
{

        int             rtc = RAW_TO_COOKED;    /* data conversion direction */

        switch (gks_opcode) {

        case    GKS_GET_COLOR_REPRESENTATION:
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->opcode = GET_COLOR_REPRESENTATION;
                break;

        case    GKS_OPEN_WORKSTATION:
                /*
                 * N.B. the string conversion function appends a C null 
                 * character to the end of the string so we need to make
                 * sure that we account for it when considering space 
                 * requirements
                 */
                if (gksc->s.list_size < (total_c+1)) {
                        if (gks_mem_manage((ComplexData *)&gksc->s,total_c+1)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->s.convert(gksc->ddp, chars, &gksc->s, &num_c_sent, rtc);
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = OPEN_WORKSTATION;
                break;

        case    GKS_ACTIVATE_WORKSTATION:
                gksc->opcode = ACTIVATE_WORKSTATION;
                break;

        case    GKS_DEACTIVATE_WORKSTATION:
                gksc->opcode = DEACTIVATE_WORKSTATION;
                break;

        case    GKS_CLOSE_WORKSTATION:
                gksc->opcode = CLOSE_WORKSTATION;
                break;

        case    GKS_CLEAR_WORKSTATION:
                gksc->opcode = CLEAR_WORKSTATION;
                break;

        case    GKS_UPDATE_WORKSTATION:
                gksc->opcode = UPDATE_WORKSTATION;
                break;

        case    GKS_POLYLINE:
                if (gksc->p.list_size < total_f) {
                        if (gks_mem_manage((ComplexData *)&gksc->p,total_f) 
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p ,&num_f_sent,rtc);
                gksc->opcode = POLYLINE;
                break;

        case    GKS_POLYMARKER:
                if (gksc->p.list_size < total_f) {
                        if (gks_mem_manage((ComplexData *)&gksc->p,total_f)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p, &num_f_sent,rtc);
                gksc->opcode = POLYMARKER;
                break;

        case    GKS_TEXT:
                /*
                 * N.B. the string conversion function appends a C null 
                 * character to the end of the string so we need to make
                 * sure that we account for it when considering space 
                 * requirements
                 */
                if (gksc->s.list_size < (total_c+1)) {
                        if (gks_mem_manage((ComplexData *)&gksc->s,total_c+1)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p, &num_f_sent,rtc);
                gksc->s.convert(gksc->ddp, chars, &gksc->s, &num_c_sent,rtc);
                gksc->opcode = TEXT;
                break;

        case    GKS_FILL_AREA:
                if (gksc->p.list_size < total_f) {
                        if (gks_mem_manage((ComplexData *)&gksc->p,total_f)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p, &num_f_sent,rtc);
                gksc->opcode = FILL_AREA;
                break;

        case    GKS_CELL_ARRAY:
                if (gksc->i.list_size < total_i) {
                        if (gks_mem_manage((ComplexData *)&gksc->i,total_i)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                if (gksc->p.list_size < total_f) {
                        if (gks_mem_manage((ComplexData *)&gksc->p,total_f)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                if (gksc->x.list_size < total_x) {
                        if (gks_mem_manage((ComplexData *)&gksc->x,total_x)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }

                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent,rtc);
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p, &num_f_sent,rtc);
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent,rtc);
                gksc->opcode = CELL_ARRAY;
                break;

        case    GKS_SET_LINETYPE:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_LINETYPE;
                break;

        case    GKS_SET_LINEWIDTH_SCALE_FACTOR:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_LINEWIDTH_SCALE_FACTOR;
                break;

        case    GKS_SET_POLYLINE_COLOR_INDEX:
                if (gksc->x.list_size < total_x) {
                        if (gks_mem_manage((ComplexData *)&gksc->x,total_x)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->opcode = SET_POLYLINE_COLOR_INDEX;
                break;

        case    GKS_SET_MARKER_TYPE:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_MARKER_TYPE;
                break;

        case    GKS_SET_MARKER_SIZE_SCALE_FACTOR:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_MARKER_SIZE_SCALE_FACTOR;
                break;

        case    GKS_SET_POLYMARKER_COLOR_INDEX:
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->opcode = SET_POLYMARKER_COLOR_INDEX;
                break;

        case    GKS_SET_TEXT_FONT_AND_PRECISION:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_TEXT_FONT_AND_PRECISION;
                break;

        case    GKS_SET_CHARACTER_EXPANSION_FACTOR:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_CHARACTER_EXPANSION_FACTOR;
                break;

        case    GKS_SET_CHARACTER_SPACING:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_CHARACTER_SPACING;
                break;

        case    GKS_SET_TEXT_COLOR_INDEX:
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->opcode = SET_TEXT_COLOR_INDEX;
                break;

        case    GKS_SET_CHARACTER_HEIGHT_AND_UP_VECTOR:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->f.convert(gksc->ddp, fys, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_CHARACTER_HEIGHT_AND_UP_VECTOR;
                break;

        case    GKS_SET_TEXT_PATH:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_TEXT_PATH;
                break;

        case    GKS_SET_TEXT_ALIGNMENT:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_TEXT_ALIGNMENT;
                break;

        case    GKS_SET_FILL_AREA_INTERIOR_STYLE:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_FILL_AREA_INTERIOR_STYLE;
                break;

        case    GKS_SET_FILL_AREA_STYLE_INDEX:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->opcode = SET_FILL_AREA_STYLE_INDEX;
                break;

        case    GKS_SET_FILL_AREA_COLOR_INDEX:
                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->opcode = SET_FILL_AREA_COLOR_INDEX;
                break;

        case    GKS_SET_COLOR_REPRESENTATION:
                if (gksc->x.list_size < total_x) {
                        if (gks_mem_manage((ComplexData *)&gksc->x,total_x)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }
                if (gksc->rgb.list_size < total_f) {
                        if (gks_mem_manage((ComplexData *)&gksc->rgb,total_f)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }

                gksc->x.convert(gksc->ddp, indexes, &gksc->x, &num_x_sent, rtc);
                gksc->rgb.convert(gksc->ddp, fxs, &gksc->rgb, &num_f_sent, rtc);
                gksc->opcode = SET_COLOR_REPRESENTATION;
                break;

        case    GKS_SET_CLIP_INDICATOR:
                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->p.convert(gksc->ddp, fxs, fys, &gksc->p, &num_f_sent,rtc);
                gksc->opcode = SET_CLIP_INDICATOR;
                break;

        case    GKS_SET_WINDOW:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->f.convert(gksc->ddp, fys, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_WINDOW;
                break;

        case    GKS_SET_VIEWPORT:
                gksc->f.convert(gksc->ddp, fxs, &gksc->f, &num_f_sent, rtc);
                gksc->f.convert(gksc->ddp, fys, &gksc->f, &num_f_sent, rtc);
                gksc->opcode = SET_VIEWPORT;
                break;

        case    GKS_ESCAPE:
                if (gksc->s.list_size < (total_c+1)) {
                        if (gks_mem_manage((ComplexData *)&gksc->s,total_c+1)
                                        == (GKSC_Ptr) NULL) {
                                ESprintf(ERR_INTRNL_MEMORY, "malloc");
                                return(ERR_INTRNL_MEMORY);
                        }
                }

                gksc->i.convert(gksc->ddp, ints, &gksc->i, &num_i_sent, rtc);
                gksc->s.convert(gksc->ddp, chars, &gksc->s, &num_c_sent, rtc);
                gksc->opcode = ESCAPE;
                break;

        default:
                return(ERR_INV_OPCODE);
        }
        return(0);
}

/*ARGSUSED*/
int     ReadFromGKSC(gksc, gks_opcode, total_i, num_i_sent, ints, 
                total_x, num_x_sent, indexes, total_f, num_f_sent, 
                fxs, fys, total_c, num_c_sent, chars)

        GKSC    *gksc;
        int     gks_opcode;

        int     *total_i,
                *num_i_sent, 
                *ints;

        int     *total_x,
                *num_x_sent, 
                *indexes;

        int     *total_f,
                *num_f_sent;
        float   *fxs, *fys;

        int     *total_c,
                *num_c_sent,
                *chars;

{
        int     n;

        int             ctr = COOKED_TO_RAW;    /* data conversion direction */

        /*
         * on exit total_? and num_? will be set to the number of 
         * elements transfered into the raw arrays appropriate for that
         * gks element.
         */
        *total_i = *num_i_sent = *total_x = *num_x_sent = 0;
        *total_f = *num_f_sent = *total_c = *num_c_sent = 0;

        switch (gks_opcode) {

        case    GKS_GET_COLOR_REPRESENTATION:
                n = 1;
                gksc->rgb.convert(gksc->ddp, fxs, &gksc->rgb, &n, ctr);
                *total_f = *num_f_sent = n;
                break;

        default:
                return(ERR_INV_OPCODE);
        }

        return(0);
}

GKSC    *IndexToGKSC(int index)
{
        return(table_lookup(index));
}

int     GKSCToIndex(GKSC *gksc)
{
        return(gksc->gd);
}

void    FreeGKSC(gksc)
        GKSC    *gksc;
{
        if (gksc->p.list) (void) free((char *) gksc->p.list);
        if (gksc->s.list) (void) free((char *) gksc->s.list);
        if (gksc->i.list) (void) free((char *) gksc->i.list);
        if (gksc->f.list) (void) free((char *) gksc->f.list);
        if (gksc->x.list) (void) free((char *) gksc->x.list);
        if (gksc->rgb.list) (void) free((char *) gksc->rgb.list);
        (void) remove_table(gksc->gd);
        free((void *) gksc);
}




