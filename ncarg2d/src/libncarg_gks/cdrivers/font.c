/*
 *      $Id: font.c,v 1.10 2008-07-23 17:28:00 haley Exp $
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
 *      File            font.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed Jun  5 18:35:24 MDT 1991
 *
 *      Description:    Read and decode a text font. The binary format of 
 *                      font file is described in the Fontcap section of 
 *                      the NCAR Graphics Installer's Guide.
 *
 */     
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <ncarg/c.h>
#include "font.h"
#include "gksc.h"
#include "common.h"


/*
 * fcap_template contains the description of the font as it is read in
 * and decoded from the fontcap file.
 */
Fcap    fcap_template;

/*
 * fcap_current contains a description of the font as modified by the 
 * various character attributes. 
 */
Fcap    fcap_current;   /* translated fontcap           */
int     leftExtent1[WDTH_SPACE];
int     rightExtent1[WDTH_SPACE];


/*
 * list of available fonts
 */
static  char    *fontList[] = {
                DEFAULTFONT,
                "font1",
                "font2",        /* HERSHEY:CARTOGRAPHIC_ROMAN   */
                "font3",        /* HERSHEY:CARTOGRAPHIC_GREEK   */
                "font4",        /* HERSHEY:SIMPLEX_ROMAN        */
                "font5",        /* HERSHEY:SIMPLEX_GREEK        */
                "font6",        /* HERSHEY:SIMPLEX_SCRIPT       */
                "font7",        /* HERSHEY:COMPLEX_ROMAN        */
                "font8",        /* HERSHEY:COMPLEX_GREEK        */
                "font9",        /* HERSHEY:COMPLEX_SCRIPT       */
                "font10",       /* HERSHEY:COMPLEX_ITALIC       */
                "font11",       /* HERSHEY:COMPLEX_CYRILLIC     */
                "font12",       /* HERSHEY:DUPLEX_ROMAN         */
                "font13",       /* HERSHEY:TRIPLEX_ROMAN        */
                "font14",       /* HERSHEY:TRIPLEX_ITALIC       */
                "font15",       /* HERSHEY:GOTHIC_GERMAN        */
                "font16",       /* HERSHEY:GOTHIC_ENGLISH       */
                "font17",       /* HERSHEY:GOTHIC_ITALIAN       */
                "font18",       /* HERSHEY:MATH_SYMBOLS         */
                "font19",       /* HERSHEY:SYMBOL_SET1          */
                "font20",       /* HERSHEY:SYMBOL_SET2          */
        };

static  unsigned int fonttblsize = sizeof (fontList) / sizeof (char *);

#ifdef  DEBUG_FCAP
/*
 *      print_fontcap
 *      [internal]
 *
 *      print out contents of the fontcap for debuging
 */
static  void    print_fontcap() 
{

        (void) fprintf(stderr,"char_start       %d\n", fontcap_raw->char_start);  
        (void) fprintf(stderr,"char_end         %d\n", fontcap_raw->char_end);  
        (void) fprintf(stderr,"c_x_start        %d\n", fontcap_raw->c_x_start);  

        (void) fprintf(stderr,"c_y_start        %d\n", fontcap_raw->c_y_start);  

        (void) fprintf(stderr,"c_pen_start      %d\n", fontcap_raw->c_pen_start);

        (void) fprintf(stderr,"pt_beg_start     %d\n", fontcap_raw->pt_beg_start);  
        (void) fprintf(stderr,"pt_end_start     %d\n", fontcap_raw->pt_end_start);  
        (void) fprintf(stderr,"pt_beg_len       %d\n", fontcap_raw->pt_beg_len);
        (void) fprintf(stderr,"pt_end_len       %d\n", fontcap_raw->pt_end_len); 
        (void) fprintf(stderr,"c_x_len          %d\n", fontcap_raw->c_x_len);  
        (void) fprintf(stderr,"c_y_len          %d\n", fontcap_raw->c_y_len);  

        (void) fprintf(stderr,"c_pen_len        %d\n", fontcap_raw->c_pen_len);  

        (void) fprintf(stderr,"char_width       %d\n", fontcap_raw->char_width); 
        (void) fprintf(stderr,"font_top         %d\n", fontcap_raw->font_top);  
        (void) fprintf(stderr,"font_cap         %d\n", fontcap_raw->font_cap);  

        (void) fprintf(stderr,"font_half        %d\n", fontcap_raw->font_half); 

        (void) fprintf(stderr,"font_base        %d\n", fontcap_raw->font_base);  
        (void) fprintf(stderr,"font_bottom      %d\n", fontcap_raw->font_bottom);
        (void) fprintf(stderr,"font_right       %d\n", fontcap_raw->font_right);
}


static  void    print_strokes(fcap) 
        Fcap    *fcap;
{
        int     i,k;

        for (i = 0; i < fcap->numchar; i++) {
                (void) fprintf(stderr, "\n\nchar [%d] \n        ", i);
                (void) fprintf(stderr, "x,y ");
                for (k = 0; k < fcap->char_des[i].numstroke; k++) {

                        (void) fprintf(stderr, "%d %d   ", 
                                fcap->char_des[i].p_c[k].x_coord,
                                fcap->char_des[i].p_c[k].y_coord);
                }
        }
}
#endif  /*      DEBUG_FCAP */



/*      decodefont:
 *      [internal]
 *
 *      This routine decodes the  integer values stored in the 
 *      fontcap that represent stroke sequences for characters into 
 *      meaningful fields
 *      
 * on entry:
 *      *fontcap_raw    : the raw undecoded fontcap as read in from disk
 * on exit
 *      *var_space      : true if font type is VAR_SPACE or VAR_FILL, else 0 
 *      fcap_template   : contain the stroke sequences
 *      fcap_current    : has memory allocated sufficient for template
 *      return          : 0 => OK, < 0 => err
 */
static  int     decodefont(fontcap_raw, var_space)
        Fontcap_raw     *fontcap_raw;
        Boolean         *var_space;
{
        int k,i;
        unsigned        size;           /*number of strokes in a sequence*/
        unsigned        index = 0;      /*index into strokearray        */



        F_CHAR_START(fcap_template)         = fontcap_raw->char_start;
        F_CHAR_END(fcap_template)           = fontcap_raw->char_end;
        F_CHAR_WIDTH(fcap_template)         = fontcap_raw->char_width;
        F_CHAR_HEIGHT(fcap_template)        = fontcap_raw->font_cap -
                                                fontcap_raw->font_base;

        F_FONT_TOP(fcap_template)           = fontcap_raw->font_top;
        F_FONT_CAP(fcap_template)           = fontcap_raw->font_cap;
        F_FONT_HALF(fcap_template)          = fontcap_raw->font_half;
        F_FONT_BASE(fcap_template)          = fontcap_raw->font_base;
        F_FONT_BOTTOM(fcap_template)        = fontcap_raw->font_bottom;
        F_FONT_TYPE(fcap_template)          = fontcap_raw->font_type;
        F_FONT_RIGHT(fcap_template)         = fontcap_raw->font_right;

        *var_space = (fontcap_raw->font_type == VAR_SPACE ||
                        fontcap_raw->font_type == VAR_FILL);

        F_NUMCHAR(fcap_template) = 
        F_NUMCHAR(fcap_current) = 
                fontcap_raw->char_end - fontcap_raw->char_start + 1;

        /*
         *      place terminating index value for fontcap_raw.strokes in
         *      the stroke array
         */
        fontcap_raw->pointers[F_NUMCHAR(fcap_template)] = 
                                fontcap_raw->last_pointer;


        for (i = 0, index = 0;i < F_NUMCHAR(fcap_template); i++) { 
                size = fontcap_raw->pointers[i+1] - fontcap_raw->pointers[i];

                /*
                 * in variable space fonts "width" information is stored
                 * with the stroke information. We don't want to include
                 * it here
                 */
                size = *var_space ? size - 1 : size;

                if (size) {
                        /* 
                         * malloc only enough memory necessary to describe a 
                         * particular character. Free any memory alloced in
                         * a previous call.
                         */
                        if (fcap_template.char_des[i].p_c) {
                                free((char *) fcap_template.char_des[i].p_c);
                                fcap_template.char_des[i].p_c = NULL;
                        }

                        fcap_template.char_des[i].p_c = (Pen_coord *) 
                                malloc (size * sizeof(Pen_coord));

                        if (! fcap_template.char_des[i].p_c) {
                                ESprintf(errno, "malloc()");
                                return(ERR_FCAP_MEMORY);
                        }


                        if (fcap_current.char_des[i].p_c) {
                                free((char *) fcap_current.char_des[i].p_c);
                                fcap_current.char_des[i].p_c = NULL;
                        }

                        fcap_current.char_des[i].p_c = (Pen_coord *) 
                                malloc (size * sizeof(Pen_coord));

                        if (! fcap_current.char_des[i].p_c) {
                                ESprintf(errno, "malloc()");
                                return(ERR_FCAP_MEMORY);
                        }
                }

                F_NUMSTROKE(fcap_template, i) = 
                F_NUMSTROKE(fcap_current, i) = size;

                /*
                 * variable space fonts store individual character widths
                 * in first stroke position. See "FONTCAP Files" in the
                 * Installers Guide
                 */
                if (*var_space) {
                        rightExtent1[i] =
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->c_y_start,fontcap_raw->c_y_len);

                        leftExtent1[i] =
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->c_x_start,fontcap_raw->c_x_len);

                index++;
                }


                for (k = 0; k < size; k++) {
                        F_X_COORD(fcap_template, i, k) = 
                                GETBITS(fontcap_raw->strokes[index], 
                                fontcap_raw->c_x_start,fontcap_raw->c_x_len); 

                        F_Y_COORD(fcap_template, i, k) = 
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->c_y_start,fontcap_raw->c_y_len); 


                        F_PEN(fcap_template, i, k) = 
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->c_pen_start,
                                fontcap_raw->c_pen_len); 

                        F_PAINT_ST(fcap_template, i, k) = 
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->pt_beg_start,
                                fontcap_raw->pt_beg_len); 

                        F_PAINT_ED(fcap_template, i, k) = 
                                GETBITS(fontcap_raw->strokes[index],
                                fontcap_raw->pt_end_start,
                                fontcap_raw->pt_end_len); 

                        index++;
                }
        }
        return(0);
}

/*
 *      read_fontcap
 *      [internal]
 *
 *      Read and decode the fontcap
 *
 *      on entry
 *              fontcap : is the name of a binary version of a fontcap
 *      on exit
 *              file fontcap has been opened and processing can begin.
 */
static  int     read_fontcap(fontcap, var_space)
        char    *fontcap;               /*fontcap name  */
        Boolean *var_space;
{
        int             fd;             /* fontcap file descriptor      */
        int             status;         /* return status                */
        Fontcap_raw     *fontcap_raw = NULL;


        /*
         *      malloc memory for fontcap 
         */
        if ((fontcap_raw = (Fontcap_raw *) malloc (sizeof (Fontcap_raw))) == 0){
                ESprintf(errno, "malloc()");
                return(ERR_FCAP_MEMORY);
        }

        /*
         *      open and read the fontcap
         */
        if ((fd = open(fontcap,0)) < 0) {
                if (fontcap_raw) free ((char *) fontcap_raw);
                ESprintf(errno, "open(%s, 0)", fontcap);
                return(ERR_FCAP_OPEN);
        }

        if ((read(fd,(char *) fontcap_raw, sizeof(Fontcap_raw))) 
                                != sizeof(Fontcap_raw)) {

                if (fontcap_raw) free ((char *) fontcap_raw);
                (void) close (fd);
                ESprintf(errno, "read(%d, ,)", fd);
                return(ERR_FCAP_READ);
        }

        /*
         *      translate the fontcap into a more meaningful form
         */
        status = decodefont(fontcap_raw, var_space);

#ifdef  DEBUG_FCAP
        print_fontcap();
        print_strokes(&fcap_template);
#endif

        /*
         *      de alloc raw fontcap
         */
        free((char *) fontcap_raw);
        (void) close (fd);

        return (status);
}

/*
 *      SetFont
 *      [exported]
 *
 *      Set the current font. SetFont() will attempt to read in and decode
 *      a given font into the fcap_template structure
 *
 * on entry
 *      font_index      : index into fontList of font to read in        
 * on exit
 *      *var_space      : True => variable space font, False => mono space
 *      return          : 0 => Ok, 1 => Warning, else fatal error.
 */
int     SetFont(font_index, var_space)
        unsigned        font_index;
        Boolean         *var_space;
{
        char    *fcap;
        int     status = 0;
        int     rc;

        if (font_index >= fonttblsize) {
                ESprintf(ERR_FONT_INDEX, "Invalid font index(%d)", font_index);
                font_index = DEFAULTFONTINDEX;
                status = ERR_FONT_INDEX;
        }

        /*
         *      get full path to the fontcap
         */
        if ((fcap = getFcapname(fontList[font_index])) == NULL) {
                return(ERR_FCAP_NAME);
        }

        /* 
         * initialize the font processor with the selected font
         */
        if ((rc = read_fontcap(fcap, var_space)) < 0) {
                status = rc;
        }

        free(fcap);

        return(status);
}
