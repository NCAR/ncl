/*
 *      $Id: text.c,v 1.8 2008-07-23 17:28:01 haley Exp $
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
 *      File            text.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed Jun  5 18:35:24 MDT 1991
 *
 *      Description:    This file was taken from ctrans. It contains very
 *                      ugly code.
 *
 *              This file contains the font processing routines that handle 
 *      GKS commands for text. text.c uses information stored in the fontcap 
 *      to produce the necessary commands to stroke text via polylines. The 
 *      data stored in the fontcap must first be decoded by font.c and stored 
 *      in the table "fcap_template". After text.c is initialized and the 
 *      fontcap is decoded text processing may begin. The coordinates for 
 *      the stroke sequences stored in fcap_template must be transformed  
 *      in response to character control commands received. The transformed 
 *      version of fcap_template is stored in fcap_current.
 *
 *      Note to programmer:
 *              In order to understand these routines one must first understand
 *      the meaning of the GKS text commands as described in ISO/DIS 1. In 
 *      particular the commands; Text, Character Expansion Factor, Character 
 *      Spacing, Character Height, Character Orientation, Text Path, and Text
 *      Alignment.
 */     
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "font.h"
#include "gks.h"
#include "gksc.h"
#include "common.h"
#include "text.h"

/*
 *      Current text attributes and other stuff used to translate the font
 */
static  TextInfoType    TextInfo = {
        {
                DEFAULTFONTINDEX,               /* current font         */
                STROKE_PREC, 
                1.0,                            /* character expansion  */
                0.0,                            /* character spacing    */
                0.01,                           /* character height     */
                {
                        0.0, 0.01,
                        0.01, 0.0
                },                              /* char orientation     */
                RIGHT_TEXT_PATH,                /* text path            */
                {
                        NORMAL_ALIGNMENT_HORIZ, /* horizontal alignment */
                        NORMAL_ALIGNMENT_VERT   /* vertical alignment   */
                }
        },
        TRUE,                                   /* dirty bit            */
        FALSE,                                  /* valid font opened    */
        /*
         * the rest are dummy values to be filled in later
         */
        {
                {0.0, 0.0}, 
                {0.0, 0.0}
        },
        FALSE,
        0.0, 0.0, 0.0, 0.0,
        1, 
        1,
        1
        };

/* 
 * macro for computing scaling factors in x and y directions
 */
#define Y_SCALE(HEIGHT) (((float) TextInfo.ta.char_height / (float) HEIGHT))
#define X_SCALE(HEIGHT) ((float) (((float) TextInfo.ta.char_height / (float) \
        HEIGHT)) * TextInfo.ta.char_expan_factor * (float) \
        (MAG(TextInfo.ta.orientation.x_base,TextInfo.ta.orientation.y_base) \
        / MAG(TextInfo.ta.orientation.x_up,TextInfo.ta.orientation.y_up)))

        

/* 
 * array containing widths of each  character for variable space fonts
 */
static  int     Widtharray2[WDTH_SPACE];

/*
 * arrays containing left and right extent of each character
 * for variable space fonts
 */
static  int     leftExtent2[WDTH_SPACE];
static  int     rightExtent2[WDTH_SPACE];

/*
 *      SetTextAttribute
 *      [exported]
 *
 *      Set a gks text attribute
 *
 * on entry
 *      *ta             : contains desired text attributes
 *      mask            : indicates which fields in *ta to read
 * on exit
 *      return          : 0 => ok, else gks error
 *      
 */
int     SetTextAttribute(ta, mask)
        TextAttribute   *ta;
        unsigned long   mask;
{
        int     status = 0;

        if (mask & TEXT_FONT_SG) {
                /*
                 * if text_font is negative it is a locally defined font
                 * (not part of gks standard). For now thats all we support
                 */
                if (ta->text_font < 0) {
                        ta->text_font = -(ta->text_font);
                }


                status = SetFont(ta->text_font, &TextInfo.var_space);
                if (status != 0) {
                        /*
                         *  return unless warning message.
                         */
                        if (status != ERR_FONT_INDEX) {
                                return(status);
                        }
                }

                TextInfo.ta.text_font = ta->text_font;
                TextInfo.font_init = TRUE;
                TextInfo.dirty = TRUE;
        }

        /*SUPPRESS570*/
        if (mask & TEXT_PRECISION_SG) {
                /*
                 * only support stroke precision
                 */
        }

        if (mask & CHAR_EXPAN_FACTOR_SG) {
                TextInfo.dirty = TRUE;
                TextInfo.ta.char_expan_factor = ta->char_expan_factor; 
        }

        if (mask & CHAR_SPACING_SG) {
                TextInfo.dirty = TRUE;
                TextInfo.ta.char_spacing = ta->char_spacing; 
        }

        if (mask & CHAR_HEIGHT_SG) {
                TextInfo.dirty = TRUE;
                TextInfo.ta.char_height = ta->char_height; 
        }
        if (mask & CHAR_ORIENTATION_SG) {
                TextInfo.dirty = TRUE;
                TextInfo.ta.orientation = ta->orientation; 
        }

        if (mask & TEXT_PATH_SG) {
                TextInfo.dirty = TRUE;
                TextInfo.ta.text_path = ta->text_path; 
        }

        if (mask & TEXT_ALIGNMENT_SG) {
                TextInfo.ta.text_alignment = ta->text_alignment; 
        }

        return(status);
}

static  float   X_scale(height)
        int     height;
{
        float   mag1 = 
        MAG(TextInfo.ta.orientation.x_base, TextInfo.ta.orientation.y_base); 

        float   mag2 = 
        MAG(TextInfo.ta.orientation.x_up, TextInfo.ta.orientation.y_up);

        return (((float) TextInfo.ta.char_height / (float) height) 
                * TextInfo.ta.char_expan_factor * (mag1 / mag2));
}

/*      
 *      make_matrix:
 *      [internal]
 *
 *      This routine calculates the transformation matrix necessary
 *      for handling character orientation, character expansion and
 *      character height. 
 * on entry:
 *      fcap_template has been filled in
 * on exit
 *      TextInfo.matrix : contains data necessary to compute transformations
 */
static  void    make_matrix()
{
        float   sx,sy;          /* scale in x and y direction   */

        float   x_base = TextInfo.ta.orientation.x_base;
        float   y_base = TextInfo.ta.orientation.y_base;
        float   x_up = TextInfo.ta.orientation.x_up;
        float   y_up = TextInfo.ta.orientation.y_up;


        sy = Y_SCALE(F_CHAR_HEIGHT(fcap_template));
        sx = X_scale(F_CHAR_HEIGHT(fcap_template)); 


        /* 
         * The origin of the font coordinate system body must be at
         * x = y = 0
         */

        TextInfo.cos_base = (float) x_base / MAG(x_base, y_base);
        TextInfo.sin_base = (float) y_base / MAG(x_base, y_base);
        TextInfo.cos_up = (float)   y_up / MAG(x_up, y_up);
        TextInfo.sin_up = (float) -x_up / MAG(x_up, y_up);

        /* calculate transformation matrix      */
        TextInfo.matrix[0][0] = sx * TextInfo.cos_base; 
        TextInfo.matrix[0][1] = sx * TextInfo.sin_base; 
        TextInfo.matrix[1][0] = -sy * TextInfo.sin_up; 
        TextInfo.matrix[1][1] = sy * TextInfo.cos_up; 
}
  
/*      trans_coord:
 *              perform coordinate transformations on list of character strokes
 *              using the transformation matrix.
 *      on entry
 *              f1 : a pointer to the un altered font table as it is represented
 *                      in the fontcap.
 *      on exit
 *              f2 : transformed version of f1 via the transformation matrix
 */
static void     trans_coord(f1,f2)
        Fcap    *f1, *f2;       /* font lists   */
{
        int     i,k;

        float   yscale = Y_SCALE(f1->char_height);
        float   xscale = X_scale(f1->char_height);

        for (i = 0; i < F_NUMCHAR(*f1); i++) {
                for (k = 0; k < F_NUMSTROKE(*f1, i); k++) {

                        F_X_COORD(*f2, i, k) = 
                                (F_X_COORD(*f1, i, k) * TextInfo.matrix[0][0]) +
                                (F_Y_COORD(*f1, i, k) * TextInfo.matrix[1][0]);

                        F_Y_COORD(*f2, i, k) = 
                                (F_X_COORD(*f1, i, k) * TextInfo.matrix[0][1]) +
                                (F_Y_COORD(*f1, i, k) * TextInfo.matrix[1][1]);
                }

        }



        /* scale other portions of font coordinate system 
         * note: only scaling is done here. Rotation is done in the alignment
         * routines
         */

        F_CHAR_HEIGHT(*f2) = F_CHAR_HEIGHT(*f1) * yscale;

        F_CHAR_WIDTH(*f2) = F_CHAR_WIDTH(*f1) * xscale;
        
                /* scale y coordinates parameters*/
        F_FONT_TOP(*f2) = F_FONT_TOP(*f1) * yscale;
        F_FONT_CAP(*f2) = F_FONT_CAP(*f1) * yscale;
        F_FONT_HALF(*f2) = F_FONT_HALF(*f1) * yscale;
        F_FONT_BASE(*f2) = F_FONT_BASE(*f1) * yscale;
        F_FONT_BOTTOM(*f2) = F_FONT_BOTTOM(*f1) * yscale;

                /*scale x coordinates parameters*/
        F_FONT_RIGHT(*f2) = F_FONT_RIGHT(*f1) * xscale;

        /*
         * compute left and right extent of each character in the font. 
         * For mono space characters this value is fixed for all characters
         */
        if (TextInfo.var_space) {
                for (i=0;i<F_NUMCHAR(*f1) ;i++)  {
                        leftExtent2[i] = xscale * leftExtent1[i];
                        rightExtent2[i] = xscale * rightExtent1[i];
                        Widtharray2[i] = rightExtent2[i] - leftExtent2[i];
                }
        }
        else {
                /*
                 * for mono-spaced fonts the character body is fixed for
                 * all characters. We assume that left and right extents
                 * of the character body are the left and right extents
                 * of the font coordinate system.
                 */
                for (i=0;i<F_NUMCHAR(*f1) ;i++)  {
                        leftExtent2[i] = 0;
                        rightExtent2[i] = F_FONT_RIGHT(*f2);
                        Widtharray2[i] = rightExtent2[i] - leftExtent2[i];
                }
        }
}


/*      path_spacing:
 *              Determine necessary spacing between characters (either 
 *              horizontal or vertical depending on path ) based on "text 
 *              path" and "character spacing". These values are effected and
 *              therefore scaled appropriately by "Character Height", "Character
 *              expansion" and "Character Orientation".                         
 * on exit
 *      TextInfo.y_spacing,
 *      TextInfo.x_spacing:     contain spacing in x and/or y directions. 
 *                      NOTE : This spacing is in the along the Untransformed
 *                      coordinate system, not along base and up 
 *                      vector.
 *      TextInfo.spacing:       the amount of space added to the character
 *                      body in either the x or y direction depending
 *                      on the path
 *              
 */
static void     path_spacing (text_path)
        int     text_path;
{
        int     dir = 1;

        float   char_spacing = TextInfo.ta.char_spacing;
        float   char_height = TextInfo.ta.char_height;
        float   x_base = TextInfo.ta.orientation.x_base;
        float   y_base = TextInfo.ta.orientation.y_base;
        float   x_up = TextInfo.ta.orientation.x_up;
        float   y_up = TextInfo.ta.orientation.y_up;

        int     spacing = TextInfo.spacing;

        spacing = (int) ((char_spacing * char_height) 
                * (MAG(x_base, y_base) / MAG(x_up, y_up)));

        switch (text_path) { 

        case LEFT_TEXT_PATH: 
                dir = -1;       /* negate value         */
        default:                /* invalid text path    */
        case RIGHT_TEXT_PATH:

                TextInfo.x_spacing = (int) 
                        (spacing * ( x_base / MAG(x_base,y_base)) * dir);

                TextInfo.y_spacing = (int)
                        (spacing * (y_base / MAG(x_base,y_base)) * dir);

                break;

        case DOWN_TEXT_PATH:
                dir = -1;       /* negate value */
        case UP_TEXT_PATH:

                TextInfo.x_spacing = (int)
                                (spacing * (x_up / MAG(x_up,y_up))* dir);

                TextInfo.y_spacing = (int)
                                (spacing * (y_up / MAG(x_up,y_up))* dir);

                break;
        }
        TextInfo.spacing = spacing;
}

/*      left_most:
 *              This function finds the right-most extented character
 *              in a string and returns its right extent.
 *              It assumes that the coordinate origin is on the left side.
 *
 * on entry
 *      *s              : the string
 *      strlen          : length of s
 * on exit
 *      return          = left-most extent
 */
static  int     left_most(s, strlen)
        const char      *s;
        int             strlen;
{
        int     i;
        int     index;
        unsigned        min;

        for (i=0, min=~0; i<strlen; i++) {
                index = s[i] - F_CHAR_START(fcap_template);
                min = MIN(leftExtent2[index], min);
        }

        return((int) min);
}

/*      right_most:
 *              This function finds the right-most extented character
 *              in a string and returns its right extent.
 *              It assumes that the coordinate origin is on the right side.
 *
 * on entry
 *      *s              : the string
 *      strlen          : length of s
 * on exit
 *      return          = right-most extent
 */
static  int     right_most(s, strlen)
        const char      *s;
        int             strlen;
{
        int     i;
        int     max, index;

        for (i=0, max=0; i<strlen; i++) {
                index = s[i] - F_CHAR_START(fcap_template);
                max = MAX(rightExtent2[index], max);
        }

        return(max);
}

/*      middle_most:
 *              This function finds the right-most extented character
 *              and the left-most extended charcter in a string and returns 
 *              the middle of these two characters.
 *              It assumes that the coordinate origin is on the right side.
 *
 * on entry
 *      *s              : the string
 *      strlen          : length of s
 * on exit
 *      return          = middle 
 */
static  int     middle_most(s, strlen)
        const char      *s;
        int             strlen;
{
        int     left, right;

        left = left_most(s, strlen);
        right = right_most(s, strlen);
        return (left + ((right - left) / 2));
}


/*      left_extent:
 *
 *      Return the left extent of the given character.
 *
 * on entry
 *      c               : the character
 * on exit
 *      return          = left extent of character
 */
static  int     left_extent(c)
        char    c;
{
        int     index;

        index = c - F_CHAR_START(fcap_template);
        return(leftExtent2[index]);
}

/*      right_extent:
 *
 *      Return the right extent of the given character.
 *
 * on entry
 *      c               : the character
 * on exit
 *      return          = right extent of character
 */
static  int     right_extent(c)
        char    c;
{
        int     index;

        index = c - F_CHAR_START(fcap_template);
        return(rightExtent2[index]);
}

/*      txt_ext_length:
 *
 *      Calculate the width of the text extent rectangle bounding the
 *      given string which includes with of the character bodies and the 
 *      inter-character spacing. This function assumes that text path 
 *      is either right or left.
 *
 * on entry
 *      var_space       : true or false;
 *      strlen          : length of string to print
 * on exit
 *      return          = absolute length of text extent rectangle
 */
static  int     text_extent_length(s,strlen,path)
        char    *s;
        int     strlen;
        int     path;
{
        int     i;
        int     index;
        int     total = 0;

        if (! strlen) return(0);

        if (path == RIGHT_TEXT_PATH || path == LEFT_TEXT_PATH) {
                for (i=0;i<strlen;i++) {
                        index = s[i] - F_CHAR_START(fcap_template);
                        total += (Widtharray2[index]);
                }
        }
        else {  /* path is up or down   */
                total = strlen * 
                        (F_FONT_TOP(fcap_current) -F_FONT_BOTTOM(fcap_current));
        }

        /* 
         * add inter-character spacing
         */
        total += (strlen - 1) * TextInfo.spacing;       
        return(total);
}


/*      str_width:
 *              This function calculates the translation along baseline that
 *      in necessary for horizontal  alignment of  the text. The value returned
 *      must be "rotated" to accommodate the rotation of the string due to 
 *      orientation and path.
 *      
 * on entry:
 *      strlen          : length of the text string.
 *      s               : the string
 *      TextInfo.width  : if mono space font or path is up or down 
 *                      absolute Width of string along base vector
 *                      else  contain spacing between characters
 * on exit:
 *      return          : x translation value along  base vector.
 *               
 */ 

static int      str_width(strlen, s)
        int     strlen;         
        char    *s;
{
        int     horizontal_align = TextInfo.ta.text_alignment.horizontal;
        int     text_path = TextInfo.ta.text_path;

        switch (horizontal_align) {

        /*
         * normal alignment see ISO/DIS 8632/1 description on Text Alignment
         */
        default:        /* invalid alignment (use normal)       */
        
        case NORMAL_ALIGNMENT_HORIZ:    
                switch (text_path) {

                case RIGHT_TEXT_PATH:   /*left align*/
                        return(left_extent(s[0]));

                case LEFT_TEXT_PATH:    /*right align*/
                        return(right_extent(s[0]));

                case UP_TEXT_PATH:      /*centre align*/ 
                case DOWN_TEXT_PATH: 
                        return(middle_most(s, strlen));
                }

        case LEFT_ALIGNMENT_HORIZ :     
                switch (text_path) {
                case LEFT_TEXT_PATH  :
                        return(- (text_extent_length(s, strlen, text_path) -
                                right_extent(s[0]))
                        );

                case RIGHT_TEXT_PATH :
                        return(left_extent(s[0]));
                case UP_TEXT_PATH    :
                case DOWN_TEXT_PATH  : return(left_most(s, strlen));
                }


        case CENTER_ALIGNMENT_HORIZ:    
                switch (text_path) {
                case RIGHT_TEXT_PATH : 
                        return((text_extent_length(s,strlen,text_path) / 2) +
                                left_extent(s[0]));

                case LEFT_TEXT_PATH: 
                        return(-((text_extent_length(s,strlen,text_path) / 2) -
                                right_extent(s[0]))
                        );


                case UP_TEXT_PATH: 
                case DOWN_TEXT_PATH:
                         return(middle_most(s, strlen));
                }

        case RIGHT_ALIGNMENT_HORIZ:     
                switch (text_path) {

                case RIGHT_TEXT_PATH: 
                        return(text_extent_length(s,strlen,text_path) + 
                                left_extent(s[0]));

                case LEFT_TEXT_PATH:  
                        return(right_extent(s[0]));

                case UP_TEXT_PATH: 
                case DOWN_TEXT_PATH:  
                        return(right_most(s,strlen));

                }
        }
        /*NOTREACHED*/
	return(0);
}

/*      str_height:
 *              This function calculates the translation along the up 
 *      vector that is necessary for vertical alignment of the text. 
 *      The value returned must be "rotated" to accommodate the rotation 
 *      of the string due to orientation and path.
 *      
 * on entry:
 *      strlen          : the length of the string.
 *      fcap_current    : has been filled in.
 * on exit:
 *      return          : y translation value along up vector.
 *               
 */ 
static int      str_height(strlen, s)
        int     strlen;
        char    *s;
{
        int     vertical_align = TextInfo.ta.text_alignment.vertical;
        int     text_path = TextInfo.ta.text_path;

        switch (vertical_align) {

        default :               /* invalid vertical alignment   */

        case NORMAL_ALIGNMENT_VERT:
                switch (text_path) {

                case RIGHT_TEXT_PATH: 
                case LEFT_TEXT_PATH: 
                case UP_TEXT_PATH:  
                        return(F_FONT_BASE(fcap_current));

                case DOWN_TEXT_PATH : 
                        return(F_FONT_TOP(fcap_current)); 
                }

        case TOP_ALIGNMENT_VERT : 
                switch(text_path) {

                case RIGHT_TEXT_PATH :
                case LEFT_TEXT_PATH :
                case DOWN_TEXT_PATH :
                        return (F_FONT_TOP(fcap_current));

                case UP_TEXT_PATH :  
                        return(text_extent_length(s, strlen, text_path) +
                                F_FONT_BOTTOM(fcap_current)
                        );

                }

        case CAP_ALIGNMENT_VERT :
                switch(text_path) {

                case RIGHT_TEXT_PATH :
                case LEFT_TEXT_PATH :
                case DOWN_TEXT_PATH : 
                        return (F_FONT_CAP(fcap_current));
                case UP_TEXT_PATH :  
                        return(text_extent_length(s, strlen, text_path) +
                                F_FONT_BOTTOM(fcap_current) -
                                (F_FONT_TOP(fcap_current) -
                                F_FONT_CAP(fcap_current))
                        );

                }

        case HALF_ALIGNMENT_VERT :
                switch(text_path) {

                case RIGHT_TEXT_PATH :
                case LEFT_TEXT_PATH :
                        return (F_FONT_HALF(fcap_current));

                case DOWN_TEXT_PATH : 
                        return(-((text_extent_length(s, strlen, text_path)/2) -
                                F_FONT_TOP(fcap_current))
                        );
                case UP_TEXT_PATH :
                        return((text_extent_length(s, strlen, text_path) / 2) +
                                F_FONT_BOTTOM(fcap_current)
                        );
                }

        case BASE_ALIGNMENT_VERT :
                switch(text_path) {

                case RIGHT_TEXT_PATH :
                case LEFT_TEXT_PATH :
                case UP_TEXT_PATH :  
                        return (F_FONT_BASE(fcap_current));

                case DOWN_TEXT_PATH : 
                        return(- (text_extent_length(s, strlen, text_path) -
                                F_FONT_TOP(fcap_current) -
                                (F_FONT_BASE(fcap_current) -
                                F_FONT_BOTTOM(fcap_current)))
                        );

                }

        case BOTTOM_ALIGNMENT_VERT :
                switch(text_path) {
                case RIGHT_TEXT_PATH :
                case LEFT_TEXT_PATH :
                case UP_TEXT_PATH :  
                        return (F_FONT_BOTTOM(fcap_current));

                case DOWN_TEXT_PATH : 
                        return(- (text_extent_length(s, strlen, text_path) -
                                F_FONT_TOP(fcap_current))
                        );

                }
        }
        /*NOTREACHED*/
	return(0);
}
 
/*      text_align:
 *              This routine calculates the necessary rotation and translation
 *      necessary to support text alignment.
 *
 *      on entry:
 *              strlen : length of string of text
 *              s      : the string
 *      on exit:
 *              transx : calculated adjustment of x coordinate
 *              transy : calculated adjustment of y coordinate
 */
static void     text_align
#ifdef  NeedFuncProto
(
        long            *transx,
        long            *transy,
        int             slen,
        char            *s
)
#else
(transx,transy,slen,s)
        long            *transx,*transy;
        int             slen;
        char            *s;
#endif
{
        float   x,y;    /*temp storage  */

        x = (float) str_width(slen,s);  /*translation of x      */
        y = (float) str_height(slen,s); /*translation of y      */


        *transx = -((x * TextInfo.cos_base) - (y * TextInfo.sin_up));
        *transy = -((x * TextInfo.sin_base) + (y * TextInfo.cos_up)); 
}



        


/*      Text_:
 *
 *      Stroke a string text using whatever font is currently loaded.
 *      The font is translated into a sequence stroked in Device Coordinate
 *      space.
 *
 *      Note:
 *              Characters contained in the text that are note described
 *      in the fontcap are ignored. This INCLUDES any control
 *      characters. eg. newline, tab.
 *
 *              It is assumed the coordinate origin of device which is 
 *      being translated for is in the lower left corner
 *
 * on entry
 *      x,y             : position of text in DC space
 *      *string         : string to translate
 *      *lines()        : a polyline routine that operates in device 
 *                      coordinate space.
 *      *lines_data     : data to be passed to the lines() routine
 * on exit
 *      return          : 0
 *
 */
int     Text_(x,y, string, lines, lines_data)
        int     x,y;
        char    *string;
#ifdef  __STDC__
        void    (*lines)(int *, int *, unsigned, Voidptr);
#else
        void    (*lines)();
#endif
        void    *lines_data;
{
        unsigned index;         /* index into the transformed fontlist  */
        int     x_space,        
                y_space;        /* used to position chars in text string*/
        int     prev_width;     /* width of previous character in string*/
        int     left_ext;       /* left extent of character body        */
        int     i,k;
        long    trans_x, trans_y;
        int     numstroke;      /* number of strokes making up the font */
        int     char_ind;

        int     text_path = TextInfo.ta.text_path;
        float   x_base = TextInfo.ta.orientation.x_base;
        float   y_base = TextInfo.ta.orientation.y_base;
        float   x_up = TextInfo.ta.orientation.x_up;
        float   y_up = TextInfo.ta.orientation.y_up;

        int     x_spacing;
        int     y_spacing;

        int     xArray[BUFSIZ];
        int     yArray[BUFSIZ];
        int     num;

        if (! TextInfo.font_init) return(0);

        if (TextInfo.dirty) {
                make_matrix();
                trans_coord(&fcap_template,&fcap_current);
                path_spacing(TextInfo.ta.text_path);
                TextInfo.dirty = FALSE;
        }

        y_spacing = TextInfo.y_spacing;
        x_spacing = TextInfo.x_spacing;


        prev_width = 0;
        x_space = -x_spacing;
        y_space = -y_spacing;

        /*
         * make sure every character in the string has a definition. If
         * not change that character to a space so we can do something
         * reasonable when computing the alignment of the entire string
         */
        for (i=0; i<strlen(string); i++) {

                index = string[i] - F_CHAR_START(fcap_template);
                numstroke = F_NUMSTROKE(fcap_current, index);
                if (numstroke <= 1) {
                        string[i] = ' ';
                }
        }


        /* calculate text alignment     */
        text_align(&trans_x, &trans_y, strlen(string), string); 

        /*
         * move starting postition of string to its transformed position
         */
        x += trans_x; 
        y += trans_y;


        left_ext = leftExtent2[string[0] - F_CHAR_START(fcap_template)];

        /* transform text in to cgmc polylines  */
        for (char_ind=0; char_ind < strlen(string); char_ind++) {
                k = i = 0;      /* stroke index in Fontable and cgmc    */

                /*
                 * index into Fontable
                 */
                index = string[char_ind] - F_CHAR_START(fcap_template); 

                if (index >= F_NUMCHAR(fcap_template)){
                        continue;
                }

                /* 
                 * number of strokes making up a character.
                 */
                numstroke = F_NUMSTROKE(fcap_current, index);



                /* 
                 * compute position of character as a function of 
                 * text path and orientation, inter-character spacing, and
                 * size of previous character's body.
                 */
                switch (text_path) {
                
                case    RIGHT_TEXT_PATH:
                        x_space += x_spacing + 
                                ((prev_width + left_ext - leftExtent2[index]) * 
                                x_base/MAG(x_base,y_base));

                        y_space += y_spacing +
                                ((prev_width + left_ext - leftExtent2[index]) * 
                                y_base/MAG(x_base,y_base));
                        break;

                case    LEFT_TEXT_PATH:
                        x_space += x_spacing - 
                                ((prev_width + left_ext - leftExtent2[index]) * 
                                x_base/MAG(x_base,y_base));

                        y_space += y_spacing -
                                ((prev_width + left_ext - leftExtent2[index]) * 
                                y_base/MAG(x_base,y_base));
                        break;

                case    UP_TEXT_PATH:
                        x_space += x_spacing + 
                                (prev_width * x_up/MAG(x_up,y_up));
 
                        y_space += y_spacing +
                                (prev_width * y_up/MAG(x_up,y_up));
                        break;

                case    DOWN_TEXT_PATH:
                        x_space += x_spacing -
                                (prev_width * x_up/MAG(x_up,y_up));
 
                        y_space += y_spacing -
                                (prev_width * y_up/MAG(x_up,y_up));
                        break;
                }

                xArray[k] = F_X_COORD(fcap_current, index, i)
                        + x_space + x;

                yArray[k] = F_Y_COORD(fcap_current, index, i)
                        + y_space + y;

                k++; i++; 



                for (;i < numstroke;i++) { 
                        if (!(F_PEN(fcap_template, index, i))) {
                                num = k;

                                if (num > 1) {
                                        (*lines)(xArray, yArray,num,lines_data);
                                }
                                k = 0;
                        } 

                        xArray[k] = F_X_COORD(fcap_current, index, i)
                                + x_space + x;

                        yArray[k] = F_Y_COORD(fcap_current, index, i)
                                + y_space + y;
                        k++; 
                }

                num = k;

                if (num > 1) {
                        (*lines)(xArray, yArray, num, lines_data);
                }
                k = 0;

                /*
                 * store width/height of this character so we can use
                 * it to compute the position of the next character
                 */
                if (text_path == UP_TEXT_PATH || text_path == DOWN_TEXT_PATH) {
                        prev_width =
                                F_FONT_TOP(fcap_current) -
                                F_FONT_BOTTOM(fcap_current);
                }
                else {
                        prev_width = Widtharray2[index];
                        left_ext = leftExtent2[index];
                }


        }       /* for loop     */
        return(0);
}
 


