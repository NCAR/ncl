/*
 *      $Id: fort_c.c,v 1.13 2009-04-16 06:51:33 fred Exp $
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
 *      File:           fort_c.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri May  3 11:50:09 MDT 1991
 *
 *      Description:    This file provides the fortran interface for the gks
 *                      driver. The inteface is implemented via a single 
 *                      Fotran callable C-function, ggkdvr.
 *
 *      Warning:        This package may be preprocessed at install time
 *                      to reflect FORTRAN-to-C calling conventions on a
 *                      specific system.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ncarg/c.h>
#ifdef cray
#include <fortran.h>
#endif

#include "gks.h"
#include "gksc.h"
#include "fort_c.h"

/*
 *  Original GKS workstation ID to be made available to the various
 *  drivers at open workstaiton time.
 */
int     orig_wks_id;

/*
 *      C-string-to-Fortran-integer-array strncpy. Remaining array
 *      elements are space padded
 */
static  int     *c2f_strncpy(fs, cs, n)
        int     *fs;
        char    *cs;
        int     n;
{
        int     *ret = fs;
        int     i;

        for (i=0; i<n && *cs; i++, cs++, fs++) {
                *fs = (int) *cs;
        }
        for( ;i < n; i++, fs++) {
                *fs = (int) ' ';
        }
        return(ret);
}

/*
 *      ggkwdr_()
 *      [exported, Fortran-callable]
 *
 *      ggkwdr() provides the connection between the lower-level C gks
 *      driver and the high-level Fortran GKS library.
 *
 *      Warning: This procedure may be rewritten at install time
 *      to reflect the C/FORTRAN calling characteristics of
 *      the compilers in use.
 *
 * on entry
 *      wk_id           : A workstation identifier which is created by a 
 *                        driver at OPEN WORKSTATION time and passed back to 
 *                        GKS (cf.  OPEN WORKSTATION below).
 *      gks_opcode      : function code flagging the current instruction
 *                        being processed (for example, gks_opcode = 11 
 *                        indicates POLYLINE).
 *      continuation    : A continuation flag (1 means more to come in this 
 *                        instruction sequence; 0 means done).  One GKS 
 *                        function may result in many calls to the interface 
 *                        subroutine.
 *      total_i         : Total number of elements to be passed in the 'ints'
 *                        array for the current instruction.
 *      num_i_sent      : Number of elements in the 'ints' array for the given
 *                        workstation interface invocation.
 *      ints            : Array for passing integers.
 *      total_x         : Total number of elements to be passed in the 'indexs'
 *                        array for the current instruction.
 *      num_x_sent      : Number of elements in the 'indexs' array for the given
 *                        workstation interface invocation.
 *      indexes         : Array for passing color indeces.
 *      total_f         : Total number of elements to be passed in the 'fxs' 
 *                        and 'fys' arrays for the current instruction.
 *                        (actually in some instances only the 'fxs' array is
 *                        used and 'total_f' only applies to 'fxs'
 *      num_f_sent      : Number of elements in the 'fxs' and 'fys' arrays for 
 *                      : the given workstation interface invocation.
 *      fxs             : Array for passing real X coordinate values (all
 *                        coordinates are passed in NDC).  This array can also
 *                        be used for passing any real value, such as the
 *                        line width scale factor.
 *      fys             : Array for passing real Y coordinate values.
 *      total_c         : Total number of characters to be passed in the
 *                        variable 'chars' for the current instruction.
 *      num_c_sent      : Number of characters in the character variable 'chars'
 *                        for the current invocation of the workstation
 *                        interface.
 *      chars           : An integer array containing the ASCII decimal
 *                        equivalents of characters.  It will probably
 *                        be more portable to pass the characters this way
 *                        rather than via a Fortran CHARACTER variable.
 *      status          : An error return parameter.  If returned as zero,
 *                        the call was executed without error.  Otherwise
 *                        a specifically defined error number is returned.
 *                        Most errors will be intercepted above the workstation
 *                        interface.
 *      err_msg         : An error message return array with ERR_MSG_MAX bytes
 *                        of storage. If on return status is set to -113
 *                        err_msg will contain an error message.
 */
int
NGCALLF(ggkwdr,GGKWDR)
#ifdef  NeedFuncProto
(
        unsigned int    *wk_id,
        int             *gks_opcode,
        int             *continuation,
        int             *total_i,
        int             *num_i_sent,
        int             *ints,
        int             *total_x,
        int             *num_x_sent,
        int             *indexes,
        int             *total_f,
        int             *num_f_sent,
        float           *fxs,
        float           *fys,
        int             *total_c,
        int             *num_c_sent,
        int             *chars,
        int             *status,
#ifdef UNICOS
        _fcd    err_msg_
#else
        char    *err_msg
#endif
)
#else
(wk_id,gks_opcode,continuation,total_i,num_i_sent,ints,total_x,num_x_sent,
indexes,total_f,num_f_sent,fxs,fys,total_c,num_c_sent,chars,status,
#ifdef UNICOS
err_msg_)
#else
err_msg)
#endif
        unsigned int    *wk_id;
        int             *gks_opcode;
        int             *continuation;
        int             *total_i;
        int             *num_i_sent;
        int             *ints;
        int             *total_x;
        int             *num_x_sent;
        int             *indexes;
        int             *total_f;
        int             *num_f_sent;
        float           *fxs;
        float           *fys;
        int             *total_c;
        int             *num_c_sent;
        int             *chars;
        int             *status;
#ifdef UNICOS
        _fcd    err_msg_;
#else
        char    *err_msg;
#endif
#endif  /* NeedFuncProto */
{
        char    *dev_name;
        GKSC    *gksc;
        int     dev_type = 0;

#ifdef cray
        unsigned        length = _fcdlen(err_msg_);
        char            *err_msg;

        err_msg = (char *)malloc(sizeof(char)*length);
        strncpy( err_msg, _fcdtocp(err_msg_), length );
#endif
        *status = 0;    /* no errors yet        */

        /*
         * if this is an open_workstation command then we need to create
         * the gksc for this workstation
         */
        if (*gks_opcode == GKS_OPEN_WORKSTATION) {
                orig_wks_id = *wk_id;

                /*
                 * decode device type
                 */
                dev_type = ints[1];

                /*
                 * For driver codes that are variations on a common
                 * set of functions, set dev_type to the generic name.
                 */
                if ((dev_type >= DEV_PS_MIN) && (dev_type <= DEV_PS_MAX)) {
                        dev_type = DEV_PS;
                }
                if ((dev_type >= DEV_CRO_MIN) && (dev_type <= DEV_CRO_MAX)) {
                        dev_type = DEV_CRO;
                }
                switch (dev_type) {
                case    DEV_CGM:
                        dev_name = "CGM";
                        break;
                case    DEV_X11:
                case    DEV_X11P:
                        dev_name = "X11";
                        break;
                case    DEV_PIX:
                        dev_name = "PIX";
                        break;
                case    DEV_WISS:
                        dev_name = "WISS";
                        break;
                case    DEV_CTXT:
                        dev_name = "ctxt";
                        break;
                case    DEV_PS:
                        dev_name = "ps";
                        break;
                case    DEV_PDF_P:
                        dev_name = "pdf";
                        break;
                case    DEV_PDF_L:
                        dev_name = "pdf";
                        break;
                case    DEV_CRO:
                        dev_name = "cro";
                        break;
                default:
                        *status = ERR_OPN_DEV;
                        return(-1);
                }

#ifdef  DEBUG
        if (getenv("GKS_CTXT")) {
                dev_name = "ctxt";
                fprintf(stderr, "Debugging on - Using clear text driver\n");
        }
#endif

                        
                /*
                 * create a gksc for this device
                 */

                if ((gksc = CreateGKSC((char*)dev_name)) == 0) {
                        *status = ErrGetNum();
                        c2f_strncpy((int*)err_msg, (char*)ErrGetMsg(),
                                                                ERR_MSG_MAX);
#ifdef cray
                        strncpy( _fcdtocp(err_msg_), err_msg, length );
#endif
                        return(-1);
                }

        } 
        else {
                gksc = IndexToGKSC(*wk_id);
        }

#ifdef  IDEBUG
        printf("\nopcode -> %d\n", *gks_opcode);
#endif


        /*
         * load the data into the appropriate fields of the gksc
         */
        *status = WriteToGKSC(gksc, *gks_opcode, 
                        *total_i, *num_i_sent, ints,
                        *total_x, *num_x_sent, indexes,
                        *total_f, *num_f_sent, fxs, fys,
                        *total_c, *num_c_sent, chars);

        if (*status != 0) {
                c2f_strncpy((int*)err_msg,(char*)ErrGetMsg(), ERR_MSG_MAX);
#ifdef cray
                strncpy( _fcdtocp(err_msg_), err_msg, length );
#endif
                return(-1);
        }
        

        /*
         * kludge: we return the index of the gksc in one of out
         * input parameters, num_i_sent
         */
        if (*gks_opcode == GKS_OPEN_WORKSTATION) {
                *num_i_sent = (int) GKSCToIndex(gksc);
        }
        
        /*
         * if there is more data coming don't do anything until we get 
         * the rest of it.
         */
        if (*continuation) {
                return(-1);
        }

        /*
         * execute the gks command
         */
        if(gksc->exec_gksc)
                *status = (*(gksc->exec_gksc))(gksc);
        else
	      
	        *status = (*(gksc->operations[gksc->opcode]))(gksc);
		if (*status != 0) {
			c2f_strncpy((int*)err_msg,(char*)ErrGetMsg(), ERR_MSG_MAX);
#ifdef cray
			strncpy( _fcdtocp(err_msg_), err_msg, length );
#endif
			ClearGKSC(gksc);
			return(-1);
		}

		/*
		 * if this is an inquiry function retreive the data now
		 */
		if (*gks_opcode < -100) {       /* gks inquiry function */
			*status = ReadFromGKSC(gksc, *gks_opcode, 
				total_i, num_i_sent, ints,
				total_x, num_x_sent, indexes,
				total_f, num_f_sent, fxs, fys,
				total_c, num_c_sent, chars);

			if (*status != 0) {
				c2f_strncpy((int*)err_msg,(char*)ErrGetMsg(),
									ERR_MSG_MAX);
#ifdef cray
				strncpy( _fcdtocp(err_msg_), err_msg, length );
#endif
			}
		}
		ClearGKSC(gksc);

		/*
		 * if this was a close_workstation gks_opcodeuction free the gksc
		 */
		if (*gks_opcode == GKS_CLOSE_WORKSTATION) {
			FreeGKSC(gksc);
		} 

		return(0);
}
