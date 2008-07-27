/*
 *	$Id: vms.c,v 1.6 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - VMS Release                   *
*                                                                      *
***********************************************************************/
/*
 * bogus CVS 1.3beta test
 */

#ifdef	VMS

/* VMSTTY has been modified in this copy -- details below (wea)       */

/*      Author: Chris J. Phillips NCAR-MMM                             *
*       Purpose: To provide functions utilized by CTRANS that are      *
*               not supported directly by VAX C but are by most        *
*               UNIX implementations.  This module also contains       *
*               functions that are supported by VAX but do not work    *
*               in quite the same way.  This module also conatains     *
*               the code for the functions that are needed by the      *
*               new version.                                          */

#include <stdio.h>
#include <iodef.h> 
#include <string.h>
#include <file.h>


/* The following two routines are designed to read in the graphcap and fontcap
through the procedures VMSgcread() and VMSfcread() respectively.  These are
needed in place of the entire file reads for two reasons.  First, the C
compiler used for VMS, VAXC, doesn't allow for one read to cross record
boundries.  Secondly, since the file was written from a FORTRAN application,
there are extraneous bytes associated with each record.  One last item to be
taken into account is that any record which was intended to be longer than the
maximum record length for the binary files was broken up by VMS into ones which
were either that maximum or less.  This needs to be accounted for also. */

#define MAXRECLEN       2042    /* 2044 minus EXTRA reserved for file use. */
#define EXTRA           2
#define ERROR           -1

#include "boolean.h"
#include "graphcap.h"   /* The graphcap information. */

/*  Name: VMSgcread
    Author: Chris J. Phillips  NCAR/MMM
    Purpose: This function will read in the graphcap under VMS.  It will
                provide the same functionality as the single read() statement
                in the UNIX version of CTRANS.
    Arguments:  fd:     File descriptor for the binary graphcap file.
                gcbuf:  Pointer to the graphcap buffer.
                gcsiz:  The size, in bytes, of the graphcap struct. (Not used.)
*/
#define MAXGCRECS 10            /* The maximum number of graphcap (intended) 
                                  records. */

int  VMSgcread(fd, gcbuf, gcsiz)
        int  fd, gcsiz;
        char *gcbuf;
{
        register int i;         /* Loop varible. */
        int nbytes = 0;         /* The number of bytes read in so far. */
        int reclentab[MAXGCRECS], numrecs = 0, temp;
        char *ptr, tempbuf[EXTRA];

        /* Set up the pointer to the buffer and the table of record lengths. */
        ptr = gcbuf;
        reclentab[numrecs++] = sizeof(Dev_class);
        reclentab[numrecs++] = sizeof(Dev_line);
        reclentab[numrecs++] = sizeof(Dev_user_prompt);
        reclentab[numrecs++] = sizeof(Dev_colour);
#ifdef USE_DEV_TEXT
        reclentab[numrecs++] = sizeof(Dev_text);
#endif
#ifdef USE_DEV_MARKER
        reclentab[numrecs++] = sizeof(Dev_marker);
#endif
        reclentab[numrecs++] = sizeof(Dev_polygon);
#ifdef USE_DEV_BUNDLE
        reclentab[numrecs++] = sizeof(Dev_bundle);
#endif
        reclentab[numrecs++] = sizeof(Dev_raster);
#ifdef USE_FREE_SPACE
        reclentab[numrecs++] = sizeof(Free_space);
#endif

/* Read in each record while breaking them down if necessary and skipping the
extra bytes. */

        for (i =0; i < numrecs; i++)
        {
                while (reclentab[i] > MAXRECLEN)
                {
                        if ((temp=read(fd, tempbuf, EXTRA)) != EXTRA)
                                return(ERROR);
                        if ((temp=read(fd, ptr, MAXRECLEN)) != MAXRECLEN)
                                return(ERROR);
                        reclentab[i] -= MAXRECLEN;
                        ptr += MAXRECLEN;
                        nbytes += MAXRECLEN;
                }
                if ((temp=read(fd, tempbuf, EXTRA)) != EXTRA)
                        return(ERROR);
                if ((temp=read(fd, ptr, reclentab[i])) != reclentab[i])
                        return(ERROR);
                ptr += reclentab[i];
                nbytes += reclentab[i];
        }

        return(nbytes);
}

/*  Name: VMSfcread
    Author: Chris J. Phillips  NCAR/MMM
    Purpose: This function will read in the fontcap under VMS.  It will
                provide the same functionality as the single read() statement
                in the UNIX version of CTRANS.
    Arguments:  fd:     File descriptor for the binary fontcap file.
                fcbuf:  Pointer to the fontcap buffer.
                fcsiz:  The size, in bytes, of the fontcap struct.
*/

int  VMSfcread(fd, fcbuf, fcsiz)
        int  fd, fcsiz;
        char *fcbuf;
{
        register int i;         /* Loop varible. */
        int nbytes = 0,temp;         /* The number of bytes read in so far. */
        char *ptr, tempbuf[EXTRA];

        /* Set up the pointer to the buffer. */
        ptr = fcbuf;

/* Read in the entire struct while breaking it down if necessary and skipping 
the extra bytes. */

        while (fcsiz > MAXRECLEN)
        {
                if ((temp=read(fd, tempbuf, EXTRA)) != EXTRA)
                        return(ERROR);
                if ((temp=read(fd, ptr, MAXRECLEN)) != MAXRECLEN)
                        return(ERROR);
                fcsiz -= MAXRECLEN;
                ptr += MAXRECLEN;
                nbytes += MAXRECLEN;
        }
        if ((temp=read(fd, tempbuf, EXTRA)) != EXTRA)
                return(ERROR);
        if ((temp=read(fd, ptr, fcsiz)) != fcsiz)
                return(ERROR);
        nbytes += fcsiz;
        
        return(nbytes);
}

/*  Name: VMSswap
    Author: Chris J. Phillips
    Purpose: To take an integer array and perform byte swapping with its
            contents.
    Arguments:  ptr - Pointer to the integer array.
                size - Size of the integer array.
*/

int  VMSswap(ptr, size)
        int *ptr, size;
{
        char *buf, temp;
        int i;

        for (i = 0; i < size; i++)
        {
                buf = (char *) ptr;
                temp = buf[0];
                buf[0] = buf[3];
                buf[3] = temp;
                temp = buf[1];
                buf[1] = buf[2];
                buf[2] = temp;
                ptr++;
        }
}


/*  Name: VMStty
    Author: Chris J. Phillips & Bill Boyd

	Modified by Warren Auld (2 May, 1991) to fix loop infinite in 
	VMSTTY. Increased the size of tmpbuf to handle the 3 record case.
 
    Purpose: To simulate raw graphics mode in VMS and to allow batch jobs to 
                be written to a file which has a fixed record format with the
                record size being RECLEN.
    Arguments:  mode:   Integer indicating the function to be performed.
                            1 -- Assign a channel to the terminal or open
                                  a file.
                            2 -- Read block from the channel.
                            3 -- Write block to the channel or file.
                            4 -- Deassign the channel to the terminal or 
                                  close a file.
                            5 -- Flush the output buffer.
                buffer: The buffer to be written or read.
                arglen: The length of buffer.
*/
#define OUTPUT_BUF_SIZE 1024
#define RECLEN  800
#define TEMP_BUF_SIZE  (OUTPUT_BUF_SIZE + RECLEN + RECLEN)
#define BATCH_FILE      "batdev.dat"

extern  char    *cur_dev_name;  /* The character string which 
                                  contains the device name. */

extern  int     ci_append;      /* The ci flag which tells VMStty whether to
                                  open the batch file under append or not. */

int  VMStty (mode, buffer, arglen)
        char          *buffer;
        int           mode, arglen;
{
 /* This buffer will contain the information which needs to be written.  It
    will hold the data which will not fit evenly in a record. */ 
 static char          tmpbuf[TEMP_BUF_SIZE];
 static int           index;    /* The index into tmpbuf[] */
        int           i, numitems, error;
        short         status[4];
 static short         tty;      /* The terminal channel. */
 static int           fd;   /* The file discriptor.  */
 static boolean       opened; /* Indicates whether a batch file is open or not. */

        struct  string_descr {
                short   length;         /*  Length of the string   */
                char    type;           /*  Type                   */
                char    cgmclass;       /*  Class                  */
                char    *address;       /*  Address of the string  */
        };
        struct  string_descr descr;


/* If not batch then use the terminal manipulation modes.*/
        if (!BATCH) 
        {
                if (mode == 1) 
                {
                        descr.length  = strlen (cur_dev_name);                
                        descr.type    = 14;
                        descr.cgmclass   = 1;
                        descr.address = cur_dev_name;
                        error = sys$assign (&descr, &tty, 0, 0);
                } 
                else if (mode == 2) 
                {
                        error = sys$qiow (0, tty,
                                  IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
                                  status, 0, 0, buffer, arglen, 0, 0, 0, 0);
                } 
                else if ((mode == 3) || (mode == 5)) 
                {
                        error = sys$qiow (0, tty,
                                  IO$_WRITEVBLK | IO$M_NOFORMAT,
                                  status, 0, 0, buffer, arglen, 0, 0, 0, 0);
                } 
                else if (mode == 4) 
                {
                        error = sys$dassgn (&tty);
                } 
                else 
                {
                        error = 1;
                }
        }
/* Else use the file manipulation modes. */
        else
        {
                if (mode == 1)
                {
                        /* Open the file and set the index to zero. */
                        index = 0;
                        if (ci_append)
                                fd = open(BATCH_FILE, O_WRONLY | O_CREAT | 
                                        O_APPEND, 0, "ctx=rec",
                                                "mrs=800", "rfm=fix");
                        else
                                fd = open(BATCH_FILE, O_WRONLY | O_CREAT | 
                                        O_TRUNC, 0, "ctx=rec", 
                                                "mrs=800", "rfm=fix");
                        if (fd == -1)
                        {
                                error = fd;
                        }
                        opened = TRUE;
                }
                else if (mode == 2)
                {
                        /* Not currently used. */
                }
                else if (mode == 3)
                {
                        if (!opened)
                                return(0);
                        /* Write out the buffer pointed to by buffer.
                           If there is not enough data to fill another
                           record then leave it in tmpbuf. */
                        for (i = 0; i < arglen; i++)
                                tmpbuf[i + index] = buffer[i]; 
                        index += arglen;
                        numitems = 0;
                        while (index >= RECLEN)
                        {
                                numitems++;
                                index  -= RECLEN;
                        }
                        error = write(fd, tmpbuf, RECLEN*numitems);
                        if (error < 0)
                        {
                        }
                        for (i = 0; i < index; i++)
                                tmpbuf[i] = tmpbuf[numitems*RECLEN + i];
                }
                else if (mode == 4)
                {
                        if (!opened)
                                return(0);
                        /* Close the file. */
                        error = close(fd);
                        opened = FALSE;
                }
                else if (mode == 5)
                {
                       if (!opened)
                                return(0);
                        /* Flush the buffer. */
                        for (i = 0; i < arglen; i++)
                                tmpbuf[i + index] = buffer[i]; 
                        index += arglen;
                        numitems = 0;
                        while (index > RECLEN)
                        {
                                numitems++;
                                index  -= RECLEN;
                        }
                        while (index != RECLEN)
                        {
                                tmpbuf[numitems*RECLEN + index] = ' ';
                                index++;
                        }
                        numitems++;
                        error = write(fd, tmpbuf, RECLEN*numitems);
                        if (error < 0)
                        {
                        }
                        index = 0;
                }
                else
                {
                        /* Error. */
                        error = -1;
                }
        }
        return (error);
}

#endif VMS
