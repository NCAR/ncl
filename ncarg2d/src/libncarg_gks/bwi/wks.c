/*
 *      $Id: wks.c,v 1.10 2008-12-28 13:28:39 haley Exp $
 */
/***********************************************************************
*                                                                      *
*               NCAR Graphics - UNIX Version 3.1.3b                    *
*               Copyright (C) 1987, 1988, 1989, 1991, 2000             *
*               University Corporation for Atmospheric Research        *
*               All Rights Reserved                                    *
*                                                                      *
************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*
*       File:           wks.c
*
*       Description:
*
*       This file contains a set of routines for basic file and pipe 
*       I/O for NCAR Graphics. These functions are generally called
*       from the Fortran layer of NCAR Graphics. The calling routines 
*       believe that these routines are performing I/O using Fortran
*       logical units. This is only simulated, however, and standard 
*       C I/O is used.
*
*       Output is by default directed to a file named "gmeta".
*       The environment variable NCARG_GKS_OUTPUT can be used
*       to redirect output to a different file name, if desired,
*       or even a translator process.
*
*       Examples:
*                       setenv NCARG_GKS_OUTPUT myfile
*
*       causes CGM output to be place in "myfile".
*
*                       setenv NCARG_GKS_OUTPUT "|ctrans"
*
*       causes the "ctrans" translator to be forked and CGM output
*       piped to it.
*
*                       setenv NCARG_GKS_OUTPUT "|"
*
*       causes a default translator, defined in wks.h, to be forked
*       and CGM output to be piped to it. Note that not just any
*       process can be used here because wks.c assumes that
*       the translator is invoked as "translatorname -" where
*       the "-" indicates that the translator is to read from stdin.
*
*       If output is to a standard file, buffered I/O is used. On
*       some systems it is desirable to adjust this and several
*       options for this are available.
*       Buffer size is adjustable as follows:
*
*       1. If environment variable NCARG_GKS_BUFSIZE is set to N
*
*                       Buffer Size = N * 1024 bytes.
*
*       2. Otherwise, if the value of DEFAULT_GKS_BUFSIZE in wks.h
*          is edited by the Makefile to be non-zero.
*
*                       Buffer Size = DEFAULT_GKS_BUFSIZE * 1024 bytes.
*
*       3. Otherwise, (when DEFAULT_GKS_BUFSIZE is 0)
*
*                       Buffer Size = BUFSIZ (from stdio.h)
*
*       Segments, or "flash buffers", are allocated a buffer that is
*       the default size. This is because they are generally smaller
*       than the main CGM output streams.
*
*
*       Author:         Don Middleton
*                       NCAR Scientific Computing Division
*
*       Last Revised:   October, 2003
*
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncarg/c.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#ifdef	__FreeBSD__
#include <sys/filio.h>
#else
#include <sys/file.h>
#endif	/* __FreeBSD__ */
#ifdef cray
#include <fortran.h>
#endif

#include "wks.h"

static int              wks_init   = FALSE;

/* Environment variables. */

static char             *gks_gencgm_env  = (char *) NULL;
static char             *gks_output_env  = (char *) NULL;
static char             *gks_debug_env   = (char *) NULL;
static char             *gks_bufsize_env = (char *) NULL;

/*
The table "mftab" is used to maintain the list of fake Fortran
logical units. The file pointer, type of file, any associated
I/O buffers, and a flag indicating if the stream is the "main"
stream are maintained here.
*/

static struct
{
        char    *name;
        FILE    *fp;
        int     type;
        char    *buf;
        int     segment;
} mftab[MAX_UNITS];


/*************************************************************************
*
*       Function:       opnwks_(unit, openf, string, status)
*
*                       The filename contained in "string" is
*                       opened and associated with a fake Fortran
*                       LU number, "unit". The argument "status"
*                       is loaded with a status code on return.
*                       "openf" is a flag for the open mode:
*                         = 0  open for reading only
*                         = 1  truncate and open for reading and writing
*                         = 2  open a segment for writing and reading.
*                         = 3  open a CGM for appending.
*
*       Called From:    Mostly Fortran routines in the NCAR GKS library.
*
*       Returns:        "status", which is encoded as per NCAR Graphics
*                       GKS manual. Does not return a meaningful
*                       function value.
*
************************************************************************/

#ifdef cray
NGCALLF(opnwks,OPNWKS)(unit, openf, fname_, status)
        _fcd    fname_;
#else
int     NGCALLF(opnwks,OPNWKS)(unit, openf, fname, status)
        char    *fname;
#endif
        int     *unit, *openf, *status;
{
        int             i, pipes[2], stat;
        char    *p;
        int             default_bufsize;
        int             bufsize = 0;
        char    *otype, *tpath, *tname;
#ifdef cray
        unsigned        length = _fcdlen(fname_);
        char            *fname;

        fname = (char *) calloc(sizeof(char)*(length+1),sizeof(char));
        strncpy( fname, _fcdtocp(fname_), length );
#endif

        /* Initialize the table that is used to track LU's. */

        if (!wks_init)
        {
                /* Pick up the environment variables. */
                gks_debug_env   = getenv("NCARG_GKS_DEBUG");
                gks_output_env  = getenv("NCARG_GKS_OUTPUT");
                gks_gencgm_env  = getenv("NCARG_GKS_GENCGM");
                gks_bufsize_env = getenv("NCARG_GKS_BUFSIZE");

                /* Initialize device table. */
                for(i=0; i<MAX_UNITS; i++)
                {
                        mftab[i].name    = (char *) NULL;
                        mftab[i].fp      = MF_CLOSED;
                        mftab[i].type    = NO_OUTPUT;
                        mftab[i].buf     = (char *) NULL;
                        mftab[i].segment = FALSE;
                }
                wks_init = TRUE;
        }

        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: opnwks_(%d,%10s)\n",*unit,fname);
        }

        if (*unit >= MAX_UNITS)
        {
                (void) fprintf(stderr,
                "Error in opnwks_(): Max workstations (%d) exceeded\n"
                ,MAX_UNITS);
                *status = 304;
#ifdef cray
                free(fname);
#endif
                return(0);
        }

        if (mftab[*unit].fp != MF_CLOSED)
        {
                (void) fprintf(stderr, 
                "Error in opnwks_(): Attempt to reopen unit %d\n", *unit);
                *status = 304;
#ifdef cray
                free(fname);
#endif
                return(0);
        }


        /*
        Segments, or "flash buffers", are different from
        all other files. They must be files rather than
        processes, and they must be NCAR-blocked CGM, so they
        can be read back in.
        */

 
        if ((*openf == 0) || (*openf == 2)) {

                /* Is a segment. */

                mftab[*unit].type    = FILE_OUTPUT;

                if (!strncmp(fname, "GSEG", 4)) {
                
                        /* The file is in the NGTMPDIR directory */
                        tpath = (char *) GetNCARGPath(NGTMPDIR);
                        tname = (char *) calloc(strlen(tpath) + strlen("/") +
                                        strlen(fname) + 1, sizeof(char));
                        (void) strcpy(tname, tpath);
                        (void) strcat(tname, "/");
                        (void) strcat(tname, fname);    
                        mftab[*unit].name    = tname;
                }
                else {

                        /* File is not a temporary */
                        mftab[*unit].name = calloc(strlen(fname)+1,
                                                sizeof(char));
                        (void) strcpy(mftab[*unit].name, fname);
                }

                mftab[*unit].segment = TRUE;
        }
        else if (*openf == 3) {
               
                /* Open CGM for append */

                mftab[*unit].type = FILE_OUTPUT;
                mftab[*unit].name = calloc(strlen(fname)+1, sizeof(char));
                (void) strcpy(mftab[*unit].name, fname);
                mftab[*unit].segment = FALSE;
        }
        else {

                /* Output is not a segment */

                if (gks_output_env  == (char *) NULL)
                {
                        /*
                        NCARG_GKS_OUTPUT is not set so output
                        filename is whatever comes down from
                        above.
                        */
                        mftab[*unit].type = FILE_OUTPUT;

                        /*
                        If file name is GMETA convert to lower case.
                        Provide for the case where the string is properly
                        terminated, and where it is not (as on some
                        poorly constructed DEC compilers). Make sure
                        that a name such as "GMETASTUFF" passes on
                        through ok.
                        */

                        if (!strncmp(fname, "GMETA", 5) &&
                                        (fname[5] == '\0'||fname[5] == ' ') )
                                tname = "gmeta";
                        else
                                tname = fname;
                        mftab[*unit].name =calloc(strlen(tname)+1,sizeof(char));
                        (void) strcpy(mftab[*unit].name, tname);
                }
                else
                {
                        /*
                        NCARG_GKS_OUTPUT is set so find out
                        if it's to be a file or a translator
                        process.
                        */

                        /* Skip blanks. */
                        for(p=gks_output_env; *p==' ' && *p != '\0'; ) p++;
        
                        if (*p == '|')
                        {
                                /* Skip blanks. */
                                for(p++; *p==' ' && *p != '\0'; ) p++;
                                mftab[*unit].type = PIPE_OUTPUT;
                                mftab[*unit].name =
                                        calloc(strlen(p)+1,sizeof(char));
                                (void) strcpy(mftab[*unit].name,p);
                        }
                        else
                        {
                                mftab[*unit].type = FILE_OUTPUT;
                                mftab[*unit].name =
                                        calloc(strlen(gks_output_env)+1,
                                                        sizeof(char));
                                (void)strcpy(mftab[*unit].name,gks_output_env);
                        }
                }
        }

        /*
        If GKS output is a file, open it for reading and writing.
        Unless the file is being opened for appending, Truncate 
        a pre-existing file or create a new one for writing. A 
        stream is attached to the file and a buffer allocated
        of the size constant BUFSIZ from stdio.h or from
        the user-defined environment variable NCARG_GKS_BUFSIZE,
        which specifies the number of 1024-byte blocks to
        use for I/O.

        If GKS output is a process, it is assumed to be
        a translator invoked as follows:

                        translatorname -
        
        It is assumed to read from stdin. The process is forked and
        file streams are connected to pipe file descriptors. Stream
        I/O is unbuffered.
        */

        if (mftab[*unit].type == PIPE_OUTPUT) {
                if (strlen(mftab[*unit].name) == 0)
                {
                        mftab[*unit].name = calloc(strlen(DEFAULT_TRANSLATOR)+1,
                                                        sizeof(char));
                        (void)strcpy(mftab[*unit].name,DEFAULT_TRANSLATOR);
                }

                (void) pipe(pipes);

                if ( fork() == 0 )
                {
                        (void) close(pipes[1]);
                        (void) close(0); 
                        (void) dup(pipes[0]);
                        (void) execlp(mftab[*unit].name, 
                                      mftab[*unit].name, "-", (char *) 0);
                        (void) fprintf(stderr, 
                        "wks.c: opngks_() - Could not fork translator \"%s\"\n",
                        mftab[*unit].name);
                        *status = 304;
#ifdef cray
                        free(fname);
#endif
                        return(0);
                }
                else
                {
                        (void) fflush(stderr);
                        (void) close(pipes[0]);
                        mftab[*unit].fp = fdopen(pipes[1], "w");
                        /*setvbuf(mftab[*unit].fp, (char *) NULL, _IONBF, 0);*/
                }

        }
        else {
                /*
                 * Set the read/write mode of the file according to the
                 * read/write flag argument.  Files to be written will
                 * be truncated if they exist.
                 */
                if (*openf == 0)  {
                        otype = "r";
                }
                else if (*openf == 3) {
                        otype = "r+";
                }
                else {
                        otype = "w+";
                }

                mftab[*unit].fp = fopen(mftab[*unit].name, otype);
                if (mftab[*unit].fp == (FILE *) NULL) {
                        (void) fprintf(stderr, 
                        "wks.c: Error in opngks_(): Could not open \"%s\"\n", 
                        mftab[*unit].name);
                        *status = 304;
#ifdef cray
                        free(fname);
#endif
                        return(0);
                }

                /*
                The environment variable NCARG_GKS_BUFSIZE, a macro
                definition, or a constant from stdio.h can determine
                buffer size used. See notes at the top of this file.
                Segments, or "flash buffers", are assigned a
                buffer size that is the system default.
                */

                if (gks_bufsize_env == (char *) NULL) {
                        default_bufsize = DEFAULT_GKS_BUFSIZE;
                        if (default_bufsize == 0) {
                                bufsize = BUFSIZ; /* from stdio.h */
                        }
                        else {
                                if (mftab[*unit].segment == TRUE) {
                                        bufsize = BUFSIZ;
                                }
                                else {
                                        bufsize = 1024 * default_bufsize;
                                }
                        }
                }
                else {
                        if (mftab[*unit].segment == TRUE) {
                                bufsize = BUFSIZ;
                        }
                        else {
                                bufsize = 1024 * atoi(gks_bufsize_env);
                        }
                        if (bufsize <= 0) {
                          (void) fprintf(stderr,
                          "opnwks(): User-supplied buffer size too small\n");
                          (void) fprintf(stderr,
                          "opnwks(): Using system default (%d)\n", BUFSIZ);
                          bufsize = BUFSIZ;
                        }
                }

                if (gks_debug_env) {
                        (void)fprintf(stderr,
                        "wks.c: Using stream bufsize    = %d\n", bufsize);
                        (void)fprintf(stderr,
                        "wks.c: Streams default bufsize = %d\n", BUFSIZ);
                }

                /*
                Dynamically allocate a buffer for the stream. Segments
                are generally allocated default size buffers, whereas
                the major output streams are allocated what has been
                configured, either in the config file, or using
                the environment variable.

                Freeing the buffers didn't used to work on the Cray
                and this should be kept in mind if problems arise
                in this part of the code.
                */

                if (gks_debug_env) {
                        (void) fprintf(stderr,
                                "wks.c: Allocating %d bytes for LU %d\n",
                                bufsize, *unit);
                }

                mftab[*unit].buf = (char *) malloc((unsigned) bufsize);

                if ( mftab[*unit].buf == (char *) NULL) {
                        (void) fclose(mftab[*unit].fp);
                        (void) fprintf(stderr,
                        "wks.c: Error in opngks_(): Memory allocation\n");
                        *status = 304;
#ifdef cray
                        free(fname);
#endif
                        return(0);
                }

                stat = setvbuf(mftab[*unit].fp, mftab[*unit].buf,
                               _IOFBF, bufsize);
                
                if (stat < 0) {
                        (void) free(mftab[*unit].buf);
                        (void) fclose(mftab[*unit].fp);
                        (void) fprintf(stderr, 
                                       "Error in opngks_(): setvbuf failed\n");
                        *status = 304;
#ifdef cray
                        free(fname);
#endif
                        return(0);
                }
                        
        }

        if (gks_debug_env) {
        (void) fprintf(stderr, 
                "wks.c: Opened %s as LU %d of type %d on fp %x and fd %d\n", 
                mftab[*unit].name, *unit, mftab[*unit].type, mftab[*unit].fp, 
                fileno(mftab[*unit].fp));
        }

#ifdef cray
        free(fname);
#endif

        return(0);
}

/*************************************************************************
*
*       Function:       clswks_(unit, status)
*
*                       The fake Fortran LU, "unit", is closed.
*                       The file or pipe associated with it is
*                       closed and any memory buffers associated
*                       with it are freed. "status" is loaded
*                       with a return code.
*
*       Called From:    Mostly Fortran routines in the NCAR GKS library.
*
*       Returns:        "status", which is encoded as per NCAR Graphics
*                       GKS manual. Does not return a meaningful
*                       function value.
*
*************************************************************************/

NGCALLF(clswks,CLSWKS)(unit, status)
        int     *unit;
        int     *status;
{
        int     child_status;

        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: clswks_(%d)\n", *unit);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: clswks() - Programming Error, not initialized\n");
                *status = 304;
                return(0);
        }

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
        {
                (void) fprintf(stderr,
                        "wks.c: Invalid close on unit %d\n", *unit);
                *status = 304;
                return(0);
        }
        else
        {
                /*
                Close the file and adjust the table. The buffers are 
                not explicitly freed because this is apparently accomplished 
                in the file close.  In any event, a Cray will go flakey
                if you attempt such an act.
                */

                (void) fflush(mftab[*unit].fp);
                (void) fclose(mftab[*unit].fp);

                /*
                Wait on any child translators out there. This is important 
                because, if the parent terminates while the translator is 
                still drawing to a graphics device, the shell prompt could
                be issued and then mixed with the graphics stream (on a tty). 
                The result is unpredictable, but generally unwanted.
                */

                if ( mftab[*unit].type == PIPE_OUTPUT ) {
                        (void) wait(&child_status);
                }

                /*
                Close the LU.  The memory
                pointer is left for the next use of the LU.
                */

                (void)free(mftab[*unit].name);
                mftab[*unit].name    = (char *) NULL;
                mftab[*unit].fp      = MF_CLOSED;
                mftab[*unit].type    = NO_OUTPUT;

                if (gks_debug_env) {
                        (void) fprintf(stderr,
                        "wks.c: clswks() - Freeing memory for LU %d\n",
                        *unit);
                }
                (void) free(mftab[*unit].buf);
                mftab[*unit].buf     = (char *) NULL;
                mftab[*unit].segment = FALSE;
        }
        return(0);
}

/*************************************************************************
*
*       Function:       wrtwks_(unit, buffer, length, status)
*
*                       The integer buffer, "buffer", of "length"
*                       *words* is written to the file or process 
*                       associated with the fake Fortran LU "unit".
*
*       Called From:    Mostly Fortran routines in the NCAR GKS library.
*
*       Returns:        "status", which is encoded as per NCAR Graphics
*                       GKS manual. The function does not return a
*                       meaningful value.
*
*************************************************************************/

NGCALLF(wrtwks,WRTWKS)(unit, buffer, length, status)
        int     *unit;
        int     *buffer;
        int     *length;
        int     *status;
{
        int             nb;
        unsigned char   *p;
        unsigned int    len;
#if defined(ByteSwapped)
        static unsigned char    locbuf[RECORDSIZE];
#endif

        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: wrtwks_(%d,%d)\n",*unit,*length);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: wrtwks() - Programming Error, not initialized\n");
                *status = 304;
                return(0);
        }

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
        {
                (void) fprintf(stderr, "wks.c: Invalid unit (%d)\n", *unit);
                *status = 304;
                return(0);
        }

        if ( (*length * sizeof(int)) != RECORDSIZE)
        {
                (void) fprintf(stderr, 
                        "wks.c: Error in wrtwks_() - Invalid length (%d)\n",
                        (*length * 4));
                *status = 304;
                return(0);
        }

        /*
        If the NCARG_GKS_GENCGM env variable is not set or
        the output stream IS a segment, put out NCGM (NCAR CGM).
        */
        if (!gks_gencgm_env || mftab[*unit].segment) {

                if (gks_debug_env) {
                        (void) fprintf(stderr,
                        "wks.c: Writing %d bytes of NCAR CGM\n", RECORDSIZE);
                }
                        
#if defined(ByteSwapped)
                bcopyswap(buffer, locbuf, RECORDSIZE);
                nb = fwrite((char *) locbuf, 1, RECORDSIZE, mftab[*unit].fp);
#else
                nb = fwrite((char *) buffer, 1, RECORDSIZE, mftab[*unit].fp);
#endif
        
                if (nb != RECORDSIZE)
                {
                        (void) fprintf(stderr, 
                        "wks.c: Error in wrtwks_() - Writing metafile\n");
                        *status = 304;
                        return(0);
                }
        }
        else {
                p = (unsigned char *) buffer;
                len = ( (*p) << 8 ) + *(p + 1);

                if (len % 2) len++;

                if (gks_debug_env) {
                        (void) fprintf(stderr,
                          "wks.c: Writing %d bytes of standard CGM\n", len);
                }

#if defined(ByteSwapped)
                bcopyswap(buffer, locbuf, RECORDSIZE);
                nb = fwrite((char *) &locbuf[4], 1, len, mftab[*unit].fp);
#else
                nb = fwrite(&p[4], 1, len, mftab[*unit].fp);
#endif
        
                if (nb != len)
                {
                        (void) fprintf(stderr, 
                        "wks.c: Error in wrtwks_() - Writing metafile\n");
                        *status = 304;
                        return(0);
                }
        }

        return(0);
}


/*************************************************************************
*
*       Function:       rdwks_(unit, buffer, length, status)
*
*                       "length" *words* are read into "buffer"
*                       from the file or pipe associated with
*                       "unit". "status" is used to return any
*                       error codes.
*
*       Called From:    Mostly Fortran routines in the NCAR GKS library.
*
*       Returns:        "status", which is encoded as per NCAR Graphics
*                       GKS manual. The function does not return a
*                       meaningful value.
*
*************************************************************************/
NGCALLF(rdwks,RDWKS)(unit, buffer, length, status)
        int     *unit;
        int     *buffer;
        int     *length;
        int     *status;
{
        int             nb;
#if defined(ByteSwapped)
        static char     locbuf[RECORDSIZE];
#endif

        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: rdwks_(%d,%d)\n",*unit,*length);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: rdwks() - Programming Error, not initialized\n");
                *status = 304;
                return(0);
        }

        if (mftab[*unit].type != FILE_OUTPUT)
        {
                (void) fprintf(stderr, 
        "wks.c: Error in rdwks_() - Cannot read from non-file output type\n");
                *status = 302;
                return(0);
        }

        if ( (*length * sizeof(int)) != RECORDSIZE)
        {
                (void) fprintf(stderr, 
                        "wks.c: Error in rdwks_() - Invalid length (%d)\n",
                        (*length * 4));
                *status = 302;
                return(0);
        }

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
        {
                (void) fprintf(stderr, 
                "wks.c: Error in rdwks_() - Invalid unit (%d)\n", *unit);
                *status = 302;
                return(0);
        }

#if defined(ByteSwapped)
        nb = fread( (char *) locbuf, 1, RECORDSIZE, mftab[*unit].fp );
        bcopyswap(locbuf, buffer, RECORDSIZE);
#else
        nb = fread( (char *) buffer, 1, RECORDSIZE, mftab[*unit].fp );
#endif
        
        if (nb == 0) {
                *status = -1;
        }
        else if (nb != RECORDSIZE) {
                (void) fprintf(stderr, 
                        "Error in rdwks_() : Reading metafile\n");
                *status = 302;
                return(0);
        }
        return(0);
}

NGCALLF(begwks,BEGWKS)(unit, status)
        int     *unit;
        int     *status;
{
        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: begwks_(%d)\n",*unit);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: begwks() - Programming Error, not initialized\n");
                *status = 304;
                return(0);
        }
        if (mftab[*unit].type != FILE_OUTPUT)
        {
                (void) fprintf(stderr, 
                "Error in begwks_() : Cannot seek on non-file output type\n");
                *status = 304;
                return(0);
        }

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
        {
                (void) fprintf(stderr, 
                "Error in begwks_() : Invalid unit (%d)\n", *unit);
                *status = 304;
                return(0);
        }

        if (mftab[*unit].type != PIPE_OUTPUT) {
          if (fseek(mftab[*unit].fp, 0L, L_SET) == -1)
          {
                  (void) fprintf(stderr, "Error in begwks_() : Seek failed\n");
                  *status = 304;
                  return(0);
          }
        }
        return(0);
}

NGCALLF(lstwks,LSTWKS)(unit, status)
        int     *unit;
        int     *status;
{
        if (gks_debug_env) {
                (void) fprintf(stderr, "lstwks_(%d)\n",*unit);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: lstwks() - Programming Error, - not initialized\n");
                *status = 304;
                return(0);
        }

        if (mftab[*unit].type != FILE_OUTPUT)
        {
                (void) fprintf(stderr, 
                "Error in lstwks_() : Cannot seek on non-file output type\n");
                *status = 304;
                return(0);
        }

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
        {
                (void) fprintf(stderr, 
                "Error in lstwks_() : Invalid unit (%d)\n", *unit);
                *status = 304;
                return(0);
        }

        if (mftab[*unit].type != PIPE_OUTPUT) {
          if ( fseek(mftab[*unit].fp, -1440L, L_INCR) == -1)
          {
                  (void) fprintf(stderr, "Error in lstwks_() : Seek failed\n");
                  *status = 304;
                  return(0);
          }
        }
        return(0);
}

NGCALLF(flswks,FLSWKS)(unit, status)
        int     *unit;
        int     *status;
{
        int     rc;

        if (gks_debug_env) {
                (void) fprintf(stderr, "wks.c: flswks_(%d)\n",*unit);
        }

        if (!wks_init) {
                (void) fprintf(stderr,
                "wks.c: flswks() - Programming Error, not initialized\n");
                *status = 304;
                return(0);
        }

        /* Make sure the requested unit is valid. */

        if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED) {
                (void) fprintf(stderr, "Invalid unit (%d)\n", *unit);
                *status = 304;
                return(0);
        }

        if (mftab[*unit].type != PIPE_OUTPUT) {
          if (fseek(mftab[*unit].fp, 0L, SEEK_CUR) != 0) {
                  (void) fprintf(stderr, "Error in flswks_() : Seek failed\n");
                  *status = 304;
                  return(0);
          }
        }

        rc = fflush(mftab[*unit].fp);
        if (rc != 0) {
                (void) fprintf(stderr, "Flush failure (%d)\n", *unit);
                *status = 304;
        }

        return(0);
}

/*************************************************************************
*
*       Function:       delfil_(string, status)
*
*                       The filename contained in "string" is
*                       deleted from the file system.  This is
*                       used for deleting segments that were
*                       created during job execution (and hence
*                       stored in the NGTMPDIR directory).
*
*       Called From:    Fortran routines in the NCAR GKS library.
*
*       Returns:        "status", which is always returned as "0".
*                       Since this function may possibly be legally 
*                       called with invalid file names, all errors
*                       are ignored.  Does not return a meaningful 
*                       function value.
*
************************************************************************/
#ifdef cray
NGCALLF(delfil,DELFIL)(fname_, status)
        _fcd    fname_;
#else
int     NGCALLF(delfil,DELFIL)(fname, status)
        char    *fname;
#endif
        int     *status;
{
        char    *tpath, *tname;
#ifdef cray
        unsigned        length = _fcdlen(fname_);
        char            *fname;

        fname = (char *) calloc(sizeof(char)*(length+1),sizeof(char));
        strncpy( fname, _fcdtocp(fname_), length );
#endif
        /*
         *  Any errors are ignored since this function may possibly
         *  be legally called with invalid file names.
         */
        tpath = (char *) GetNCARGPath(NGTMPDIR);
        tname = calloc(strlen(tpath) + strlen("/") + strlen(fname) + 1,
                                sizeof(char));
        (void) strcpy(tname, tpath);
        (void) strcat(tname, "/");
        (void) strcat(tname, fname);    
        (void) remove(tname);
        free(tname);
#ifdef cray
        free(fname);
#endif
        *status = 0;
        return(0);
}

/*************************************************************************
*
*       Function:       userid_(iuid)
*
*       Called From:    Fortran routines in the NCAR GKS library.
*
*       Returns:        The unique user id in iuid.
*
************************************************************************/
void
NGCALLF(gzgids,GZGIDS)(uid, pid)
        int     *uid,*pid;
{
        *uid = (int) getuid();
        *pid = (int) getpid();
}
