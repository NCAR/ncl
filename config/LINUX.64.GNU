/*
 *      $Id: LINUX.64.GNU,v 1.2 2005-05-01 21:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       LINUX.64.GNU
 *
 *  Author:     Tim Fredrick
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Oct 25 14:56:41 MDT 1995
 *
 *  Description:    This file contains all the directives needed to
 *          tell ymake how to build NCAR Graphics on a 64-bit LINUX
 *          system using the GNU compilers.
 */
#define HdfDefines  -DLINUX
#define StdDefines  -DSYSV -D_POSIX_SOURCE -D_XOPEN_SOURCE -DByteSwapped
#define ByteSwapped
#define Cstatic
#define Cdynamic
#define CppCommand '/usr/bin/cpp -traditional'
#define CCompiler   gcc
#define FCompiler   g77
#define CtoFLibraries   -lg2c -lgcc -lm
#define CcOptions      -ansi
#define XToolLibrary    -lXt -lSM -lICE
#define BuildShared NO
#define XLibrary -lX11 -lXext
#define ExtraExportFlags -rdynamic

#define ArchRecLibSearch    -L/usr/X11R6/lib64
#define ArchRecIncSearch    -I/usr/X11R6/include

FC = $(F77)

/*************** Redefine Macros from Rules ********************************/

/*
 * Macro:   MakeDir
 *
 * Description: This rule creates a directory - if a parent dir doesn't exist
 *      it attempts to create it.
 */
#ifndef MakeDir
#define MakeDir(dir)    @if (test ! -d dir); then ($(MKDIRHIER) dir); fi
#endif
