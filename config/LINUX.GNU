/*
 *  Description:   This file contains all the directives needed to
 *                 tell ymake how to build NCARG/NCL on a 32-bit LINUX
 *                 system using the GNU "g77" and "gcc" compilers.
 *
 *                 In general, "g77" shouldn't be used anymore. It's been
 *                 superceded by "gfortran", which is the compiler used
 *                 in the "LINUX" file in this directory.
 *
 *                 Copy this file to the LINUX file in this directory
 *                 before you start the NCL/NCARG build.
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
#define CtoFLibrariesUser   -lg2c -lgcc -lm
#define CcOptions      -ansi -fPIC -fopenmp
#define FcOptions      -fPIC -fno-second-underscore
#define XToolLibrary    -lXt -lSM -lICE
#define BuildShared NO
#define XLibrary -lX11 -lXext
#define ExtraExportFlags -rdynamic

#define LibSearchUser    -L/usr/X11R6/lib
#define IncSearchUser    -I/usr/X11R6/include

#define ArchRecLibSearch    -L/usr/X11R6/lib
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

