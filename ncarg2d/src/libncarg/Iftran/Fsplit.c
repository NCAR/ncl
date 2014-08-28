/*
 *      $Id: Fsplit.c,v 1.4 2008-07-23 17:06:38 kennison Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * This program (Fsplit) is meant to replace the system "fsplit", which varies
 * from one Unix system to another (and sometimes isn't there).  Fsplit creates
 * files with lower-case names.  It leaves out comment lines between the end
 * of one routine and the beginning of the next.  By default, it supplies the
 * appropriate CVS comment lines and copyright lines at the beginning of each
 * file; use "-cvs", "-copyright", or "-both" to suppress the CVS comment lines,
 * the copyright lines, or both, respectively.
 *
 * Fsplit reports the presence of tabs and the presence of non-blank characters
 * past column 72.  It depends on the input to be in undisguised FORTRAN form
 * (for example, the word "SUBROUTINE" should not be spread over more than one
 * line, and, if there is a main program, it should have a "PROGRAM" statement).
 *
 * (03/25/2008) I have added a new feature:  If any comment line between the
 * end of one routine and the beginning of the next contains, in columns 2-8,
 * the characters "NOSPLIT", the two routines will be put in the same file.
 */

main(int argc, char **argv) {

  char *objectname;

  char *type[] = {"program","function","blockdata","subroutine",
                  "realfunction","doublefunction","integerfunction",
                  "logicalfunction","characterfunction",
                  "doubleprecisionfunction"};

  int tlen[sizeof(type)/sizeof(char*)];

  char keyword[73],*pkeyword=keyword;

  char chsaved[73],*pchsaved=chsaved;

  char lastfile[73];

  char nscode[]="NOSPLIT";

  int c,collecting,column=0,comment,cvs=3,chrspast72=0,
      errorflag=0,found,i,match,nosplit=0,tabs=0;

  FILE *psrc,*pdst;

  for (i=0;i<sizeof(type)/sizeof(char*);i++) tlen[i]=strlen(type[i]);

  lastfile[1]='\0';

  objectname=*argv;

  if (argc<2||argc>3) {
    fprintf(stderr,"%s: Wrong number of arguments.\n",objectname);
    fprintf(stderr,"Use \"%s [-cvs|-copyright|-both] source_file_name\".\n",
                                                                 objectname);
    return errorflag=1;
  } else if (argc==3) {
    if        (strcmp(*++argv,"-cvs")==0) {
      cvs=1;
    } else if (strcmp(*argv,"-copyright")==0) {
      cvs=2;
    } else if (strcmp(*argv,"-both")==0) {
      cvs=0;
    } else {
      fprintf(stderr,"%s: Bad argument \"%s\".\n",objectname,*argv);
      fprintf(stderr,"Use \"%s [-cvs|-copyright|-both] source_file_name\".\n",
                                                                   objectname);
      return errorflag=2;
    }
  }

  if ((psrc=fopen(*++argv,"r"))==(FILE*)NULL) {
    fprintf(stderr,"%s: Can't open \"%s\".\n",objectname,*argv);
    return errorflag=3;
  }

  pdst=(FILE*)NULL;

  while ((c=getc(psrc))!=EOF) {

    ++column;

    if (column==1) {
      collecting=!(comment=(c=='c'||c=='C'))&&isspace(c);
      pchsaved=chsaved;
    } else if (column<7) {
      if (!isspace(c)) collecting=0;
    } else if (column>72 && c!='\n') {
      collecting=0;
      chrspast72=1;
    } else {
      if (collecting && c!=' ' && c!='\t') {
        if (isalpha(c))
          *pkeyword++=tolower(c),*pkeyword='\0';
        else if (pkeyword!=keyword)
          if (pkeyword-keyword==9 &&
              strcmp(keyword,"character")==0 &&
              (c=='*'||isdigit(c)) )
            ;
          else if (isdigit(c))
            *pkeyword++=c,*pkeyword='\0';
          else
            collecting=0;
        else if (c!='\n')
          collecting=0;
      }
    }

    if (pdst==(FILE*)NULL) {
      if (!comment) {
        if (collecting)
          *pchsaved++=c,*pchsaved='\0';
        else {
          *pkeyword++='.',*pkeyword++='f',*pkeyword='\0';
          for (i=0,found=0;i<sizeof(type)/sizeof(char*);i++) {
            if (pkeyword-keyword<=tlen[i]) break;
            if (strncmp(keyword,type[i],tlen[i])==0) {
              found=1;
              pkeyword=keyword+tlen[i];
              break;
            }
          }
          if (!found) {
            fprintf(stderr,"%s: Can't find output file name.\n",objectname);
            return errorflag=4;
          } else if (!nosplit&&(pdst=fopen(pkeyword,"w"))==(FILE*)NULL) {
            fprintf(stderr,"%s: Can't open file \"%s\".\n",objectname,pkeyword);
            return errorflag=5;
          } else if (nosplit&&(pdst=fopen(pkeyword=lastfile,"a"))==(FILE*)NULL) {
            fprintf(stderr,"%s: Can't reopen file \"%s\".\n",objectname,pkeyword);
            return errorflag=6;
          } else printf("%s: Opening file \"%s\".\n",objectname,pkeyword);
          if (lastfile != pkeyword) strcpy(lastfile,pkeyword);
          if (!nosplit) {
            if (cvs==2||cvs==3) {
              fprintf(pdst,"C\nC $I");
              fprintf(pdst,       "d$\nC\n");
            }
            if (cvs==1||cvs==3) {
              fprintf(pdst,"C                Copyright (C)  2000\n");
              fprintf(pdst,"C        University Corporation for Atmospheric Research\n");
              fprintf(pdst,"C                All Rights Reserved\n");
              fprintf(pdst,"C\n");
              fprintf(pdst,"C The use of this Software is governed by a License Agreement.\n");
              fprintf(pdst,"C\n");
            }
          }
          fprintf(pdst,"%s",chsaved);
          pchsaved=chsaved;
          putc(c,pdst);
        }
      } else if (lastfile[1]!='\0') {
        if (column==1) match=0;
        else if (column<=8&&c==nscode[column-2]) if (++match==7) nosplit=1;
      }
    } else {
      putc(c,pdst);
      if (c=='\n' && pkeyword-keyword==3 && strcmp(keyword,"end")==0) {
        if (fclose(pdst)!=0) {
          fprintf(stderr,"%s: Can't close output file.\n",objectname);
          return errorflag=7;
        }
        pdst=(FILE*)NULL;
        nosplit=0;
      }
    }

    if (c=='\t') column=((column+7)/8)*8,tabs=1;
    else if (c=='\n') column=0,*(pkeyword=keyword)='\0';

  }

  if (pdst!=(FILE*)NULL) {
    fprintf(stderr,"%s: No \"END\" statement in final routine.\n",objectname);
    errorflag=8;
    if (fclose(pdst)!=0) {
      fprintf(stderr,"%s: Can't close output file.\n",objectname);
      errorflag=9;
    }
  }

  if (fclose(psrc)!=0) {
    fprintf(stderr,"%s: Can't close source file.\n",objectname);
    errorflag=10;
  }

  if (tabs!=0) {
    fprintf(stderr,"%s: There were tabs in the source file.\n",objectname);
    errorflag=11;
  }

  if (chrspast72!=0) {
    fprintf(stderr,"%s: There were non-blank",objectname);
    fprintf(stderr," characters past column 72 in the source file.\n");
    errorflag=12;
  }

  return errorflag;

}
