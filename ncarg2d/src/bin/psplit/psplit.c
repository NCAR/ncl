/*
 * $Id: psplit.c,v 1.6 2008-07-27 00:59:06 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2001                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  This program takes a PostScript file created from NCAR
 *  Graphics 3.2 or later (either directly or via ctrans) 
 *  and splits it into individual PS files, one for each 
 *  page in the original file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINEL 256
char line[LINEL];

enum file_type {PSNCGM, PSDSC, NOPE};
enum file_type ftype(FILE *);

void from_ncgm(FILE *, char *);
void begin_picture(FILE *);
void picture_body(FILE *, FILE*, fpos_t *);
void end_picture(FILE *);

void from_ps(FILE *, char *);
void begin_picture_ps(FILE *, FILE *);
void picture_body_ps(FILE *, FILE *, fpos_t *);
void end_picture_ps(FILE *, FILE *, fpos_t *);

int count_only=0;

main(int argc, char *argv[])
{
   FILE *input_file;
   enum file_type ft;
   char *root_name,*input_name;

/*
 *  Check to see if an input file was specified.
 */
   if (argc < 2) {
     printf("\nusage: psplit input_file [output_file_root]\n");
     printf("  where \"input_file\" is the name of the input PostScript"
            " file and\n  \"output_file_root\" is an optional argument"
            " specifying the root\n  name for the output ps files.\n\n");
     printf("or: psplit -c input_file\n"
            "  where \"input_file\" is the name of an input PostScript"
            " file for\n  which you only want the number of pages.\n\n");
     exit(1);
   }
     
/*
 *  Establish the root name for the output files.
 */
   if (!strcmp(argv[1],"-c")) {
     if (argc < 3) {
       printf("\n  Must specify an input file when using -c.\n\n");
       exit(3);
     }
     input_name = argv[2];
     count_only = 1;
     root_name = argv[2];
   }
   else if (argc >= 3) {
     input_name = argv[1];
     root_name = argv[2];
   }    
   else {
     input_name = argv[1];
     root_name = "pict";
   }

/*
 *  Open the input file for reading.
 */
   input_file = fopen(input_name,"r");
   if (input_file == (FILE *) NULL) {
      printf("Cannot open input file %s\n",input_name);
      exit(2);
   }

/*
 *  Determine if the input file is PostScript from an NCGM, a PostScript
 *  file conforming to the DSC, or neither of these, and process
 *  accordingly.
 */
   ft = ftype(input_file);
   switch (ft) {
     case PSNCGM:
       from_ncgm(input_file, root_name);
       break;
     case PSDSC:
       from_ps(input_file, root_name);
       break;
     case NOPE:
       printf("\n  Input file does not conform to the file\n"
              " structuring requirements.\n\n");
       exit(5);
       break;
   }
   exit(0);
}

enum file_type ftype(FILE *ifile) 
{
  int page_flag=0, trailer_flag=0;

  while (fgets(line, LINEL, ifile) != (char *) NULL) {
    if (!strncmp("/w {setlinewidth} def",line,21)) {
      if (fseek(ifile,0L,SEEK_SET)) {
        printf("Error in repositioning the input file to the start\n");
        exit(2);
      }
      return(PSNCGM);
    }
    if (!strncmp("%%Page:",line,7)) {
      page_flag=1;
    }
    if (!strncmp("%%Trailer",line,9)) {
      trailer_flag=1;
    }
  }
  if (page_flag && trailer_flag) {
    if (fseek(ifile,0L,SEEK_SET)) {
        printf("Error in repositioning the input file to the start\n");
        exit(2);
    }
    return(PSDSC);
  }
  else {
    return(NOPE);
  }
}

void from_ncgm(FILE *ifile, char *rname)
{
  FILE *output_file;
  int i,pict_count=0;
  fpos_t *pict_start;
  char *pnum, *output_name;

  int line_count = 0;

/*
 *  Determine how many pictures are in the file.
 */
  while (fgets(line, LINEL, ifile) != (char *) NULL) {
    if (!strncmp(" h",line,2) || !strncmp("h ",line,2)) {
      pict_count++;
    }
  }

/*
 *  Simply report the picture count and exit if requested.
 */
  if (count_only) {
    printf("For PostScript file %s:\n"  
           "  Number of pictures = %d\n", rname, pict_count);
    fclose(ifile);
    exit(4);
  }

/*
 *  Record the position numbers of where the pictures start.
 */
  pict_start = (fpos_t *) calloc(pict_count+1,sizeof(fpos_t));
  if ( pict_start == (fpos_t *) NULL ) {
    printf("Error in getting memory for file position pointers\n");
    exit(19);
  }
  pict_count = 1;
  if (fseek(ifile,0L,SEEK_SET)) {
    printf("Error in repositioning the input file to the start\n");
    exit(9);
  }
  while (fgets(line, LINEL, ifile)) {
    line_count++;
    if (!strncmp(" h",line,2) || !strncmp("h ",line,2)) {
      if (fgetpos(ifile, pict_start+pict_count)) {
        printf("Error return from fgetpos.\n");
        exit(20);
      }
      pict_count++;
    }
    else if (!strncmp("/o {",line,4)) {
      if (fgetpos(ifile, pict_start)) {
        printf("Error return from fgetpos.\n");
        exit(20);
      }
    }
  }

/*
 *  pict_count is actually one larger than the total number
 *  of pictures, since we searched for picture end flags and
 *  added in where the first picture started.
 */
  pict_count--;

  pnum = (char *) calloc(5,sizeof(char));
  output_name = (char *) calloc(strlen(rname)+1+4+4,sizeof(char));

/*
 *  Loop through the pictures.
 */
  for (i = 0; i < pict_count; i++) {

/*
 *  Create the output file name.
 */
    sprintf(pnum,"%04d",i+1);
    strcpy(output_name,"\0");
    strcat(output_name,rname);
    strcat(output_name,pnum);
    strcat(output_name,".ps");

/*
 *  Open the output file.
 */
    output_file = fopen(output_name,"w");
    if (output_file == (FILE *) NULL) {
       printf("Cannot open output file %s\n",output_name);
       exit(5);
    }
/*
 *  Write out the header, the prolog, and the color table.
 */
    begin_picture(output_file);
    fflush(output_file);

/*
 *  Write out the picture body, pict_start+i is the start
 *  position for picture i.
 *  
 */
    picture_body(ifile, output_file, pict_start+i);
    fflush(output_file);

/*
 *  Write out the picture termination.
 */
    end_picture(output_file);
    fflush(output_file);

/*
 *  Close the output file.
 */
    fclose(output_file);
  }

/*
 *  Free memory.
 */
  free(pict_start);
  free(pnum);
  free(output_name);

/*
 *  Close the input file.
 */
  fclose(ifile);
}

void begin_picture(FILE *ofile)
{
  fprintf(ofile,"%%!PS-Adobe-2.0\n");
  fprintf(ofile,"%%%%BoundingBox: 36 126 577 667\n");
  fprintf(ofile,"%%%%EndComments\n");

  fprintf(ofile,"%%%%BeginProlog\n");
  fprintf(ofile,"/w {setlinewidth} def\n");
  fprintf(ofile,"/l {lineto} def\n");
  fprintf(ofile,"/n {newpath} def\n");
  fprintf(ofile,"/m {moveto} def\n");
  fprintf(ofile,"/s {scale} def\n");
  fprintf(ofile,"/c {setlinecap} def\n");
  fprintf(ofile,"/j {setlinejoin} def\n");
  fprintf(ofile,"/h {showpage} def\n");
  fprintf(ofile,"/t {stroke} def\n");
  fprintf(ofile,"/f {eofill} def\n");
  fprintf(ofile,"/ct 256 array def\n");
  fprintf(ofile,"/o {ct exch get aload pop setrgbcolor} def\n");
  fprintf(ofile,"%%%%EndProlog\n");

  fprintf(ofile,"ct 0 [1.000 1.000 1.000] put\n");
  fprintf(ofile,"ct 1 [0.000 0.000 0.000] put\n");
  fprintf(ofile,"ct 2 [1.000 0.000 0.000] put\n");
  fprintf(ofile,"ct 3 [0.000 1.000 0.000] put\n");
  fprintf(ofile,"ct 4 [0.000 0.000 1.000] put\n");
  fprintf(ofile,"ct 5 [0.000 1.000 1.000] put\n");
  fprintf(ofile,"ct 6 [1.000 0.000 0.827] put\n");
  fprintf(ofile,"ct 7 [1.000 1.000 0.000] put\n");
  fprintf(ofile,"ct 8 [1.000 0.498 0.000] put\n");
  fprintf(ofile,"ct 9 [0.600 0.827 0.000] put\n");
  fprintf(ofile,"ct 10 [0.000 1.000 0.600] put\n");
  fprintf(ofile,"ct 11 [0.000 0.498 1.000] put\n");
  fprintf(ofile,"ct 12 [0.549 0.000 0.827] put\n");
  fprintf(ofile,"ct 13 [1.000 0.000 0.549] put\n");
  fprintf(ofile,"ct 14 [0.329 0.329 0.329] put\n");
  fprintf(ofile,"ct 15 [0.667 0.667 0.667] put\n");
  fprintf(ofile,"ct 16 [1.000 1.000 0.329] put\n");
  fprintf(ofile,"ct 17 [0.749 1.000 0.447] put\n");
  fprintf(ofile,"ct 18 [0.447 1.000 0.600] put\n");
  fprintf(ofile,"ct 19 [0.169 1.000 0.749] put\n");
  fprintf(ofile,"ct 20 [0.247 0.827 0.827] put\n");
  fprintf(ofile,"ct 21 [0.498 0.667 0.827] put\n");
  fprintf(ofile,"ct 22 [0.749 0.549 0.827] put\n");
  fprintf(ofile,"ct 23 [1.000 0.329 0.898] put\n");
  fprintf(ofile,"ct 24 [0.667 0.898 0.447] put\n");
  fprintf(ofile,"ct 25 [0.400 0.898 0.549] put\n");
  fprintf(ofile,"ct 26 [0.169 0.898 0.667] put\n");
  fprintf(ofile,"ct 27 [0.169 0.667 0.898] put\n");
  fprintf(ofile,"ct 28 [0.169 0.498 1.000] put\n");
  fprintf(ofile,"ct 29 [0.447 0.329 1.000] put\n");
  fprintf(ofile,"ct 30 [0.749 0.169 1.000] put\n");
  fprintf(ofile,"ct 31 [0.898 0.086 1.000] put\n");
  fprintf(ofile,"ct 32 [0.827 1.000 0.169] put\n");
  fprintf(ofile,"ct 33 [0.667 1.000 0.247] put\n");
  fprintf(ofile,"ct 34 [0.447 1.000 0.329] put\n");
  fprintf(ofile,"ct 35 [0.169 1.000 0.498] put\n");
  fprintf(ofile,"ct 36 [0.169 0.827 0.600] put\n");
  fprintf(ofile,"ct 37 [0.169 0.667 0.749] put\n");
  fprintf(ofile,"ct 38 [0.169 0.549 0.827] put\n");
  fprintf(ofile,"ct 39 [0.247 0.447 0.898] put\n");
  fprintf(ofile,"ct 40 [0.400 0.329 0.898] put\n");
  fprintf(ofile,"ct 41 [0.667 0.169 0.898] put\n");
  fprintf(ofile,"ct 42 [0.827 0.169 0.827] put\n");
  fprintf(ofile,"ct 43 [0.898 0.329 0.667] put\n");
  fprintf(ofile,"ct 44 [0.827 0.447 0.600] put\n");
  fprintf(ofile,"ct 45 [0.827 0.600 0.498] put\n");
  fprintf(ofile,"ct 46 [0.898 0.667 0.400] put\n");
  fprintf(ofile,"ct 47 [0.898 0.667 0.247] put\n");
  fprintf(ofile,"ct 48 [1.000 0.898 0.086] put\n");
  fprintf(ofile,"ct 49 [0.827 1.000 0.086] put\n");
  fprintf(ofile,"ct 50 [0.600 1.000 0.169] put\n");
  fprintf(ofile,"ct 51 [0.447 1.000 0.247] put\n");
  fprintf(ofile,"ct 52 [0.169 0.898 0.447] put\n");
  fprintf(ofile,"ct 53 [0.169 0.827 0.549] put\n");
  fprintf(ofile,"ct 54 [0.169 0.667 0.667] put\n");
  fprintf(ofile,"ct 55 [0.169 0.549 0.749] put\n");
  fprintf(ofile,"ct 56 [0.169 0.400 0.898] put\n");
  fprintf(ofile,"ct 57 [0.400 0.247 0.898] put\n");
  fprintf(ofile,"ct 58 [0.549 0.169 0.898] put\n");
  fprintf(ofile,"ct 59 [0.827 0.169 0.749] put\n");
  fprintf(ofile,"ct 60 [1.000 0.169 0.667] put\n");
  fprintf(ofile,"ct 61 [1.000 0.247 0.600] put\n");
  fprintf(ofile,"ct 62 [1.000 0.400 0.498] put\n");
  fprintf(ofile,"ct 63 [1.000 0.498 0.400] put\n");
  fprintf(ofile,"ct 64 [1.000 0.827 0.086] put\n");
  fprintf(ofile,"ct 65 [0.749 1.000 0.000] put\n");
  fprintf(ofile,"ct 66 [0.600 1.000 0.129] put\n");
  fprintf(ofile,"ct 67 [0.400 1.000 0.169] put\n");
  fprintf(ofile,"ct 68 [0.129 1.000 0.329] put\n");
  fprintf(ofile,"ct 69 [0.129 0.827 0.447] put\n");
  fprintf(ofile,"ct 70 [0.169 0.749 0.498] put\n");
  fprintf(ofile,"ct 71 [0.129 0.600 0.667] put\n");
  fprintf(ofile,"ct 72 [0.129 0.498 0.749] put\n");
  fprintf(ofile,"ct 73 [0.129 0.400 0.827] put\n");
  fprintf(ofile,"ct 74 [0.169 0.247 0.898] put\n");
  fprintf(ofile,"ct 75 [0.169 0.169 1.000] put\n");
  fprintf(ofile,"ct 76 [0.329 0.086 1.000] put\n");
  fprintf(ofile,"ct 77 [0.549 0.086 0.898] put\n");
  fprintf(ofile,"ct 78 [0.749 0.000 0.827] put\n");
  fprintf(ofile,"ct 79 [0.898 0.000 0.749] put\n");
  fprintf(ofile,"ct 80 [0.898 0.749 0.086] put\n");
  fprintf(ofile,"ct 81 [0.667 0.898 0.086] put\n");
  fprintf(ofile,"ct 82 [0.549 0.898 0.129] put\n");
  fprintf(ofile,"ct 83 [0.247 0.898 0.247] put\n");
  fprintf(ofile,"ct 84 [0.129 0.898 0.329] put\n");
  fprintf(ofile,"ct 85 [0.169 0.749 0.400] put\n");
  fprintf(ofile,"ct 86 [0.129 0.667 0.498] put\n");
  fprintf(ofile,"ct 87 [0.129 0.549 0.600] put\n");
  fprintf(ofile,"ct 88 [0.129 0.447 0.667] put\n");
  fprintf(ofile,"ct 89 [0.169 0.329 0.749] put\n");
  fprintf(ofile,"ct 90 [0.169 0.247 0.827] put\n");
  fprintf(ofile,"ct 91 [0.329 0.169 0.827] put\n");
  fprintf(ofile,"ct 92 [0.549 0.129 0.749] put\n");
  fprintf(ofile,"ct 93 [0.749 0.129 0.667] put\n");
  fprintf(ofile,"ct 94 [0.898 0.169 0.549] put\n");
  fprintf(ofile,"ct 95 [0.827 0.247 0.498] put\n");
  fprintf(ofile,"ct 96 [0.827 0.749 0.000] put\n");
  fprintf(ofile,"ct 97 [0.600 0.827 0.086] put\n");
  fprintf(ofile,"ct 98 [0.447 0.898 0.086] put\n");
  fprintf(ofile,"ct 99 [0.329 0.898 0.129] put\n");
  fprintf(ofile,"ct 100 [0.129 0.898 0.247] put\n");
  fprintf(ofile,"ct 101 [0.169 0.827 0.247] put\n");
  fprintf(ofile,"ct 102 [0.169 0.749 0.329] put\n");
  fprintf(ofile,"ct 103 [0.169 0.600 0.447] put\n");
  fprintf(ofile,"ct 104 [0.129 0.498 0.549] put\n");
  fprintf(ofile,"ct 105 [0.129 0.447 0.600] put\n");
  fprintf(ofile,"ct 106 [0.169 0.329 0.667] put\n");
  fprintf(ofile,"ct 107 [0.329 0.247 0.667] put\n");
  fprintf(ofile,"ct 108 [0.447 0.247 0.600] put\n");
  fprintf(ofile,"ct 109 [0.667 0.247 0.498] put\n");
  fprintf(ofile,"ct 110 [0.827 0.247 0.400] put\n");
  fprintf(ofile,"ct 111 [1.000 0.247 0.329] put\n");
  fprintf(ofile,"ct 112 [0.749 0.667 0.000] put\n");
  fprintf(ofile,"ct 113 [0.549 0.749 0.086] put\n");
  fprintf(ofile,"ct 114 [0.400 0.827 0.086] put\n");
  fprintf(ofile,"ct 115 [0.247 0.898 0.086] put\n");
  fprintf(ofile,"ct 116 [0.169 0.898 0.129] put\n");
  fprintf(ofile,"ct 117 [0.169 0.827 0.169] put\n");
  fprintf(ofile,"ct 118 [0.129 0.749 0.247] put\n");
  fprintf(ofile,"ct 119 [0.129 0.667 0.329] put\n");
  fprintf(ofile,"ct 120 [0.129 0.600 0.400] put\n");
  fprintf(ofile,"ct 121 [0.129 0.447 0.498] put\n");
  fprintf(ofile,"ct 122 [0.129 0.400 0.549] put\n");
  fprintf(ofile,"ct 123 [0.247 0.329 0.549] put\n");
  fprintf(ofile,"ct 124 [0.447 0.329 0.447] put\n");
  fprintf(ofile,"ct 125 [0.549 0.329 0.400] put\n");
  fprintf(ofile,"ct 126 [0.667 0.329 0.329] put\n");
  fprintf(ofile,"ct 127 [0.827 0.329 0.247] put\n");
  fprintf(ofile,"ct 128 [0.667 0.600 0.000] put\n");
  fprintf(ofile,"ct 129 [0.498 0.667 0.086] put\n");
  fprintf(ofile,"ct 130 [0.400 0.667 0.129] put\n");
  fprintf(ofile,"ct 131 [0.247 0.667 0.169] put\n");
  fprintf(ofile,"ct 132 [0.129 0.667 0.247] put\n");
  fprintf(ofile,"ct 133 [0.086 0.600 0.329] put\n");
  fprintf(ofile,"ct 134 [0.086 0.498 0.400] put\n");
  fprintf(ofile,"ct 135 [0.129 0.400 0.447] put\n");
  fprintf(ofile,"ct 136 [0.129 0.329 0.498] put\n");
  fprintf(ofile,"ct 137 [0.086 0.169 0.667] put\n");
  fprintf(ofile,"ct 138 [0.169 0.129 0.667] put\n");
  fprintf(ofile,"ct 139 [0.400 0.129 0.549] put\n");
  fprintf(ofile,"ct 140 [0.498 0.129 0.498] put\n");
  fprintf(ofile,"ct 141 [0.600 0.129 0.447] put\n");
  fprintf(ofile,"ct 142 [0.749 0.129 0.400] put\n");
  fprintf(ofile,"ct 143 [0.898 0.129 0.329] put\n");
  fprintf(ofile,"ct 144 [0.549 0.498 0.086] put\n");
  fprintf(ofile,"ct 145 [0.447 0.600 0.086] put\n");
  fprintf(ofile,"ct 146 [0.329 0.600 0.129] put\n");
  fprintf(ofile,"ct 147 [0.169 0.600 0.169] put\n");
  fprintf(ofile,"ct 148 [0.086 0.549 0.247] put\n");
  fprintf(ofile,"ct 149 [0.129 0.447 0.329] put\n");
  fprintf(ofile,"ct 150 [0.086 0.400 0.400] put\n");
  fprintf(ofile,"ct 151 [0.169 0.329 0.400] put\n");
  fprintf(ofile,"ct 152 [0.169 0.247 0.447] put\n");
  fprintf(ofile,"ct 153 [0.247 0.169 0.498] put\n");
  fprintf(ofile,"ct 154 [0.247 0.129 0.549] put\n");
  fprintf(ofile,"ct 155 [0.329 0.129 0.498] put\n");
  fprintf(ofile,"ct 156 [0.447 0.129 0.447] put\n");
  fprintf(ofile,"ct 157 [0.549 0.129 0.400] put\n");
  fprintf(ofile,"ct 158 [0.667 0.129 0.329] put\n");
  fprintf(ofile,"ct 159 [0.827 0.129 0.247] put\n");
  fprintf(ofile,"ct 160 [0.498 0.447 0.086] put\n");
  fprintf(ofile,"ct 161 [0.400 0.498 0.086] put\n");
  fprintf(ofile,"ct 162 [0.329 0.600 0.000] put\n");
  fprintf(ofile,"ct 163 [0.169 0.600 0.086] put\n");
  fprintf(ofile,"ct 164 [0.129 0.667 0.086] put\n");
  fprintf(ofile,"ct 165 [0.129 0.600 0.129] put\n");
  fprintf(ofile,"ct 166 [0.129 0.498 0.169] put\n");
  fprintf(ofile,"ct 167 [0.086 0.447 0.247] put\n");
  fprintf(ofile,"ct 168 [0.129 0.400 0.247] put\n");
  fprintf(ofile,"ct 169 [0.129 0.329 0.329] put\n");
  fprintf(ofile,"ct 170 [0.086 0.247 0.400] put\n");
  fprintf(ofile,"ct 171 [0.086 0.169 0.498] put\n");
  fprintf(ofile,"ct 172 [0.086 0.129 0.549] put\n");
  fprintf(ofile,"ct 173 [0.129 0.000 0.600] put\n");
  fprintf(ofile,"ct 174 [0.329 0.000 0.498] put\n");
  fprintf(ofile,"ct 175 [0.498 0.000 0.400] put\n");
  fprintf(ofile,"ct 176 [0.749 0.329 0.000] put\n");
  fprintf(ofile,"ct 177 [0.667 0.400 0.000] put\n");
  fprintf(ofile,"ct 178 [0.549 0.400 0.086] put\n");
  fprintf(ofile,"ct 179 [0.329 0.400 0.169] put\n");
  fprintf(ofile,"ct 180 [0.247 0.329 0.247] put\n");
  fprintf(ofile,"ct 181 [0.247 0.247 0.329] put\n");
  fprintf(ofile,"ct 182 [0.247 0.169 0.400] put\n");
  fprintf(ofile,"ct 183 [0.329 0.129 0.400] put\n");
  fprintf(ofile,"ct 184 [0.498 0.129 0.329] put\n");
  fprintf(ofile,"ct 185 [0.600 0.129 0.247] put\n");
  fprintf(ofile,"ct 186 [0.749 0.086 0.247] put\n");
  fprintf(ofile,"ct 187 [0.827 0.000 0.247] put\n");
  fprintf(ofile,"ct 188 [1.000 0.000 0.169] put\n");
  fprintf(ofile,"ct 189 [1.000 0.129 0.129] put\n");
  fprintf(ofile,"ct 190 [0.898 0.169 0.086] put\n");
  fprintf(ofile,"ct 191 [0.827 0.247 0.086] put\n");
  fprintf(ofile,"ct 192 [0.600 0.247 0.000] put\n");
  fprintf(ofile,"ct 193 [0.447 0.400 0.000] put\n");
  fprintf(ofile,"ct 194 [0.247 0.400 0.129] put\n");
  fprintf(ofile,"ct 195 [0.247 0.329 0.169] put\n");
  fprintf(ofile,"ct 196 [0.169 0.247 0.247] put\n");
  fprintf(ofile,"ct 197 [0.169 0.169 0.329] put\n");
  fprintf(ofile,"ct 198 [0.247 0.129 0.329] put\n");
  fprintf(ofile,"ct 199 [0.329 0.086 0.329] put\n");
  fprintf(ofile,"ct 200 [0.498 0.086 0.247] put\n");
  fprintf(ofile,"ct 201 [0.667 0.086 0.169] put\n");
  fprintf(ofile,"ct 202 [0.827 0.000 0.169] put\n");
  fprintf(ofile,"ct 203 [0.898 0.000 0.129] put\n");
  fprintf(ofile,"ct 204 [1.000 0.000 0.086] put\n");
  fprintf(ofile,"ct 205 [1.000 0.129 0.086] put\n");
  fprintf(ofile,"ct 206 [0.827 0.169 0.000] put\n");
  fprintf(ofile,"ct 207 [0.749 0.247 0.000] put\n");
  fprintf(ofile,"ct 208 [0.498 0.169 0.000] put\n");
  fprintf(ofile,"ct 209 [0.400 0.329 0.000] put\n");
  fprintf(ofile,"ct 210 [0.129 0.447 0.086] put\n");
  fprintf(ofile,"ct 211 [0.086 0.400 0.129] put\n");
  fprintf(ofile,"ct 212 [0.086 0.329 0.169] put\n");
  fprintf(ofile,"ct 213 [0.086 0.247 0.247] put\n");
  fprintf(ofile,"ct 214 [0.129 0.169 0.329] put\n");
  fprintf(ofile,"ct 215 [0.169 0.086 0.400] put\n");
  fprintf(ofile,"ct 216 [0.247 0.000 0.329] put\n");
  fprintf(ofile,"ct 217 [0.447 0.000 0.247] put\n");
  fprintf(ofile,"ct 218 [0.600 0.000 0.169] put\n");
  fprintf(ofile,"ct 219 [0.749 0.000 0.129] put\n");
  fprintf(ofile,"ct 220 [0.827 0.000 0.086] put\n");
  fprintf(ofile,"ct 221 [0.749 0.086 0.086] put\n");
  fprintf(ofile,"ct 222 [0.667 0.129 0.086] put\n");
  fprintf(ofile,"ct 223 [0.600 0.169 0.000] put\n");
  fprintf(ofile,"ct 224 [0.400 0.169 0.000] put\n");
  fprintf(ofile,"ct 225 [0.169 0.329 0.000] put\n");
  fprintf(ofile,"ct 226 [0.086 0.329 0.000] put\n");
  fprintf(ofile,"ct 227 [0.086 0.247 0.086] put\n");
  fprintf(ofile,"ct 228 [0.086 0.169 0.129] put\n");
  fprintf(ofile,"ct 229 [0.086 0.129 0.247] put\n");
  fprintf(ofile,"ct 230 [0.086 0.129 0.329] put\n");
  fprintf(ofile,"ct 231 [0.086 0.000 0.329] put\n");
  fprintf(ofile,"ct 232 [0.129 0.000 0.247] put\n");
  fprintf(ofile,"ct 233 [0.169 0.000 0.169] put\n");
  fprintf(ofile,"ct 234 [0.329 0.000 0.169] put\n");
  fprintf(ofile,"ct 235 [0.329 0.000 0.129] put\n");
  fprintf(ofile,"ct 236 [0.400 0.000 0.086] put\n");
  fprintf(ofile,"ct 237 [0.447 0.086 0.086] put\n");
  fprintf(ofile,"ct 238 [0.447 0.129 0.000] put\n");
  fprintf(ofile,"ct 239 [0.169 0.086 0.000] put\n");
}

void picture_body(FILE *ifile, FILE *ofile, fpos_t *pict_start)
{

  int i;

  if (fsetpos(ifile, pict_start)) {
    printf("Error return from fsetpos.\n");
    exit(20);
  }

/*
 *  Copy up through the showpage operator (" h").
 */
  
  fgets(line, LINEL, ifile);
  while (strncmp(" h",line,2) && strncmp("h ",line,2)) {
    fputs(line, ofile);
    if (fgets(line, LINEL, ifile) == (char *) NULL)  {
      return;
    }
  }
  fputs(line, ofile);
}

void end_picture(FILE *ofile)
{
  fprintf(ofile,"%%%%Trailer\n");
  fprintf(ofile,"%%%%EOF\n");
}

void from_ps(FILE *ifile, char *rname)
{
  FILE *output_file;
  int i,pict_count=0, line_count=0, npict;
  fpos_t *pict_start, tpos;
  char *pnum, *output_name;

/*
 *  Determine how many pictures and how many lines are in the file.
 */
  if (fseek(ifile,0L,SEEK_SET)) {
    printf("Error in repositioning the input file to the start\n");
    exit(10);
  }
  while (fgets(line, LINEL, ifile) != (char *) NULL) {
    if (!strncmp("%%Page:",line,7)) {
      pict_count++;
    }
    line_count++;
  }

/*
 *  Simply report the picture count and exit if requested.
 */
  if (count_only) {
    printf("For PostScript file %s:\n"  
           "  Number of pictures = %d\n", rname, pict_count);
    fclose(ifile);
    exit(4);
  }

/*
 *  Record the position numbers of where the pictures start and where
 *  the %%Trailer is.
 */
  pict_start = (fpos_t *) calloc(pict_count+1,sizeof(fpos_t));
  if ( pict_start == (fpos_t *) NULL ) {
    printf("Error in getting memory for file position pointers\n");
    exit(19);
  }
  if (fseek(ifile,0L,SEEK_SET)) {
    printf("Error in repositioning the input file to the start\n");
    exit(11);
  }
  npict = 0;
  for (i = 0; i < line_count; i++) {
    if (fgetpos(ifile,&tpos)) {
      printf("Error return from fgetpos.\n");
      exit(20);
    }
    fgets(line, LINEL, ifile);
    if (!strncmp("%%Page:",line,7)) {
      pict_start[npict] = tpos;
      npict++;
    } 
    else if (!strncmp("%%Trailer",line,9)) {
      pict_start[pict_count] = tpos;
    }
  }
  pnum = (char *) calloc(5,sizeof(char));
  output_name = (char *) calloc(strlen(rname)+1+4+4,sizeof(char));

/*
 *  Loop through the pictures.
 */
  for (i = 0; i < pict_count; i++) {

/*
 *  Create the output file name.
 */
    sprintf(pnum,"%04d",i+1);
    strcpy(output_name,"\0");
    strcat(output_name,rname);
    strcat(output_name,pnum);
    strcat(output_name,".ps");

/*
 *  Open the output file.
 */
    output_file = fopen(output_name,"w");
    if (output_file == (FILE *) NULL) {
       printf("Cannot open output file %s\n",output_name);
       exit(5);
    }
/*
 *  Write out the header, the prolog, and the color table.
 */
    begin_picture_ps(ifile,output_file);
    fflush(output_file);

/*
 *  Write out the picture body.
 */
    picture_body_ps(ifile, output_file, pict_start+i);
    fflush(output_file);

/*
 *  Write out the picture termination.
 */
    end_picture_ps(ifile, output_file, pict_start+pict_count);
    fflush(output_file);

/*
 *  Close the output file.
 */
    fclose(output_file);
  }

/*
 *  Free memory.
 */
  free(pict_start);
  free(pnum);
  free(output_name);

/*
 *  Close the input file.
 */
  fclose(ifile);
}

void begin_picture_ps(FILE *ifile, FILE *ofile)
{
  if (fseek(ifile,0L,SEEK_SET)) {
    printf("Error in repositioning the input file to the start\n");
    exit(2);
  }
  while (fgets(line, LINEL, ifile) != (char *) NULL) {
    if (!strncmp("%%Page:",line,7)) {
      return;
    }
    else {
      fputs(line,ofile);
    }
  }
}

void picture_body_ps(FILE *ifile, FILE *ofile, fpos_t *pict_start)
{

/*
 *  Copy until we hit a %%Page: or a %%Trailer.
 */
  int i;

  if (fsetpos(ifile, pict_start)) {
    printf("Error return from fsetpos.\n");
    exit(20);
  }

/*
 *  Copy up through the showpage operator (" h").
 */
  fgets(line, LINEL, ifile);
  fputs("%%Page: 1 1\n", ofile);
  fgets(line, LINEL, ifile);
  while (strncmp("%%Page:",line,7) && strncmp("%%Trailer",line,9)) {
    fputs(line, ofile);
    if (fgets(line, LINEL, ifile) == (char *) NULL)  {
      printf("Premature EOF\n");
      exit(12);
    }
  }
}

void end_picture_ps(FILE *ifile, FILE *ofile, fpos_t *pict_end_pos)
{
  if (fsetpos(ifile, pict_end_pos)) {
    printf("Error return from fsetpos.\n");
    exit(20);
  }
  while (fgets(line, LINEL, ifile) != (char *) NULL) {
    fputs(line,ofile);
  }
}
