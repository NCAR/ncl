#include <stdio.h>
#include "input.h"
#include <stdlib.h>
#include <inttypes.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# ifndef YYLMAX 
# define YYLMAX BUFSIZ
# endif 
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int my_yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	int yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):my_getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):my_getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
#define YYISARRAY
char yytext[YYLMAX];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
/*
FILE *yyin = {stdin}, *yyout = {stdout};
*/
FILE *yyin, *yyout;
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
#include <ctype.h>
#include "lex.h"
static	short	have_command = 0;
# define YYNEWLINE 10
my_yylex(){
static int first = 1;
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
if(first) {
    first = 0;
    yyin = stdin;
    yyout = stdout;
}
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 24 "lex.l"
;
break;
case 2:

# line 26 "lex.l"
		{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_TYPE);
			} 
			else 
				REJECT;
			}
break;
case 3:

# line 35 "lex.l"
	{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_LIST_TYPE);
			} 
			else 
				REJECT;
			}
break;
case 4:

# line 44 "lex.l"
	{
			if (!have_command) {
				have_command = 1;
				return(COMMAND_TYPE);
			}
			else
				REJECT;
			}
break;
case 5:

# line 54 "lex.l"
		{
			if (!have_command) {
				REJECT;
			}
			else {
				yyleng = nuke_lead_whitespace(yytext);
				return(DATA_TYPE);
			}
			}
break;
case 6:

# line 64 "lex.l"
		{
			have_command = 0;
			return(END_LINE);
			}
break;
case 7:

# line 69 "lex.l"
		{
			have_command = 0;
			(void) skip_line();
			return(ERROR_TYPE);
			}
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of my_yylex */

# line 76 "lex.l"



nuke_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (*s1) {
		if (!isspace(*s1)) {
			*s2++ = *s1++;
			len++;
		}
		else
			s1++;
	}
	*s2 = '\0';
	return (len);
}

nuke_lead_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (isspace(*s1))
		s1++;

	while (*s1) {
		*s2++ = *s1++;
		len++;
	}
	*s2 = '\0';
	return (len);
}

#ifdef	DEAD
nuke_quotes(s1, len)
	char	*s1;
	int	len;
{

	len -= 2;
	memmove(s1, s1 + 1, len);
	s1[len] = '\0';

	return (len);
}
#endif

skip_line()
{
	int	c;

	while (((c = input()) != EOF) && c != '\n')
	;

	if (c == EOF) unput(c);

	return(1);
}

int yyvstop[] = {
0,

1,
0, 

1,
0, 

7,
0, 

6,
0, 

1,
5,
7,
0, 

4,
5,
7,
0, 

5,
7,
0, 

2,
5,
7,
0, 

7,
0, 

5,
7,
0, 

2,
5,
7,
0, 

4,
5,
7,
0, 

1,
5,
0, 

5,
0, 

5,
0, 

5,
0, 

5,
0, 

2,
0, 

2,
5,
0, 

2,
5,
0, 

4,
5,
0, 

3,
5,
0, 

5,
0, 

3,
5,
0, 

5,
0, 

5,
0, 

3,
0, 

3,
5,
0, 

3,
0, 

3,
0, 
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,4,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,5,	1,6,	0,0,	
1,7,	1,8,	1,7,	1,7,	
1,7,	1,7,	1,7,	1,7,	
1,9,	1,7,	1,10,	1,8,	
1,7,	1,11,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,17,	
17,17,	19,27,	27,27,	1,7,	
1,7,	1,7,	1,7,	1,7,	
1,7,	1,7,	1,12,	8,18,	
17,18,	19,28,	27,28,	0,0,	
0,0,	0,0,	2,7,	2,8,	
2,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,9,	2,7,	
2,10,	2,8,	2,7,	0,0,	
9,15,	0,0,	0,0,	0,0,	
0,0,	1,7,	0,0,	1,7,	
1,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,7,	2,7,	
9,19,	9,19,	9,19,	9,19,	
9,19,	9,19,	9,19,	9,19,	
9,19,	9,19,	0,0,	23,30,	
0,0,	0,0,	29,29,	10,16,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,7,	23,24,	1,7,	
23,25,	16,16,	29,18,	2,7,	
0,0,	2,7,	2,7,	10,20,	
10,20,	10,20,	10,20,	10,20,	
10,20,	10,20,	10,20,	10,20,	
10,20,	16,20,	16,20,	16,20,	
16,20,	16,20,	16,20,	16,20,	
16,20,	16,20,	16,20,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	2,7,	
0,0,	2,7,	5,13,	5,14,	
0,0,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,15,	5,14,	5,16,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	0,0,	5,14,	0,0,	
5,14,	5,14,	0,0,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	0,0,	5,14,	0,0,	
5,14,	6,14,	6,14,	0,0,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
0,0,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
0,0,	6,14,	0,0,	6,14,	
6,14,	0,0,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
11,17,	6,14,	30,30,	6,14,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
11,18,	30,24,	0,0,	30,25,	
11,21,	11,21,	11,21,	11,21,	
11,21,	11,21,	11,21,	11,21,	
11,21,	11,21,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
18,18,	0,0,	0,0,	0,0,	
18,23,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,24,	
0,0,	18,25,	18,23,	20,29,	
18,26,	18,26,	18,26,	18,26,	
18,26,	18,26,	18,26,	18,26,	
18,26,	18,26,	0,0,	20,18,	
0,0,	21,17,	0,0,	20,20,	
20,20,	20,20,	20,20,	20,20,	
20,20,	20,20,	20,20,	20,20,	
20,20,	21,18,	0,0,	0,0,	
0,0,	21,21,	21,21,	21,21,	
21,21,	21,21,	21,21,	21,21,	
21,21,	21,21,	21,21,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	24,24,	33,35,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	25,25,	
0,0,	33,24,	0,0,	33,24,	
0,0,	24,31,	24,31,	24,31,	
24,31,	24,31,	24,31,	24,31,	
24,31,	24,31,	24,31,	25,32,	
25,32,	25,32,	25,32,	25,32,	
25,32,	25,32,	25,32,	25,32,	
25,32,	26,30,	31,31,	31,31,	
31,31,	31,31,	31,31,	31,31,	
31,31,	31,31,	31,31,	31,31,	
26,24,	0,0,	26,25,	0,0,	
0,0,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	28,28,	
0,0,	0,0,	0,0,	28,33,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	28,24,	0,0,	
28,24,	28,33,	0,0,	28,34,	
28,34,	28,34,	28,34,	28,34,	
28,34,	28,34,	28,34,	28,34,	
28,34,	32,32,	32,32,	32,32,	
32,32,	32,32,	32,32,	32,32,	
32,32,	32,32,	32,32,	34,35,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	34,24,	0,0,	
34,24,	0,0,	0,0,	34,34,	
34,34,	34,34,	34,34,	34,34,	
34,34,	34,34,	34,34,	34,34,	
34,34,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		yyvstop+1,
yycrank+-39,	yysvec+1,	yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+7,
yycrank+134,	0,		yyvstop+9,
yycrank+229,	0,		yyvstop+13,
yycrank+0,	yysvec+6,	yyvstop+17,
yycrank+23,	yysvec+5,	yyvstop+20,
yycrank+56,	0,		yyvstop+24,
yycrank+87,	yysvec+6,	yyvstop+26,
yycrank+320,	yysvec+5,	yyvstop+29,
yycrank+313,	yysvec+6,	yyvstop+33,
yycrank+0,	yysvec+5,	yyvstop+37,
yycrank+0,	yysvec+6,	yyvstop+40,
yycrank+0,	yysvec+9,	0,	
yycrank+97,	yysvec+6,	yyvstop+42,
yycrank+24,	yysvec+5,	yyvstop+44,
yycrank+404,	yysvec+5,	yyvstop+46,
yycrank+25,	yysvec+9,	yyvstop+48,
yycrank+419,	yysvec+6,	yyvstop+50,
yycrank+433,	yysvec+5,	yyvstop+53,
yycrank+426,	yysvec+6,	yyvstop+56,
yycrank+83,	yysvec+5,	yyvstop+59,
yycrank+517,	0,		0,	
yycrank+527,	yysvec+6,	yyvstop+62,
yycrank+553,	yysvec+5,	yyvstop+64,
yycrank+26,	0,		0,	
yycrank+579,	0,		0,	
yycrank+86,	yysvec+6,	yyvstop+67,
yycrank+322,	yysvec+5,	yyvstop+69,
yycrank+538,	0,		yyvstop+71,
yycrank+589,	yysvec+6,	yyvstop+73,
yycrank+518,	0,		yyvstop+76,
yycrank+615,	0,		yyvstop+78,
yycrank+0,	yysvec+33,	0,	
0,	0,	0};
struct yywork *yytop = yycrank+672;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,  10,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
 32,  33,   1,  33,  33,  33,  33,  33, 
 33,  33,  33,   1,  33,  33,  33,  33, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 48,  48,  33,  33,  33,  33,  33,  33, 
 33,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,   1,  33,   1,  33,  33, 
  1,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,   1,  33,   1,  33,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.12	97/12/08 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
#ifdef YYISARRAY
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
#else
			if (yylastch >= &yytext[ yytextsz ]) {
				int	x = yylastch - yytext;

				yytextsz += YYTEXTSZINC;
				if (yytext == yy_tbuf) {
				    yytext = (char *) malloc(yytextsz);
				    memcpy(yytext, yy_tbuf, sizeof (yy_tbuf));
				}
				else
				    yytext = (char *) realloc(yytext, yytextsz);
				if (!yytext) {
				    fprintf(yyout,
					"Cannot realloc yytext\n");
				    exit(1);
				}
				yylastch = yytext + x;
			}
#endif
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (uintptr_t)yyt > (uintptr_t)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((uintptr_t)yyt < (uintptr_t)yycrank) {	/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
