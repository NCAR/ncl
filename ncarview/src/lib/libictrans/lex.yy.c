# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
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
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
;
break;
case 2:
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
		{
			have_command = 0;
			return(END_LINE);
			}
break;
case 7:
		{
			have_command = 0;
			(void) skip_line();
			return(ERROR_TYPE);
			}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */



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
	bcopy(s1 + 1, s1, len);
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

2,
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

3,
5,
0,

5,
0,

3,
0,

3,
0,

3,
0,
0};
# define YYTYPE char
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
1,9,	1,7,	1,9,	1,8,	
1,7,	1,10,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,23,	
23,23,	0,0,	27,29,	1,7,	
1,7,	1,7,	1,7,	1,7,	
1,7,	1,7,	1,11,	17,24,	
23,24,	27,21,	0,0,	27,21,	
0,0,	0,0,	2,7,	2,8,	
2,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,9,	2,7,	
2,9,	2,8,	2,7,	0,0,	
9,14,	0,0,	0,0,	0,0,	
0,0,	1,7,	0,0,	1,7,	
1,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,7,	2,7,	
9,17,	9,17,	9,17,	9,17,	
9,17,	9,17,	9,17,	9,17,	
9,17,	9,17,	0,0,	0,0,	
0,0,	0,0,	0,0,	21,21,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,7,	
0,0,	0,0,	0,0,	2,7,	
0,0,	2,7,	2,7,	21,26,	
21,26,	21,26,	21,26,	21,26,	
21,26,	21,26,	21,26,	21,26,	
21,26,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	2,7,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,0,	5,0,	5,0,	
5,0,	5,12,	5,13,	5,0,	
0,0,	24,24,	0,0,	0,0,	
0,0,	24,27,	0,0,	0,0,	
5,14,	0,0,	5,14,	0,0,	
24,21,	5,13,	24,21,	24,27,	
28,29,	24,28,	24,28,	24,28,	
24,28,	24,28,	24,28,	24,28,	
24,28,	24,28,	24,28,	28,21,	
0,0,	28,21,	5,13,	0,0,	
28,28,	28,28,	28,28,	28,28,	
28,28,	28,28,	28,28,	28,28,	
28,28,	28,28,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
5,0,	0,0,	5,0,	0,0,	
0,0,	5,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
5,0,	5,0,	5,0,	0,0,	
5,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,0,	6,0,	6,0,	6,0,	
6,13,	0,0,	6,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	6,0,	
0,0,	6,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,0,	7,0,	7,0,	
7,0,	7,13,	0,0,	7,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
7,0,	0,0,	7,0,	6,0,	
0,0,	6,0,	0,0,	0,0,	
6,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	6,0,	
6,0,	6,0,	0,0,	6,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
7,0,	0,0,	7,0,	0,0,	
0,0,	7,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
7,0,	7,0,	7,0,	0,0,	
7,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,0,	8,0,	8,0,	8,0,	
8,15,	0,0,	8,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,14,	
8,16,	8,14,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,15,	0,0,	
10,0,	0,0,	0,0,	8,0,	
0,0,	8,0,	0,0,	0,0,	
8,0,	10,14,	10,16,	10,14,	
0,0,	0,0,	10,18,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,0,	
8,0,	8,0,	0,0,	8,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	10,0,	0,0,	10,0,	
0,0,	0,0,	10,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	10,0,	10,0,	10,0,	
0,0,	10,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,0,	11,0,	11,0,	
11,0,	11,13,	0,0,	11,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
11,0,	0,0,	11,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	11,19,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
11,0,	0,0,	11,0,	0,0,	
0,0,	11,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
11,0,	11,0,	11,0,	0,0,	
11,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
12,0,	12,0,	12,0,	12,0,	
0,0,	0,0,	12,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	12,14,	
0,0,	12,14,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,0,	
13,0,	13,13,	0,0,	13,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
13,0,	0,0,	13,0,	12,0,	
0,0,	12,0,	0,0,	0,0,	
12,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	12,0,	
12,0,	12,0,	0,0,	12,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
13,0,	0,0,	13,0,	0,0,	
0,0,	13,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
13,0,	13,0,	13,0,	0,0,	
13,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,0,	15,0,	15,0,	15,0,	
15,15,	0,0,	15,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	15,14,	
15,16,	15,14,	0,0,	0,0,	
0,0,	0,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,0,	16,0,	16,0,	
16,0,	16,16,	16,13,	16,0,	
0,0,	16,20,	0,0,	0,0,	
0,0,	0,0,	0,0,	15,0,	
16,21,	15,0,	16,21,	16,20,	
15,0,	16,22,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	16,13,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	15,0,	
15,0,	15,0,	0,0,	15,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
16,0,	0,0,	16,0,	0,0,	
0,0,	16,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
16,0,	16,0,	16,0,	0,0,	
16,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,0,	18,0,	18,0,	18,0,	
18,15,	0,0,	18,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,14,	
18,16,	18,14,	0,0,	0,0,	
18,18,	0,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,0,	19,0,	19,0,	
19,0,	19,13,	0,0,	19,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,0,	
19,0,	18,0,	19,0,	0,0,	
18,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	19,19,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,0,	
18,0,	18,0,	0,0,	18,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
19,0,	0,0,	19,0,	0,0,	
0,0,	19,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
19,0,	19,0,	19,0,	0,0,	
19,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,0,	20,0,	20,0,	20,0,	
20,25,	0,0,	20,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	20,21,	
0,0,	20,21,	0,0,	0,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,0,	
22,0,	22,0,	22,0,	22,25,	
0,0,	22,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	22,21,	20,0,	
22,21,	20,0,	0,0,	22,22,	
20,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	20,0,	
20,0,	20,0,	0,0,	20,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	22,0,	0,0,	
22,0,	0,0,	0,0,	22,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	22,0,	22,0,	
22,0,	0,0,	22,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,0,	25,0,	
25,0,	25,0,	25,25,	0,0,	
25,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	25,21,	0,0,	25,21,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	25,0,	0,0,	25,0,	
0,0,	0,0,	25,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	25,0,	25,0,	25,0,	
0,0,	25,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		yyvstop+1,
yycrank+-39,	yysvec+1,	yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+7,
yycrank+-165,	0,		yyvstop+9,
yycrank+-292,	yysvec+5,	yyvstop+13,
yycrank+-337,	yysvec+5,	yyvstop+17,
yycrank+-464,	yysvec+5,	yyvstop+20,
yycrank+56,	0,		yyvstop+24,
yycrank+-518,	yysvec+5,	yyvstop+26,
yycrank+-645,	yysvec+5,	yyvstop+30,
yycrank+-772,	yysvec+5,	yyvstop+34,
yycrank+-817,	yysvec+5,	yyvstop+37,
yycrank+0,	yysvec+9,	0,	
yycrank+-944,	yysvec+5,	yyvstop+39,
yycrank+-993,	0,		yyvstop+41,
yycrank+23,	yysvec+9,	yyvstop+43,
yycrank+-1120,	yysvec+5,	yyvstop+45,
yycrank+-1169,	yysvec+5,	yyvstop+48,
yycrank+-1296,	yysvec+5,	yyvstop+51,
yycrank+87,	0,		0,	
yycrank+-1343,	yysvec+5,	yyvstop+54,
yycrank+24,	0,		0,	
yycrank+169,	0,		0,	
yycrank+-1470,	yysvec+5,	yyvstop+57,
yycrank+97,	0,		yyvstop+59,
yycrank+26,	0,		yyvstop+61,
yycrank+184,	0,		yyvstop+63,
yycrank+0,	yysvec+27,	0,	
0,	0,	0};
struct yywork *yytop = yycrank+1597;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,'!' ,01  ,'!' ,'!' ,'!' ,'!' ,'!' ,
'!' ,'!' ,'!' ,01  ,'!' ,01  ,'!' ,'!' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,'!' ,'!' ,'!' ,'!' ,'!' ,'!' ,
'!' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,'!' ,01  ,'!' ,'!' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,'!' ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
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
			*yylastch++ = yych = input();
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
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
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
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
