extern double   armin(int, float *);
extern double   armax(int, float *);

extern void     Initialize(int, float [], float [], int, int, 
                           float [], float []);
int             ReadData(int, float *, float *, float *);
float           **MakeGrid(int, int, float *, float *);

void            c_nngetslopes(int, int, float *, int *);
void            c_nngetaspects(int, int, float *, int *);
void            c_nnpntinits(int, float [], float [], float []);
extern void     c_nnpnts(float, float, float *);
void            c_nnpntend();
