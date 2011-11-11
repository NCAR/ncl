#ifndef NclOptions_h
#define NclOptions_h
typedef struct _NCLOptions		NCLOptions;

struct _NCLOptions
{
    NclQuark          name;
    NclBasicDataTypes type;
    int               size;
    void             *values;
};
#endif

