int _MachineIsBigEndian()
{
    short int word = 0x0001;
    char *byte = (char *) &word;
    if(byte[0])
       return 0;
    else
       return 1;
}

