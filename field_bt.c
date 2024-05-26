#include <string.h>


#include <execinfo.h>
#include <stdio.h>

void field_bt_ (char * str, int * p1, int * p2, int * plen)
{
  void * addr[*p2];
  int size;
  int i;
  char * pstr = str;

  memset (str, ' ', *plen);

  size = backtrace (addr, *p2);

  for (i = *p1; i < size; i++)
    {
      sprintf (pstr, " 0x%16.16llx", addr[i]);
      pstr += 3 + 16;
    }

  *plen = pstr - str;

}


#ifdef UNDEF

void field_bt_ (char * str, int * p1, int * p2, int * plen)
{
  memset (str, ' ', *plen);
  *plen = 0;
}

#endif


