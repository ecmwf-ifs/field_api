#include <string.h>

#ifdef HAVE_BACKTRACE

#include <execinfo.h>
#include <stdio.h>

void field_backtrace_ (char * str, int * p1, int * p2, int * plen)
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


#else

void field_backtrace_ (char * str, int * p1, int * p2, int * plen)
{
  memset (str, ' ', *plen);
  *plen = 0;
}

#endif


