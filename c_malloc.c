/*
 * (C) Copyright 2022- ECMWF.
 * (C) Copyright 2022- Meteo-France.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <stdlib.h>

 void c_malloc (size_t siz, void ** ptr)
{
  *ptr = malloc (siz);
}

 void c_free (void * ptr)
{
   free(ptr);
}

 void c_ptr_incr (size_t siz, void * ptr)
{
  ptr += siz;
}

 void c_ptr_decr (size_t siz, void * ptr)
{
  ptr -= siz;
}

 void print_c_addr (void * ptr)
{
  printf (" GOT HERE C ADDR = %lld\n", ptr);
}