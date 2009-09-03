/* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/** @author Granarolo Jean-Henri **/

%module swig_svn    
%{                                                                              
#include "svn_client.h"
#include "svn_cmdline.h"
#include "svn_pools.h"
#include "svn_string.h"
#include "svn_fs.h"
#include "svn_error.h"
#include "apr_errno.h"
#include <stdlib.h>
#include <stdio.h>
#include "swig_svn.h"
%}

%typemap(out) apr_array_header_t *{
  int i;
  const char *s;
  SWIG_CAMLlocal1(res_list);
  res_list = Val_unit;
  SWIG_contract_assert(($1 != NULL),caml_val_string("svn error"));
  for(i=0; i<($1->nelts); i++) {
    s = ((const char**)$1->elts)[i];
    res_list = caml_list_append(res_list,caml_val_string(s));
  }
  //swig_result = caml_list_append(swig_result, res_list);
  CAMLreturn(res_list);  
}

void svn_init(void);
apr_array_header_t *svn_support_list(char *,int);
apr_array_header_t *svn_support_log(char *, int, int, int);
apr_array_header_t *svn_support_diff(char *, char *, int, int);
apr_array_header_t *svn_support_cat(char *, int);
apr_array_header_t *svn_support_blame(char *,int);
