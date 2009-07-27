%module swig_svn    
%{                                                                              
#include "svn_client.h"
#include "svn_cmdline.h"
#include "svn_pools.h"
#include "svn_string.h"
#include "svn_fs.h"
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
  for(i=0; i<$1->nelts; i++) {
    s = ((const char**)$1->elts)[i];
    res_list = caml_list_append(res_list,caml_val_string(s));
  }
  //swig_result = caml_list_append(swig_result, res_list);
  CAMLreturn(res_list);  
}

apr_array_header_t *svn_support_list(char *,int);
apr_array_header_t *svn_support_log(char *, int, int, int);
apr_array_header_t *svn_support_diff(char *, char *, int, int);
apr_array_header_t *svn_support_cat(char *, int);
apr_array_header_t *svn_support_blame(char *,int);