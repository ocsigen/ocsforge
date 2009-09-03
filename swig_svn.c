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

#include "svn_client.h"
#include "svn_cmdline.h"
#include "svn_pools.h"
#include "svn_string.h"
#include "svn_fs.h"
#include "svn_utf.h"
#include "svn_props.h"
#include "svn_error.h"
#include "apr_errno.h"
#include "caml/signals.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

static apr_pool_t *pool;
svn_client_ctx_t *ctx;


void svn_cstring_split_endline_append (apr_array_header_t *array,
				       const char *input,
				       apr_pool_t *pool) {
  int start = 0, end;
  int len = strlen(input);
  for (end = 0; end < len; end++){
    if (input[end] == '\n') {
      char *sub = apr_pstrndup(pool,(input+start),(end-start));
      *(char **)apr_array_push(array) = sub;
      start = end+1;
    }
  }
  if (start < end) {
    char *sub = apr_pstrndup(pool,(input+start),(len-start));
    *(char **)apr_array_push(array) = sub;
  }
} 	



/* fonction déterminant le comportement en cas d'annulation 
   de l'opération courante */
svn_error_t *cancel(void *cancel_baton)
{
  return SVN_NO_ERROR;
} 


/* fonction permettant d'initialiser le contexte avant toute opération svn */
svn_client_ctx_t *initialize_context(){  
  svn_error_t *err;
  svn_config_t *cfg;
  
  err = svn_config_ensure (NULL, pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }
  
  // -- Creation du contexte --
  svn_client_create_context(&ctx,pool);
  svn_config_get_config (&(ctx->config), NULL, pool);
 
  // -- Récupération du fichier de config dans ~/.subversion --
  const char *config_path;
  svn_config_get_user_config_path(&config_path,
                                  NULL,
                                  SVN_CONFIG_CATEGORY_CONFIG,
                                  pool);
  svn_config_read(&cfg,
		  config_path,
		  TRUE,
		  pool);
  
  // -- Initialisation des parametres d'authentification --
  svn_cmdline_create_auth_baton(&ctx->auth_baton,
                                TRUE,
                                NULL,
                                NULL,
                                config_path,
                                FALSE,
                                FALSE,
                                cfg,
                                cancel,
                                ctx->cancel_baton,
                                pool);
  return ctx;
}
  

/* Fonction d'initialisation SVN / APR */
void svn_init(){
  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return;
  pool = svn_pool_create(NULL);
  ctx = initialize_context();
  /*err = svn_fs_initialize(pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_init: ");
    return;
    }*/
}
 


/* création d'un stringbuf dont la chaine se termine par un "\n" */
svn_stringbuf_t *stringbuf_endline_utf8(const char *string,apr_pool_t *subpool)
{  
  const svn_stringbuf_t *endline = svn_stringbuf_create("\n",subpool);
  svn_stringbuf_t *res_utf8,*res = svn_stringbuf_create(string,subpool);
  svn_stringbuf_appendstr(res,endline);
  svn_utf_stringbuf_to_utf8(&res_utf8,res,subpool);  
  return res_utf8;
}

/* fonction callback pour l'appel a svn_client_list2 */
svn_error_t *list_callback(void *baton,
			   const char *path,
			   const svn_dirent_t *dirent,
			   const svn_lock_t *lock,
			   const char *abs_path,
			   apr_pool_t *subpool)
{  
  if (strcmp(path,"") == 0) {
    svn_error_t *err = svn_error_create(1,NULL,"Wrong node kind");
    if (dirent->kind != svn_node_dir) 
      return err;
    return SVN_NO_ERROR;
  }
  svn_stringbuf_t *temp = NULL; 
  if (dirent->kind == svn_node_dir){
    const svn_stringbuf_t *slash = svn_stringbuf_create("/",subpool);
    svn_stringbuf_t *dir_format = svn_stringbuf_create(path,subpool);
    svn_stringbuf_appendstr(dir_format,slash);
    temp = stringbuf_endline_utf8(dir_format->data,subpool);
  } 
  else
    temp = stringbuf_endline_utf8(path,subpool);
  svn_stringbuf_t *author = stringbuf_endline_utf8(dirent->last_author,subpool);
  char *rev = apr_psprintf(subpool,"%d\n",(int)dirent->created_rev);
  svn_stringbuf_t *revbuf = stringbuf_endline_utf8(rev,subpool);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton, (const svn_stringbuf_t *)temp);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)author);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)revbuf);
  return SVN_NO_ERROR;
}

/* fonction callback pour l'appel a svn_client_log4 */
svn_error_t* log_callback(void *baton, svn_log_entry_t *log_entry, apr_pool_t *pool)
{  
  apr_hash_t *revprops = log_entry->revprops;
  int rev = (int)log_entry->revision;
  svn_string_t *author_s, *date_s,  *message_s;
  svn_stringbuf_t *revnum, *author, *day, *parsed_time, *parsed_message;
  const char *date, *day_tmp, *time, *parsed_time_tmp, *message, *parsed_message_tmp;
  apr_array_header_t *date_array, *time_array, *message_array;
  
  // -- Recuperation du champ "revision" --
  char *n = apr_psprintf(pool,"%d\n",rev);
  revnum = svn_stringbuf_create(n,pool); 
 
  // -- Recuperation du champ "auteur" --
  if ((author_s = apr_hash_get(revprops, 
			       SVN_PROP_REVISION_AUTHOR,
			       APR_HASH_KEY_STRING))) {
    author = stringbuf_endline_utf8(author_s->data,pool);
  }
  else
    author = svn_stringbuf_create("Unknown author\n",pool);
   
  // -- Recuperation & traitement du champ "date" --
  if ((date_s = apr_hash_get(revprops, 
			     SVN_PROP_REVISION_DATE,
			     APR_HASH_KEY_STRING))){
    date = date_s->data;
    date_array = svn_cstring_split(date,"T",TRUE,pool);
    day_tmp = ((const char**)date_array->elts)[0];
    day = stringbuf_endline_utf8(day_tmp,pool);
    time = ((const char**)date_array->elts)[1];
    time_array = svn_cstring_split(time,".",TRUE,pool);
    parsed_time_tmp = ((const char**)time_array->elts)[0];
    parsed_time = stringbuf_endline_utf8(parsed_time_tmp,pool);
  }  
  else {
    day = svn_stringbuf_create("???\n",pool);
    parsed_time = svn_stringbuf_create("???\n",pool);
  }
  // -- Recuperation & traitement du champ "message" --
  if ((message_s = apr_hash_get(revprops, 
				SVN_PROP_REVISION_LOG,
				APR_HASH_KEY_STRING))){
    message = message_s->data;
    if (strcmp(message,"") == 0) {
      parsed_message = svn_stringbuf_create(" \n",pool);
    }
    else {
      message_array = svn_cstring_split(message,"\n",TRUE,pool);
      parsed_message_tmp = svn_cstring_join(message_array,"<br/>",pool);
      parsed_message = stringbuf_endline_utf8(parsed_message_tmp,pool);
    }
  }
  else{
    parsed_message = svn_stringbuf_create(" \n",pool);
  }   
   
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)revnum);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)author);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)day);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)parsed_time);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)parsed_message);
  return SVN_NO_ERROR;
}

svn_error_t* blame_callback(void *baton, apr_int64_t line_no, 
                            svn_revnum_t revision, const char *author,
                            const char *date, svn_revnum_t merged_revision,
                            const char *merged_author, const char *merged_date,
                            const char *merged_path, const char *line, 
                            apr_pool_t *pool){
  
  svn_stringbuf_t *aut = stringbuf_endline_utf8(author,pool);
  svn_stringbuf_t *contents;
  if (strcmp(line,"") == 0) {
    contents = stringbuf_endline_utf8(" ",pool);
  }
  else
    contents = stringbuf_endline_utf8(line,pool);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)aut);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)contents);
  
  return SVN_NO_ERROR;
}

/* fonction faite pour récuperer un unique objet info et le stocker dans baton */
svn_error_t* svn_info_callback(void *baton, const char *path, 
                               const svn_info_t *info, apr_pool_t *pool)
{
  char *rev = apr_psprintf(pool,"%d\n",(int)info->last_changed_rev);
  svn_stringbuf_t *res = svn_stringbuf_create(rev,pool);
  svn_stringbuf_appendstr((svn_stringbuf_t *)baton,(const svn_stringbuf_t *)res);
  return SVN_NO_ERROR;
} 


/*const svn_info_t **/ 
int svn_support_info(apr_pool_t *pool,svn_client_ctx_t *ctx,const char *path)
{
  svn_error_t *err;
  svn_opt_revision_t peg_revision;
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);
  peg_revision.kind = svn_opt_revision_head;
  
  err = svn_client_info2 (path, 
                          &peg_revision, 
                          &peg_revision, 
                          svn_info_callback,
                          res,
                          svn_depth_empty,
                          NULL,
                          ctx,
                          pool);
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support_info: ");
  }
  return atoi(res->data);
}

/* fonction simulant la commande svn list */

apr_array_header_t *svn_support_list_call(char *rep_path, int rev,apr_pool_t *subpool)
{  
  svn_error_t *err;
  svn_opt_revision_t revision;
  
  // -- Initialisation de la revision --
  if (rev != -1) {
    revision.kind = svn_opt_revision_number;
    revision.value.number = rev;
  }
  else{
    revision.kind = svn_opt_revision_unspecified;
  }

  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(subpool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",subpool);	
  
  // -- Appel de svn list --
  
  err = svn_client_list2(rep_path,
			 &revision,
			 &revision,
			 svn_depth_immediates,
			 SVN_DIRENT_ALL,
			 FALSE,
			 list_callback,
			 res,
			 ctx,
			 subpool);
  
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support_list: ");
    svn_pool_destroy(subpool);
    return NULL;
  } 
  
  svn_cstring_split_append(list_result,res->data,"\n",TRUE,subpool);  
  
  svn_pool_destroy(subpool);
  return list_result;
}

apr_array_header_t *svn_support_list(char *rep_path, int revision){
  apr_pool_t * subpool = svn_pool_create(pool);
  svn_stringbuf_t * rep = svn_stringbuf_create (rep_path, subpool);
  caml_enter_blocking_section ();
  apr_array_header_t *res = svn_support_list_call(rep->data,revision,subpool); 
  caml_leave_blocking_section ();
  return res;
}

/* fonction simulant la commande svn log */
apr_array_header_t *svn_support_log_call(char *rep_path, 
                                         int start, 
                                         int end, 
                                         int limit, 
                                         apr_pool_t *subpool)
{  
  svn_error_t *err;
  svn_opt_revision_t revision_start;
  svn_opt_revision_t revision_end;

  
  // -- Initialisation de la cible --
  apr_array_header_t *targets = apr_array_make (subpool, 1, sizeof (char *));
  *(char **)apr_array_push(targets) = rep_path;
  
  // -- Initialisation de l'intervalle de revisions --
  if (end == -1) { 
    if (start == -1) {
      revision_start.kind = svn_opt_revision_head;
      revision_end.kind = svn_opt_revision_number;
      revision_end.value.number = 1;
    }
    else {
      revision_start.kind = svn_opt_revision_number;
      revision_start.value.number = start;
      revision_end.kind = svn_opt_revision_head;
    }
  }
  else {
    if (start == -1) {
      revision_start.kind = svn_opt_revision_number;
      revision_start.value.number = end;
      revision_end.kind = svn_opt_revision_number;
      revision_end.value.number = 1;
    }
    else {
      revision_start.kind = svn_opt_revision_number;
      revision_start.value.number = start;
      revision_end.kind = svn_opt_revision_number;
      revision_end.value.number = end;
    }
  }
    
  // -- Choix des propriétés a récupérer --
  apr_array_header_t *revprops = apr_array_make (subpool, 3, sizeof (char *));
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_AUTHOR;
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_DATE;
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_LOG;

  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(subpool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",subpool);	
  err = svn_client_log4(targets,
			&revision_start,
			&revision_start,
			&revision_end,
			limit,
			FALSE,
			TRUE,
			FALSE,
			revprops,
			log_callback,
			res,
			ctx,
			subpool);
  
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support_log: ");
    svn_pool_destroy(subpool);
    return NULL;
  }
  svn_cstring_split_append(list_result,res->data,"\n",FALSE,subpool);
  svn_pool_destroy(subpool);
  return list_result;
}

apr_array_header_t *svn_support_log(char *rep_path, int start, int end, int limit){
  apr_pool_t * subpool = svn_pool_create(pool);
  svn_stringbuf_t * rep = svn_stringbuf_create(rep_path, subpool);
  caml_enter_blocking_section ();
  apr_array_header_t *res = svn_support_log_call(rep->data,start,end,limit,subpool); 
  caml_leave_blocking_section ();
  return res;
}

/* fonction simulant la commande svn diff */
apr_array_header_t *svn_support_diff_call(char *rep_path,
                                          char *filename,
                                          int revision1,
                                          int revision2,
                                          apr_pool_t* subpool)
{
  svn_error_t *err;
  apr_file_t *outfile;
  apr_file_t *errfile;
  apr_status_t rv;
  svn_opt_revision_t rev1;
  svn_opt_revision_t rev2;
 
  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(subpool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",subpool);	
  
  // -- Initialisation des chemins des fichiers --
  const svn_stringbuf_t *slash = svn_stringbuf_create("/",subpool);
  const svn_stringbuf_t *filepath = svn_stringbuf_create(filename,subpool);
  svn_stringbuf_t *path = svn_stringbuf_create(rep_path,subpool);
  if (strcmp(filename,"") != 0) {
    svn_stringbuf_appendstr(path,slash);
    svn_stringbuf_appendstr(path,filepath);
  }
  
  // -- Initialisation des revisions --
  rev1.kind = rev2.kind = svn_opt_revision_number;
  rev1.value.number=revision1;
  rev2.value.number=revision2;
  
  // -- Initialisation des fichiers d'output --
  if ((rv = apr_file_open_stdout(&outfile,subpool)) != APR_SUCCESS) {
    svn_pool_destroy(subpool);
    return NULL;
  }
  if ((rv = apr_file_open_stdout(&errfile,subpool)) != APR_SUCCESS) {
    svn_pool_destroy(subpool);
    return NULL;
  }
  const apr_array_header_t *diff_options =  apr_array_make(subpool, 1, sizeof (const char *));
  //apr_file_t *write_end;
  //apr_file_t *read_end;
  //apr_file_pipe_create(&write_end,&read_end,subpool);
  int tube[2];
  pipe(tube);
  switch(fork()){
  case -1: 
    perror("fork");
    svn_pool_destroy(subpool);
    return NULL;
  case 0:
    //apr_file_close(read_end);
    //apr_file_dup2(outfile,write_end,subpool);
    close(tube[0]);
    dup2(tube[1],STDOUT_FILENO);
    err = svn_client_diff4(diff_options,
			   path->data,
			   &rev1,
			   path->data,
			   &rev2,
			   NULL,
			   svn_depth_infinity,
			   TRUE,// ignore_ancestry
			   TRUE,// no_diff_deleted
			   FALSE, // ignore_content_type
			   SVN_APR_LOCALE_CHARSET,
			   outfile,
			   errfile,
			   NULL,
			   ctx,
			   subpool);
    if (err) {
      svn_handle_error2 (err, stderr, FALSE, "svn_support_diff: ");
      svn_pool_destroy(subpool);
      return NULL;
    }
    exit(0);
  }
  //apr_file_close(write_end);
  close(tube[1]);
  int buf_size = 4096;
  char buf[buf_size];
  int rc;
  const svn_stringbuf_t *tmp;
  while((rc=read(tube[0],buf,buf_size))>0){//(rv=apr_file_gets(buf,buf_size,read_end))){
      //if(APR_STATUS_IS_EOF(rv))
      //break;
    buf[rc]='\0';
    tmp = svn_stringbuf_create(buf,subpool);
    svn_stringbuf_appendstr(res,tmp);
    //*(char **)apr_array_push(list_result) = buf;
  }
  wait(NULL);
  svn_cstring_split_endline_append(list_result,res->data,subpool);
  svn_pool_destroy(subpool);
  return list_result;
}


apr_array_header_t *svn_support_diff(char *rep_path, 
                                    char *filename, 
                                    int revision1, 
                                    int revision2)
{
  apr_pool_t *subpool = svn_pool_create(pool);
  svn_stringbuf_t * rep = svn_stringbuf_create(rep_path, subpool);
  svn_stringbuf_t * file = svn_stringbuf_create(filename, subpool);
  caml_enter_blocking_section ();
  apr_array_header_t *res = svn_support_diff_call(rep->data,file->data,revision1,revision2,subpool); 
  caml_leave_blocking_section ();
  return res;
}

/* fonction simulant la commande svn cat */  
apr_array_header_t *svn_support_cat_call(char *file_path, 
                                         int revision, 
                                         apr_pool_t *subpool){
  svn_error_t *err;
  svn_opt_revision_t rev;
  svn_stream_t *out;
  
  // -- Initialisation de la révision --
  if (revision != -1) {
    rev.kind = svn_opt_revision_number;
    rev.value.number = revision;
  }
  else 
    rev.kind = svn_opt_revision_unspecified;
 
  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(subpool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",subpool);	
  
  // -- Initialisation du flux de sortie --
  out = svn_stream_from_stringbuf(res,subpool);
  
  err = svn_client_cat2(out, 
                        file_path,
                        &rev,
                        &rev,
                        ctx,
                        subpool);
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support_cat: ");
    svn_pool_destroy(subpool);
    return NULL;
  }

  svn_cstring_split_endline_append(list_result,res->data,subpool);
  svn_pool_destroy(subpool);
  return list_result;
}

apr_array_header_t *svn_support_cat(char *file_path, int revision){
  apr_pool_t *subpool = svn_pool_create(pool);
  svn_stringbuf_t * file = svn_stringbuf_create(file_path, subpool);
  caml_enter_blocking_section ();
  apr_array_header_t *res = svn_support_cat_call(file->data,revision,subpool); 
  caml_leave_blocking_section ();
  return res;
}


apr_array_header_t *svn_support_blame_call(char *file_path, 
                                           int revision, 
                                           apr_pool_t *subpool)
{
  svn_error_t *err;
  svn_opt_revision_t peg_rev,start,end;
  svn_diff_file_options_t *diff_options;
  
  // -- Initialisation des révisions --
  if (revision != -1) {
    peg_rev.kind = end.kind = svn_opt_revision_number;
    peg_rev.value.number = end.value.number = revision;
    
  }
  else {
    peg_rev.kind = svn_opt_revision_unspecified;
    end.kind = svn_opt_revision_head;
  }
  
  start.kind = svn_opt_revision_number;
  start.value.number = 1;

  // -- Initialisation des diff_options --

  diff_options = svn_diff_file_options_create(pool);
  

  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(pool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);
  

  err = svn_client_blame4(file_path,
                          &peg_rev,
                          &start,
                          &end,
                          diff_options,
                          FALSE, // ignore_mime_type
                          FALSE,  // include_merged_revisions
                          blame_callback,
                          res,
                          ctx,
                          pool);
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support_blame: ");
    svn_pool_destroy(subpool);
    return NULL;
  }
  svn_cstring_split_append(list_result,res->data,"\n",FALSE,pool);
  svn_pool_destroy(subpool);
  return list_result;
}


apr_array_header_t *svn_support_blame(char *file_path, int revision){
  apr_pool_t *subpool = svn_pool_create(pool);
  svn_stringbuf_t *file = svn_stringbuf_create(file_path,subpool);
  caml_enter_blocking_section ();
  apr_array_header_t *res = svn_support_blame_call(file->data,revision,subpool); 
  caml_leave_blocking_section ();
  return res;
}

/*
int main(void){
  svn_init();
  svn_support_cat("https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge/README",149);
  }*/

