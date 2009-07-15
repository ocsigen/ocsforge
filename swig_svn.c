#include "svn_client.h"
#include "svn_cmdline.h"
#include "svn_pools.h"
#include "svn_string.h"
#include "svn_fs.h"
#include "svn_utf.h"
#include "svn_props.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>


/* création d'un stringbuf dont la chaine se termine par un "\n" */
svn_stringbuf_t *stringbuf_endline_utf8(const char *string,apr_pool_t *pool)
{  
  const svn_stringbuf_t *endline = svn_stringbuf_create("\n",pool);
  svn_stringbuf_t *res_utf8,*res = svn_stringbuf_create(string,pool);
  svn_stringbuf_appendstr(res,endline);
  svn_utf_stringbuf_to_utf8(&res_utf8,res,pool);  
  return res_utf8;
}

/* fonction callback pour l'appel a svn_client_list2 */
svn_error_t *list_callback(void *baton,
			   const char *path,
			   const svn_dirent_t *dirent,
			   const svn_lock_t *lock,
			   const char *abs_path,
			   apr_pool_t *pool)
{  
  if (strcmp(path,"") == 0) return SVN_NO_ERROR;
  svn_stringbuf_t *temp = NULL; 
  if (dirent->kind == svn_node_dir){
    const svn_stringbuf_t *slash = svn_stringbuf_create("/",pool);
    svn_stringbuf_t *dir_format = svn_stringbuf_create(path,pool);
    svn_stringbuf_appendstr(dir_format,slash);
    temp = stringbuf_endline_utf8(dir_format->data,pool);
  } 
  else
    temp = stringbuf_endline_utf8(path,pool);
  svn_stringbuf_t *author = stringbuf_endline_utf8(dirent->last_author,pool);
  char *rev = apr_psprintf(pool,"%d\n",(int)dirent->created_rev);
  svn_stringbuf_t *revbuf = stringbuf_endline_utf8(rev,pool);
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

/* fonction déterminant le comportement en cas d'annulation 
   de l'opération courante */
svn_error_t *cancel(void *cancel_baton)
{
  return SVN_NO_ERROR;
} 


/* fonction permettant d'initialiser le contexte avant toute opération svn */
svn_client_ctx_t *initialize_context(apr_pool_t *pool)
{  
  svn_error_t *err;
  svn_client_ctx_t *ctx;
  svn_config_t *cfg;
  
  err = svn_config_ensure (NULL, pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }
  
  // -- Creation du contexte --
  svn_client_create_context(&ctx,pool);
  svn_config_get_config (&(ctx->config), NULL, pool);
  
  // -- Initialisation de l'utf-8 --
  svn_utf_initialize(pool);
  
  // -- Récupération du fichier de config dans ~/.subversion --
  const svn_stringbuf_t *svn_path = 
    svn_stringbuf_create("/.subversion/config",pool);
  svn_stringbuf_t *cfg_path = svn_stringbuf_create(getenv("HOME"),pool);
  svn_stringbuf_appendstr(cfg_path,svn_path);
  svn_config_read(&cfg,
		  cfg_path->data,
		  TRUE,
		  pool);
  
  // -- Initialisation des parametres d'authentification --
  svn_cmdline_create_auth_baton(&ctx->auth_baton,
                                FALSE,
                                NULL,
                                NULL,
                                NULL,
                                FALSE,
                                TRUE,
                                cfg,
                                cancel,
                                ctx->cancel_baton,
                                pool);
  return ctx;
}


/* fonction simulant la commande svn list */
apr_array_header_t *svn_support_list(char *rep_path, int rev)
{  
  apr_pool_t *pool ;
  svn_error_t *err;
  svn_client_ctx_t *ctx; 
  svn_opt_revision_t revision;
  
  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return NULL;

  pool = svn_pool_create(NULL);
  err = svn_fs_initialize(pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }

  // -- Initialisation du contexte --
  ctx = initialize_context(pool);
  
  // -- Initialisation de la revision --
  if (rev != -1) {
    revision.kind = svn_opt_revision_number;
    revision.value.number = rev;
  }
  else{
    revision.kind = svn_opt_revision_unspecified;
  }

  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(pool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);		 
  
  // -- Appel de svn list --
  err = svn_client_list2(rep_path,
			 &revision,
			 &revision,
			 svn_depth_infinity,
			 SVN_DIRENT_ALL,
			 FALSE,
			 list_callback,
			 res,
			 ctx,
			 pool);
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support: ");
  } 
  svn_cstring_split_append(list_result,res->data,"\n",TRUE,pool);    
  return list_result;
}

/* fonction simulant la commande svn log */
apr_array_header_t *svn_support_log(char *rep_path)
{  
  apr_pool_t *pool ;
  svn_error_t *err;
  svn_client_ctx_t *ctx; 
  svn_opt_revision_t revision;
  svn_opt_revision_t revision_start;
  svn_opt_revision_t revision_end;
  
  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return NULL;
  
  pool = svn_pool_create(NULL);
  err = svn_fs_initialize(pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }

  // -- Initialisation du contexte --
  ctx = initialize_context(pool);
  
  // -- Initialisation de la cible --
  apr_array_header_t *targets = apr_array_make (pool, 1, sizeof (char *));
  *(char **)apr_array_push(targets) = rep_path;
  
  // -- Initialisation de l'intervalle de revisions --
  revision.kind = svn_opt_revision_unspecified;
  revision_start.kind = svn_opt_revision_number;
  revision_start.value.number=1; 
  revision_end.kind = svn_opt_revision_head;
  
  // -- Choix des propriétés a récupérer --
  apr_array_header_t *revprops = apr_array_make (pool, 3, sizeof (char *));
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_AUTHOR;
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_DATE;
  *(char **)apr_array_push(revprops) = SVN_PROP_REVISION_LOG;

  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(pool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);	
  
  err = svn_client_log4(targets,
			&revision,
			&revision_start,
			&revision_end,
			0,
			TRUE,
			TRUE,
			FALSE,
			revprops,
			log_callback,
			res,
			ctx,
			pool);
  
  if (err) {
    svn_handle_error2(err, stderr, FALSE, "svn_support: ");
  }
  svn_cstring_split_append(list_result,res->data,"\n",FALSE,pool);
  return list_result;
}

/* fonction simulant la commande svn diff */
apr_array_header_t *svn_support_diff(char *rep_path,
				     char *filename,
   				     int revision1,
				     int revision2)
{
  apr_pool_t *pool ;
  svn_error_t *err;
  svn_client_ctx_t *ctx; 
  apr_file_t *outfile;
  apr_file_t *errfile;
  apr_status_t rv;
  svn_opt_revision_t rev1;
  svn_opt_revision_t rev2;

  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return NULL;
  
  pool = svn_pool_create(NULL);
  err = svn_fs_initialize(pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }

  // -- Initialisation du contexte --
  ctx = initialize_context(pool);
  
  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(pool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);	
  
  // -- Initialisation des chemins des fichiers --
  const svn_stringbuf_t *slash = svn_stringbuf_create("/",pool);
  const svn_stringbuf_t *filepath = svn_stringbuf_create(filename,pool);
  svn_stringbuf_t *path = svn_stringbuf_create(rep_path,pool);
  if (strcmp(filename,"") != 0) {
    svn_stringbuf_appendstr(path,slash);
    svn_stringbuf_appendstr(path,filepath);
  }
  
  // -- Initialisation des revisions --
  rev1.kind = rev2.kind = svn_opt_revision_number;
  rev1.value.number=revision1;
  rev2.value.number=revision2;
  
  // -- Initialisation des fichiers d'output --
  if ((rv = apr_file_open_stdout(&outfile,pool)) != APR_SUCCESS) {
    return NULL;
  }
  if ((rv = apr_file_open_stdout(&errfile,pool)) != APR_SUCCESS) {
    return NULL;
  }
  const apr_array_header_t *diff_options =  apr_array_make(pool, 1, sizeof (const char *));
  //apr_file_t *write_end;
  //apr_file_t *read_end;
  //apr_file_pipe_create(&write_end,&read_end,pool);
  int tube[2];
  pipe(tube);
  switch(fork()){
  case -1: 
    perror("fork");
    return NULL;
  case 0:
    //apr_file_close(read_end);
    //apr_file_dup2(outfile,write_end,pool);
    close(tube[0]);
    dup2(tube[1],STDOUT_FILENO);
    err = svn_client_diff4(diff_options,
			   path->data,
			   &rev1,
			   path->data,
			   &rev2,
			   NULL,
			   svn_depth_empty,
			   TRUE,// ignore_ancestry
			   TRUE,// no_diff_deleted
			   FALSE, // ignore_content_type
			   SVN_APR_LOCALE_CHARSET,
			   outfile,
			   errfile,
			   NULL,
			   ctx,
			   pool);
    if (err) {
      svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
      return NULL;
    }
    exit(0);
  }
  wait(NULL);
  //apr_file_close(write_end);
  close(tube[1]);
  int buf_size = 512;
  char buf[buf_size];
  int rc;
  const svn_stringbuf_t *tmp;
  while((rc=read(tube[0],buf,buf_size))>0){//(rv=apr_file_gets(buf,buf_size,read_end))){
      //if(APR_STATUS_IS_EOF(rv))
      //break;
    buf[rc]='\0';
    tmp = svn_stringbuf_create(buf,pool);
    svn_stringbuf_appendstr(res,tmp);
    //*(char **)apr_array_push(list_result) = buf;
  }

  svn_cstring_split_append(list_result,res->data,"\n",FALSE,pool);
  return list_result;
}


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
  else if (input[len-1] == '\n') {
    *(char **)apr_array_push(array) = " ";
  }
} 	

/* fonction simulant la commande svn cat */
apr_array_header_t *svn_support_cat(char *file_path, int revision){
  apr_pool_t *pool ;
  svn_error_t *err;
  svn_client_ctx_t *ctx; 
  svn_opt_revision_t rev;
  svn_stream_t *out;
 
  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return NULL;

  pool = svn_pool_create(NULL);
  err = svn_fs_initialize(pool);
 
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }

  // -- Initialisation du contexte --
  ctx = initialize_context(pool);
 
  // -- Initialisation de la révision --
  if (revision != -1) {
    rev.kind = svn_opt_revision_number;
    rev.value.number = revision;
  }
  else 
    rev.kind = svn_opt_revision_unspecified;
 
  // -- Initialisation du tableau et du buffer de résultats -- 
  apr_array_header_t *list_result = apr_array_make(pool, 1, sizeof (const char *));
  svn_stringbuf_t *res = svn_stringbuf_create("",pool);	
  
  // -- Initialisation du flux de sortie --
  out = svn_stream_from_stringbuf(res,pool);

  svn_client_cat2(out, 
		  file_path,
		  &rev,
		  &rev,
		  ctx,
		  pool);

  svn_cstring_split_endline_append(list_result,res->data,pool);
  return list_result;
}


apr_array_header_t *svn_support_blame(char *file_path, int revision){
  apr_pool_t *pool ;
  svn_error_t *err;
  svn_client_ctx_t *ctx; 
  svn_opt_revision_t peg_rev,start,end;
  svn_diff_file_options_t *diff_options;

  if (svn_cmdline_init ("svn_support", stderr) != EXIT_SUCCESS)
    return NULL;
  
  pool = svn_pool_create(NULL);
  err = svn_fs_initialize(pool);
  
  if (err) {
    svn_handle_error2 (err, stderr, FALSE, "svn_support: ");
    return NULL;
  }

  // -- Initialisation du contexte --
  ctx = initialize_context(pool);
 
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
    svn_handle_error2(err, stderr, FALSE, "svn_support: ");
  }
  svn_cstring_split_append(list_result,res->data,"\n",FALSE,pool);
  return list_result;
}

/*
int main(void){
  svn_support_cat("https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge/README",149);
}
*/
