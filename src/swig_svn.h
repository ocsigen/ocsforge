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

void svn_init(void);
apr_array_header_t *svn_support_list(char *,int);
apr_array_header_t *svn_support_log(char *, int, int, int);
apr_array_header_t *svn_support_diff(char *, char *, int, int);
apr_array_header_t *svn_support_cat(char *, int);
apr_array_header_t *svn_support_blame(char *, int);
