/*
 * GLdb tools function header file
 *
 * GLdb is a Multi-thread customed Key-Value No-SQL memory database.
 * GLdb atomic insert voucher & update balance, provide interface for ERP.
 * GLdb have its own Async IO system, support Windows & Linux by IOCP & epoll.
 * GLdb request large memory, so only support 64bit system.
 *
 * Copyright (c) 2015 Raymon SHan <quickhorse77 at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modifica-
 * tion, are permitted provided that the following conditions are met:
 *
 *   1.  Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *   2.  Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MER-
 * CHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTH-
 * ERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef     GLdb_TOOLS_HPP
#define     GLdb_TOOLS_HPP

#include    "GCommon.hpp"

#define     TOZERO(desc)					\
  bzero(desc, sizeof(desc))


#define     ESC_SAVE_CURSOR                     "\e[s"
#define     ESC_RESTORE_CURSOR                  "\e[u"
#define     ESC_SET_CURSOR                      "\e[1;1f"
#define     ESC_RESTORE_COLOR                   "\e[0;37m"
#define     ESC_SET_COLOR                       "\e[0;36m"

#define     ESC_PRINT(mess)					\
  printf("%s%s%s%s%s%s",					\
	   ESC_SAVE_CURSOR, ESC_SET_CURSOR, ESC_SET_COLOR,	\
	   mess, ESC_RESTORE_COLOR, ESC_RESTORE_CURSOR)

#define     ESC_WRITE(str, mess)				\
  snprintf(str, sizeof(str), "%s%s%s%s%s%s",			\
	   ESC_SAVE_CURSOR, ESC_SET_CURSOR, ESC_SET_COLOR,	\
	   mess, ESC_RESTORE_COLOR, ESC_RESTORE_CURSOR)

#endif   // GLdb_TOOLS_HPP
