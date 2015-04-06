/*
 * GLdb error message header file
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
 *
 * I just following 'Converting C++ enums to strings' by Marcos F. Cardoso,
 *   in http://www.codeproject.com/Articles/10500/Converting-C-enums-to-strings
 */

#if         (!defined(GLdb_ERROR_HPP) ||			\
	      defined(GLdb_ENUM_NAMES) ||			\
	      defined(GLdb_ENUM_STRINGS))

#undef      ENUM
#undef      BEGIN_ENUM
#undef      END_ENUM

#if         (!defined(GLdb_ENUM_NAMES) &&	                \
            !defined(GLdb_ENUM_STRINGS))
#define     GLdb_ERROR_HPP

#define     GLdb_ERROR_BASE                     0x6700000000

#define     BEGIN_ENUM(name, base)              	        \
            typedef enum type##name {				\
              name##Base = base - 1,
#define     ENUM(val, str)                                      \
            val
#define     END_ENUM(name, base, def)				\
            }def;						\
            const char* GetVal##name(enum type##name index);	\
            const char* GetStr##name(enum type##name index);
#endif   // (!defined(GLdb_ENUM_NAMES) &&
         //  !defined(GLdb_ENUM_STRINGS))


#ifdef      GLdb_ENUM_NAMES
#define     BEGIN_ENUM(name, base)                              \
            const char* val##name[] = {
#define     ENUM(val, str)                                      \
            #val
#define     END_ENUM(name, base, def)				\
            };							\
            const char* GetVal##name(enum type##name index) {	\
	      return val##name[index - base]; }
#endif   // GLdb_ENUM_NAMES


#ifdef      GLdb_ENUM_STRINGS
#define     BEGIN_ENUM(name, base)                              \
            const char* str##name[] = {
#define     ENUM(val, str)                                      \
            str
#define     END_ENUM(name, base, def)				\
            };							\
            const char* GetStr##name(enum type##name index) {	\
	      return str##name[index - base]; }
#endif   // GLdb_ENUM_STRINGS

/*
 * error defined for GLdbIOCP
 */

BEGIN_ENUM(GLdbError, GLdb_ERROR_BASE)
  ENUM(GL_MEMORY_BASE,      "GLdb Memory error base."),

  ENUM(GL_MEMORY_MMAPFAIL,  "System mmap() fail"),
  ENUM(GL_MEMORY_NOBORDER,  "GetMemory not in SIZE_THREAD_STACK borden"),

  ENUM(GL_STACK_FULL,       "RArrayStack is full, no room for +="),
  ENUM(GL_STACK_EMPTY,      "RArrayStack is empty, no item for -="),
  ENUM(GL_QUERY_FULL,       "RArrayQuery is full, no room for +="),
  ENUM(GL_QUERY_EMPTY,      "RArrayQuery is empty, no item for -="),

  ENUM(GL_BLOCK_FREETWICE,  "CMemoryBlock free same item twice"),
  ENUM(GL_LIST_NOTYPE,      "CListItem::allocType is NULL when free"),

  ENUM(GL_IOCP_INPUT_ZERO,  "IOCP input parameter is invalid"),
  ENUM(GL_IOCP_INPUT_NOSUP, "IOCP input parameter is not support yet"),

  ENUM(GL_IP_BINDZEROSOCKET,"IP BindLocalSocket, input is 0"),
  ENUM(GL_TCP_INPUT_ZERO,   "PROTOCOL_TCP input parameter is invalid"),
  ENUM(GL_FILE_INPUT_ZERO,  "PROTOCOL_FILE input parameter is invalid"),
  
  ENUM(GL_APPLICATION_ZERO, "Application input parameter is invalid"),

  ENUM(GL_CLONE_NUMBER_OVER,"Thread Number reach MAX"),
  ENUM(GL_CLONE_INIT_ERROR, "ThreadClone error"),

END_ENUM(GLdbError, GLdb_ERROR_BASE, GERROR)

#endif   // (!defined(GLdb_ERROR_HPP) ||
         //   defined(GLdb_ENUM_NAMES) ||
         //   defined(GLdb_ENUM_STRINGS))

#ifndef     GLdb_ERROR_HPP_OTHER
#define     GLdb_ERROR_HPP_OTHER

/*
 * Other declare should be here
 */
GERROR      GetGLdbError(void);
const char* GetGLdbErrorVal(GERROR err);
const char* GetGLdbErrorMessage(GERROR err);
void        SetGLdbError(GERROR err);
void        DisplayGLdbError(void);

#define     ERROR                               SetGLdbError

#endif   // GLdb_ERROR_HPP_OTHER
