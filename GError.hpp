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

#if         ( !defined(GLdb_ERROR_HPP) || defined(GLdb_ENUM_STRINGS) )

#ifndef     GLdb_ENUM_STRINGS
#define     GLdb_ERROR_HPP
#endif   // GLdb_ENUM_STRINGS

#undef      ENUM
#undef      BEGIN_ENUM
#undef      END_ENUM

#ifndef     GLdb_ENUM_STRINGS
  #define   BEGIN_ENUM(name, base)              	        \
            typedef enum type##name {				\
              name##Base = base - 1,
  #define   ENUM(val, str)                                      \
            val
  #define   END_ENUM(name, base)				\
            }name;						\
            const char* Get##name(enum type##name index);
#else
  #define   BEGIN_ENUM(name, base)                              \
            const char* str##name[] = {
  #define   ENUM(val, str)                                      \
            str
  #define   END_ENUM(name, base)                                \
            };							\
            const char* Get##name(enum type##name index) {	\
	      return str##name[index - base]; }
#endif   // GLdb_ENUM_STRINGS

/*
 * error defined for GLdbIOCP
 */
#define     GLdb_ERROR_BASE                     0x6700000000
BEGIN_ENUM(GLdbError, GLdb_ERROR_BASE)
  ENUM(GL_MEMORY_BASE,    
            "GLdb Memory error base."),
  ENUM(GL_IOCP_BASE,    
	    "GLdb IOCP error base"),
END_ENUM(GLdbError, GLdb_ERROR_BASE)

#endif   // (!defined(DAYS_H) || defined(GENERATE_ENUM_STRINGS))

#ifndef     GLdb_ERROR_HPP_OTHER
#define     GLdb_ERROR_HPP_OTHER

/*
 * Other declare should be here
 */

#endif   // GLdb_ERROR_HPP_OTHER
