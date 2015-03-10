/*
 * GLdb error message implementy file
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

#undef      GLdb_ENUM_NAMES
#undef      GLdb_ENUM_STRINGS
#include    "GError.hpp"

#define     GLdb_ENUM_NAMES
#include    "GError.hpp"
#undef      GLdb_ENUM_NAMES

#define     GLdb_ENUM_STRINGS
#include    "GError.hpp"             
#undef      GLdb_ENUM_STRINGS

#include    "GCommon.hpp"

GERROR      GetGLdbError(void)
{
  PTINFO    ptinfo;
  getTraceInfo(ptinfo);
  return (ptinfo->GLError);
};

const char* GetGLdbErrorVal(GERROR err)
{
  return GetValGLdbError(err);
};

const char* GetGLdbErrorMessage(GERROR err)
{
  return GetStrGLdbError(err);
};

void        SetGLdbError(GERROR err)
{
  PTINFO    ptinfo;
  getTraceInfo(ptinfo);
  ptinfo->GLError = err;
};

void        DisplayGLdbError(void)
{
  GERROR    err = GetGLdbError();
  if (err >= GLdb_ERROR_BASE) {
    printf("GLdb error:0x67-%04ld:%s:\"%s\"\n", err - GLdb_ERROR_BASE, 
	    GetGLdbErrorVal(err), GetGLdbErrorMessage(err));
  } else if (err) {
    printf("System error: %ld\n", err);
  }
};
