/*
 * GLdb common implement file
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

#include    "GLdbCommon.hpp"
#include    "GLdbMemory.hpp"
#include    "GLdbIOCP.hpp"

// Following in RThreadResource
// for every RThreadResource to register, record the now offset in TLS
volatile    UINT GlobalResourceOffset         = PAD_THREAD_STACK + SIZE_TRACE_INFO;
// current server time
volatile    UINT GlobalTime                   = time(NULL);

// Following in RThread
// total number of thread inherit from RThread. 
volatile    UINT GlobalThreadNumber           = 0;
// for quit sign
volatile    UINT GlobalShouldQuit             = 0;
// sequence thread initialized order
EVENT       ThreadStartEvent;

INT StrCmp(STRING &one, STRING &two)
{
  INT onelen, twolen, shortlen, shortlen8, i;
  ADDR oneaddr, twoaddr;

  onelen = one.strEnd - one.strStart;
  twolen = two.strEnd - two.strStart;
  shortlen = onelen < twolen ? onelen : twolen;
  shortlen8 = shortlen & (-1 * sizeof(INT));

  oneaddr = one.strStart;
  twoaddr = two.strStart;
  for (i = 0; i < shortlen8; i += sizeof(INT), oneaddr += 8, twoaddr += 8)
    if (*(oneaddr.pLong) != *(twoaddr.pLong)) break;
  if ((i == shortlen) && (i == shortlen8)) return (onelen - twolen);

  for (; i < shortlen; i++, oneaddr += 1, twoaddr += 1)
    if (*oneaddr.pChar != *twoaddr.pChar)
      return (*oneaddr.pChar - *twoaddr.pChar);

  return (onelen - twolen);
};

void __MESSAGE(INT level, const char * _Format, ...) 
{
  va_list ap;
  threadTraceInfo *info;

  if (!level) return;
  if (_Format) {
    va_start(ap, _Format);
    vprintf(_Format, ap);
    va_end(ap);
    printf("\n");
    displayTraceInfo(info);
  }
};

