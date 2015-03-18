/*
 * GLdb Session head file
 *
 * If every session have its own thread. session equal thread.
 * but there are many session and few thread. session have it context, while thread 
 * only occupt cpu resource to running. 
 *
 * just as TLS, there are Session Local Storage for save session imformations
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

#ifndef     GLdb_SESSION_HPP
#define     GLdb_SESSION_HPP

#include    "GCommon.hpp"

typedef     class GSession
{
private:
  INT       offset;
  CHAR      buff[10000];
public:
  void Init()
  {
    offset = 0;
  };
  PCHAR Add(INT off) 
  {
    INT oldoffset = LockAdd(offset, off);
    if (oldoffset + off > 10000) return 0;
    return &buff[oldoffset];
    
  };

}GSession, *PGSession;

typedef     class testSession
{
public:
  INT s_a; 
}testSession, *PtestSession;

INT test(GSession *psess)
{
__TRY
  PtestSession testSession =(PtestSession) psess->Add(sizeof(testSession));
  if (testSession == 0) __BREAK;
  PINT      a;
  a = &(testSession->s_a);

  psess->Add(-1*sizeof(testSession));
  return (*a);
__CATCH
}


#endif   // GLdb_SESSION_HPP
