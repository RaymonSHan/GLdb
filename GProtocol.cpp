/*
 * GLdb PROTOCOL implement file
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

#include    "GIOCP.hpp"
#include    "GProtocol.hpp"
#include    "GApplication.hpp"

RESULT      GNoneProtocol::PostAccept(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
__TRY__
  pbuff->nOper = op;
  // if (pcont->bHandle == 0) {
  //   FreeBuffer(pbuff);
  //   __BREAK_OK;
  // } else {
    return ProFunc(pcont->pProtocol, fPostAccept)(pcont, pbuff, size, op);
    //  }
__CATCH__
};

RESULT      GNoneProtocol::PostConnect(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
__TRY__
  pbuff->nOper = op;
  return ProFunc(pcont->pProtocol, fPostConnect)(pcont, pbuff, size, op);
__CATCH__
};

RESULT      GNoneProtocol::PostSend(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
__TRY__
  pbuff->nOper = op;
  ReflushTimeout(pcont, 0);
  return ProFunc(pcont->pProtocol, fPostSend)(pcont, pbuff, size, op, opside);
__CATCH__
};

// this is easy way for test now.
RESULT      GNoneProtocol::PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
__TRY__
  pbuff->nOper = op;
  ReflushTimeout(pcont, 0);
  return ProFunc(pcont->pProtocol, fPostReceive)(pcont, pbuff, size, op, opside);
__CATCH__
};


RESULT      GIPProtocol::BindLocalSocket(
	    PCONT &pcont, PPROT pProtocol, PSOCK sock)
{
__TRY
  int       ptype;
  int       pnumber;

  __DO (pcont == 0);
  if (pProtocol->ProtocolNumber == PROTOCOL_TCP) {
    ptype = SOCK_STREAM;
    pnumber = PROTOCOL_TCP;
  } else if (pProtocol->ProtocolNumber == PROTOCOL_UDP) {
    ptype = SOCK_DGRAM;
    pnumber = PROTOCOL_UDP;
  } else __BREAK;

  __DO1(pcont->bHandle, socket(AF_INET, ptype, pnumber));
  bind(pcont->bHandle, (sockaddr*)sock, sizeof(SOCK));
  CreateIoCompletionPort(pcont, pcont->pApplication->handleIOCP, (ULONG_PTR)pcont, 0);

__CATCH
};


RESULT      GTCPProtocol::CreateNew(
            PCONT pcont, ADDR para, UINT size)
{
  (void)size;
__TRY
  PSOCK     sock = (PSOCK)para.pVoid;

  pcont->dwFlags |= WSA_FLAG_ISLISTEN;
  __DO (BindLocalSocket(pcont, this, sock));
  ReflushTimeout(pcont, TIMEOUT_INFINITE);
  listen(pcont->bHandle, SOMAXCONN);
__CATCH
};

RESULT      GTCPProtocol::CreateRemote(
            PCONT pcont, ADDR para, UINT size)
{
__TRY__
__CATCH__
};

RESULT      GTCPProtocol::PostAccept(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  (void)size;
  (void)op;
__TRY
  PCONT     clicont;
  D(InPostAccept);
  __DO (GetContext(clicont));
  pbuff->oLapped.accSocket = clicont;
  AcceptEx((SOCKET)pcont, clicont, NULL, 0, 0, 0, NULL, &(pbuff->oLapped));
__CATCH
};

RESULT      GTCPProtocol::PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
__TRY__

__CATCH__
};

RESULT      GTCPProtocol::PostSend(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
__TRY__
  DWORD     dwflags = 0;
  pbuff->wsaBuf.len = size;
  WSASend((SOCKET)pcont, &(pbuff->wsaBuf), 1, &(pbuff->nSize), dwflags, &(pbuff->oLapped), NULL);
__CATCH__
};

RESULT      GTCPProtocol::PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
__TRY__
__CATCH__
};
