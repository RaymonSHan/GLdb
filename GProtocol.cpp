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

RESULT      GNoneProtocol::CreateNew(
            PCONT pcont, ADDR addr, UINT size)
{
  return ProFunc(pcont, fCreateNew)(pcont, addr, size);
};

RESULT      GNoneProtocol::CreateRemote(
            PCONT pcont, ADDR addr, UINT size)
{
  return ProFunc(pcont, fCreateRemote)(pcont, addr, size);
};

RESULT      GNoneProtocol::PostAccept(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  pbuff->nOper = op;
  // if (pcont->bHandle == 0) {
  //   FreeBuffer(pbuff);
  //   __BREAK_OK;
  // } else {
  return ProFunc(pcont, fPostAccept)(pcont, pbuff, size, op);
    //  }
};

RESULT      GNoneProtocol::PostConnect(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  pbuff->nOper = op;
  return ProFunc(pcont, fPostConnect)(pcont, pbuff, size, op);
};

RESULT      GNoneProtocol::PostSend(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
  pbuff->nOper = op;
  ReflushTimeout(pcont, 0);
  return ProFunc(pcont, fPostSend)(pcont, pbuff, size, op, opside);
};

// this is easy way for test now.
RESULT      GNoneProtocol::PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
  pbuff->nOper = op;
  ReflushTimeout(pcont, 0);
  return ProFunc(pcont, fPostReceive)(pcont, pbuff, size, op, opside);
};


RESULT      GIPProtocol::BindLocalSocket(
	    PCONT &pcont, PPROT pProtocol)
{
__TRY
  int       ptype;

  __DO (pcont == 0);
  if (pProtocol->ProtocolNumber == PROTOCOL_TCP) {
    ptype = SOCK_STREAM | SOCK_NONBLOCK;
 
  } else if (pProtocol->ProtocolNumber == PROTOCOL_UDP) {
    ptype = SOCK_DGRAM | SOCK_NONBLOCK;

  } else __BREAK;

  __DO1(pcont->bHandle, socket(AF_INET, ptype, 0));
__CATCH
};


RESULT      GTCPProtocol::CreateNew(
            PCONT pcont, ADDR para, UINT size)
{
  (void)    size;
__TRY__
  PSOCK     sock = (PSOCK)para.pVoid;

  pcont->dwFlags |= WSA_FLAG_ISLISTEN;
  BindLocalSocket(pcont, this);
  bind(pcont->bHandle, (sockaddr*)sock, sizeof(SOCK));
  CreateIoCompletionPort(pcont, pcont->pApplication->handleIOCP, (ULONG_PTR)pcont, 0);
  ReflushTimeout(pcont, TIMEOUT_INFINITE);
  listen(pcont->bHandle, SOMAXCONN);
__CATCH__
};

RESULT      GTCPProtocol::CreateRemote(
            PCONT pcont, ADDR para, UINT size)
{
__TRY__
  memcpy(&(pcont->localSocket), para.pVoid, size);
__CATCH__
};

RESULT      GTCPProtocol::PostAccept(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  (void)    size;
  (void)    op;
__TRY
  PCONT     clicont;
  PWSABUF   wsabuf = &(pbuff->wsaBuf);

  __DO (GetDupContext(clicont, pcont));
  pbuff->oLapped.accSocket = clicont;
  pbuff->oLapped.doneSize = 0;
  wsabuf->len = 0;
  AcceptEx((SOCKET)pcont, clicont, wsabuf, 0, 0, 0, NULL, &(pbuff->oLapped));
__CATCH
};

RESULT      GTCPProtocol::PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  (void)    size;
  (void)    op;
__TRY__
  PWSABUF   wsabuf = &(pbuff->wsaBuf);

  pcont->dwFlags |= WSA_FLAG_ISCONNECT;
  BindLocalSocket(pcont, this);
  pbuff->oLapped.doneSize = 0;
  wsabuf->len = 0;
  CreateIoCompletionPort(pcont, pcont->pApplication->handleIOCP, (ULONG_PTR)pcont, 0);
  ConnectEx((SOCKET)pcont, &(pcont->localSocket), sizeof(SOCK), wsabuf, 0, 0, &(pbuff->oLapped));
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
  DWORD     dwflags = 0;
  pbuff->wsaBuf.len = size;
  WSARecv((SOCKET)pcont, &(pbuff->wsaBuf), 1, &(pbuff->nSize), &dwflags, &(pbuff->oLapped), NULL);
__CATCH__
};
