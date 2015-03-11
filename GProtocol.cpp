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
#include    "GEncapsulate.hpp"
#include    "GProtocol.hpp"

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
  return ProFunc(pcont, fPostAccept)(pcont, pbuff, size, op);
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

  __DOe(pcont == 0,
            GL_IP_BINDZEROSOCKET);
  if (pProtocol->ProtocolNumber == PROTOCOL_TCP) {
    ptype = SOCK_STREAM | SOCK_NONBLOCK;
  } else if (pProtocol->ProtocolNumber == PROTOCOL_UDP) {
    ptype = SOCK_DGRAM | SOCK_NONBLOCK;
  } else {
    ERROR(GL_TCP_INPUT_ZERO);
    __BREAK;
  }
  __DO1(pcont->bHandle, 
            socket(AF_INET, ptype, 0));
__CATCH
};

RESULT      GTCPProtocol::CreateNew(
            PCONT pcont, ADDR para, UINT size)
{
  (void)    size;
__TRY
  PSOCK     sock = (PSOCK)para.pVoid;
  HANDLE    iocphandle;
  int       result;

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  pcont->dwFlags |= WSA_FLAG_ISLISTEN;
  __DO (BindLocalSocket(pcont, this));
  __DO1(result, 
	    bind(pcont->bHandle, (sockaddr*)sock, sizeof(SOCK)));
  iocphandle = CreateIoCompletionPort(
	    pcont, pcont->pApplication->handleIOCP, (ULONG_PTR)pcont, 0);
  __DO (iocphandle == 0);
  ReflushTimeout(pcont, TIMEOUT_INFINITE);
  __DO1(result,
	    listen(pcont->bHandle, SOMAXCONN));
  __INFO(MESSAGE_INFO, "Add TCP Listening %s:%d", 
	    inet_ntoa(sock->saddrin.sin_addr), 
	    ntohs(sock->saddrin.sin_port));
__CATCH
};

RESULT      GTCPProtocol::CreateRemote(
            PCONT pcont, ADDR para, UINT size)
{
__TRY
  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(para == ZERO,
            GL_TCP_INPUT_ZERO);
  memcpy(&(pcont->localSocket), para.pVoid, size);
__CATCH
};

RESULT      GTCPProtocol::PostAccept(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  (void)    size;
  (void)    op;
__TRY
  PCONT     clicont;
  PWSABUF   wsabuf;
  BOOL      result;

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pcont->bHandle == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pbuff == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(&pbuff->wsaBuf == NULL,
            GL_TCP_INPUT_ZERO);
  wsabuf = &(pbuff->wsaBuf);
  __DO (GetDupContext(clicont, pcont));
            /* MARK */  __MARK(AfterGetContext);

  pbuff->oLapped.accSocket = clicont;
  pbuff->oLapped.doneSize = 0;
  wsabuf->len = 0;
  result = AcceptEx(
	    (SOCKET)pcont, clicont, wsabuf, 0, 0, 0, NULL, &(pbuff->oLapped));
  __DO (result == 0);
__CATCH_BEGIN
  __AFTER(AfterGetContext) FreeContext(clicont);
__CATCH_END
};

RESULT      GTCPProtocol::PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op)
{
  (void)    size;
  (void)    op;
__TRY
  PWSABUF   wsabuf;
  HANDLE    iocphandle;
  BOOL      result;

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pbuff == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(&pbuff->wsaBuf == NULL,
            GL_TCP_INPUT_ZERO);
  wsabuf = &(pbuff->wsaBuf);
  pcont->dwFlags |= WSA_FLAG_ISCONNECT;
  __DO (BindLocalSocket(pcont, this));
  pbuff->oLapped.doneSize = 0;
  wsabuf->len = 0;
  result = ConnectEx(
	    (SOCKET)pcont, &(pcont->localSocket), sizeof(SOCK), 
	    wsabuf, 0, 0, &(pbuff->oLapped));
  __DO (result == 0);
  iocphandle = CreateIoCompletionPort(
	    pcont, pcont->pApplication->handleIOCP, (ULONG_PTR)pcont, 0);
  __DO (iocphandle == 0);
__CATCH
};

RESULT      GTCPProtocol::PostSend(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
  (void)    op;
  (void)    opside;
__TRY
  DWORD     dwflags = 0;
  int       result;

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pbuff == 0,
            GL_TCP_INPUT_ZERO);
  pbuff->wsaBuf.len = size;
  result = WSASend(
            (SOCKET)pcont, &(pbuff->wsaBuf), 1, &(pbuff->nSize), 
	    dwflags, &(pbuff->oLapped), NULL);
  if (!result && WSAGetLastError() != WSA_IO_PENDING) {
    __BREAK;
  }
__CATCH
};

RESULT      GTCPProtocol::PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside)
{
  (void)    op;
  (void)    opside;
__TRY
  DWORD     dwflags = 0;
  int       result;

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pbuff == 0,
            GL_TCP_INPUT_ZERO);
  pbuff->wsaBuf.len = size;
  result = WSARecv(
            (SOCKET)pcont, &(pbuff->wsaBuf), 1, &(pbuff->nSize), 
	    &dwflags, &(pbuff->oLapped), NULL);
  if (!result && WSAGetLastError() != WSA_IO_PENDING) {
    __BREAK;
  }
__CATCH
};
