/*
 * GLdb APPLICATION implement file
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

#include    "GMemory.hpp"
#include    "GApplication.hpp"
#include    "GEncapsulate.hpp"

RESULT      GNoneApplication::OnAccept(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;

__TRY
  PCONT     newcont;
  PBUFF     newbuff;

  __DO (GetBufferSmall(newbuff));
  __DO (NoneProFunc(pcont->pProtocol, fPostAccept)
	(pcont, newbuff, SIZE_BUFF_S, OP_ACCEPT));
  pbuff->nOper = OP_CLIENT_READ;
  // to make sure, maybe changed when accept
  newcont = (PCONT)pbuff->oLapped.accSocket;
  pbuff->oLapped.accSocket = 0;
  CreateIoCompletionPort(
        newcont, pcont->pApplication->handleIOCP, (ULONG_PTR)newcont, 0);
  __DO (NoneProFunc(newcont->pProtocol, fPostReceive)
	(newcont, pbuff, SIZE_BUFF_S, OP_CLIENT_READ, OPSIDE_CLIENT));
__CATCH
};

RESULT      GNoneApplication::OnConnect(
            PCONT pcont, PBUFF &pbuff, UINT size)
{ return 0; };

RESULT      GNoneApplication::OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  D(NONEOnClientRead);
  __DO (AppFunc(pcont->pApplication, fOnClientRead)
	(pcont, pbuff, size));
__CATCH
};

RESULT      GNoneApplication::OnClientWrite(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;
__TRY
  D(NONEOnClientWrite);
  __DO (NoneProFunc(pcont->pProtocol, fPostReceive)
	(pcont, pbuff, SIZE_BUFF_S, OP_CLIENT_READ, OPSIDE_CLIENT));
__CATCH
};

RESULT      GNoneApplication::OnServerRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
  { return 0; };
RESULT      GNoneApplication::OnServerWrite(
            PCONT pcont, PBUFF &pbuff, UINT size)
  { return 0; };
RESULT      GNoneApplication::OnClose(
            PCONT pcont, PBUFF &pbuff, UINT size)
  { return 0; };
RESULT      GNoneApplication::OnPassby(
            PCONT pcont, PBUFF &pbuff, UINT size)
  { return 0; };



RESULT      GEchoApplication::OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  D(EchoOnClientRead);
  *((PUINT)(pbuff->wsaBuf.buf)) = 0x4041424360616263;
  NoneProFunc(pcont->pProtocol, fPostSend)(pcont, pbuff, 8, OP_CLIENT_WRITE, OPSIDE_CLIENT);
__CATCH
};
