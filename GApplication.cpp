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

/*
 * pPeer relation
 *
 * Client Context and Server Context are peer. every context may point to NULL,
 *   self, its peer. so there are nine conditions, as following
 *
 * 1. cli->pPeer = NULL;            ser->pPeer = NULL;
 * 2. cli->pPeer = NULL;            ser->pPeer = ser;
 * 3. cli->pPeer = NULL;            ser->pPeer = cli;
 * 4. cli->pPeer = cli;             ser->pPeer = NULL;
 * 5. cli->pPeer = cli;             ser->pPeer = ser;
 * 6. cli->pPeer = cli;             ser->pPeer = cli;
 * 7. cli->pPeer = ser;             ser->pPeer = NULL;
 * 8. cli->pPeer = ser;             ser->pPeer = ser;
 * 9. cli->pPeer = ser;             ser->pPeer = cli;
 */

RESULT      GNoneApplication::OnAccept(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;
__TRY
  PCONT     clicont, sercont;
  PBUFF     newbuff;
  HANDLE    iocphandle;
  DEF_MARK(AfterGetContext);
  DEF_MARK(AfterPostReceive);

  __DOe(pcont == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pbuff == 0,
            GL_TCP_INPUT_ZERO);
  __DOe(pcont->pPeer == NULL,
            GL_TCP_INPUT_ZERO);
  __DO (GetBufferSmall(newbuff));
            /* MARK */  __MARK(AfterGetBuffer);
  __DO (NoneProFunc(fPostAccept)
	    (pcont, newbuff, SIZE_BUFF_S, OP_ACCEPT));
            /* MARK */  __MARK(AfterPostAccept);
  pbuff->nOper = OP_CLIENT_READ;

  clicont = (PCONT)pbuff->oLapped.accSocket;
  __DOe(clicont == 0,
            GL_TCP_INPUT_ZERO);
  pbuff->oLapped.accSocket = 0;

  iocphandle = CreateIoCompletionPort(
            clicont, pcont->pApplication->handleIOCP, (ULONG_PTR)clicont, 0);
  __DO (iocphandle == 0);

/*
 * pcont->pPeer->pPeer != NULL means pcont == pcont->pPeer in normal
 *   or duplicate all info from CONT, which created by CreateRemote()
 */
  if (pcont->pPeer->pPeer != NULL) {
    clicont->pPeer = clicont;
    __DO (NoneProFunc(fPostReceive)
	    (clicont, pbuff, SIZE_BUFF_S, OP_CLIENT_READ, OPSIDE_CLIENT));
  } else {
    __DO (GetDupContext(sercont, pcont->pPeer, true));
            /* MARK */  __MARK_(AfterGetContext);
    clicont->pPeer = sercont;
    sercont->pPeer = clicont;
    __DO (NoneProFunc(fPostConnect)
	    (sercont, pbuff, 0, OP_CONNECT));
            /* MARK */  __MARK_(AfterPostReceive);
  }
__CATCH_BEGIN
  __BETWEEN(AfterGetBuffer, AfterPostAccept) FreeBuffer(newbuff);
  __BETWEEN(AfterGetContext, AfterPostReceive) FreeContext(sercont);
__CATCH_END
};

RESULT      GNoneApplication::OnConnect(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;
__TRY
  PBUFF     newbuff;
  DEF_MARK(AfterGetBuffer);
  DEF_MARK(AfterPostReceive);

  __DO (NoneProFunc(fPostReceive)
	    (pcont->pPeer, pbuff, SIZE_BUFF_S, OP_CLIENT_READ, OPSIDE_CLIENT));

  if (IS_DUPLEX(pcont)) {
    __DO (GetBufferSmall(newbuff));
            /* MARK */  __MARK_(AfterGetBuffer);
    __DO (NoneProFunc(fPostReceive)
	    (pcont, newbuff, SIZE_BUFF_S, OP_SERVER_READ, OPSIDE_SERVER));
            /* MARK */  __MARK_(AfterPostReceive);
  }
__CATCH_BEGIN
  __BETWEEN(AfterGetBuffer, AfterPostReceive) FreeBuffer(newbuff);
__CATCH_END
};

RESULT      GNoneApplication::OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  __DO (AppFunc(pcont, fOnClientRead)
	    (pcont, pbuff, size));
__CATCH
};

RESULT      GNoneApplication::OnClientWrite(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;
__TRY
  __DO (NoneProFunc(fPostReceive)
	    (pcont->pPeer, pbuff, SIZE_BUFF_S, OP_SERVER_READ, OPSIDE_SERVER));
__CATCH
};

RESULT      GNoneApplication::OnServerRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  __DO (AppFunc(pcont, fOnServerRead)
	    (pcont, pbuff, size));
__CATCH
};

RESULT      GNoneApplication::OnServerWrite(
            PCONT pcont, PBUFF &pbuff, UINT size)
{ 
  (void)    size;
__TRY
  __DO (NoneProFunc(fPostReceive)
	    (pcont->pPeer, pbuff, SIZE_BUFF_S, OP_CLIENT_READ, OPSIDE_CLIENT));
__CATCH
};

RESULT      GNoneApplication::OnClose(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
  (void)    size;
  (void)    pbuff;
__TRY
  static    LOCK inClose = NOT_IN_PROCESS;
  static    PBUFF isNULL = NULL;
  PCONT     peer;

  if (pbuff) {
    FreeBuffer(pbuff);
  }
  if (!pcont) __BREAK_OK;
  __LOCK(inClose);
  peer = pcont->pPeer;
  if (peer && peer->pPeer == pcont) {
    peer->pPeer = NULL;   // change this line, old is peer->pPeer = peer;
    pcont->pPeer = NULL;
    __FREE(inClose);
    D(closepeer);Dp(pcont);Dn;
    OnClose(peer, isNULL, 0);
  } else {
    __FREE(inClose);
  }
  FreeProtocolContext(pcont);
__CATCH
};

RESULT      GNoneApplication::OnPassby(
            PCONT pcont, PBUFF &pbuff, UINT size)
{ return 0; };


RESULT      GMultiApplication::AddPeerGroup(
	    PCONT pcont, PSTRING keyword, PSTRING host)
{
__TRY
  PCONT     sercont = ZERO;
  UINT      peernumber;
  PGROUP    nowpeer;
  ADDR      addr;

  __DO (PeerNumber == MAX_PEER_GROUP);
  peernumber = LockInc(PeerNumber);
  nowpeer = &ServerPeer[peernumber];
  nowpeer->peerKeyword = *keyword;
  nowpeer->peerHost = *host;
  addr = host->strStart;
  __DO (GetDupContext(sercont, pcont));
  __DO (ProFunc(sercont, fCreateRemote)(sercont, addr, host->strLen()));
  nowpeer->peerCont = sercont;

__CATCH_BEGIN
  if (sercont) FreeContext(sercont);
__CATCH_END
};

RESULT      GMultiApplication::PreparePeer(
            PCONT &pcont, PSTRING keyword)
{
__TRY
  UINT      i;
  for (i=0; i<PeerNumber; i++) {
    if (ServerPeer[i].peerKeyword == *keyword) {
      __DO(GetDupContext(pcont, ServerPeer[i].peerCont));
      __BREAK_OK;
    }
  }
  pcont = 0;
  __BREAK;
__CATCH
};


RESULT      GEchoApplication::OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  __DO (NoneProFunc(fPostSend)
            (pcont, pbuff, size, OP_SERVER_WRITE, OPSIDE_CLIENT));
__CATCH
};

RESULT      GForwardApplication::OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  __DO (NoneProFunc(fPostSend)
            (pcont->pPeer, pbuff, size, OP_SERVER_WRITE, OPSIDE_SERVER));
__CATCH
};

RESULT      GForwardApplication::OnServerRead(
            PCONT pcont, PBUFF &pbuff, UINT size)
{
__TRY
  __DO (NoneProFunc(fPostSend)
            (pcont->pPeer, pbuff, size, OP_CLIENT_WRITE, OPSIDE_CLIENT));
__CATCH
};
