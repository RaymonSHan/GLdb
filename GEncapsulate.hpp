/*
 * GLdb Encapsulate common network server head file
 *
 * GEncapsulate create network server framework via GLdbIOCP.
 * Application should implement interface defined by GEncapsulate
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

#include    "GCommon.hpp"

#ifdef    __GLdb_SELF_USE

#ifndef     GLdb_ENCAPSULATE_HPP
#define     GLdb_ENCAPSULATE_HPP

#include    "GMemory.hpp"
#include    "GIOCP.hpp"

#define     GlobalMemory                        GEncapsulate::globalMemory
#define     GlobalIOCP                          GEncapsulate::globalIOCP

#define     FPHandle                            GEncapsulate::fPHandle
#define     FAHandle                            GEncapsulate::fAHandle


#define     NUMBER_CONTEXT                      50
#define     NUMBER_BUFFER_SMALL                 100
#define     NUMBER_BUFFER_MIDDLE                20

#define	    fOnAccept                           0
#define	    fOnConnect                          1
#define     fOnClientRead                       2
#define     fOnClientWrite                      3
#define     fOnServerRead                       4
#define     fOnServerWrite                      5
#define     fOnClose                            6
#define     fOnPassby                           7

#define     OP_BASE                             0x100
#define     OP_ACCEPT                          (OP_BASE+fOnAccept)
#define     OP_CONNECT                         (OP_BASE+fOnConnect)
#define     OP_CLIENT_READ                     (OP_BASE+fOnClientRead)
#define     OP_CLIENT_WRITE                    (OP_BASE+fOnClientWrite)
#define     OP_SERVER_READ                     (OP_BASE+fOnServerRead)
#define     OP_SERVER_WRITE                    (OP_BASE+fOnServerWrite)
#define     OP_CLOSE                           (OP_BASE+fOnClose)
#define     OP_PASSBY                          (OP_BASE+fOnPassby)
#define     OP_SHUTDOWN                         0x400

#define     OPSIDE_CLIENT                      (OP_BASE+OP_CLIENT_WRITE)
#define     OPSIDE_SERVER                      (OP_BASE+OP_SERVER_WRITE)
#define     OPSIDE_PEER                        (OPSIDE_SERVER+1)
#define     OPSIDE_PEER_CLIENT                 (OPSIDE_SERVER+2)
#define     OPSIDE_PEER_SERVER                 (OPSIDE_SERVER+3)
	

typedef	    RESULT (GProtocol::*PNew)   (PCONT, ADDR, UINT);
typedef     RESULT (GProtocol::*PHandle)(PCONT, PBUFF&, UINT, UINT);
typedef     RESULT (GProtocol::*PAction)(PCONT, PBUFF&, UINT, UINT, UINT);
typedef     RESULT (GApplication::*AHandle)(PCONT, PBUFF&, UINT);

#define     NoneProFunc(p, f)					\
  (p->* (&FPHandle[0])->f)
#define     NoneAppFunc(p, f)					\
  (p->* (FAHandle[0].fFunction[f]))
#define     ProFunc(p, f)					\
  (p->* (&FPHandle[p->ProtocolNumber])->f)
#define     AppFunc(p, f)					\
  (p->* (FAHandle[p->ApplicationNumber].fFunction[f]))

#define     MAX_PROTOCOL                        32
#define     MAX_APPLICATION                     32

typedef     struct ProtocolHandles
{
  PNew      fCreateNew;
  PNew      fCreateRemote;
  PHandle   fPostAccept;
  PHandle   fPostConnect;
  PAction   fPostSend;
  PAction   fPostReceive;
  PHandle   fPostClose;
}PHANDLE;

typedef     struct ApplicationHandles
{
  AHandle   fFunction[16];
}AHANDLE;

#define     RegisterProtocl(cname, protocol, num, flag)			\
{									\
  protocol->ProtocolNumber = num;					\
  protocol->PrococolFlag = flag;					\
  ENCAP::fPHandle[num].fCreateNew =					\
    (PNew)&cname::CreateNew;						\
  ENCAP::fPHandle[num].fCreateRemote =					\
    (PNew)&cname::CreateRemote;						\
  ENCAP::fPHandle[num].fPostAccept =					\
    (PHandle)&cname::PostAccept;					\
  ENCAP::fPHandle[num].fPostConnect =					\
    (PHandle)&cname::PostConnect;					\
  ENCAP::fPHandle[num].fPostSend =					\
    (PAction)&cname::PostSend;						\
  ENCAP::fPHandle[num].fPostReceive =					\
    (PAction)&cname::PostReceive;					\
  ENCAP::fPHandle[num].fPostClose =					\
    (PHandle)&cname::PostClose;						\
}

#define     RegisterApplication(cname, app, num, flag)			\
{									\
  app->ApplicationNumber = num;						\
  app->ApplicationFlag = flag;						\
  ENCAP::fAHandle[num].fFunction[fOnAccept] =				\
    (AHandle)&cname::OnAccept;						\
  ENCAP::fAHandle[num].fFunction[fOnConnect] =				\
    (AHandle)&cname::OnConnect;						\
  ENCAP::fAHandle[num].fFunction[fOnClientRead] =			\
    (AHandle)&cname::OnClientRead;					\
  ENCAP::fAHandle[num].fFunction[fOnClientWrite] =			\
    (AHandle)&cname::fOnClientWrite;					\
  ENCAP::fAHandle[num].fFunction[fOnServerRead] =			\
    (AHandle)&cname::OnServerRead;					\
  ENCAP::fAHandle[num].fFunction[fOnServerWrite] =			\
    (AHandle)&cname::OnServerWrite;					\
  ENCAP::fAHandle[num].fFunction[fOnClose] =				\
    (AHandle)&cname::OnClose;						\
  ENCAP::fAHandle[num].fFunction[fOnPassby] =				\
    (AHandle)&cname::OnPassby;						\
}

typedef     class GEncapsulate
{
public:
  static    MEMORY globalMemory;
  static    IOCP globalIOCP;

  static    PHANDLE fPHandle[MAX_PROTOCOL];
  static    AHANDLE fAHandle[MAX_APPLICATION];
public:
  //  HANDLE    handleIOCP;

public:
  static    RESULT InitEncapsulate(void);
  static    RESULT FreeEncapsulate(void);
  static    RESULT Doing(void);

public:
  RESULT    CreateApplication(
            PCONT          &cliCont,
	    PCONT          &serCont,
	    PAPP            pApp,
	    PPROT           cliProt,
	    ADDR            cliPara,
	    UINT            cliSize,
	    PPROT           serProt,
	    ADDR            serPara,
            UINT            serSize);
  RESULT    StateApplication(PAPP papp, UINT state);
}ENCAP;

#endif   // GLdb_ENCAPSULATE_HPP

#endif  //__GLdb_SELF_USE
