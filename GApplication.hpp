/*
 * GLdb APPLICATION head file
 *
 * for application following GEncapsulate define
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
#include    "GTools.hpp"

#ifdef    __GLdb_SELF_USE

#ifndef     GLdb_APPLICATION_HPP
#define     GLdb_APPLICATION_HPP

#define     APPLICATION_FLAG_DUPLEX             (1 << 0)

#define     APPLICATION_NONE                    0
#define     APPLICATION_ECHO                    7
#define     APPLICATION_FORWARD                 8
#define     APPLICATION_SESSION                 31

typedef     class GApplication
{
public:
  UINT      ApplicationNumber;
  UINT      ApplicationFlag;
  HANDLE    handleIOCP;
  SOCKET    listenSocket;

public:
  GApplication()
  {
    handleIOCP = 0;
  };
  RESULT    OnAccept(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnAccept);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnConnect(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnConnect);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnClientRead(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnClientRead);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnClientWrite(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnClientWrite);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnServerRead(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnServerRead);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnServerWrite(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnServerWrite);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnClose(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnClose);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
  RESULT    OnPassby(PCONT, PBUFF&, UINT) {
#ifdef      __PROCESS_APPLICATION
    DF(OnPassby);DN;
#endif   // __PROCESS_APPLICATION
    return 0; 
  };
}APP, *PAPP;

typedef     class GNoneApplication : public GApplication
{
public:
  RESULT    OnAccept(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnConnect(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnClientWrite(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnServerRead(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnServerWrite(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnClose(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnPassby(
            PCONT pcont, PBUFF &pbuff, UINT size);
}NAPP, *PNAPP;

#define     MAX_PEER_GROUP                      32

typedef     struct GPeerGroup
{
  PCONT     peerCont;
  STR_S     peerKeyword;
  STR_M     peerHost;
}PEERGROUP, *PGROUP;

typedef     class GMultiApplication : public GApplication
{
private:
  UINT      PeerNumber;
  PEERGROUP ServerPeer[MAX_PEER_GROUP];
public:
  GMultiApplication() : GApplication() {
    PeerNumber = 0;
    TOZERO(ServerPeer);
  };
  RESULT    AddPeerGroup(
            PCONT pcont, PSTRING keyword, PSTRING host);
  RESULT    PreparePeer(
            PCONT &pcont, PSTRING keyword);
}MAPP, *PMAPP;

typedef     class GEchoApplication : public GApplication
{
public:
  RESULT    OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size);
}ECHO, *PECHO;

typedef     class GForwardApplication : public GApplication
{
public:
  RESULT    OnClientRead(
            PCONT pcont, PBUFF &pbuff, UINT size);
  RESULT    OnServerRead(
            PCONT pcont, PBUFF &pbuff, UINT size);
}FORWARD, *PFORWARD;

#endif   // GLdb_APPLICATION_HPP

#endif // __GLdb_SELF_USE
