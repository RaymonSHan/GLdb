/*
 * GLdb PROTOCOL head file
 *
 * for common interface for TCP, TCPPool, FILE, maybe UDP
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
#include    "GMemory.hpp"

#ifdef    __GLdb_SELF_USE

#ifndef     GLdb_PROTOCOL_HPP
#define     GLdb_PROTOCOL_HPP

#define     PROTOCOL_NONE                       0
#define     PROTOCOL_TCP                        6
#define     PROTOCOL_UDP                        17
#define     PROTOCOL_FILE                       21

inline      BOOL CloseSocket(SOCKET sock) 
{
  return close(sock->bHandle); 
};
inline      BOOL CloseFileHandle(SOCKET sock) 
{
  return close(sock->bHandle); 
};
inline      BOOL CloseOtherHandle(SOCKET sock) 
{
  return close(sock->bHandle); 
};
inline      BOOL CloseNullHandle(SOCKET sock) 
{
  (void)    sock;
  return 0; 
};

RESULT      FreeProtocolContext(PCONT pcont, UINT flag = 0);

typedef     class GProtocol
{
public:
  long      ProtocolNumber;
  long      ProtocolFlag;
  BOOL      (*MyCloseHandle)(SOCKET sock);	
public:
  GProtocol()
  {
    ProtocolNumber = 0;
    ProtocolFlag = 0;
    MyCloseHandle = CloseOtherHandle;
  };
  RESULT    CreateNew(PCONT, ADDR, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(CreateNew);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    CreateRemote(PCONT, ADDR, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(CreateRemote);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    PostAccept(PCONT, PBUFF&, UINT, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(PostAccept);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    PostConnect(PCONT, PBUFF&, UINT, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(PostConnect);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    PostSend(PCONT, PBUFF&, UINT, UINT, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(PostSend);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    PostReceive(PCONT, PBUFF&, UINT, UINT, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(PostReceive);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };
  RESULT    PostClose(PCONT, PBUFF&, UINT, UINT) {
#ifdef      __PROCESS_PROTOCOL
    DF(PostClose);DN;
#endif   // __PROCESS_PROTOCOL
    return 0; 
  };

  //  UINT virtual GetContextLength(PCONT pcont, PBUFF pbuff);
}PROT, *PPROT;

typedef     class GNoneProtocol : public GProtocol
{
public:
  GNoneProtocol() : GProtocol() {};
  RESULT    CreateNew(
            PCONT pcont, ADDR addr, UINT size);
  RESULT    CreateRemote(
            PCONT pcont, ADDR addr, UINT size);
  RESULT    PostAccept(
	    PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostSend(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
  RESULT    PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
}NPROT, *PNPROT;

typedef     class GIPProtocol : public GProtocol
{
protected:
  RESULT    BindLocalSocket(PCONT &pcont, PPROT pProtocol);
}IPPROT;

typedef     class GTCPProtocol : public GIPProtocol
{
public:
  GTCPProtocol() : GIPProtocol()
  {
    MyCloseHandle = CloseSocket;
  };
  RESULT    CreateNew(
            PCONT pcont, ADDR para, UINT size);
  RESULT    CreateRemote(
            PCONT pcont, ADDR para, UINT size);
  RESULT    PostAccept(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostSend(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
  RESULT    PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
}GTCP, *PGTCP;

typedef     class GFileProtocol : public GProtocol
{
public:
  GFileProtocol() : GProtocol()
  {
    MyCloseHandle = CloseFileHandle;
  };
  RESULT    CreateNew(
            PCONT pcont, ADDR para, UINT size);
  RESULT    CreateRemote(
            PCONT pcont, ADDR para, UINT size);
  RESULT    PostAccept(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostConnect(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op);
  RESULT    PostSend(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
  RESULT    PostReceive(
            PCONT pcont, PBUFF &pbuff, UINT size, UINT op, UINT opside);
}GFILE, *PGFILEw;

#endif   // GLdb_PROTOCOL_HPP

#endif // __GLdb_SELF_USE
