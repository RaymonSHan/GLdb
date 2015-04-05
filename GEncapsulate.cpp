/*
 * GLdb Encapsulate common netwrok server implementy file
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

#include    "GMemory.hpp"
#include    "GEncapsulate.hpp"

RESULT      GEncapsulate::Doing(void)
{
__TRY
  UINT      nowt;
  PTINFO    tinfo;
  PRINFO    rinfo;
  ADDR      tstack;

  RegisterProtocol(
            NPROT, nonePro, PROTOCOL_NONE, 0);
  RegisterProtocol(
            GTCP, tcpProt, PROTOCOL_TCP, 0);
  RegisterProtocol(
            GFILE, fileProt, PROTOCOL_FILE, 0);
  RegisterApplication(
            NAPP, noneApp, APPLICATION_NONE, 0);
  RegisterApplication(
            ECHO, echoApp, APPLICATION_ECHO, 0);
  RegisterApplication(
            FORWARD, forwardApp, APPLICATION_FORWARD, APPLICATION_FLAG_DUPLEX);
  RegisterApplication(
            FORWARD, forwardSingle, APPLICATION_FORWARD, 0);

  __DO (GlobalMemory.InitMemoryAlloc(
	    NUMBER_SIGN, NUMBER_CONTEXT, NUMBER_BUFFER_SMALL, NUMBER_BUFFER_MIDDLE));
  __DO (GlobalMemory.InitThreadMemory(1));
  __DO (GlobalIOCP.InitGLdbIOCP());
  __DO (InitEncapsulate());
  setThreadName();
  __DO (InitThreadInfo());

  while (!GlobalShouldQuit) {
    __DO (CalcThreadTime());
    GlobalTime = time(NULL);
    for (nowt = 0; nowt < GlobalThreadNumber; nowt++) {
      tstack = GlobalStackPlace[nowt];
      tinfo = GetTraceInfo(tstack);
      rinfo = GetThreadInfo(tstack);
      printf("%s:%lld:%f\n", tinfo->threadName, 
	    rinfo->threadRunTime.aLong, rinfo->threadRunPercent);
    }
    tinfo = GetTraceInfo();
    rinfo = GetThreadInfo();
    printf("%s:%lld:%f\n", tinfo->threadName, 
	    rinfo->threadRunTime.aLong, rinfo->threadRunPercent);
    //    GlobalMemory.DisplayMemoryInfo();
    sleep(1);
  }
  __DO (FreeEncapsulate());
  __DO (GlobalIOCP.FreeGLdbIOCP());
  __DO (GlobalMemory.FrreeMemoryBlock());
__CATCH_BEGIN
  __DO (FreeEncapsulate());
  __DO (GlobalIOCP.FreeGLdbIOCP());
  __DO (GlobalMemory.FrreeMemoryBlock());
  GlobalShouldQuit = 1;
__CATCH_END
};

//#define     DOING_ECHO_APPLICATION
//#define     DOING_FORWARD_APPLICATION
#define     DOING_FORWARDSINGLE_APPLICATION

RESULT      GEncapsulate::InitEncapsulate(void)
{
__TRY
  PCONT     nullcont = NULL;
  char      local_addr[] = "0.0.0.0";
  int       local_port = 8998;
  char      remote_addr[] = "127.0.0.1";
  int       remote_port = 8999;
  SOCK      sockcli, sockser;
  ADDR      addrcli, addrser;
  UCHAR     pname[] = "~/";
  static    STR_M pathname;
  
  bzero(&sockcli.saddrin, sizeof(sockaddr_in));
  sockcli.saddrin.sin_family = AF_INET; 
  inet_aton(local_addr,&(sockcli.saddrin.sin_addr));
  sockcli.saddrin.sin_port=htons(local_port);
  addrcli = &sockcli;

  bzero(&sockser.saddrin, sizeof(sockaddr_in));   
  sockser.saddrin.sin_family = AF_INET; 
  inet_aton(remote_addr,&(sockser.saddrin.sin_addr));
  sockser.saddrin.sin_port=htons(remote_port);
  addrser = &sockser;

#ifdef      DOING_ECHO_APPLICATION
  ADDR      nulladdr;
  nulladdr = ZERO;
  __DO (CreateApplication(
            echoApp.listenSocket, nullcont, (PAPP)&echoApp, 
	    (PPROT)&tcpProt, addrser, sizeof(SOCK), 
	    NULL, nulladdr, 0));
  __DO (GlobalIOCP.StartWork(
            (PEVENT)echoApp.handleIOCP, 1));
#endif   //  DOING_ECHO_APPLICATION

#ifdef      DOING_FORWARD_APPLICATION
  __DO (CreateApplication(
            forwardApp.listenSocket, nullcont, (PAPP)&forwardApp, 
	    (PPROT)&tcpProt, addrcli, sizeof(SOCK), 
	    (PPROT)&tcpProt, addrser, sizeof(SOCK)));
  __DO (GlobalIOCP.StartWork(
	    (PEVENT)forwardApp.handleIOCP, 1));
#endif   // DOING_FORWARD_APPLICATION

#ifdef      DOING_FORWARDSINGLE_APPLICATION
  pathname = pname;
  addrser = &pathname;
  __DO (CreateApplication(
            forwardSingle.listenSocket, nullcont, (PAPP)&forwardSingle, 
	    (PPROT)&tcpProt, addrcli, sizeof(SOCK), 
	    (PPROT)&fileProt, addrser, 0));
  __DO (GlobalIOCP.StartWork(
	    (PEVENT)forwardSingle.handleIOCP, 1));
#endif   // DOING_FORWARDSINGLE_APPLICATION

  __DO (GlobalIOCP.StartFile(NUMBER_MAX_FILE));                 // ThreadClone(false)
__CATCH
};

RESULT      GEncapsulate::FreeEncapsulate(void)
{
__TRY__
__CATCH__
};

#define     ACCEPTNUMBER                        1

RESULT      GEncapsulate::CreateApplication(
            PCONT           &cliCont,
	    PCONT           &serCont,
	    PAPP            pApp,
	    PPROT           cliProt,
	    ADDR            cliPara,
	    UINT            cliSize,
	    PPROT           serProt,
	    ADDR            serPara,
            UINT            serSize)
{
__TRY
  UINT      i;
  PBUFF     newbuff = 0;

  __DOe(pApp == 0,
            GL_APPLICATION_ZERO);
  cliCont = serCont = 0;
  if (!pApp->handleIOCP) {
    pApp->handleIOCP = CreateIoCompletionPort(0, 0, 0, 0);
    __DO (pApp->handleIOCP == 0);
  }

  __DO (pApp->handleIOCP == 0);
  if (cliProt) {
    __DO (GetContext(cliCont));
    cliCont->pApplication = pApp;
    cliCont->pProtocol = cliProt;
    __DO (NoneProFunc(fCreateNew)
	    (cliCont, cliPara, cliSize));
    cliCont->pPeer = cliCont;
    for(i=0; i<ACCEPTNUMBER; i++) {
      __DO (GetBufferSmall(newbuff));
      __DO (NoneProFunc(fPostAccept)
	    (cliCont, newbuff, SIZE_BUFF_S, OP_ACCEPT));
    }
  }
  if (serProt) {
    __DO (GetContext(serCont));
    serCont->pApplication = pApp;
    serCont->pProtocol = serProt;
    __DO (NoneProFunc(fCreateRemote)
	    (serCont, serPara, serSize));
    serCont->pPeer = NULL;
  }
  if (cliCont && serCont) {
    cliCont->pPeer = serCont;
  }
__CATCH_BEGIN
  if (cliCont) FreeContext(cliCont);
  if (serCont) FreeContext(serCont);
__CATCH_END
};

RESULT      GEncapsulate::StateApplication(PAPP papp, UINT state)
{
__TRY__
__CATCH__
};

#endif  //__GLdb_SELF_USE
