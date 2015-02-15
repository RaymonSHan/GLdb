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
  __DO(GlobalMemory.InitMemoryBlock(NUMBER_CONTEXT,
				    NUMBER_BUFFER_SMALL,
				    NUMBER_BUFFER_MIDDLE));
  __DO(GlobalIOCP.InitGLdbIOCP());
  __DO(InitEncapsulate());

  //  CreateApplication

  if (!GlobalShouldQuit) {
    sleep(1);
  }
  __DO(FreeEncapsulate());
  __DO(GlobalIOCP.FreeGLdbIOCP());
  __DO(GlobalMemory.FrreeMemoryBlock());
  
__CATCH
};

RESULT      GEncapsulate::InitEncapsulate(void)
{
__TRY__
  PCONT     nullcont = NULL;
  char      local_addr[] = "127.0.0.1";  
  SOCK      sock;
  ADDR      addr, nulladdr;

  RegisterProtocol(GTCP, tcpProt, PROTOCOL_TCP, 0);
  RegisterApplication(ECHO, echoApp, APPLICATION_ECHO, 0);

  bzero(&sock.saddrin, sizeof(sockaddr_in));   
  sock.saddrin.sin_family = AF_INET; 
  inet_aton(local_addr,&(sock.saddrin.sin_addr));
  sock.saddrin.sin_port=htons(8998);
  addr = &sock;
  nulladdr = ZERO;

  CreateApplication(listenCont, nullcont, (PAPP)&echoApp, 
		    (PPROT)&tcpProt, addr, sizeof(SOCK), 
		    NULL, nulladdr, 0);
__CATCH__
};

RESULT      GEncapsulate::FreeEncapsulate(void)
{
__TRY__
__CATCH__
};

#define     ACCEPTNUMBER                        5

RESULT      GEncapsulate::CreateApplication(
            PCONT          &cliCont,
	    PCONT          &serCont,
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
  PBUFF     newbuff;

  cliCont = serCont = 0;
  __DO (pApp == 0);
  if (!pApp->handleIOCP)
    globalIOCP.GetIOCPItem((ADDR &)pApp->handleIOCP);
  __DO (pApp->handleIOCP == 0);

  if (cliProt) {
    __DO (GetContext(cliCont));
    cliCont->pApplication = pApp;
    cliCont->pProtocol = cliProt;
    __DO (ProFunc(cliProt, fCreateNew)
	  (cliCont, cliPara, cliSize));
    cliCont->pPeer = cliCont;

    for(i=0; i<ACCEPTNUMBER; i++) {
      __DO (GetBufferSmall(newbuff));
      __DO (NoneProFunc(cliProt, fPostAccept)
	    (cliCont, newbuff, SIZE_SMALL_BUFFER, OP_ACCEPT));
    }
  }
  if (serProt) {
    __DO (GetContext(serCont));
    serCont->pApplication = pApp;
    serCont->pProtocol = serProt;
    __DO (ProFunc(serProt, fCreateRemote)
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