/*
 * GLdb thread & IOCP implementy file
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

void SIGSEGV_Handle(int sig, siginfo_t *info, void *secret)
{
  ADDR  stack, erroraddr;
  ucontext_t *uc = (ucontext_t *)secret;
  threadTraceInfo *tinfo;

  stack.pAddr = &stack;
  stack &= NEG_SIZE_THREAD_STACK;
  erroraddr.pVoid = info->si_addr;
  erroraddr &= NEG_SIZE_THREAD_STACK;

  if (stack == erroraddr) {
    stack.pVoid = mmap (stack.pChar + PAD_THREAD_STACK, sizeof(threadTraceInfo), 
  			PROT_READ | PROT_WRITE,
  			MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED, -1, 0);
  } else {
    printf("Got signal %d, faulty address is %p, from %llx\n Calling: \n",
	   sig, info->si_addr, uc->uc_mcontext.gregs[REG_RIP]);
    if (sig != SIGTERM) displayTraceInfo(tinfo);
    //    RpollApp.KillAllChild();
    exit(-1);
  }
}
void SetupSIG(int num, SigHandle func)
{
  struct sigaction sa;
 
  sa.sa_sigaction = func;
  sigemptyset (&sa.sa_mask);
  sa.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(num, &sa, NULL);
}

#define     THREAD_NUM                          1



CMemoryAlloc globalMemory;
EVENT globalWait;
int         main(int, char**)
{
  int status;
  UINT i;
  ADDR addr;
  TIME rtime(CLOCK_MONOTONIC_RAW);
  struct timespec timestruct;

  SetupSIG(SIGSEGV, SIGSEGV_Handle);                            // sign 11
  SetupSIG(SIGILL, SIGSEGV_Handle);                             // sign 4
  SetupSIG(SIGTERM, SIGSEGV_Handle);                            // sign 15

__TRY__
  class RThreadTest test[THREAD_NUM];
  globalMemory.SetMemoryBuffer(1000, 4*1024, 64, 0);
  globalWait.InitArrayEvent();

  for (i=0; i<THREAD_NUM; i++) {
    test[i].ThreadClone(); 
  };
  usleep(100000);

  rtime += &timestruct;
  for (i=0; i<THREAD_NUM; i++) 
    globalWait += addr;

  GlobalShouldQuit = 1;
  for (i=0; i<THREAD_NUM; i++)
    waitpid(-1, &status, __WCLONE);
  rtime += &timestruct;

  rtime.OutputTime();
__CATCH__
}
