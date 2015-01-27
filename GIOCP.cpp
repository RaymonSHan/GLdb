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

void        SIGSEGV_Handle(int sig, siginfo_t *info, void *secret)
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
void        SetupSIG(int num, SigHandle func)
{
  struct sigaction sa;
 
  sa.sa_sigaction = func;
  sigemptyset (&sa.sa_mask);
  sa.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(num, &sa, NULL);
};

MEMORY      GlobalMemory;
EVENT       GlobalWait;

RESULT    RThreadTest::ThreadInit(void)
{
__TRY
  setThreadName();
  __DO (GlobalBufferSmall.SetThreadArea(4,8,4,0));
__CATCH
};
RESULT    RThreadTest::ThreadDoing(void)
{
__TRY
  QUERY_s mquery;
  int  j;
  ADDR    addr;
  /*
   * Should NOT use Globalxx function, for it NOT initialize number
   * Freexx and DecRefCount are same. but for better habit,
   * use Get/Free for pair, and IncRefCount/DecRefCount in pair
   */
  GlobalWait -= addr;
  for (j=0; j<1000*1000*100; j++) {
    //   __DO_(GlobalBufferSmall.GetMemoryList(addr), "NOT GET\n");
    __DO_(GetBufferSmall(addr), "NOT GET\n");

    //   __DO_(GlobalBufferSmall.FreeMemoryList(addr), "NOT FREE\n");
    __DO_(FreeBufferSmall(addr), "NOT FREE\n");
    //   __DO_(addr.DecRefCount(), "NOT FREE\n");
  }
__CATCH
}

#define     NUMBER_CONTEXT                      5
#define     NUMBER_BUFFER_SMALL                 10
#define     NUMBER_BUFFER_MIDDLE                2

#define     THREAD_NUM                          1

int         main(int, char**)
{
  int       status;
  UINT      i;
  ADDR      addr;
  TIME      rtime(CLOCK_MONOTONIC_RAW);
  struct timespec timestruct;

  SetupSIG(SIGSEGV, SIGSEGV_Handle);                            // sign 11
  SetupSIG(SIGILL, SIGSEGV_Handle);                             // sign 4
  SetupSIG(SIGTERM, SIGSEGV_Handle);                            // sign 15

__TRY__
  class RThreadTest test[THREAD_NUM];
  GlobalMemory.InitMemoryBlock(NUMBER_CONTEXT,
			       NUMBER_BUFFER_SMALL,
			       NUMBER_BUFFER_MIDDLE);
  GlobalWait.InitArrayEvent();

  for (i=0; i<THREAD_NUM; i++) {
    test[i].ThreadClone(); 
  };
  usleep(10000);

  rtime += &timestruct;
  for (i=0; i<THREAD_NUM; i++) 
    GlobalWait += addr;
  usleep(10000);

  GlobalShouldQuit = 1;
  for (i=0; i<THREAD_NUM; i++)
    waitpid(-1, &status, __WCLONE);
  rtime += &timestruct;

  rtime.OutputTime();
  GlobalWait.FreeArrayEvent();
  GlobalMemory.FrreeMemoryBlock();
  
__CATCH__
};
