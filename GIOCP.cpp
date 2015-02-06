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

MEMORY      GlobalMemory;
IOCP        GlobalIOCP;

/*
 * return -1 for NOT socket, return >0 is listening, return 0 is not.
 */
int         isListeningSocket(HANDLE handle)
{
  int       val;
  int       result;
  socklen_t len = sizeof(val);
  result = getsockopt(handle, SOL_SOCKET, SO_ACCEPTCONN, &val, &len);
  if (result == -1) return result;
  else return val;
};

/*
 * For the following compatible function for Windows, 
 *   should remember some type convert. All have written in GCommon.hpp, but again.
 *
 * The most important is SOCKET and HANDLE.
 *
 * SOCKET : as a pointer of CContextItem. 64bits
 * HANDLE : as unsigned long long int. 64bits
 */

/*
 * In Windows, a better way to get SOCKET for IOCP is WSASocket. in GLdbIOCP, 
 *   it is necessarily.
 *
 * dwFlags must with OVERLAP flags, although i ignore it.
 */
SOCKET      WSASocket(int       af, 
		      int       type, 
		      int       protocol,
		      LPWSAPROTOCOL_INFO  lpProtocolInfo, 
		      GROUP     g, 
		      DWORD     dwFlags)
{
  (void)     lpProtocolInfo;
  (void)     g;
  (void)     dwFlags;
  PCONT      pcont;
  RESULT     result;

  result = GetContext(pcont);
  if (result) return 0;
  InitContextItem(pcont);
  pcont->bHandle = socket(af, type, protocol);;
  return pcont;
};

/*
 * Same as Windows version. three parameter is 0 for create new handle, or
 *   join the given FileHandle join ExistingCompletionPOrt with COmpetionKey.
 *
 * CreateIoCompletionPort() use GetIOCPItem() to get handle, 64 at most.
 * All FileHandle add EPOLLIN into same epollHandle. with each IOCP handle.
 */
HANDLE      CreateIoCompletionPort(SOCKET       FileHandle,
				   HANDLE       ExistingCompletionPort,
				   ULONG_PTR    CompletionKey,
				   DWORD        NumberOfConcurrentThreads)
{
  (void)    NumberOfConcurrentThreads;
__TRY__
  ADDR      addr;
  int       state;
  struct    epoll_event ev;
  if (!FileHandle && !ExistingCompletionPort && !CompletionKey) {
    __DO_(GlobalIOCP.GetIOCPItem(addr), "Errorr in Create IOCP handle\n");
    return addr.aLong;
  } else {
    __DO (!ExistingCompletionPort);
    __DO (!FileHandle);
    FileHandle->iocpHandle = (PEVENT)ExistingCompletionPort;
    FileHandle->completionKey = CompletionKey;
    ev.events = EPOLLET | EPOLLIN;
    ev.data.ptr = CompletionKey;
    __DO1 (state,
	   epoll_ctl(GlobalIOCP.epollHandle,
		     EPOLL_CTL_ADD, 
		     FileHandle->bHandle, 
		     &ev));
    return ExistingCompletionPort;
  }
__CATCH_1
};

/*
 * Wait for eventfd do NOT timeout option. Maybe I should change to another way.
 * So periodicial trigger is given by another thread.
 *
 * HAVE NOT IMPLEMENT YET. IGNORE THE OPTION NOW, ALL ARE INFINITE.
 *
 * Other things are simple, it wait for eventfd as IOCP hanle, and translate
 *   struct member as Windows define.
 */
BOOL        GetQueuedCompletionStatus(HANDLE    CompletionPort,
				      LPDWORD   lpNumberOfBytes,
				      PULONG_PTR lpCompletionKey,
				      LPOVERLAPPED *lpOverlapped,
				      DWORD     dwMilliseconds)
{
  (void)dwMilliseconds;
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;

  iocpHandle = (PEVENT)CompletionPort;
  __DO(*iocpHandle -= addr);
  *lpOverlapped = (LPOVERLAPPED)addr.pVoid;
  if (addr != ZERO) {
    (*lpCompletionKey) = (*lpOverlapped)->Internal->completionKey;
    (*lpNumberOfBytes) = (*lpOverlapped)->doneSize;
  }
__CATCH_1
};

/*
 * in normal IOCP, lpOverlapped can be NULL.
 * but in GLdbIOCP, must use valid OVERLAPPED struct for lpOverlapped
 */
BOOL        PostQueuedCompletionStatus(HANDLE   CompletionPort,
				       DWORD    dwNumberOfBytesTransferred,
				       ULONG_PTR dwCompletionKey,
				       LPOVERLAPPED lpOverlapped)
{
__TRY__
  PEVENT    iocpHandle;
  ADDR      addr;
  iocpHandle = (PEVENT)CompletionPort;
  addr.pVoid = lpOverlapped;
  lpOverlapped->Internal->completionKey = dwCompletionKey;
  lpOverlapped->doneSize = dwNumberOfBytesTransferred;
  __DO (*iocpHandle += addr);
__CATCH_1
};

/*
 * WSASend & WSARecv do almost same thing, the difference is events value,
 *   one for EPOLLWRITE and other for EPOLLREAD.
 * The only thing should attention is WSASend add writeBuffer, while WSARecv not add
 *   readBuffer.
 */
int         WSASend(SOCKET      s, 
		    LPWSABUF    lpBuffers, 
		    DWORD       dwBufferCount, 
		    LPDWORD     lpNumberOfBytesSent, 
		    DWORD       dwFlags, 
		    LPWSAOVERLAPPED     lpOverlapped, 
		    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine)
{
  (void)dwFlags;
  (void)lpCompletionRoutine;
__TRY
  ADDR      overlap;
  __DO_(dwBufferCount != 1, "GLdbIOCP only support one WSABUF now\n");
  __DO_(lpOverlapped == NULL, "GLdbIOCP not support NULL OVERLAPPED\n");
  overlap.pVoid = lpOverlapped;
  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
  lpOverlapped->events = EPOLLWRITE;
  lpOverlapped->doneSize = 0;
  lpBuffers->len = *lpNumberOfBytesSent;
  __DO (s->writeBuffer += overlap);
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH
};

int         WSARecv(SOCKET      s,
                    LPWSABUF    lpBuffers,
                    DWORD       dwBufferCount,
                    LPDWORD     lpNumberOfBytesRecvd,
                    LPDWORD     lpFlags,
                    LPWSAOVERLAPPED     lpOverlapped,
                    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine)
{
  (void)lpFlags;
  (void)lpCompletionRoutine;
__TRY
  ADDR      overlap;
  __DO_(dwBufferCount != 1, "GLdbIOCP only support one WSABUF now\n");
  __DO_(lpOverlapped == NULL, "GLdbIOCP not support NULL OVERLAPPED\n");
  overlap.pVoid = lpOverlapped;
  lpOverlapped->Internal = s;
  lpOverlapped->InternalHigh = lpBuffers;
  lpOverlapped->events = EPOLLREAD;
  lpOverlapped->doneSize = 0;
  lpBuffers->len = *lpNumberOfBytesRecvd;
/*__DO (s->readBuffer += overlap);*/
  __DO (*(GlobalIOCP.eventHandle) += overlap);
__CATCH
};

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


#define     NUMBER_CONTEXT                      5
#define     NUMBER_BUFFER_SMALL                 10
#define     NUMBER_BUFFER_MIDDLE                2

#define     THREAD_NUM                          1

int         main(int, char**)
{
  TIME      rtime(CLOCK_MONOTONIC_RAW);
  struct timespec timestruct;

  SetupSIG(SIGSEGV, SIGSEGV_Handle);                            // sign 11
  SetupSIG(SIGILL, SIGSEGV_Handle);                             // sign 4
  SetupSIG(SIGTERM, SIGSEGV_Handle);                            // sign 15

__TRY__
  GlobalMemory.InitMemoryBlock(NUMBER_CONTEXT,
			       NUMBER_BUFFER_SMALL,
			       NUMBER_BUFFER_MIDDLE);
  GlobalIOCP.InitGLdbIOCP();
  rtime += &timestruct;


  GlobalShouldQuit = 1;
  rtime.OutputTime();
  GlobalIOCP.FreeGLdbIOCP();
  GlobalMemory.FrreeMemoryBlock();
  
__CATCH__
};
