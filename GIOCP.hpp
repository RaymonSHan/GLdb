/*
 * GLdb thread & IOCP header file
 *
 * Relative to IOCP, epoll can be called IO before complete.
 * If system return before complete, application can get complete event,
 *   but if system return after complete, application could NOT get before it.
 * So, the unified model for application is IOCP. I do encapsulation epoll to IOCP.
 *
 * In my logic, epoll will received data order error, if multi-thread epoll_wait() 
 *   same socket, whether LT or ET; but IOCP will not, if you post only one buffer for
 *   receive, but multi-thread wait complete.
 * In GLdb, an IOCP handle assembled by an epoll handle and two buffered eventfd.
 *   Buffered eventfd means : eventfd work on EFD_SEMAPHORE mode with an array to
 *   storage value. The array is small, for when system is busy, should tell client
 *   explicitly, instead of make system more busy.
 * In GLdbIOCP, there are two thread wait for epoll and one eventfd. All application
 *   wait for another eventfd, act as IOCP handle.
 * Every socket handle seem by application is ADDR of CContextItem, with really
 *   handle and a received buffer pointer.
 *
 * for READ :
 * When epolled, epoll-thread looking for the buffer, if exist, it trigger eventfd; 
 *   if buffer is NULL, it omit it. Don't worry about lost the data, buffer is NULL
 *   means application is not ready. When the application ready, it will PostReceive
 *   a buffer by WSARecv() (I use this function name in Linux too), which trigger
 *   eventfd.
 * When eventfded, eventfd-thread always read socket, whether by epoll or application.
 *   if EAGAIN, continue wait; or trigger IOCP handle with CContextItem and buffer 
 *   pointer as the return value of IOCP for complete read. And of course, empty
 *   the buffer pointer.g
 *
 * for WRITE :
 * Application WSASend() only trigger eventfd-thread. This thread write socket, 
 *   if OK, eventfd-thread trigger IOCP for write complete; if EAGAIN, add EPOLLOUT
 *   to epoll, write more when epolled till OK. Then remove EPOLLOUT and trigger
 *   IOCP for complete write.
 *
 * Of course, epoll work on ET, efficiency always the first goal for GLdb.
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

#ifndef     GLdb_IOCP_HPP
#define     GLdb_IOCP_HPP

#include    "GCommon.hpp"
#include    "GMemory.hpp"


/*
 * This is buffered eventfd
 *
 * TEST : continuous += & -=, loop in one thread about 460ns 
 *        two thread nest, one loop is (2us, 3us, 6us)
 *        ten thread nest, one loop is (4us, 8us, 23us)
 */
#define     MAX_HANDLE_LOCK                     30

typedef     class RMultiEvent
{
private:
  QUERY_S   eventQuery;

public:
  int       eventFd;

public:
  RMultiEvent()
  {
    eventFd = 0;
  };
  ~RMultiEvent()
  {
    //    if (eventFd) close(eventFd);
  };
  UINT      InitArrayEvent()
  {
  __TRY
    __DO1(eventFd,
	  eventfd(0, EFD_SEMAPHORE));
  __CATCH
  };
  UINT      operator += (ADDR addr)
  {
  __TRY
    ADDR    WRITEADDR = {1};
    int     status;
    __DO (eventQuery += addr);
    __DO1(status,
	  write(eventFd, &WRITEADDR, SIZEADDR));
  __CATCH
  };
  UINT      operator -= (ADDR &addr)
  {
  __TRY
    ADDR    READADDR;
    int     status;
    __DO1(status,
	  read(eventFd, &READADDR, SIZEADDR));
    __DO (eventQuery -= addr);
  __CATCH
  };
}EVENT, *PEVENT;


/* 
 * Basic thread, only finish clone, init, loop, kill
 */
#define     GlobalThreadNumber                  RThread::globalThreadNumber
#define     GlobalShouldQuit                    RThread::globalShouldQuit
#define     ThreadStartEvent                    RThread::threadStartEvent


typedef   __class(RThread)
private:
  pid_t     threadId;
  ADDR      threadStack;
  UINT      shouldQuit;

protected:
  UINT      threadNumber;
  threadTraceInfo *threadInfo;
public:
  static    volatile UINT globalThreadNumber;
  static    volatile UINT globalShouldQuit;
  static    EVENT threadStartEvent;

public:
  RThread()
  {
    shouldQuit = 0;
  };
  UINT      ThreadClone(void)
  {
  __TRY
    ADDR    result = {0};
    if (ThreadStartEvent.eventFd) ThreadStartEvent -= result;
    else ThreadStartEvent.InitArrayEvent();
    __DO (result.aLong);
    LockInc(GlobalThreadNumber);
    __DO (GetStack(threadStack));
    sleep(1);
    __DO1(threadId,
	  clone(&(RThread::RThreadFunc), 
		threadStack.pChar + REAL_SIZE_THREAD_STACK,
		CLONE_VM | CLONE_FILES, this));
  __CATCH
  };
  static    int RThreadFunc(void* point)
  {
  __TRY
    RThread *thread = (RThread*) point;
    ADDR    result;
    result = thread->ThreadInit();
    __DO (ThreadStartEvent += result);
    __DO (result.aLong);
    while ((!thread->shouldQuit) && (!GlobalShouldQuit))
   __DO (      thread->ThreadDoing() );
  __CATCH
  };
  virtual   UINT ThreadInit(void) = 0;
  virtual   UINT ThreadDoing(void) = 0;

}THREAD;

/*
 * The thread wait for epoll in GLdbIOCP
 * 
 * acceptBuffer only for AcceptEx use, There are only one buffer for PostReceive per socket,
 *   but there are multi buffer for PostAccept per socket. There is a lazy way, for
 *   I use same STACK for all listening.
 */
__class_    (RThreadEpoll, RThread)
private:
  int       epollHandle;
  STACK     acceptBuffer;

public:
  RThreadEpoll()
  {
    epollHandle = 0;
  };
  UINT      ThreadInit(void)
  { 
  __TRY
    __DO1_(epollHandle, epoll_create(1), "Error in create epoll");
  __CATCH
  };
  UINT      ThreadDoing(void)
  {
    return 0;
  };
  UINT      CreateListen(SOCKADDR /*addr*/)
  {
  __TRY__
  // int   evNumber, i;
  // socklen_t clilen;
  // struct epoll_event ev;
  // ADDR  acceptaddr, listenaddr

    // __DO_ (GetContent(listenAddr), 
    // 	   "Error in getcontent");
    // __DO1_(listenAddr.SHandle, 
    // 	   socket(AF_INET, SOCK_STREAM, 0), 
    // 	   "Error in create socket");

    // ev.data.u64 = listenAddr.aLong;
    // ev.events = EPOLLIN | EPOLLET;
    // __DO1_(status, 
    // 	   epoll_ctl(epollHandle, EPOLL_CTL_ADD, listenAddr.SHandle, &ev), 
    // 	   "Error in epoll ctl");
    // listenAddr.ServerSocket.saddr = serveraddr;
    // __DO1_(status, 
    // 	   bind(listenAddr.SHandle, &serveraddr, sizeof(sockaddr_in)), 
    // 	   "Error in bind");
    // __DO1_(status, 
    // 	   listen(listenAddr.SHandle, query), 
    // 	   "Error in begin listen");
  __CATCH__
  };
};

typedef     class GLdbIOCP
{
}IOCP;


/*
 * Following line is for test, not request for other application
 */
extern CMemoryAlloc globalMemory;
extern EVENT globalWait;

typedef void SigHandle(int, siginfo_t *, void *);
void SIGSEGV_Handle(int sig, siginfo_t *info, void *secret);
void SetupSIG(int num, SigHandle func);

__class_    (RThreadTest, RThread)
public:

public:

  UINT      ThreadInit(void)
  {
  __TRY
    setThreadName();
    __DO (globalMemory.SetThreadArea(4,8,4,0));
  __CATCH
  };
  UINT      ThreadDoing(void)
  {
  __TRY
    QUERY_s mquery;
    int i, j;
    ADDR    addr;
    globalWait -= addr;
    for (j=0; j<1000*1000*25*4; j++) {
      globalMemory.GetMemoryList(addr);
      addr.pList->DecRefCount();
      //      globalMemory.FreeMemoryList(addr);
      // for (i=0; i<4; i++) {
      // 	__DO (globalMemory.GetMemoryList(addr));
      // 	__DO (mquery += addr);
      // }
      // while (!(mquery -= addr)) {
      // 	if (addr.AllocType)
      // 	__DO (globalMemory.FreeMemoryList(addr));
      // 	  //     	  __DO (addr.AllocType->FreeMemoryList(addr));
      // }
    }
  __CATCH
  };
};

#endif   // GLdb_IOCP_HPP
