/*
 * GLdb thread & IOCP header file
 *
 * Relative to IOCP, epoll can be called IO before complete.
 * If system return before complete, application can get complete event,
 *   but if system return after complete, application could NOT get before it.
 * So, the unified model for application is IOCP. I do encapsulation epoll to IOCP.
 *
 * In my logic, epoll will received data order error, if multi thread epoll_wait() 
 *   same socket, whether LT or ET; but IOCP will not, if you post only one buffer for
 *   receive, while multi-thread wait complete.
 * In GLdb, IOCP system assembled by one epoll handle and one buffered eventfd. And
 *   two threads wait for each.
 * Buffered eventfd means : eventfd work on EFD_SEMAPHORE mode with an array to
 *   storage value. The array is small, for when system is busy, should tell client
 *   explicitly, instead of make system more busy.
 * In GLdbIOCP, there are a thread wait for epoll accept, read & write.
 *   and one thread wait eventfd. Each IOCP handle is another eventfd. 
 *   Application wait for it.
 * Every socket handle wait epoll, with ev.u64 for a OVERLAPPED struct, which Internal
 *   save CContextItem, and InternalHigh save WSABUF.
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
 *   the buffer for this socket.
 *
 * for WRITE :
 * Application WSASend() only trigger eventfd thread. eventfd thread write socket, 
 *   if OK, eventfd thread trigger IOCP for write complete; if EAGAIN, add EPOLLOUT
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

#define     IOCPBaseAddress                     GLdbIOCP::iocpBaseAddress
#define     ADDR_TO_IOCPHANDLE(addr)		                \
  ((int)(addr - IOCPBaseAddress) + (int)('C'<<24))		
#define     IOCPHANDLE_TO_ADDR(handle)		                \
  (PEVENT)(IOCPBaseAddress.pChar + ((UINT)(handle & 0xffffff)))

int         isListeningSocket(HANDLE handle);

#ifdef      __linux

int         WSAStartup(WORD     wVersionRequested, 
		       LPWSADATA lpWSAData);
int         WSACleanup(void);
SOCKET      WSASocket(int       af, 
		      int       type, 
		      int       protocol,
		      LPWSAPROTOCOL_INFO  lpProtocolInfo, 
		      GROUP     g, 
		      DWORD     dwFlags);

HANDLE      CreateIoCompletionPort(SOCKET       FileHandle,
				   HANDLE       ExistingCompletionPort,
				   ULONG_PTR    CompletionKey,
				   DWORD        NumberOfConcurrentThreads);
BOOL        GetQueuedCompletionStatus(HANDLE    CompletionPort,
				      LPDWORD   lpNumberOfBytes,
				      PULONG_PTR lpCompletionKey,
				      LPOVERLAPPED *lpOverlapped,
				      DWORD     dwMilliseconds);
BOOL        PostQueuedCompletionStatus(HANDLE   CompletionPort,
				       DWORD    dwNumberOfBytesTransferred,
				       ULONG_PTR dwCompletionKey,
				       LPOVERLAPPED lpOverlapped);

int         WSASend(SOCKET      s, 
		    LPWSABUF    lpBuffers, 
		    DWORD       dwBufferCount, 
		    LPDWORD     lpNumberOfBytesSent, 
		    DWORD       dwFlags, 
		    LPWSAOVERLAPPED     lpOverlapped, 
		    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
int         WSARecv(SOCKET      s,
                    LPWSABUF    lpBuffers,
                    DWORD       dwBufferCount,
                    LPDWORD     lpNumberOfBytesRecvd,
                    LPDWORD     lpFlags,
                    LPWSAOVERLAPPED     lpOverlapped,
                    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
#endif   // __linux

/*
 * This is buffered eventfd
 *
 * something different than my habit
 *   this is static struct, but use Initxx & Freexx, instead of constructor
 *
 * The total value of eventFd always small than MAX_HANDLE_LOCK, it seemed read 
 *   and write of eventFd will not fail at all.
 * The value read & write for eventFd always 1, for its SEMAPHORE mode. Uuseful 
 *   value is in eventQuery.
 *
 * TEST : continuous += & -=, loop in one thread about 460ns 
 *        two thread nest, one loop is (2us, 3us, 6us)
 *        ten thread nest, one loop is (4us, 8us, 23us)
 */
#define     MAX_HANDLE_LOCK                     LIST_SMALL-1

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
  RESULT    InitArrayEvent()
  {
  __TRY
    __DO1(eventFd,
	  eventfd(0, EFD_SEMAPHORE));
  __CATCH
  };
  RESULT    FreeArrayEvent()
  {
    if (eventFd) close(eventFd);
    return 0;
  };
  RESULT    operator += (ADDR addr)
  {
  __TRY
    ADDR    WRITEADDR = {1};
    int     status;
    __DO (eventQuery += addr);
    __DO1(status,
	  write(eventFd, &WRITEADDR, SIZEADDR));
  __CATCH
  };
  RESULT    operator -= (ADDR &addr)
  {
  __TRY
    ADDR    READADDR;
    int     status;
    __DO1(status,
	  read(eventFd, &READADDR, SIZEADDR));
    __DO (eventQuery -= addr);
  __CATCH
  };
}EVENT;

/* 
 * Basic thread, only finish clone, init, loop, kill
 *
 * In GLdb project, all working threads are devide from RThread, which cloned by 
 *   main thread.
 * Next thread will NOT be cloned until the previous one have finish its initialize. 
 *   This is controlled by ThreadStartEvent. child will set this when it initialized,
 *   while main thread wait for this.
 * Child Thread always have to use TLS, which initialized when it cloned. While the
 *   address of TLS is depend on RSP, so the ThreadInit() MUST be called by child,
 *   instead of main thread.
 *
 * The virtual base class RThread do all the above.
 * 
 * For inherit class, MUST implement virtual function ThreadInit(), ThreadDoing().
 *   and implement ThreadFree() if necessary.
 * For parent thread, call static member function ThreadClone(), to start child.
 *   after clone all child, MUST call ThreadStart() to set ThreadInitFinish eventfd.
 *   then all child begin do main loop.
 */
#define     GlobalThreadNumber                  RThread::globalThreadNumber
#define     GlobalShouldQuit                    RThread::globalShouldQuit
#define     ThreadStartEvent                    RThread::threadStartEvent
#define     ThreadInitFinish                    RThread::threadInitFinish

typedef   __class(RThread)
private:
  pid_t     threadId;
  ADDR      threadStack;
  UINT      shouldQuit;

protected:
  UINT      threadNumber;

public:
  static    volatile UINT globalThreadNumber;
  static    volatile UINT globalShouldQuit;
  static    EVENT threadStartEvent;
  static    EVENT threadInitFinish;

public:
  RThread()
  {
    shouldQuit = 0;
    threadId = 0;
  };
  ~RThread()
  {
    threadStartEvent.FreeArrayEvent();
  }

/*
 * The first time, ThreadStartEvent hsa not initialized, or wait for sign for
 *   last child thread finish its initialize.
 * Then clone this child with started at RThreadFunc().
 */
  RESULT    ThreadClone(void)
  {
  __TRY
    ADDR    result = {0};

    if (ThreadStartEvent.eventFd) ThreadStartEvent -= result;
    else {
      ThreadStartEvent.InitArrayEvent();
      ThreadInitFinish.InitArrayEvent();
    }
    __DO_(result.aLong, "GlobalEvent Initialize Error\n");
    LockInc(GlobalThreadNumber);
    __DO_(GetStack(threadStack), "Stack Initialize Error\n");
    __DO1_(threadId,
	   clone(&(RThread::RThreadFunc), 
		 threadStack.pChar + REAL_SIZE_THREAD_STACK,
		 CLONE_VM | CLONE_FILES, this),
	   "Thread Clone Error\n");
  __CATCH
  };

/*
 * After clone all child, parent MUST call this.
 *
 * At first, this function wait for the last child finish its initialize.
 * Then set eventfd to start child's main loop.
 * Finish free the event, but when free ThreadStartEvent? wait some seconds?
 */
  static    RESULT ThreadStart(void)
  {
  __TRY
    ADDR    result = {0};
    UINT    i;

    __DO (ThreadStartEvent -= result);
    for (i = 0; i < GlobalThreadNumber; i++) {
      __DO (ThreadInitFinish += result);
    }
    ThreadStartEvent.FreeArrayEvent();
/*
 * When call this ?
 *  ThreadInitFinish.FreeArrayEvent();
 */
//   
  __CATCH
  };

/*
 * First setThreadName() and other system initialize, and customize init by child.
 * After it, set sign for parent thread can clone next child. and wait parent's sign
 * Then do main loop till quit sign for this thread or all threads.
 * The final, free resource to quit.
 */
  static    int RThreadFunc(void* point)
  {
  __TRY
    RThread *thread = (RThread*) point;
    ADDR    result;

    result = thread->SystemThreadInit();
    __DO_(ThreadStartEvent += result, "Set GlobalEvent Error\n");
    __DO_(result.aLong, "Thread Initialize Error\n");

    __DO (ThreadInitFinish -= result);
    while ((!thread->shouldQuit) && (!GlobalShouldQuit))
      __DOc_(thread->ThreadDoing(), "Thread Doing Error\n");
    __DO_(thread->ThreadFree(), "Thread Free Error\n");
  __CATCH
  };
  RESULT    SystemThreadInit(void)
  {
    setThreadName();
    return ThreadInit();
  };
  virtual   RESULT ThreadInit(void) = 0;
  virtual   RESULT ThreadDoing(void) = 0;
  virtual   RESULT ThreadFree(void) { return 0; };

}THREAD;


#define     NUMBER_MAX_EV                       20


#define     TimeoutEpollWait                    RThreadEpoll::timeoutEpollWait

/*
 * for EPOLLIN is 1, EPOLLOUT is 4, i define other for other sign
 */
#define     EPOLLTIMEOUT                        (1 << 8)
#define     EPOLLREAD                           (1 << 9)
#define     EPOLLWRITE                          (1 << 10)

/*
 * GLdbIOCP assembled by one epoll handle and one buffered eventfd. And two
 *   threads wait for each.
 * This thread is wait for epoll in GLdbIOCP.
 * 
 * Typical, RThreadEpoll wait for EPOLLIN & EPOLLOUT. but do NOT read & write, 
 *   these reality work is done by RThreadEvent.
 * If RThreadEpoll and RThreadEvent do read & write at the same time, concurrent
 *   error will happen.
 * In fact, if it can wait for BOTH eventfd and epoll, one thread is enough, and
 *   more effciently. (maybe epoll_pwait is ok?)
 * This thread translate epoll sign to eventfd by send an OVERLAPPED to RThreadEvent.
 * The field in OVERLAPPED is following define, which in GCommon.hpp.
 *
 * Summary : RThreadEpoll is a very simple class
 *
 * timeoutEpollWait : trigger RThreadEvent periodically, more detail in GCommon.cpp
 * epoll_event  : epoll_wait() use, for store event from epoll
 * epollHandle  : as its name
 * evHandle     : RThreadEvent's member, set by parent thread after initialize
 * overlapStack : struct for translate to RThreadEvent
 * overlapBuffer: really memory to store message
 */
typedef   __class_ (RThreadEpoll, RThread)
public:
  static    int timeoutEpollWait;

protected:
  struct    epoll_event waitEv[NUMBER_MAX_EV];
  
public:
  int       epollHandle;
  PEVENT    evHandle;
  STACK_S   overlapStack;
  OVERLAPPED overlapBuffer[LIST_SMALL + 1];
public:
  RThreadEpoll()
  {
    epollHandle = 0;
    evHandle = 0;
  };
  RESULT    ThreadInit(void)
  {
  __TRY
    ADDR    addr;
    addr = (PVOID)overlapBuffer;
    __DO1_(epollHandle, epoll_create(1), "Error in create epoll\n");
    overlapStack.FullArrayStack(addr, sizeof(OVERLAPPED), LIST_SMALL);
  __CATCH
  };
  RESULT    ThreadDoing(void)
  {
  __TRY
    int     evNumber, i;
    ADDR    overlapaddr;
    LPOVERLAPPED &overlap = (LPOVERLAPPED &)overlapaddr;

    __DO1 (evNumber,
	   epoll_wait(epollHandle, waitEv, NUMBER_MAX_EV, TimeoutEpollWait));
    __DO (overlapStack -= overlapaddr);

    if (waitEv == 0) {
      overlap->events = EPOLLTIMEOUT;
      __DO (*evHandle += overlapaddr);
    }
    for (i = 0; i < evNumber; i++) {
      overlap->events = waitEv[i].events;
      overlap->Internal = (PCONT)waitEv[i].data.u64;
      overlap->InternalHigh = 0;
      __DO (*evHandle += overlapaddr);
    }
  __CATCH
  };
  RESULT    ThreadFree(void)
  {
    close(epollHandle);
    return 0;
  };
}TEPOLL, *PTEPOLL;

/*
 * RThreadEvent do really working make epoll like IOCP
 */
typedef   __class_ (RThreadEvent, RThread)
public:
  int       epollHandle;
  EVENT     evHandle;
  PSTACK_S  pOverlapStack;
public:
  RThreadEvent()
  {
    epollHandle = 0;
  }; 
  RESULT    ThreadInit(void)
  {
  __TRY
    __DO_(evHandle.InitArrayEvent(), "Error in create eventEv\n");
  __CATCH
  };
  RESULT    ThreadDoing(void)
  {
  __TRY
    int     readed, writed, state;
    ADDR    contextaddr, overlapaddr, bufferaddr;
    LPOVERLAPPED &overlap = (LPOVERLAPPED &)overlapaddr;
    PCONT   &context = (PCONT &)contextaddr;
    LPWSABUF &buffer = (LPWSABUF &)bufferaddr; 
    struct  epoll_event ev;

    __DO (evHandle -= overlapaddr);
    contextaddr = overlap->Internal;
    bufferaddr = overlap->InternalHigh;
    
    if (overlap->events == EPOLLIN) {
      if (bufferaddr == ZERO) {                 // Get sign from Epoll, no overlap yet.
	*pOverlapStack += bufferaddr;           // free tran info struct
	context->readBuffer -= overlapaddr;     // get real overlap
	if (overlapaddr == ZERO) __BREAK_OK;    // application not, no overlap
	bufferaddr = overlap->InternalHigh;     // get wsabuf in overlap
	overlap->events = EPOLLIN;              // Is it necessary ?
      }

      __DO1c(readed,                            // read
	     read(context->bHandle,
		  buffer->buf,
		  buffer->len));
      if (readed == EAGAIN) {                   // no data read, break;
	context->readBuffer += overlapaddr;     // DO not control, add to readBuffer
      } else if (readed > 0) {                  // have data
	overlap->doneSize = readed;             // save size
	__DO (*context->iocpHandle += overlapaddr); // sign iocp
      } else {
	// close socket
	// maybe merge to > 0 condition
      }
    }
    else if (overlap->events == EPOLLOUT) {
      if (bufferaddr == ZERO) {                 // Get sign from Epoll, no overlap yet.
	*pOverlapStack += bufferaddr;           // free tran info struct
      }
      __DOc(context->writeBuffer.TryGet(overlapaddr));// MUST have return
      __DO1c(writed,                            // write
	     write(context->bHandle,
		   buffer->buf + overlap->doneSize,
		   buffer->len - overlap->doneSize));
      if (writed + overlap->doneSize == buffer->len) {// write all
	context->writeBuffer -= overlapaddr;    // have written, remove overlap
	overlap->doneSize += writed;            // total write size
	overlap->events = EPOLLOUT;             // Is it necessary ?
	__DO (*context->iocpHandle += overlapaddr);// sign iocp
	if (!context->inEpollOut) __BREAK_OK;   // not set EPOLLOUT, break;
	if (!(context->writeBuffer.TryGet(overlapaddr))) __BREAK_OK;// should write more
	__DO1 (state,                           // write job is finish, remove EPOLLOUT
	       epoll_ctl(epollHandle, 
			 EPOLL_CTL_DEL, 
			 context->bHandle, 
			 &ev));
	context->inEpollOut = 0;
      }
      else {                                    // EAGAIN or part write
	if (writed != EAGAIN) overlap->doneSize += writed;// part write, add
	if (context->inEpollOut) __BREAK_OK;    // already add EPOLLOUT, break;
	ev.events = EPOLLET | EPOLLOUT;
	ev.data.u64 = contextaddr.aLong;
	__DO1 (state,                           // add EPOLLOUT
	       epoll_ctl(epollHandle, 
			 EPOLL_CTL_ADD, 
			 context->bHandle, 
			 &ev));
	context->inEpollOut = 1;
      }
    }
  __CATCH
  };
  RESULT    ThreadFree(void)
  {
    evHandle.FreeArrayEvent();
    return 0;
  };
}TEVENT, *PTEVENT;

/*
 * GLdb always use static memory, 63 is enough for any application
 *
 * GLdbIOCP has only one instance
 */
#define     NUMBER_MAX_IOCP                     LIST_MIDDLE     // 63

typedef     class GLdbIOCP {
private:
  EVENT     iocpHandle[NUMBER_MAX_IOCP];
  QUERY_M   iocpHandleFree;
  QUERY_M   iocpHandleUsed;

  TEPOLL    threadEpoll;
  TEVENT    threadEv;

public:
  int       epollHandle;
  PEVENT    evHandle;
  PSTACK_S  pOverlapStack;

public:
  static    ADDR iocpBaseAddress;

public:
  RESULT    InitGLdbIOCP()
  {
  __TRY__
    int     i;
    ADDR    addr;

    for (i=0; i<NUMBER_MAX_IOCP; i++) {
      addr = (PVOID)&(iocpHandle[i]);
      iocpHandleFree += addr;
    }
    iocpBaseAddress = (PVOID)&(iocpHandle[0]);
    threadEv.ThreadClone();
    threadEpoll.ThreadClone();

    epollHandle = threadEv.epollHandle = threadEpoll.epollHandle;
    evHandle = threadEpoll.evHandle = &threadEv.evHandle;
    pOverlapStack = threadEv.pOverlapStack = &threadEpoll.overlapStack;
    RThread::ThreadStart();

  __CATCH__
  };
  RESULT    FreeGLdbIOCP()
  {
    ADDR    addr;
    PEVENT  pevent;
    int     status;
    while (!(iocpHandleUsed -= addr)) {
      pevent = (PEVENT)(addr.pVoid);
      pevent->FreeArrayEvent();
    }
    waitpid(-1, &status, __WCLONE);
    waitpid(-1, &status, __WCLONE);
    return 0;
  };
  RESULT    GetIOCPItem(ADDR &addr)
  {
  __TRY
    PEVENT  pevent;
    __DO (iocpHandleFree -= addr);
    __DO (iocpHandleUsed += addr);
    pevent = (PEVENT)(addr.pVoid);

    __DO (pevent->InitArrayEvent());
  __CATCH
  };
}IOCP;


/*
 * Following line is for test, not request for other application
 */

typedef     void SigHandle(int, siginfo_t *, void *);
void        SIGSEGV_Handle(int sig, siginfo_t *info, void *secret);
void        SetupSIG(int num, SigHandle func);

__class_    (RThreadTest, RThread)
public:
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);
};

#endif   // GLdb_IOCP_HPP

