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

int         isListeningSocket(
	    HANDLE handle);

#ifdef    __linux

int         WSAStartup(
            WORD            wVersionRequested, 
	    LPWSADATA       lpWSAData);
int         WSACleanup(void);
SOCKET      WSASocket(
            int             af, 
	    int             type, 
	    int             protocol,
	    LPWSAPROTOCOL_INFO  lpProtocolInfo, 
	    GROUP           g, 
	    DWORD           dwFlags);

HANDLE      CreateIoCompletionPort(
            SOCKET          FileHandle,
	    HANDLE          ExistingCompletionPort,
	    ULONG_PTR       CompletionKey,
	    DWORD           NumberOfConcurrentThreads);
BOOL        GetQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    LPDWORD         lpNumberOfBytes,
	    PULONG_PTR      lpCompletionKey,
	    LPOVERLAPPED   *lpOverlapped,
	    DWORD           dwMilliseconds);
BOOL        PostQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    DWORD           dwNumberOfBytesTransferred,
	    ULONG_PTR       dwCompletionKey,
	    LPOVERLAPPED    lpOverlapped);

int         WSASend(
	    SOCKET          s, 
	    LPWSABUF        lpBuffers, 
	    DWORD           dwBufferCount, 
	    LPDWORD         lpNumberOfBytesSent, 
	    DWORD           dwFlags, 
	    LPWSAOVERLAPPED lpOverlapped, 
	    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
int         WSARecv(
	    SOCKET          s,
	    LPWSABUF        lpBuffers,
	    DWORD           dwBufferCount,
	    LPDWORD         lpNumberOfBytesRecvd,
	    LPDWORD         lpFlags,
	    LPWSAOVERLAPPED lpOverlapped,
	    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);

BOOL        AcceptEx(
            SOCKET          sListenSocket,
	    SOCKET          sAcceptSocket,
	    PVOID           lpOutputBuffer,
	    DWORD           dwReceiveDataLength,
	    DWORD           dwLocalAddressLength,
	    DWORD           dwRemoteAddressLength,
	    LPDWORD         lpdwBytesReceived,
	    LPOVERLAPPED    lpOverlapped);
BOOL        ConnectEx(
            SOCKET          s,
	    const struct sockaddr *name,
	    int             namelen,
	    PVOID           lpSendBuffer,
	    DWORD           dwSendDataLength,
	    LPDWORD         lpdwBytesSent,
	    LPOVERLAPPED    lpOverlapped);
BOOL        DisconnectEx(
            SOCKET          hSocket,
	    LPOVERLAPPED    lpOverlapped,
	    DWORD           dwFlags,
	    DWORD           reserved);

#define     WSA_FLAG_OVERLAPPED                 (1 << 0)
#endif // __linux

#ifdef    __GLdb_SELF_USE
#define     WSA_FLAG_ISLISTEN                   (1 << 10)
#define     WSA_FLAG_ISACCEPT                   (1 << 11)

#define     IS_LISTEN(pcont)                    (pcont->dwFlags & WSA_FLAG_ISLISTEN)
#define     IS_ACCEPT(pcont)                    (pcont->dwFlags & WSA_FLAG_ISACCEPT)
#endif // __GLdb_SELF_USE

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
 * for EPOLLIN is 1, EPOLLOUT is 4, i define other sign
 *
 * EPOLLIN     : for RThreadEpoll receive EPOLLIN, then send this in 
 *               OVERLAPPED to RThreadEvent.
 * EPOLLOUT    : same for RThreadEpoll receive EPOLLOUT
 * EPOLLTIMEOUT: timeout in RThreadEpoll, trigger RThreadEvent
 * EPOLLREAD   : WSARecv send this in OVERLAPPED to trigger RThreadEvent.
 * EPOLLWRITE  : WSASend send this in OVERLAPPED to trigger RThreadEvent.
 * EPOLLACCEPT : for define accept
 */
#define     EPOLLTIMEOUT                        (1 << 8)
#define     EPOLLREAD                           (1 << 9)
#define     EPOLLWRITE                          (1 << 10)
#define     EPOLLACCEPT                         (1 << 11)

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
 *   which include EPOLLIN/EPOLLOUT and address of CContextItem.
 *
 * The field in OVERLAPPED is defined in GCommon.hpp.
 *
 * Summary : RThreadEpoll is a very simple class
 *
 * timeoutEpollWait : trigger RThreadEvent periodically, more detail in GCommon.cpp
 * epoll_event  : epoll_wait() use, for store event from epoll
 * pepollHandle : as its name
 * evHandle     : RThreadEvent's member, set by parent thread after initialize
 * overlapStack : struct for translate to RThreadEvent
 * overlapBuffer: really memory to store message
 * forAcceptOnly: const for translate EPOLLACCEPT to IOCP
 */
typedef   __class_ (RThreadEpoll, RThread)
public:
  static    int timeoutEpollWait;

protected:
  struct    epoll_event waitEv[NUMBER_MAX_EV];
  
public:
  int       epollHandle;
  PEVENT    peventHandle;
  STACK_S   overlapStack;
  OVERLAPPED overlapBuffer[LIST_SMALL + 1];
public:
  RThreadEpoll()
  {
    epollHandle = 0;
    peventHandle = 0;
  };
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);
  RESULT    ThreadFree(void)
  {
    close(epollHandle);
    return 0;
  };
}TEPOLL, *PTEPOLL;

/*
 * GLdbIOCP assembled by one epoll handle and one buffered eventfd. And two
 *   threads wait for each.
 * This thread is wait for eventfd in GLdbIOCP. And make epoll like IOCP
 *
 * It wait for evHandle, every ADDR from it, is the address of a OVERLAPPED
 *
 * RThreadEvent receive five sign, saved in member events of OVERLAPPED, 
 *   which defined like EPOLLxx. For EPOLLIN & EPOLLOUT, only Internal is valid.
 *   For EPOLLREAD & EPOLLWRITE, Internal and InternalHigh is valid. For 
 *   EPOLLTIMEOUT, all other fields in OVERLAPPED are invalid.
 * Of course for EPOLLIN & EPOLLOUT, should free OVERLAPPED to RThreadEpoll.
 *
 * EPOLLIN      : send by RThreadEpoll, for data is ready. Internal in OVERLAPPED
 *                is address of CContextItem.
 *                RThreadEvent check received buffer for this socket, which list in 
 *                readBuffer of CContextItem. If exist, get a buffer and read data.
 *                Or ignore it, DO NOTHING !!
 *
 *                !! THIS IS THE MOST MODIFIED FOR GLdbIOCP !!
 *
 *                If buffer exist, it trigger IOCP eventfd for read complete when 
 *                finish reading.
 * more for accept in
 * EPOLLOUT     : send by RThreadEpoll, for last send is ok. Internal same as above.
 *                RThreadEvent first check whether the data in this OVERLAPPED have 
 *                sent completely, if not, continous send. Or trigger IOCP eventfd
 *                for write complete. Then check whether any more OVERLAPPED sould
 *                send list in writeBuffer of CContextItem. if exist, send more.
 *                After every send, it check error for EAGAIN, if OK return, remove
 *                EPOLLOUT of this socket, and trigger write complete immediatelly.
 *                Of course, if no more OVERLAPPED in writeBuffer, remove EPOLLOUT
 *                too, 
 * EPOLLREAD    : send by WSARecv(), for application is ready for receive data.
 *                Every time RThreadEvent receive this sign, it read the socket.
 *                If get OK, trigger read complete, or if get EAGAIN, add this
 *                OVERLAPPED to readBuffer.
 * EPOLLWRITE   : send by WSASend(), for application data send.
 *                Before trigger RThreadEvent, WSASend have added OVERLAPPED for write
 *                into writeBuffer, for last OVERLAPPED may partly send.
 *                RThreadEvent do same thing as receive EPOLLOUT for EPOLLWRITE.
 * EPOLLTIMEOUT : do nothing but exit the loop.
 * EPOLLACCEPT  : AcceptEx post buffer for accept, check acceptQuery. If exist, get
 *                the accept, Or, add the buffer to readBuffer of listen SOCKET.
 *
 * epollHandle  : the epollHandle in RThreadEpoll, set by parent thread after initialize.
 * evHandle     : buffered eventfd, entity of pevHandle in RThreadEpoll
 * poverlapStack: pointer to overlapStack in RThreadEvent, for free OVERLAPPED from 
 *                RThreadEvent in EPOLLIN & EPOLLOUT.
 * acceptStack  : buffer accetp sign wait for PostAccept
 *
 * readBuffer is add by RThreadEvent, while writeBuffer is add by WSASend.
 * For receive buffer is empty, the order is unrelated. while send buffer MUST in order.
 */
typedef   __class_ (RThreadEvent, RThread)
public:
  int       epollHandle;
  EVENT     eventHandle;
  PSTACK_S  pOverlapStack;
public:
  RThreadEvent()
  {
    epollHandle = 0;
  }; 
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);
  RESULT    ThreadFree(void)
  {
    eventHandle.FreeArrayEvent();
    return 0;
  };
}TEVENT, *PTEVENT;


#ifdef    __GLdb_SELF_USE
typedef   __class_ (RThreadWork, RThread)
public:
  RThreadWork() {};
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);
}TWORK, *PTWORK;

#endif // __GLdb_SELF_USE

#define     NUMBER_MAX_IOCP                     LIST_MIDDLE
#define     NUMBER_MAX_WORK                     32

/*
 * GLdbIOCP has only one instance
 *
 * class CLdbIOCP alloc IOCP handle, and clone two thread.
 * Its behavior following the define in RThread.
 *
 * The program use this class should call InitGldbIOCP() at first, and call
 *   FreeGLdbIOCP() at last.
 * GetIOCPItem() is used by CreateIoCompletionPort(), should not be called directly.
 *
 * iocpHandle    : Application wait this as IOCP handle.
 *                 GLdb always use static memory, 63 is enough for any application
 * iocpHandleFree: list the unused IOCP handle.
 * iocpHandleUsed: list for used IOCP handle, only used when program exit.
 * threadEpoll   : RThreadEpoll class
 * threaEvent    : RThreadEvent class
 */
typedef     class GLdbIOCP {
private:
  EVENT     iocpHandle[NUMBER_MAX_IOCP];
  QUERY_M   iocpHandleFree;
  QUERY_M   iocpHandleUsed;

  TEPOLL    threadEpoll;
  TEVENT    threadEvent;
  TWORK     threadWork[NUMBER_MAX_WORK];

public:
  int       epollHandle;
  PEVENT    eventHandle;
  PSTACK_S  pOverlapStack;

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
    //    threadEpoll.ThreadClone();
    //    threadEvent.ThreadClone();
    // for test Get/Free Context/Buffer();
    threadWork[0].ThreadClone();

/*
 * YES, this code may be execute before threadEvent initialized.
 * but threadEpoll is initialized surely.
 */
    epollHandle = threadEvent.epollHandle = threadEpoll.epollHandle;
    eventHandle = threadEpoll.peventHandle = &threadEvent.eventHandle;
    pOverlapStack = threadEvent.pOverlapStack = &threadEpoll.overlapStack;
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


#endif   // GLdb_IOCP_HPP

