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

int         isListeningSocket(HANDLE handle);

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
	    POLAP          *lpOverlapped,
	    DWORD           dwMilliseconds);
BOOL        PostQueuedCompletionStatus(
            HANDLE          CompletionPort,
	    DWORD           dwNumberOfBytesTransferred,
	    ULONG_PTR       dwCompletionKey,
	    POLAP           lpOverlapped);

int         WSASend(
	    SOCKET          s, 
	    LPWSABUF        lpBuffers, 
	    DWORD           dwBufferCount, 
	    PUINT           lpNumberOfBytesSent, 
	    DWORD           dwFlags, 
	    POLAP           lpOverlapped, 
	    POLAPCR         lpCompletionRoutine);
int         WSARecv(
	    SOCKET          s,
	    LPWSABUF        lpBuffers,
	    DWORD           dwBufferCount,
	    PUINT           lpNumberOfBytesRecvd,
	    LPDWORD         lpFlags,
	    POLAP           lpOverlapped,
	    POLAPCR         lpCompletionRoutine);

BOOL        AcceptEx(
            SOCKET          sListenSocket,
	    SOCKET          sAcceptSocket,
	    PVOID           lpOutputBuffer,
	    DWORD           dwReceiveDataLength,
	    DWORD           dwLocalAddressLength,
	    DWORD           dwRemoteAddressLength,
	    LPDWORD         lpdwBytesReceived,
	    POLAP           lpOverlapped);
BOOL        ConnectEx(
            SOCKET          s,
	    PSOCK           name,
	    int             namelen,
	    PVOID           lpSendBuffer,
	    DWORD           dwSendDataLength,
	    LPDWORD         lpdwBytesSent,
	    POLAP           lpOverlapped);
BOOL        DisconnectEx(
            SOCKET          hSocket,
	    POLAP           lpOverlapped,
	    DWORD           dwFlags,
	    DWORD           reserved);

HANDLE      CreateFile(
            LPCTSTR         lpFileName,
            DWORD           dwDesiredAccess,
            DWORD           dwShareMode,
            LPSECURITY_ATTRIBUTES lpSecurityAttributes,
            DWORD           dwCreationDisposition,
            DWORD           dwFlagsAndAttributes,
            HANDLE          hTemplateFile);
BOOL        CloseHandle(
            HANDLE          hObject);
BOOL        ReadFile(
            HANDLE          hFile,
            LPVOID          lpBuffer,
            DWORD           nNumberOfBytesToRead,
            LPDWORD         lpNumberOfBytesRead,
            POLAP           lpOverlapped);
BOOL        WriteFile(
            HANDLE          hFile,
            LPVOID          lpBuffer,
            DWORD           nNumberOfBytesToWrite,
            LPDWORD         lpNumberOfBytesWritten,
            POLAP           lpOverlapped);


UINT        WSAGetLastError(void);
void        WSASetLastError(UINT err);
/*
 * Should translate every errno to WSALastError
 */
inline UINT ToWSAError(UINT err) { return err; };

#define     WSAERROR						\
            WSASetLastError(ToWSAError(errno));


#define     WSA_INFINITE                        NEGONE
#define     WSA_FLAG_OVERLAPPED                 (1 << 0)
#define     WSA_IO_PENDING                      997
#endif // __linux

#ifdef    __GLdb_SELF_USE
#define     WSA_FLAG_ISLISTEN                   (1 << 10)
#define     WSA_FLAG_ISACCEPT                   (1 << 11)
#define     WSA_FLAG_ISCONNECT                  (1 << 12)

#define     IS_LISTEN(pcont)					\
            (pcont->dwFlags & WSA_FLAG_ISLISTEN)
#define     IS_ACCEPT(pcont)					\
            (pcont->dwFlags & WSA_FLAG_ISACCEPT)
#define     IS_CONNECT(pcont)					\
            (pcont->dwFlags & WSA_FLAG_ISCONNECT)

#define     IS_DUPLEX(pcont)					\
            (pcont->pApplication->ApplicationFlag &		\
            APPLICATION_FLAG_DUPLEX)
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
#define     MAX_HANDLE_LOCK                     (LIST_SMALL-1)

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
    eventFd = 0;
    return 0;
  };
  RESULT    IsInitArrayEvent()
  {
    return (eventFd == 0);
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
}EVENT, *PEVENT;

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
#define     ThreadMainFinish                    RThread::threadMainFinish

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
  static    EVENT threadMainFinish;

public:
  RThread()
  {
    shouldQuit = 0;
    threadId = 0;
  };
  ~RThread()
  {
    threadStartEvent.FreeArrayEvent();
  };

/*
 * The first time, ThreadStartEvent hsa not initialized, or wait for sign for
 *   last child thread finish its initialize.
 * Then clone this child with started at RThreadFunc().
 *
 * For workthread, do not control init order, so init is false
 */
  RESULT    ThreadClone(BOOL init)
  {
  __TRY
    ADDR    result = {0};
    if (init) {
      if (ThreadStartEvent.eventFd) {
	__DO (ThreadStartEvent -= result);
      }
      else {
	__DOe(ThreadStartEvent.InitArrayEvent(),
	    GL_CLONE_INIT_ERROR);
	__DOe(ThreadInitFinish.InitArrayEvent(),
	    GL_CLONE_INIT_ERROR);
	__DOe(ThreadMainFinish.InitArrayEvent(),
	    GL_CLONE_INIT_ERROR);
      }
    }

    LockInc(GlobalThreadNumber);
    __DO (GetStack(threadStack));
    __DO1(threadId,
	    clone(&(RThread::RThreadFunc), 
	    threadStack.pChar + REAL_SIZE_THREAD_STACK,
	    CLONE_VM | CLONE_FILES, this));
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
    for (i=0; i < GlobalThreadNumber; i++) {
      __DO (ThreadMainFinish -= result);
    }
    __DOc(ThreadStartEvent.FreeArrayEvent());
    __DOc(ThreadInitFinish.FreeArrayEvent());
    __DOc(ThreadMainFinish.FreeArrayEvent());
  __CATCH
  };

/*
 * First setThreadName() and other system initialize, and customize init by child.
 * After it, set sign for parent thread can clone next child. and wait parent's sign
 * When get ThreadInitFinish, it send back to parent by ThreadMainFinish
 * Then do main loop till quit sign for this thread or all threads.
 * The final, free resource to quit.
 */
  static    int RThreadFunc(void* point)
  {
  __TRY
    RThread *thread = (RThread*) point;
    ADDR    result;

    result = thread->SystemThreadInit();
    if (!ThreadStartEvent.IsInitArrayEvent()) {
      __DO (ThreadStartEvent += result);
      __DO (ThreadInitFinish -= result);
      __DO (ThreadMainFinish += result);
    }
    __DO (result.aLong);
    while ((!thread->shouldQuit) && (!GlobalShouldQuit)) {
      __DOc_(thread->ThreadDoing(), "Thread Doing Error\n");
    }
    __DO (thread->ThreadFree());
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
}THREAD, *PTHREAD;


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
 * EPOLLACCEPT : for define accept, post by AcceptEx()
 * EPOLLCONNECT: for define connect, post by ConnectEx()
 */
#define     EPOLLTIMEOUT                        (1 << 18)
#define     EPOLLREAD                           (1 << 19)
#define     EPOLLWRITE                          (1 << 20)
#define     EPOLLACCEPT                         (1 << 21)
#define     EPOLLCONNECT                        (1 << 22)

#define     EPOLLFILEREAD                       (1 << 23)
#define     EPOLLFILEWRITE                      (1 << 24)
#define     EPOLLFILEOPEN                       (1 << 25)
#define     EPOLLFILECLOSE                      (1 << 26)

#ifndef   __linux
#define     EPOLLIN                             (1 << 0)        // 0x0001
#define     EPOLLPRI                            (1 << 1)        // 0x0002
#define     EPOLLOUT                            (1 << 2)        // 0x0004
#define     EPOLLERR                            (1 << 3)        // 0x0008
#define     EPOLLHUP                            (1 << 4)        // 0x0010
#define     EPOLLRDNORM                         (1 << 6)        // 0x0040
#define     EPOLLRDBAND                         (1 << 7)        // 0x0080
#define     EPOLLWRNORM                         (1 << 8)        // 0x0100
#define     EPOLLWRBAND                         (1 << 9)        // 0x0200
#define     EPOLLMSG                            (1 << 10)       // 0x0400
#define     EPOLLRDHUP                          (1 << 13)       // 0x2000
#define     EPOLLWAKEUP                         (1 << 29)
#define     EPOLLONESHOT                        (1 << 30)
#define     EPOLLET                             (1 << 31)
#endif // __linux

/*
 * size = 0 is valid for ACCEPT & CONNECT, so need a value for close
 */
#define     MARK_ERROR_CLOSE                    (-100)

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
 * overlapBuffer: really memory for OLAP to store message
 */
typedef   __class_ (RThreadEpoll, RThread)
public:
  static    int timeoutEpollWait;

protected:
  struct    epoll_event waitEv[NUMBER_MAX_EV];
  
public:
  int       epollHandle;
  PEVENT    peventHandle;
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
 *              : for LISTEN socket, EPOLLIN means accept income, it check readBuffer
 *                too. If exist, get a buffer and accept it, if not, add the accept 
 *                to writeBuffer of LISTEN socket.
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
 * EPOLLACCEPT  : AcceptEx post buffer for accept, check writeBuffer of LISTEN socket
 *                If exist, get the accept, Or, add the buffer to readBuffer of listen 
 *                SOCKET.
 * EPOLLCONNECT : ConnectEx post buffer for connect, THE handle with WSA_FLAG_CONNECT 
 *                flag, and waitEpollOut. after connect, it wait EPOLLOUT to do more.
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
//  PSTACK_S  pOverlapStack;
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
/*
 * RThreadWork wait IOCP and process application & protocol
 *
 * before clone, the thread MUST set handleIOCP by SetupHandle()
 * the only member in this class, is the IOCP handle it waited.
 */
typedef   __class_ (RThreadWork, RThread)
private:
  PEVENT    handleIOCP;
public:
  RThreadWork() {};
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);
  void      SetupHandle(PEVENT handle) {
    handleIOCP = handle;
  };
}TWORK, *PTWORK;

#endif // __GLdb_SELF_USE

#define     EventFile                           RThreadFile::eventFile

typedef   __class_ (RThreadFile, RThread)
public:
  static    EVENT eventFile;

  RThreadFile() {};
  RESULT    ThreadInit(void);
  RESULT    ThreadDoing(void);

}TFILE, *PTFILE;

//#define     NUMBER_MAX_IOCP                     LIST_MIDDLE - 1
#define     NUMBER_MAX_IOCP                     2
// In debug it is 2
#define     NUMBER_MAX_WORK                     32
#define     NUMBER_MAX_FILE                     2
//#define     NUMBER_MAX_FILE                     32

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
 * threadWork    : RThreadWork class, max number is  NUMBER_MAX_WORK
 * nowWorkThread : now RThreadWork number
 * 
 * epollHandle   : handle of threadEpoll.epollHandle
 * eventHandle   : pointer to threadEvent.eventHandle
 * pOverlapStack : pointer to threadEpoll.overlapStack
 */
typedef     class GLdbIOCP {
private:
  EVENT     iocpHandle[NUMBER_MAX_IOCP];
  QUERY_M   iocpHandleFree;
  QUERY_M   iocpHandleUsed;

  TEPOLL    threadEpoll;
  TEVENT    threadEvent;
  TWORK     threadWork[NUMBER_MAX_WORK];
  TFILE     threadFile[NUMBER_MAX_FILE];

  volatile  UINT nowWorkThread;
  volatile  UINT nowFileThread;
public:
  int       epollHandle;
  PEVENT    eventHandle;
  PSTACK_S  pOverlapStack;

public:
  GLdbIOCP();
  RESULT    InitGLdbIOCP();
  RESULT    FreeGLdbIOCP();
  RESULT    GetIOCPItem(ADDR &addr);
  RESULT    StartWork(PEVENT handle, UINT num);
  RESULT    StartFile(UINT num);
}IOCP, *PIOCP;


#endif   // GLdb_IOCP_HPP

