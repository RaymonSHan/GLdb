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
 * In GLdb, IOCP system assembled by three epoll handle and one buffered eventfd.
 *   Buffered eventfd means : eventfd work on EFD_SEMAPHORE mode with an array to
 *   storage value. The array is small, for when system is busy, should tell client
 *   explicitly, instead of make system more busy.
 * In GLdbIOCP, there are three thread wait for epoll accept, read & write.
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

#define     IOCPBaseAddress                     GLdbIOCP::iocpBaseAddress

int         isListeningSocket(HANDLE handle);


int         WSAStartup(WORD     wVersionRequested, 
		       LPWSADATA lpWSAData);
int         WSACleanup(void);
SOCKET      WSASocket(int       af, 
		      int       type, 
		      int       protocol,
		      LPWSAPROTOCOL_INFO  lpProtocolInfo, 
		      GROUP     g, 
		      DWORD     dwFlags);
/* If the ExistingCompletionPort parameter was a valid I/O completion port handle, 
   the return value is that same handle.*/
HANDLE      CreateIoCompletionPort(HANDLE       FileHandle,
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


/*
 * This is buffered eventfd
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
  ~RMultiEvent()
  {
    //    if (eventFd) close(eventFd);
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
  RESULT    ThreadClone(void)
  {
  __TRY
    ADDR    result = {0};
    if (ThreadStartEvent.eventFd) ThreadStartEvent -= result;
    else ThreadStartEvent.InitArrayEvent();
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
  static    int RThreadFunc(void* point)
  {
  __TRY
    RThread *thread = (RThread*) point;
    ADDR    result;
    result = thread->ThreadInit();
    __DO_(ThreadStartEvent += result, "Set GlobalEvent Error\n");
    __DO_(result.aLong, "Thread Initialize Error\n");
    while ((!thread->shouldQuit) && (!GlobalShouldQuit))
      __DOc_(thread->ThreadDoing(), "Thread Doing Error\n");
    __DO_(thread->ThreadFree(), "Thread Free Error\n");
  __CATCH
  };
  virtual   RESULT ThreadInit(void) = 0;
  virtual   RESULT ThreadDoing(void) = 0;
  virtual   RESULT ThreadFree(void) { return 0; };

}THREAD;


/*
 * The thread wait for epoll in GLdbIOCP
 * 
 * There are three thread work for epoll, each for accept, read, write
 * RThreadEpollAccept should be the last thread be init
 */
#define     NUMBER_MAX_EV                       20
#define     NUMBER_MAX_IOCP                     LIST_MIDDLE     // 63

#define     TIMEOUT_EPOLL_WAIT                  (1000*100)

#define     EVIN                                MAX(EPOLLIN, EPOLLOUT) + 1
#define     EVOUT                               MAX(EPOLLIN, EPOLLOUT) + 2

/*
 * Send OVERLAPPED to my IOCP handle, while Internal save CContextItem & InternalHigh
 *   save WSABUF
 */
typedef   __class_ (RThreadEpoll, RThread)
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
    overlapStack.FullArrayStack(addr, sizeof(OVERLAPPED));
  __CATCH
  };
  RESULT    ThreadDoing(void)
  {
  __TRY
    int     evNumber, i;
    ADDR    overlapaddr;
    LPOVERLAPPED &overlap = (LPOVERLAPPED &)overlapaddr;

    __DO1 (evNumber,
	   epoll_wait(epollHandle, waitEv, NUMBER_MAX_EV, TIMEOUT_EPOLL_WAIT));
    for (i = 0; i < evNumber; i++) {
      __DO (overlapStack -= overlapaddr)
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

typedef   __class_ (RThreadEv, RThread)
public:
  int       epollHandle;
  EVENT     evHandle;
  PSTACK_S  pOverlapStack;
public:
  RThreadEv()
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
    int     readed, writed;
    ADDR    contextaddr, overlapaddr, bufferaddr;
    LPOVERLAPPED &overlap = (LPOVERLAPPED &)overlapaddr;
    PCONT   &context = (PCONT &)contextaddr;
    LPWSABUF &buffer = (LPWSABUF &)bufferaddr; 

    __DO (evHandle -= overlapaddr);
    contextaddr = overlap->Internal;
    bufferaddr = overlap->InternalHigh;
    
    if (overlap->events == EPOLLIN) {
      if (bufferaddr == ZERO) {
	*pOverlapStack += bufferaddr;
	context->readBuffer -= overlapaddr;
	if (overlapaddr == ZERO) __BREAK_OK;
	bufferaddr = overlap->InternalHigh;
	overlap->events = EPOLLIN;              // Is it necessary ?
      }

      __DO1c(readed,
	     read(context->bHandle,
		  buffer->buf,
		  buffer->len));
      if (readed == EAGAIN) {
	context->readBuffer += overlapaddr;     // DO not control
      } else if (readed > 0) {
	overlap->doneSize = readed;
	__DO (*context->iocpHandle += overlapaddr);
      } else {
	// close socket
	// maybe merge to > 0 condition
      }
    }
    else if (overlap->events == EPOLLOUT) {
      __DO (context->writeBuffer.TryGet(overlapaddr));// MUST have return
      __DO1c(writed,
	     write(context->bHandle,
		   buffer->buf + overlap->doneSize,
		   buffer->len - overlap->doneSize));
      if (writed + overlap->doneSize == buffer->len) {
	context->writeBuffer -= overlapaddr;
	overlap->doneSize += writed;
	overlap->events = EPOLLOUT;             // Is it necessary ?
	__DO (*context->iocpHandle += overlapaddr);
      }
      else if (overlap->doneSize) {
	overlap->doneSize += writed;            // have added to epoll
      } else {
	overlap->doneSize += writed;
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
 *
 * HANDLE is 32bit, address is 64bit. 
 * high byte of IOCP handle is 'C', low three byte is offset of iocpHandle[i]
 *   to handleBaseAddress, in char size.
 */
#define     ADDR_TO_IOCPHANDLE(addr)		                \
  ((int)(addr - IOCPBaseAddress) + (int)('C'<<24))

#define     IOCPHANDLE_TO_ADDR(handle)		                \
  (IOCPBaseAddress + ((UINT)(handle & 0xffffff)))

typedef     class GLdbIOCP {
private:
  EVENT     iocpHandle[NUMBER_MAX_IOCP];
  QUERY_M   iocpHandleFree;
  QUERY_M   iocpHandleUsed;

  TEPOLL    threadEpoll;
public:
  static    ADDR iocpBaseAddress;

public:
  RESULT    InitGLdbIOCP()
  {
    int     i;
    ADDR    addr;
    for (i=0; i<NUMBER_MAX_IOCP; i++) {
      addr = (PVOID)&(iocpHandle[i]);
      iocpHandleFree += addr;
    }
    iocpBaseAddress = (PVOID)&(iocpHandle[0]);
    return 0;
  };
  RESULT    FreeGLdbIOCP()
  {
    ADDR    addr;
    PEVENT  pevent;
    while (!(iocpHandleUsed -= addr)) {
      pevent = (PEVENT)(addr.pVoid);
      pevent->FreeArrayEvent();
    }
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

