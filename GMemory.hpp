/*
 * GLdb memory pool header file
 *
 * GLdb use static memory alloc, alloc huge memory by mmap() once,
 *   then alloc to application by memory pool.
 * This alloc is thread-safe, and every thread have local buffer for high
 *   performance. These local buffer store in TLS. One buffer alloc by one thread 
 *   can be free by another thread, and reuse by all threads.
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
 *
 */

#ifndef     GLdb_MEMORY_HPP
#define     GLdb_MEMORY_HPP

#include   "GCommon.hpp"

/*
 * The function get memroy from system
 */
#define     SEG_START_BUFFER                    (0x52LL << 40)  // 'R'

RESULT      GetMemory(ADDR &addr, UINT size, UINT flag = 0);
RESULT      GetStack(ADDR &addr);

/*
 * TLS for memory pool
 */
#define     GetThreadMemoryInfo()		\
  threadMemoryInfo *info;			\
  getThreadInfo(info, nowOffset);

/*
 * I have tested, three thread do nothing but GET/FREE, 
 * no performance improve after thread local cache large than 10.
 * unable to use STACK_S for no constructor will be called
 */
#define     MAX_LOCAL_CACHE                     15

typedef     struct threadMemoryInfo {
  STACK     memoryStack;
  ADDR      localCache [MAX_LOCAL_CACHE + 1];
  UINT      threadFlag;
  ADDR      localUsedList;                      // usedList start
  threadMemoryInfo *threadListNext;             // pointer to next TLS
}threadMemoryInfo;


/*
 * const for MARK memory alloc
 */
#define     MARK_USED_END                       (PLIST)0x30
#define     MARK_UNUSED                         (PLIST)0x28
#define     MARK_FREE_END                       (PLIST)0x20
#define     MARK_USED                           (PLIST)0x10
#define     MARK_MAX	                        (PLIST)0x100


/*
 * for NonDirectly free memory pool, link every used item by usedList
 * when used, countDown set to CurrentTime + Timeout. when FutureTime equal
 *   countDown, set countDown to TIMEOUT_QUIT
 */
#define     TIMEOUT_QUIT                        2
#define     TIMEOUT_TCP                         20
#define     INIT_REFCOUNT                       0x100001


/*
 * ATTENTION every CListItem, must set init value
 */
typedef     class CListItem {
public:
  PLIST     usedList;                           // MUST be != MARK_UNUSED
  UINT      countDown;                          // MUST be time() will free
  PBLOCK    allocType;                          // MUST be PBLOCK value
  UINT      refCount;                           // MUST be INIT_REFCOUNT, means 1 ref

  void      incRefCount(void)
  {
    LockInc(refCount);
  };
  RESULT    decRefCount(void);
}LIST;

#define     UsedList                             pList->usedList
#define     CountDown                            pList->countDown
#define     AllocType                            pList->allocType
#define     RefCount                             pList->refCount
#define     IncRefCount                          pList->incRefCount
#define     DecRefCount                          pList->decRefCount

// return 0 for is equal
typedef     RESULT(*FUNCCMP)(ADDR, ADDR);

typedef     class RListQuery {
private:
  LOCK      inProcess;
  ADDR      listStart;
  FUNCCMP   funcCmp;

public:
  void      InitListQuery(void)
  {
    inProcess = NOT_IN_PROCESS;
    listStart = ZERO;
    funcCmp = NULL;
  };
  void      SetFuncCmp(FUNCCMP funccmp)
  {
    funcCmp = funccmp;
  };
  void      operator += (ADDR list)
  {
    __LOCK(inProcess);
    list = listStart;
    listStart = list;
    __FREE(inProcess);
  };
  ADDR      operator == (ADDR addr)
  {
    ADDR nowlist = listStart;
    while (nowlist.aLong) {
      if (!(*funcCmp)(nowlist, addr)) break;
      nowlist = nowlist.UsedList;
    }
    return nowlist;
  };
  void     FreeListQuery(void)
  {
    ADDR   nowlist = listStart;
    ADDR   nextlist;
    while (nowlist.aLong) {
      nextlist = nowlist.UsedList;
      nowlist.DecRefCount();
      nowlist = nextlist;
    } 
  };
}LQUERY, *PLQUERY;


/*
 * Memory Pool main class
 *
 * address of free item is store in address pointed by memoryArrayFree
 *
 * if TimeoutInit = 0 means directly free.
 * if timeout in AddToUsed = 0, use TimeoutInit for timeout
 *
 * for 1 thread, one GET/FREE circle about 45ns for all local
 * for 4 thread, 4 circle in 4 core about 63ns for all local
 */
typedef     class CMemoryBlock : public RThreadResource {
private:                                        // for total memory
  ADDR      RealBlock;                          // address for memory start
  UINT      BorderSize;                         // real byte size for one item
  UINT      ArraySize;                          // number * sizeof(ADDR)
  UINT      TotalSize;                          // Total memory size in byte

private:                                        // for thread info
  UINT      TotalNumber;
  STACK     globalStack;
  threadMemoryInfo *threadListStart;            // TLS list start

private:
  UINT      TimeoutInit;
  UINT      BufferSize;

public:
  CMemoryBlock()
  : RThreadResource(sizeof(threadMemoryInfo))
  { 
    RealBlock = ZERO;
    TotalNumber = BorderSize = ArraySize = TotalSize = 0;
    threadListStart = 0;
#ifdef _TESTCOUNT
    GetCount = GetSuccessCount = FreeCount = FreeSuccessCount = 0;
#endif // _TESTCOUNT
  };

  RESULT     GetOneList(ADDR &nlist)
  {
    GetThreadMemoryInfo();

  __TRY
#ifdef _TESTCOUNT
    LockInc(GetCount);
#endif // _TESTCOUNT
    __DO_(info->memoryStack -= nlist,
	  "No more list CMemoryAlloc %p", this);
#ifdef _TESTCOUNT
    LockInc(GetSuccessCount);
#endif // _TESTCOUNT
  __CATCH
  };
  RESULT      FreeOneList(ADDR nlist)
  {
    GetThreadMemoryInfo();

  __TRY
#ifdef _TESTCOUNT
    LockInc(FreeCount);
#endif // _TESTCOUNT
    __DO_((nlist < MARK_MAX || nlist.UsedList == MARK_UNUSED),
          "FreeList Twice %p\n", nlist.pList);
    nlist.UsedList = MARK_UNUSED;               // mark for unsed too
    __DO_(info->memoryStack += nlist,
	"Free More\n");
#ifdef _TESTCOUNT
    LockInc(FreeSuccessCount);
#endif // _TESTCOUNT
  __CATCH
  };
  RESULT    CountTimeout(ADDR usedStart);

public:
  RESULT    SetThreadArea(UINT getsize, UINT maxsize,
			  UINT freesize, UINT flag);
  RESULT    InitMemoryBlock(UINT number, UINT size, 
			    UINT border, UINT timeout);
  RESULT    FreeMemoryBlock();

  RESULT    GetMemoryList(ADDR &addr, UINT timeout = 0)
  {
    GetThreadMemoryInfo();
  __TRY
    __DO_(info->memoryStack -= addr,
         "No more list CMemoryAlloc %p", this);
/*
 * need NOT lock, schedule thread will NOT change usedLocalStart, 
 * and NOT remove first node in UsedList, even countdowned.
 */
    if (TimeoutInit) {
      if (!timeout) timeout = TimeoutInit;
      addr.CountDown = GlobalTime + timeout;       // it is timeout time
      addr.UsedList = info->localUsedList.pList;
      info->localUsedList = addr.pList;
    }
    else  addr.UsedList = MARK_USED;
  __CATCH
  };
  RESULT    FreeMemoryList(ADDR addr)
  {
    GetThreadMemoryInfo();
  __TRY
    if (TimeoutInit) addr.CountDown = TIMEOUT_QUIT;
    else {
      __DO_((addr < MARK_MAX || addr.UsedList == MARK_UNUSED),
	    "FreeList Twice %p\n", addr.pList);
      addr.UsedList = MARK_UNUSED;               // mark for unsed too
      __DO_(info->memoryStack += addr,
	    "Free More\n");
    }
  __CATCH
  };

  RESULT    TimeoutAll(void);
  RESULT    GetNumber() { return TotalNumber; };
  
  void      DisplayFree(void);

#ifdef     _TESTCOUNT                           // for test function
public:                                         // statistics info for debug
  UINT      GetCount, GetSuccessCount;
  UINT      FreeCount, FreeSuccessCount;
  UINT      MinFree;  
  void      DisplayLocal(threadMemoryInfo* info);
  void      DisplayArray(void);
  void      DisplayInfo(void);
  void      DisplayContext(void);
#endif  // _TESTCOUNT
}BLOCK;


/*
 * IOCP struct, for CompleteKey
 */
typedef     class CContextItem {
public:
  //  ULONG_PTR completionKey;                      // user key for the socket
  int       bHandle;
  BOOL      inEpollOut;
  PEVENT    iocpHandle;                         // eventfd handle bind for user
  QUERY_S   readBuffer;
  QUERY_S   writeBuffer;
  
  // SOCKADDR  localSocket;
  // SOCKADDR  remoteSocket;
  // PCONT     pPeer;
  // LQUERY    nextPeer;
  // PBUFF     pBuffer;
  // LQUERY    nextBuffer;
}CONT;

/*
 * IOCP struct, for Overlapped
 */
#define     BUFFER_CLASS(classname, size)		        \
  typedef   class classname {					\
  public:							\
    WSAOVERLAPPED oLapped;					\
    WSABUF  wsaBuf;						\
    INT     nOper;						\
    STR_S   bufferName;						\
    UCHAR   padData[CHAR_SMALL];				\
    UCHAR   bufferData[size];					\
  }classname, *JOIN(P,classname);

BUFFER_CLASS(BUFF_0, 0)
BUFFER_CLASS(BUFF_S, SIZE_HUGE_PAGE-sizeof(BUFF_0)-SIZEADDR)
BUFFER_CLASS(BUFF_M, SIZE_HUGE_PAGE*16-sizeof(BUFF_0)-SIZEADDR)

#define     GlobalContext                       CMemoryAlloc::globalContext
#define     GlobalBufferSmall                   CMemoryAlloc::globalBufferSmall
#define     GlobalBufferMiddle                  CMemoryAlloc::globalBufferMiddle

/*
 * Global memory manager
 *
 * THere are only one instance of this class, in every program.
 * all memory pool class get memory from this.
 */
typedef     class CMemoryAlloc  {
public:
  static    CMemoryBlock globalContext;
  static    CMemoryBlock globalBufferSmall;
  static    CMemoryBlock globalBufferMiddle;

public:
  RESULT    InitMemoryBlock(UINT numcontext,
			    UINT numbuffersmall,
			    UINT numbermiddle)
  {
  __TRY
    __DO (globalContext.InitMemoryBlock
	 (numcontext, sizeof(CONT), SIZE_CACHE, TIMEOUT_TCP));
    __DO (globalBufferSmall.InitMemoryBlock
	 (numbuffersmall, sizeof(BUFF_S), SIZE_NORMAL_PAGE, ZERO));
    __DO (globalBufferMiddle.InitMemoryBlock
	 (numbermiddle, sizeof(BUFF_M), SIZE_NORMAL_PAGE, ZERO));
  __CATCH
  };
  RESULT    AppendMemoryBlock(BLOCK &block, UINT msize);
  RESULT    FrreeMemoryBlock()
  {
  __TRY
    __DO(globalContext.FreeMemoryBlock());
    __DO(globalBufferSmall.FreeMemoryBlock());
    __DO(globalBufferMiddle.FreeMemoryBlock());
  __CATCH
  };
}MEMORY;


RESULT      GetContext(ADDR &addr, UINT timeout = 0);
RESULT      FreeContext(ADDR addr);

#define     BUFFER_FUNCTION_DECLARE(name)			\
  RESULT    JOIN(Get,name)(ADDR &addr);				\
  RESULT    JOIN(Free,name)(ADDR addr);

BUFFER_FUNCTION_DECLARE(BufferSmall)
BUFFER_FUNCTION_DECLARE(BufferMiddle)


#endif   // GLdb_MEMORY_HPP
