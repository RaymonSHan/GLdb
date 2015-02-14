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
 */

#ifndef     GLdb_MEMORY_HPP
#define     GLdb_MEMORY_HPP

#include    "GCommon.hpp"

/*
 * The function get memroy from system
 *
 * Just a start address, 0x52 is 'R'
 */
#define     SEG_START_BUFFER                    (0x52LL << 40)

RESULT      GetMemory(ADDR &addr, UINT size, UINT flag = 0);
RESULT      GetStack(ADDR &addr);


/*
 * any TLS except TraceInfo MUST be inherit from RThreadResource
 * volatile UINT RThreadResource::globalResourceOffset =
 *   PAD_TRACE_INFO + SIZE_TRACE_INFO;
 *   set in GLdbCommon.cpp
 *
 * The implement of these var in all class, is discribed in GLdbCommon.cpp 
 */
#define     GlobalResourceOffset                RThreadResource::globalResourceOffset
#define     GlobalTime                          RThreadResource::globalTime

class       RThreadResource 
{
protected:
  UINT      nowOffset;
public:
  static    volatile UINT globalResourceOffset;
  static    volatile UINT globalTime;

public:
  RThreadResource(UINT size) {
    nowOffset = LockAdd(RThreadResource::globalResourceOffset, 
			PAD_INT(size, 0, 64));
  };
};

/*
 * get TLS for RThreadResource
 * for every thread offset start at PAD_THREAD_STACK + SIZE_TRACE_INFO
 *
 * In my program, every memory pool have its nowOffset, different nowOffset
 *   means different memory pool.
 * nowOffset is border by 16M, which is SIZE_THREAD_STACK
 */
#define     getThreadInfo(info, off)				\
  asm volatile ("movq %%rsp, %0;"				\
		"andq %2, %0;"					\
		"addq %1, %0;"					\
		: "=r" (info)					\
		: "m" (off), "i"(NEG_SIZE_THREAD_STACK));

#define     GetThreadMemoryInfo()		                \
  threadMemoryInfo *info;					\
  getThreadInfo(info, nowOffset);

/*
 * I have tested, three thread do nothing but GET/FREE, 
 * no performance improve after thread local cache large than 10.
 * unable to use STACK_S for no constructor will be called
 *
 * For NON-directly free item, should be listed start by localUsedList,
 *   threadListNext pointed to NEXT thread, but SAME memory pool.
 */
#define     MAX_LOCAL_CACHE                     15

typedef     struct threadMemoryInfo {
  STACK     memoryStack;
  ADDR      localCache [MAX_LOCAL_CACHE + 1];
  UINT      threadFlag;
  ADDR      localUsedList;
  threadMemoryInfo *threadListNext;
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
 *
 * For summary, I write all timeout value here.
 */
#define     TIMEOUT_QUIT                        2
#define     TIMEOUT_TCP                         20
#define     TIMEOUT_INFINITE                    0x7fffffffffffffff
/*
 * free CListItem, only dec the refcount, it will be delete only no more reference, 
 */
#define     INIT_REFCOUNT                       0x100001

/*
 * Basic ahead unit for memory pool item.
 *
 * usedList  : list to another CListItem for many kind. 
 *             or be MARK_xx value for special use
 * countDown : for NON-directly free list only, 
 * allocType : pointer to its memory pool, it can be free only by its address.
 * refCount  : in fact, it avoid item be free, when other use it.
 *
 * in initialize :
 * usedList  : MUST be != MARK_USED.
 * countDown : MUST be the time should be free, or ZERO for directly free mode.
 * allocType : MUST be the memory pool alloc it.
 * refCount  : MUST be INIT_REFCOUNT, which means 1 reference.
 */
typedef     class CListItem {
public:
  PLIST     usedList;
  UINT      countDown;
  PBLOCK    allocType;
  UINT      refCount;

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

void        ReflushTimeout(PCONT pcont, UINT timeoutww);

/*
 * this is have not test
 *
 * FUNCCMP for compare two item, return 0 if equal
 */
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
 * There are two kind of memory pool
 * 1) free able
 * 2) no free
 *
 * for free able class, it can be directly free or NON-directly free.
 *   these item must have CListItem struct ahead.
 *   malloc frunction return the context address after CListItem, this address
 *   is also for free.
 * for no free class, no ClistItem ahead, it return raw address.
 *
 * RealBlock  : the start address by mmap(), 
 * BorderSize : item size pad to border.
 * TotalNumber: as its name.
 * globalStack: memory items will be alloced as stack, for most used item will be cached
 * ArraySize  : STACK only have pointer, the stack body is the first part of memory,
 *            : the size if TotalNumber * SIZEADDR, store in ArraySize.
 * TotalSize  : equal ArraySize pad to border plus TotalNumber * BorderSize.
 * TimeoutInit: default timeout value for this memory pool, 0 for directly free,
 *              if timeout in AddToUsed = 0, use TimeoutInit for timeout.
 * threadListStart : start of threadMemoryInfo::threadListNext.
 *
 * comment for function implement in .cpp, is in .cpp file.
 *
 * for 1 thread, one GET/FREE circle about 45ns for all local
 * for 4 thread, 4 circle in 4 core about 63ns for all local
 */
typedef     class CMemoryBlock : public RThreadResource {
private:
  ADDR      RealBlock;
  UINT      BorderSize;
  UINT      TotalNumber;
  UINT      ArraySize;
  UINT      TotalSize;
  STACK     globalStack;

public:
  UINT      TimeoutInit;
  threadMemoryInfo *threadListStart;

public:
/*
 * CMemoryBlock is a kind of RThreadResoucre, which provide TLS for thread local.
 * 
 * TLS for CMemoryBlock is threadMemoryInfo struct. 
 *   which store in stack memory of the thread. with offset set to nowOffset
 *   in RthreadResource.
 * every thread use it MUST call SetThreadArea() before it use TLS
 */
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

/*
 * GetOneList is public for NO-free class, 
 *   It only alloc memory without any initialize.
 */
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

private:
/*
 * FreeOneList is private, NO-free class will not call free at all.
 * free able class will call FreeMemoryList(), which use FreeOneList()
 * FreeOneList will set CListItem struct for mark
 */
  RESULT      FreeOneList(ADDR nlist)
  {
    GetThreadMemoryInfo();

  __TRY
#ifdef _TESTCOUNT
    LockInc(FreeCount);
#endif // _TESTCOUNT
    __DO_((nlist < MARK_MAX || nlist.UsedList == MARK_UNUSED),
          "FreeList Twice %p\n", nlist.pList);
    nlist.UsedList = MARK_UNUSED;
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

/*
 * GetMemoryList() is for free able class.
 * 
 * for directly free, it behavior like GetOneList(), only add MARK for usedList.
 *
 * for NON-directly free, it add the item to LIST. link by usedList.
 *   the start of list is localUsedList in TLS, change this value do NOT need lock,
 *   for the schedule thread will NOT change localUsedStart, and NOT remove 
 *   first node in UsedList, even countdowned.
 * In my habit, TLS only be changed by one thread.
 */
  RESULT    GetMemoryList(ADDR &addr, UINT timeout = 0)
  {
    GetThreadMemoryInfo();
  __TRY
    __DO_(info->memoryStack -= addr,
         "No more list CMemoryAlloc %p", this);
    if (TimeoutInit) {
      if (!timeout) timeout = TimeoutInit;
      addr.CountDown = GlobalTime + timeout;       // it is timeout time
      addr.UsedList = info->localUsedList.pList;
      info->localUsedList = addr.pList;
    }
    else  addr.UsedList = MARK_USED;
  __CATCH
  };
/*
 * for directly free, FreeMemoryList() really free it.
 *
 * for NON-directly free, it only MARK it to TIMEOUT_QUIT. 
 * the memory will be freed by schedule thread. 
 * If only one item in list, started by localUsedStart, it will NOT be free.
 */
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
 *
 * This struct should in kernal, but i implement it in userspace.
 * there are all information related I/O handle for IOCP
 *
 * 
 * dwFlags   : bit 0-7 same as Windows define. important WSA_FLAG_OVERLAPPED
 *             value is 0x01.
 *           : bit 10, WSA_FLAG_ISLISTEN, for whether is listneing socket.
 *           : bit 11, WSA_FLAG_ISACCEPT, for my AcceptEx use
 *
 * The field in GLdb_SELF_USE is used for GLdbDatabase and other application
 *   used __GLdbIOCP writen by myself. For all items associated to handle can 
 *   be together.
 */
typedef     class CContextItem {
public:
  int       bHandle;
  ULONG_PTR completionKey;                          // save user define key
  PEVENT    iocpHandle;                             // eventfd handle bind for user
  QUERY_S   readBuffer;
  QUERY_S   writeBuffer;
  BOOL      inEpollOut;
  DWORD     dwFlags;

#ifdef    __GLdb_SELF_USE
  PPROT     pProtocol;
  PAPP      pApplication;
  SOCKADDR  localSocket;
  SOCKADDR  remoteSocket;
  PCONT     pPeer;
  LQUERY    nextPeer;
  PBUFF     pBuffer;
  LQUERY    nextBuffer;
#endif // __GLdb_SELF_USE
}CONT;

/*
 * IOCP struct, for Overlapped
 *
 * the struct used for IOCP is OVERLAPPED, which defined in GCommon.hpp
 *
 * other member is used for GLdbAPPs, 
 * oLapped   : same as Windows use
 * wsaBuf    : InternalHigh in overlap pointer to this
 *             It pointer to bufferData, or padData if addition ahead.
 * nOper     : more information than evets in overlap
 * bufferName: name this buffer for search
 * padData   : for small encapsulation data added ahead real infomation
 * bufferData: Real information, for/from I/O
 */
typedef     class CBufferItem {
public:
  WSAOVERLAPPED oLapped;
  WSABUF    wsaBuf;
  UINT      nOper;
  STR_S     bufferName;
  UCHAR     padData[CHAR_SMALL];
}BUFF;

#define     BUFFER_CLASS(classname, size)		        \
  typedef   class classname : public CBufferItem {		\
  public:							\
    UCHAR   bufferData[size];					\
  }classname, *JOIN(P,classname);

#define     SIZE_SMALL_BUFFER                  (SIZE_HUGE_PAGE-sizeof(BUFF)-SIZEADDR)
#define     SIZE_MIDDLE_BUFFER                 (SIZE_HUGE_PAGE*16-sizeof(BUFF)-SIZEADDR)

BUFFER_CLASS(BUFF_S, SIZE_SMALL_BUFFER)
BUFFER_CLASS(BUFF_M, SIZE_MIDDLE_BUFFER)

#define     GlobalContext                       CMemoryAlloc::globalContext
#define     GlobalBufferSmall                   CMemoryAlloc::globalBufferSmall
#define     GlobalBufferMiddle                  CMemoryAlloc::globalBufferMiddle

RESULT      InitContextItem(PCONT pcont);
RESULT      GetContext(PCONT &pcont, UINT timeout = 0);
RESULT      GetDupContext(PCONT &newcont, PCONT pcont);
RESULT      ReferenceContext(PCONT pcont);
RESULT      FreeContext(PCONT addr);

#define     BUFFER_FUNCTION_DECLARE(name)			\
  RESULT    JOIN(Get,name)(PBUFF &pbuff);			\
  RESULT    JOIN(Free,name)(PBUFF pbuff);

BUFFER_FUNCTION_DECLARE(BufferSmall)
BUFFER_FUNCTION_DECLARE(BufferMiddle)

/*
 * Common free all size buffer
 */
RESULT      ReferenceBuffer(PBUFF pbuff);
RESULT      FreeBuffer(PBUFF pbuff);

/*
 * Global memory manager
 *
 * THere are only one instance of this class, in every program.
 * all memory pool class get memory from this.
 *
 * I quite like static memory alloc, for high performance.
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
	 (numcontext, sizeof(CONT), SIZE_CACHE, ZERO));
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
  RESULT    InitThreadMemory(UINT need)
  {
    UINT    getsize = 0, maxsize = 8, freesize = 4;
    if (need) getsize = 4;
    GlobalContext.SetThreadArea(getsize, maxsize, freesize, 0);
    GlobalBufferSmall.SetThreadArea(getsize, maxsize, freesize, 0);
    GlobalBufferMiddle.SetThreadArea(getsize, maxsize, freesize, 0);
    return 0;
  };
/*
 * should add Timeout, to CountTimeout all memory pool
 */
}MEMORY;

#endif   // GLdb_MEMORY_HPP
