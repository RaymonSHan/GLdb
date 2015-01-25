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

UINT        GetMemory(ADDR &addr, UINT size, UINT flag = 0);
UINT        GetStack(ADDR &addr);

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


#define     INIT_REFCOUNT                       0x100001
/*
 * ATTENTION every CListItem, must set init value
 */
typedef     class CListItem {
public:
  PLIST     usedList;                           // MUST be != MARK_UNUSED
  UINT      countDown;                          // MUST be time() will free
  PALLOC    allocType;                          // MUST be PALLOC value
  UINT      refCount;                           // MUST be INIT_REFCOUNT, means 1 ref

  void      IncRefCount(void)
  {
    LockInc(refCount);
  };
  UINT      DecRefCount(void);
}LIST;

#define     UsedList                             pList->usedList
#define     CountDown                            pList->countDown
#define     AllocType                            pList->allocType
#define     RefCount                             pList->refCount


// return 0 for is equal
typedef     UINT(*FUNCCMP)(PLIST, ADDR);

typedef     class RListQuery {
private:
  LOCK      inProcess;
  PLIST     listStart;
  FUNCCMP   funcCmp;

public:
  void InitListQuery(void)
  {
    inProcess = NOT_IN_PROCESS;
    listStart = NULL;
    funcCmp = NULL;
  };
  void      SetFuncCmp(FUNCCMP funccmp)
  {
    funcCmp = funccmp;
  };
  void      operator += (PLIST list)
  {
    __LOCK(inProcess);
    list->usedList = listStart;
    listStart = list;
    __FREE(inProcess);
  };
  PLIST     operator == (ADDR addr)
  {
    PLIST nowlist = listStart;
    while (nowlist) {
      if (!(*funcCmp)(nowlist, addr)) break;
      nowlist = nowlist->usedList;
    }
    return nowlist;
  };
  void FreeListQuery(void)
  {
    PLIST   nowlist = listStart;
    PLIST   nextlist;
    while (nowlist) {
      nextlist = nowlist->usedList;
      nowlist-> DecRefCount();
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
 */

#define     TIMEOUT_QUIT                        2

typedef     class CMemoryAlloc : public RThreadResource {
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
  CMemoryAlloc()
  : RThreadResource(sizeof(threadMemoryInfo))
  { 
    RealBlock = (UINT)0;
    TotalNumber = BorderSize = ArraySize = TotalSize = 0;
    threadListStart = 0;

#ifdef _TESTCOUNT
    GetCount = GetSuccessCount = FreeCount = FreeSuccessCount = 0;
#endif // _TESTCOUNT
  };
  ~CMemoryAlloc()
  {
    DelMemoryBuffer();
  };

private:
  UINT       GetOneList(ADDR &nlist)
  {
    GetThreadMemoryInfo();

  __TRY
#ifdef _TESTCOUNT
    LockInc(GetCount);
#endif // _TESTCOUNT
    __DO_(info->memoryStack -= nlist,
	  "No more list CMemoryAlloc %p", this);
    nlist.UsedList = MARK_USED;
    nlist.CountDown = /*should set*/ 0;
    nlist.AllocType = this;
    nlist.RefCount = INIT_REFCOUNT;

#ifdef _TESTCOUNT
    LockInc(GetSuccessCount);
#endif // _TESTCOUNT
   
  __CATCH
  };

  UINT        FreeOneList(ADDR nlist)
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
  UINT      AddToUsed(ADDR nlist, UINT timeout);
  UINT      GetListGroup(ADDR &groupbegin, UINT number);
  UINT      FreeListGroup(ADDR &groupbegin, UINT number);
  UINT      CountTimeout(ADDR usedStart);

public:
  UINT      SetThreadArea(UINT getsize, UINT maxsize,
			  UINT freesize, UINT flag);
  UINT      SetMemoryBuffer(UINT number, UINT size, 
			    UINT border, UINT timeout);
  UINT      DelMemoryBuffer(void);

  UINT        GetMemoryList(ADDR &nlist, UINT timeout = 0)
  {
    GetThreadMemoryInfo();
  __TRY
  //  __DO (GetOneList(nlist))
    __DO_(info->memoryStack -= nlist,
         "No more list CMemoryAlloc %p", this);
    nlist.UsedList = MARK_USED;
    nlist.CountDown = /*should set*/ 0;
    nlist.AllocType = this;
    nlist.RefCount = INIT_REFCOUNT;

    if (TimeoutInit) AddToUsed(nlist, timeout);
  __CATCH
  };

  UINT        FreeMemoryList(ADDR nlist)
  {
    GetThreadMemoryInfo();
  __TRY
    if (TimeoutInit) nlist.CountDown = TIMEOUT_QUIT;
    else {
      __DO_((nlist < MARK_MAX || nlist.UsedList == MARK_UNUSED),
	    "FreeList Twice %p\n", nlist.pList);
      nlist.UsedList = MARK_UNUSED;               // mark for unsed too
      __DO_(info->memoryStack += nlist,
	    "Free More\n");
    }
    //__DO(FreeOneList(nlist))
  __CATCH
  };


  UINT      TimeoutAll(void);
  UINT      GetNumber() { return TotalNumber; };
  
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
}ALLOC;


/*
 * for NonDirectly free memory pool, link every used item by usedList
 * when used, countDown set to CurrentTime + Timeout. when FutureTime equal
 *   countDown, set countDown to TIMEOUT_QUIT
 */


typedef     class CContextItem : public CListItem
{
public:
  int       bHandle;
  SOCKADDR  localSocket;
  SOCKADDR  remoteSocket;
  PCONT     pPeer;
  PCONT     nextPeer;
  PBUFF     pBuffer;
}CONT;

typedef     class CBufferItem : public CListItem
{
public:
  INT       nSize;
  INT       nOper;
  PUCHAR    realStart;
  PBUFF     nextBuffer;
  STR_S     bufferName;
  UCHAR     padData[CHAR_SMALL]; 
  UCHAR     bufferData[]; 
}BUFF;



#endif   // GLdb_MEMORY_HPP
