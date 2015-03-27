/*
 * GLdb memory pool implement file
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
#include    "GMemory.hpp"

/*
 * In Linux, I get memory by mmap(), all memory are MAP_FIXED.
 * the top address match the border. 
 *
 * totalMemoryStart only be used here, so it is static var only, not for global.
 * I may add MAP_HUGETLB, should use HAGE PAGE if possible, for same so many cache.
 */
RESULT      GetMemory(
            ADDR &addr, UINT size, UINT flag) 
{
  static ADDR totalMemoryStart = {SEG_START_BUFFER};
  UINT      padsize;

__TRY
  padsize = PAD_INT(size, PAD_THREAD_STACK, SIZE_THREAD_STACK);
  addr = LockAdd(totalMemoryStart.aLong, padsize) + PAD_THREAD_STACK;
  addr = mmap (addr.pVoid, size, PROT_READ | PROT_WRITE,
	    MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED | flag, -1, 0);
  __DOe(addr == MAP_FAILED, 
	    GL_MEMORY_MMAPFAIL);
__CATCH
};

/*
 * typical stack is 14M, in every 16M border, the lowest 2M (PAD_THREAD_SIZE) is
 *   unmapped, then the low part is TLS, first 32k (SIZE_TRECE_INFO) for traceInfo, 
 *   then for RThreadResource used, 
 * it only alloc logical memory, which total size is 128T for userspace, 
 *   it is wasteless now.
 */
RESULT      GetStack(ADDR &stack) 
{
__TRY
  __DO (GetMemory(stack, SIZE_THREAD_STACK - PAD_THREAD_STACK, MAP_GROWSDOWN));
  __DOe((stack.aLong - PAD_THREAD_STACK) & (SIZE_THREAD_STACK - 1), 
            GL_MEMORY_NOBORDER);
__CATCH
};

/*
 * the GetMemoryList() and FreeMemoryList() use raw address, which means the start
 *   address of CListItem for free able memory pool.
 * and the allocType member of CListItem for free able, MUST point to is CMemroyBlock
 *   class, 
 *
 * LockDec return old val of refCount, after LockDec, refCount is bigger than nowref
 */
RESULT      CListItem::decRefCount(void)
{
  UINT      nowref = LockDec(refCount);
  if (nowref != INIT_REFCOUNT) return 0;

__TRY
  ADDR      addr;
  addr.pList = this;
  __DOe(allocType == 0,
            GL_LIST_NOTYPE);
  __DO(allocType->FreeMemoryList(addr));
__CATCH
};

/*
 * For every NON-directly free CMemoryBlock, CountTimeout will be called periodically
 *   by CMemroyAlloc.
 *
 * Every CMemoryBlock manage its own free, schedule thread call this to free NON-directly
 *   free memory pool. it read every TLS linked by threadListNext, which started by
 *   threadListStart. For every TLS, free the list from localUsedList if countdown.
 * But it do NOT free the first node in localUsedList, for schedule thread do NOT change
 *   TLS for other thread.
 *
 * CountTimeout() for one thread, while TimeoutAll() for one CMemroyBlock
 *
 * THIS FUNCTION HAVE NOT TEST COMPELETELY.
 */
RESULT      CMemoryBlock::CountTimeout(ADDR usedStart)
{
  static    volatile INT inCountDown = NOT_IN_PROCESS;
  ADDR      thisAddr, nextAddr;

__TRY
  __DO(__LOCK__TRY(inCountDown));
  if (usedStart > MARK_MAX) {
    thisAddr = usedStart;
    nextAddr = thisAddr.UsedList;
/*
 * countdown first node but not free
 */
    if (thisAddr.CountDown <= TIMEOUT_QUIT) {                   
      if (thisAddr.CountDown) {
	if (thisAddr.CountDown-- <= 0) {
	  thisAddr = thisAddr;                                  // only for cheat compiler
	  // DO close thisAddr.Handle
	  // and thisAddr.Handle = 0;
	} } }
    else if (thisAddr.CountDown < GlobalTime) thisAddr.CountDown = TIMEOUT_QUIT;

    while (nextAddr > MARK_MAX) {
      if (nextAddr.CountDown <= TIMEOUT_QUIT) {
	if (nextAddr.CountDown-- <= 0) {                        // maybe -1
	  thisAddr.UsedList = nextAddr.UsedList;
	  // DO close nextAddr.Handle
	  // and nextAddr.Handle = 0;
	  __DO (FreeOneList(nextAddr));
	} }
      else if (nextAddr.CountDown < GlobalTime) nextAddr.CountDown = TIMEOUT_QUIT;

// have do step one, no go next
      if (thisAddr.UsedList == nextAddr.pList) thisAddr = nextAddr;   
      nextAddr = thisAddr.UsedList;
    } }
    inCountDown = NOT_IN_PROCESS;
  __FREE(inCountDown);
__CATCH
};

/*
 * CountTimeout() counted for one thread
 * TimeoutAll counted one BLOCK for all thread
 */
RESULT      CMemoryBlock::TimeoutAll(void)
{
__TRY
  GlobalTime = time(NULL);
  PMINFO    list = threadListStart;
  while (list) {
    __DO (CountTimeout(list->localUsedList));
    list = list->threadListNext;
  }
__CATCH
};

/*
 * To initialize TLS field.
 *
 * Of course, thread MUST modified threadListStart to link threadListNext list.
 *   fortunately, it not in other TLS, it follow my habit.
 */
RESULT      CMemoryBlock::SetThreadArea(
            UINT getsize, UINT maxsize, UINT freesize, UINT flag)
{
  static LOCK lockList = NOT_IN_PROCESS;
  ADDR      start;

/*
 * THIS is a BUG for g++
 *
 * in default compile, without -O2, GetThreadMemoryInfo() will be compiled to following  
    mov    -0x28(%rbp),%rax
    mov    %rsp,%rax    --------  rax is useful, but overwrited
    and    $0xffffffffff000000,%rax
    add    (%rax),%rax  --------  this means add -0x28(%rbp), $0xffffffffff000000 & %rax
    mov    %rax,-0x8(%rbp)
 * whick rax is be used for lea of nowOffset, and val of sp, so crash
 * It cost me ten hours.
 */
  GetThreadMemoryInfo();
__TRY__
  start.pAddr = &(info->localCache[0]);
  info->memoryStack.InitArrayStack(
            start, maxsize, SINGLE_THREAD,
	    &globalStack, getsize, freesize);
  info->threadFlag = flag;
  info->localUsedList = MARK_USED_END;
  __LOCK(lockList);
  info->threadListNext = threadListStart;
  threadListStart = info;
  __FREE(lockList);
__CATCH__
};

/*
 * Initialize and full STACK 
 *
 * in one memory block, real data ahead, stack array at last.
 */
RESULT      CMemoryBlock::InitMemoryBlock(
            UINT number, UINT size, UINT border, UINT timeout)
{
__TRY
  BorderSize = PAD_INT(size, 0, border);
  ArraySize = PAD_INT(number * SIZEADDR, 0, border);
  TotalSize = BorderSize * number + ArraySize;

  __DO (GetMemory(RealBlock, TotalSize));
  TotalNumber = number;
  TimeoutInit = timeout;

  globalStack.InitArrayStack(RealBlock + (TotalSize - ArraySize), number);
  __DO (globalStack.FullArrayStack(RealBlock, BorderSize, number));
__CATCH
};

RESULT      CMemoryBlock::FreeMemoryBlock(void)
{
__TRY__
  munmap (RealBlock.pVoid, TotalSize);
__CATCH__
};

void        CMemoryBlock::DisplayFree(void)
{
  INT       freenumber = 0, num = 0;;
  PMINFO    list;
  PTINFO    info;
  PSTACK    stack;
  ADDR      addr;

  // mainEnd point to size+1, while localEnd point to size ??? surley ???
  list = threadListStart;
  while(list) {
    stack = &(list->memoryStack);
    addr.pVoid = list;
    addr.aLong = (addr.aLong & NEG_SIZE_THREAD_STACK) + PAD_THREAD_STACK;
    info = (PTINFO)addr.pVoid;
    num = stack->GetNumber();
    if (info->threadName)
      printf("%s:%2llx;  ", info->threadName, num);
    else 
      printf("%s:%2llx;  ", "MainThread", num);
    freenumber += num;
    list = list->threadListNext;
  }
  num = globalStack.GetNumber();
  printf("Main:%3lld;  Total:%3lld\n", num, freenumber + num);

#ifdef _TESTCOUNT
  printf("Get  :%10lld, Succ:%10lld\n", GetCount, GetSuccessCount);
  printf("Free :%10lld, Succ:%10lld\n", FreeCount, FreeSuccessCount);
#endif // _TESTCOUNT
};


/*
 * Global Memory Function be called
 *
 * GetContext & FreeContext use the address after CListItem
 * GetBufferxx & FreeBufferxx to same thing
 */

RESULT      InitContextItem(PCONT pcont)
{
__TRY__
  pcont->bHandle = 0;
  pcont->waitEpollOut = 0;
  pcont->iocpHandle = 0;
  pcont->completionKey = 0;
  pcont->readBuffer.InitQUERY_S();
  pcont->writeBuffer.InitQUERY_S();
__CATCH__
}

#ifdef    __DEBUG_CONTEXT
#define     DEBUG_CONTEXT_GET					\
  D(GetContext);Dp(pcont);Dn;
#define     DEBUG_CONTEXT_FREE					\
  D(FreeContext);Dp(pcont);Dn;
#else  // __DEBUG_CONTEXT
#define     DEBUG_CONTEXT_GET
#define     DEBUG_CONTEXT_FREE
#endif // __DEBUG_CONTEXT
/*
 * There are four field in CListItem, 
 * usedList & countDown is initialized in GetMemoryList()
 * allocType & refCount is initialized in GetContext()
 */
RESULT      GetContext(
            PCONT &pcont, UINT timeout)
{
__TRY
  ADDR      addr;
  __DO (GlobalContext.GetMemoryList(addr, timeout));
  addr.AllocType = &GlobalContext;
  addr.RefCount = INIT_REFCOUNT;
  pcont = ADDR_TO_PCONT(addr);
  DEBUG_CONTEXT_GET
  InitContextItem(pcont);
__CATCH
};

RESULT      GetDupContext(
	    PCONT &newcont, PCONT pcont, BOOL copy)
{
__TRY

  __DO (GetContext(newcont, 0));
  newcont->pProtocol = pcont->pProtocol;
  newcont->pApplication = pcont->pApplication;
  if (copy) {
/*
 * copy  both localSocket & remoteSocket, and localFilename, 
 */
    memcpy(&newcont->addInfo, &pcont->addInfo, sizeof(pcont->addInfo));
  }
__CATCH
};

RESULT      ReferenceContext(PCONT pcont)
{
  ADDR      addr;
  addr = PCONT_TO_ADDR(pcont);
  addr.IncRefCount();
  return 0;
};

RESULT      FreeContext(PCONT pcont)
{
  ADDR      addr;
  DEBUG_CONTEXT_FREE
  addr = PCONT_TO_ADDR(pcont);
  return addr.DecRefCount();
};

void        ReflushTimeout(
            PCONT pcont, UINT timeout)
{
  ADDR      addr;
  addr = PCONT_TO_ADDR(pcont);
  if (timeout) timeout = addr.AllocType->TimeoutInit;
  addr.CountDown = timeout;
};


#ifdef    __DEBUG_BUFFER
#define     DEBUG_BUFFER_GET					\
  D(GetBuffer);Dp(pbuff);Dn;TRACE
#define     DEBUG_BUFFER_FREE					\
  D(FreeBuffer);Dp(pbuff);Dn;
#else  // __DEBUG_BUFFER
#define     DEBUG_BUFFER_GET
#define     DEBUG_BUFFER_FREE
#endif // __DEBUG_BUFFER

#define     BUFFER_FUNCTION(name, tname)			\
  RESULT    JOIN(Init, name)(PBUFF pbuff)			\
  {								\
    JOIN(P, tname) psbuff = (JOIN(P, tname)) pbuff;		\
    pbuff->wsaBuf.len = JOIN(SIZE_, tname);			\
    pbuff->wsaBuf.buf = psbuff->bufferData;			\
    return (pbuff == NULL);					\
  };								\
  RESULT    JOIN(Get, name)(PBUFF &pbuff)			\
  {								\
  __TRY								\
    ADDR    addr;						\
    __DO(JOIN(Global, name).GetMemoryList(addr, 0));		\
    addr.AllocType = &JOIN(Global, name);		        \
    addr.RefCount = INIT_REFCOUNT;				\
    pbuff = ADDR_TO_PBUFF(addr);				\
    DEBUG_BUFFER_GET						\
    JOIN(Init, name)(pbuff);					\
  __CATCH						        \
  };								\
  RESULT    JOIN(Free,name)(PBUFF pbuff)			\
  {								\
    ADDR    addr;						\
    DEBUG_BUFFER_FREE						\
    addr = PBUFF_TO_ADDR(pbuff);				\
    return JOIN(Global, name).FreeMemoryList(addr);		\
  };

BUFFER_FUNCTION(BufferSmall, BUFF_S)
BUFFER_FUNCTION(BufferMiddle, BUFF_M)

RESULT      ReferenceBuffer(PBUFF pbuff)
{
  ADDR      addr;
  addr = PBUFF_TO_ADDR(pbuff);
  addr.IncRefCount();
  return 0;
};

RESULT      FreeBuffer(PBUFF pbuff)
{
  ADDR      addr;
  DEBUG_BUFFER_FREE
  addr = PBUFF_TO_ADDR(pbuff);
  return addr.DecRefCount();
};

#ifdef    __DEBUG_SIGN
#define     DEBUG_SIGN_GET					\
  D(GetSign);Dp(psign);Dn;TRACE;
#define     DEBUG_SIGN_FREE					\
  D(FreeSign);Dp(psign);Dn;TRACE;
#else  // __DEBUG_SIGN
#define     DEBUG_SIGN_GET
#define     DEBUG_SIGN_FREE
#endif // __DEBUG_SIGN

RESULT      GetSign(PSIGN &psign)
{
__TRY
  ADDR      addr;
  __DO (GlobalSign.GetMemoryList(addr));
  addr.AllocType = &GlobalSign;
  addr.RefCount = INIT_REFCOUNT;
  psign = ADDR_TO_PSIGN(addr);
  DEBUG_SIGN_GET
__CATCH
};

RESULT      FreeSign(PSIGN psign)
{
  ADDR      addr;
  DEBUG_SIGN_FREE
  addr = PSIGN_TO_ADDR(psign);
  return GlobalSign.FreeMemoryList(addr);
  //return addr.DecRefCount();
};

/*
 * Following is for debug memory pool program
 */
#ifdef _TESTCOUNT

#define PRINT_COLOR(p) printf("\e[0;%sm", p)
#define RESTORE_COLOR printf("\e[0;37m")

void        CMemoryBlock::DisplayLocal(PMEMINFO info)
{
  ADDR list;

  printf("free:%p, start:%p, end:%p\n", info->localFreeStart.pVoid,
	    info->localArrayStart.pVoid, info->localArrayEnd.pVoid);
  for (int i=0; i<MAX_LOCAL_CACHE; i++) {
    list.pAddr = &(info->localCache[i]);
    if (list == info->localFreeStart) PRINT_COLOR("36");
    if (list.pAddr->pVoid)
      printf("%p, %p\n", list.pVoid, list.pAddr->pVoid);
    else
      printf("%p, %p, (nil)\n", list.pVoid, list.pAddr->pVoid);
  }
  RESTORE_COLOR;
};

void        CMemoryBlock::DisplayArray(void)
{
  INT       i;
  threadMemoryInfo *list;

  printf("Array Start:%p, Free:%p, End%p\n", 
	    memoryArrayStart.pVoid, memoryArrayFree.pVoid, memoryArrayEnd.pVoid);
  ADDR      arr = memoryArrayStart;

  for (i=0; i<TotalNumber; i++) {
    if (arr == memoryArrayFree) PRINT_COLOR("36");
      printf("%p, %p\n", arr.pVoid, (*arr.pAddr).pVoid);
      arr += SIZEADDR;
  }
  RESTORE_COLOR;

  list = threadListStart;
  while(list) {
    printf("thread :%lld, ", i);
    DisplayLocal(list);
    list = list->threadListNext;
  }
  printf("\n");
};

void        CMemoryBlock::DisplayInfo(void)
{
  volatile  ADDR item;

  printf("Get  :%10lld, Succ:%10lld\n", GetCount, GetSuccessCount);
  printf("Free :%10lld, Succ:%10lld\n", FreeCount, FreeSuccessCount);
  printf("MinFree:%8lld,\n", MinFree);

  item.pList = RealBlock.pList;
  for (int i=0; i<TotalNumber; i++) {
    printf("%5d:%p, %p,\n", i, item.pList, item.pList->usedList.pVoid);
    item.aLong += BorderSize;
  }
};

void        CMemoryBlock::DisplayContext(void)
{
  INT       i = 0;
  ADDR      nlist;
  PMEMINFO  info;
  GlobalTime = time(NULL);

  info = threadListStart;
  while (info) {
    if (info->threadFlag & THREAD_FLAG_GETMEMORY) {
      nlist = info->localUsedList;
      printf("thread:%lld:->", i++);
      while (nlist > MARK_MAX) {
	printf("%p:%lld->", nlist.pVoid, nlist.CountDown);
	nlist = nlist.UsedList;
      }
      if (info->localUsedList.aLong) printf("\n");
    }
    info = info->threadListNext;
  }
};

#endif // _TESTCOUNT

