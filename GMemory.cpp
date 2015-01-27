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
 */
RESULT      GetMemory(ADDR &addr, UINT size, UINT flag) 
{
  static ADDR totalMemoryStart = {SEG_START_BUFFER};
  UINT      padsize;

__TRY
  padsize = PAD_INT(size, PAD_THREAD_STACK, SIZE_THREAD_STACK);
  addr = LockAdd(totalMemoryStart.aLong, padsize) + PAD_THREAD_STACK;
  addr = mmap (addr.pVoid, size, PROT_READ | PROT_WRITE,
	       MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED | flag, -1, 0);
  __DO_(addr == MAP_FAILED, "Get memory error, size:0x%llx\n", size);
__CATCH
};

RESULT      GetStack(ADDR &stack) 
{
__TRY
  __DO (GetMemory(stack, SIZE_THREAD_STACK - PAD_THREAD_STACK, MAP_GROWSDOWN));
  __DO_((stack.aLong - PAD_THREAD_STACK) & (SIZE_THREAD_STACK - 1), 
       "Error stack place:%p", stack.pVoid);
__CATCH
};

RESULT      CListItem::decRefCount(void)
{
  UINT      nowref = LockDec(refCount);         // old val return
  if (nowref != INIT_REFCOUNT) return 0;

__TRY
  ADDR      addr;
  addr.pList = this;
  if (allocType) __DO(allocType->FreeMemoryList(addr));
__CATCH
};


// process from CMemoryAlloc::UsedItem or threadMemoryInfo::usedListStart
// it will not change the para, and do NOT remove first node even countdowned.
RESULT      CMemoryBlock::CountTimeout(ADDR usedStart)
{
  static    volatile INT inCountDown = NOT_IN_PROCESS;
  ADDR      thisAddr, nextAddr;

__TRY
  __DO(__LOCK__TRY(inCountDown));
  if (usedStart > MARK_MAX) {
    thisAddr = usedStart;
    nextAddr = thisAddr.UsedList;
// countdown first node but not free
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
	  thisAddr.UsedList = nextAddr.UsedList;                // step one
	  // DO close nextAddr.Handle
	  // and nextAddr.Handle = 0;
	  __DO (FreeOneList(nextAddr));
	} }
      else if (nextAddr.CountDown < GlobalTime) nextAddr.CountDown = TIMEOUT_QUIT;

// have do step one, no go next
      if (thisAddr.UsedList == nextAddr.pList) thisAddr = nextAddr;   
      nextAddr = thisAddr.UsedList;
    } }
  //    inCountDown = NOT_IN_PROCESS;
  __FREE(inCountDown);
__CATCH
};

RESULT      CMemoryBlock::SetThreadArea(UINT getsize, UINT maxsize, UINT freesize, UINT flag)
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
  start.pAddr = &(info->localCache[MAX_LOCAL_CACHE - maxsize]);
  //start.pAddr = &(info->localCache[0]);
  info->memoryStack.InitArrayStack(start, maxsize, SINGLE_THREAD,
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
 * in one memory block, real data ahead, stack array at last.
 */
RESULT      CMemoryBlock::InitMemoryBlock(UINT number, UINT size, UINT border, UINT timeout)
{
__TRY
  BorderSize = PAD_INT(size, 0, border);
  ArraySize = number * SIZEADDR;
  TotalSize = BorderSize * number + ArraySize;

  __DO (GetMemory(RealBlock, TotalSize));
  TotalNumber = number;
  TimeoutInit = timeout;

  globalStack.InitArrayStack(RealBlock + (TotalSize - ArraySize), number);
  __DO (globalStack.FullArrayStack(RealBlock, BorderSize));
__CATCH
};

RESULT      CMemoryBlock::FreeMemoryBlock(void)
{
__TRY__
  printf("in munmap() \n");
  munmap (RealBlock.pVoid, TotalSize);
__CATCH__
};

RESULT      CMemoryBlock::TimeoutAll(void)
{
__TRY
  GlobalTime = time(NULL);
  threadMemoryInfo *list = threadListStart;
  while (list) {
    __DO (CountTimeout(list->localUsedList));
    list = list->threadListNext;
  }
__CATCH
};

void        CMemoryBlock::DisplayFree(void)
{
  INT       freenumber = 0, num = 0;;
  threadMemoryInfo *list;
  threadTraceInfo* info;
  PSTACK    stack;
  ADDR      addr;
  INT       i = 0;

  // mainEnd point to size+1, while localEnd point to size ??? surley ???
  list = threadListStart;
  while(list) {
    stack = &(list->memoryStack);
    addr.pVoid = list;
    addr.aLong = (addr.aLong & NEG_SIZE_THREAD_STACK) + PAD_THREAD_STACK;
    info = (threadTraceInfo*)addr.pVoid;
    num = stack->GetNumber();
    if (info->threadName)
      printf("Id:%2lld:%20s:%3llx;    ", i++, info->threadName, num);
    else 
      printf("Id:%2lld:%20s:%3llx;    ", i++, "Unkonwn thread", num);
    freenumber += num;
    list = list->threadListNext;
    if (!(i % 3)) printf("\n");
  }
  if (i % 3) printf("\n");

  num = globalStack.GetNumber();
  printf("Main Free:%4lld, Total Free:%4lld\n", num, freenumber + num);

#ifdef _TESTCOUNT
  printf("Get  :%10lld, Succ:%10lld\n", GetCount, GetSuccessCount);
  printf("Free :%10lld, Succ:%10lld\n", FreeCount, FreeSuccessCount);
#endif // _TESTCOUNT
};

/*
 * Global Memory Function be called
 */
RESULT      GetContext(ADDR &addr, UINT timeout)
{
__TRY
  __DO (GlobalContext.GetMemoryList(addr, timeout));
  addr.AllocType = &GlobalContext;
  addr.RefCount = INIT_REFCOUNT;
__CATCH
};

RESULT      FreeContext(ADDR addr)
{
  return GlobalContext.FreeMemoryList(addr);
};

#define     BUFFER_FUNCTION(name)				\
  RESULT    JOIN(Get,name)(ADDR &addr)				\
  {								\
  __TRY								\
    __DO(JOIN(Global,name).GetMemoryList(addr, 0));		\
    addr.AllocType = &JOIN(Global,name);		        \
    addr.RefCount = INIT_REFCOUNT;				\
 __CATCH						        \
   };								\
  RESULT    JOIN(Free,name)(ADDR addr)				\
  {								\
    return JOIN(Global,name).FreeMemoryList(addr);		\
  }

BUFFER_FUNCTION(BufferSmall)
BUFFER_FUNCTION(BufferMiddle)


#ifdef _TESTCOUNT

#define PRINT_COLOR(p) printf("\e[0;%sm", p)
#define RESTORE_COLOR printf("\e[0;37m")

void        CMemoryBlock::DisplayLocal(threadMemoryInfo* info)
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
  INT   i;
  threadMemoryInfo *list;
  printf("Array Start:%p, Free:%p, End%p\n", 
	 memoryArrayStart.pVoid, memoryArrayFree.pVoid, memoryArrayEnd.pVoid);
  ADDR arr = memoryArrayStart;

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
  printf("Get  :%10lld, Succ:%10lld\n", GetCount, GetSuccessCount);
  printf("Free :%10lld, Succ:%10lld\n", FreeCount, FreeSuccessCount);
  printf("MinFree:%8lld,\n", MinFree);

  volatile ADDR item;
    item.pList = RealBlock.pList;
    for (int i=0; i<TotalNumber; i++) {
      printf("%5d:%p, %p,\n", i, item.pList, item.pList->usedList.pVoid);
      item.aLong += BorderSize;
  }
};

void        CMemoryBlock::DisplayContext(void)
{
  INT   i = 0;
  ADDR  nlist;
  threadMemoryInfo *info;
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

