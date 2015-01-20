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
