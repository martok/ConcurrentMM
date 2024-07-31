# ConcurrentMM

Memory manager for FreePascal, specially designed for highly parallel applications.

## Usage

To enable this memory manager, add it as the first item of the `uses` section of the project's LPR file, like this:

```pas
uses
  ConcMM,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF} 
  // ...
```

Configuration can be done by having a file called `ConcMM_User.inc` in the include file search path (`-Fi`) and removing the
dot before `{.$Include ConcMM_User.inc}` in `ConcMM.pas`. This makes it easy to have project-specific configuration without
a full copy of ConcurrentMM.

## About

This memory manager is designed to be very efficient in parallel / multi-threaded applications, even when many threads are running.
To do this, independent Pools are used with a compile-time concurrency degree. A higher degree of concurrency will result in more Pools being used (hence, more memory allocated), but the likelihood of an operation having to wait for a lock decreases dramatically.

Unlike some other memory manager replacements, ConcurrentMM doesn't include fancy fast `Move()` procedures. For the most part, the native versions provided by FreePascal are very good and used modern hardware (including AVX registers and aligned accesses) well.

## Memory Layout

Memory fragmentation (a problem of FreePascal's internal heap manager) is avoided by allocating memory in three allocation classes: *Huge*, *Small* and *Quantum* allocations (originally there were plans for *Large* allocations, but this was found to be unneccessary).

The first boundary is the OS's page allocation granularity. On Windows, this is 64K (16 pages) - smaller VirtualAlloc requests still reserve 64K and lead to strong page table fragmentation.


Huge allocations (>8192B) are directly handled as OS pages with a 32byte header. Therefore, memory allocated in these pools is aligned correctly for AVX512 use.

Small allocations (16..8192B) are handled from a series of slices of increasing size. Each slice maintains a linked list of memory regions containing between 1 and (nominally) 255 blocks of the step size, optimized to use pages fully. Reallocations are avoided if possible and done by smart over-allocation if required. This speeds up common list and array operations.

Finally, allocations less than 16B are preferentially allocated in the Quantum area. This is a special range of memory of fixed size allocated for each pool. A fast bitmap is maintained for this allocation arena. If it is fully used, Small allocations are used instead. By default, this bitmap is one page in size, resulting in 32768 Quantum blocks or 512K per Pool. Memory in this region is never moved unless growing into Small or Huge allocations. This makes many common small objects such as TEnumerator implementations very fast.


## Configuration

```pas
const
  CMM_DEBUG = false;
  {Enable runtime tests, assertions etc.}

  CMM_CONCURRENCY_POT = 2;
  {Degree of concurrency, as a power-of-two, i.e. 1 = 2 pools, 2 = 4 pools, ...}

  CMM_RELEASE_CLEAR = false;
  {Overwrite memory regions with zero before freeing}

  CMM_RELEASE_TRASH = CMM_DEBUG and not CMM_RELEASE_CLEAR;
  {Overwrite memory regions with $DEADBEEF before releasing}

  CMM_ASYNC_PAGE_RELEASE = true;
  {Instead of releasing memory to the OS immediately, add regions to a list that is
   processed asynchronously by a thread. This can greatly accelerate applications making frequent
   Huge allocations, as VirtualFree can block for long times.
  }

  CMM_ASYNC_PAGE_RELEASE_INTERVAL = 100;
  {When using async releasing, process the list every this many milliseconds. May be reduced to reduce
   memory overhead. Increasing it does not give useful benefits.
  }
```




