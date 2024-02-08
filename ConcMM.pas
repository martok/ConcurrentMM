unit ConcMM;

{$mode objfpc}

interface

{$Include ConcMM_User.inc}

{$Include ConcMM_Defaults.inc}

implementation

uses
  ConcMM_Sys;

const
  PAGE                          = 4096;
  PAGE_LOW_MASK                 = PAGE - 1;
  PAGE_BITS                     = PAGE * 8;
  CONCURRENCY                   = 1 << CMM_CONCURRENCY_POT;

  HFLAG_USED                    = 1 << 0;
  HFLAG_IS_SMALL                = 1 << 1;
  HFLAG_IS_LARGE                = 1 << 2;
  HFLAG_IS_HUGE                 = 1 << 3;

  QUANTUM_SIZE                  = 16;
  QUANTUM_COUNT                 = PAGE_BITS * 1;

  SMALL_MIN                     = QUANTUM_SIZE * 2;
  SMALL_SPLIT                   = 512;
  SMALL_MAX                     = PAGE * 2;
  SMALL_STEP_1                  = 16;
  SMALL_STEP_2                  = 256;
  SMALL_SLICE_COUNT             = (SMALL_SPLIT - SMALL_MIN) div SMALL_STEP_1 +
                                  (SMALL_MAX - SMALL_SPLIT) div SMALL_STEP_2 + 1;
  SMALL_ALLOC_SIZE              = PAGE * 4;
  SMALL_MAX_BLOCKS              = 64;

type
  PBitmapBase = ^BitmapBase;
  PHugeHeader = ^THugeHeader;
  PAsyncFreeHeader = ^TAsyncFreeHeader;
  PPoolInstance = ^TPoolInstance;
  PPoolMaskPtr = ^PoolMaskPtr;
  PoolMaskPtr = type PtrUInt;
  BitmapBase = type PtrUInt;

  {$If QUANTUM_COUNT mod (sizeof(PtrUInt)*8) <> 0}{$Fatal Type alignment error}{$IfEnd}

  PQuantumElement = ^TQuantumElement;
  TQuantumElement = packed array[0..QUANTUM_SIZE - 1] of byte;
  TQuantumBitmap = packed array[0..(QUANTUM_COUNT div BitSizeOf(BitmapBase)) - 1] of BitmapBase;
  TQuantumAllocation = record
    Lock: TRTLCriticalSection;
    Free: PBitmapBase;
    Space: PQuantumElement;
    EndOfLast: Pointer;
  end;

  PPSmallSlice = ^PSmallSlice;
  PSmallSlice = ^TSmallSlice;
  PSmallHeader = ^TSmallHeader;

  TSmallSlice = record
    NextWithFree, PrevWithFree: PSmallSlice;
    BlocksInSlice: UInt32;
    BlocksFreeInSlice: UInt32;
    __pad: array[0..32 - 2 * Sizeof(Pointer) - 2*Sizeof(UInt32) - 1] of Byte;
    { TSmallHeader[] follows }
  end;
  {$If SizeOf(TSmallSlice) mod 32 <> 0}{$Fatal Alignment error}{$IfEnd}

  TSmallHeader = record
    __pad: array[0..16 - 2 * SizeOf(Byte) - SizeOf(Word) - SizeOf(PoolMaskPtr) - 1] of byte;
    AllocIndex: Byte;
    IndexInSlice: Byte;
    UserSize: Word;
    OwnerPoolAndFlags: PoolMaskPtr;
  end;
  {$If SizeOf(TSmallHeader) mod 16 <> 0}{$Fatal Alignment error}{$IfEnd}

  TSmallAllocation = record
    Lock: TRTLCriticalSection;
    SliceMaxSize: array[0..SMALL_SLICE_COUNT - 1] of PtrUInt;
    SliceFreeList: array[0..SMALL_SLICE_COUNT - 1] of PSmallSlice;
  end;

  THugeHeader = record
    __pad: array[0..32 - 2 * SizeOf(PtrUInt) - SizeOf(PoolMaskPtr) - 1] of byte;
    AllocSize: PtrUInt;
    UserSize: PtrUInt;
    OwnerPoolAndFlags: PoolMaskPtr;
  end;
  {$If SizeOf(THugeHeader) mod 32 <> 0}{$Fatal Alignment error}{$IfEnd}

  TAsyncFreeHeader = record
    Next: PAsyncFreeHeader;
    AllocSize: PtrUInt;
  end;

  TPoolInstance = record
    QuantumAlloc: TQuantumAllocation;
    SmallAlloc: TSmallAllocation;
    AsyncFreeList: PAsyncFreeHeader;

    Status: record
      TotalUsed,
      TotalAllocated: PtrUInt;
    end;
  end;

var
  gPagePools: array[0 .. CONCURRENCY - 1] of PPoolInstance;
threadvar
  gThreadPagePool: PPoolInstance;


// Forward declarations

function CMMMemSize(p: pointer): ptruint; forward;
function CMMGetMem(Size: ptruint): Pointer; forward;
function SmallSliceAlloc(const pool: PPoolInstance; const TargetSize: PtrUInt; const allocIndex: NativeUInt): PSmallSlice; forward;
function HugeAlloc(const Size: PtrUInt; const OverAllocHint: PtrUInt = 0): Pointer; forward;
{$If CMM_ASYNC_PAGE_RELEASE}
procedure AsyncReleaseAdd(const pool: PPoolInstance; const StartOfPage: Pointer; const Size: PtrUInt); forward;
{$IfEnd}

// Data structures

function NextMultipleOf(const value: PtrUInt; const base: PtrUInt): PtrUInt; inline;
begin
  Result:= ((value + base - 1) div base) * base;
end;

function NextPageSize(const value: PtrUInt): PtrUInt; inline;
begin
  Result:= (value + PAGE - 1) and -PAGE;
end;

function Max(const A, B: PtrUInt): PtrUInt; inline;
begin
  if A > B then
    Exit(A);
  Result:= B;
end;

function Min(const A, B: PtrUInt): PtrUInt; inline;
begin
  if A < B then
    Exit(A);
  Result:= B;
end;

function RightAlign(const value: Pointer; const SizeNew: PtrUInt; const SizeOld: PtrUInt = 0): Pointer; inline;
begin
  Result:= Pointer(PByte(value) - SizeNew + SizeOld);
end;

procedure BitmapInitialize(const bitmap: PBitmapBase; const BitCount: integer);
begin
  FillChar(bitmap^, BitCount div 8, $FF);
end;

function BitmapFindFree(const bitmap: PBitmapBase; const BitCount: PtrUInt): integer;
var
  i, idx: Cardinal;
  cur: BitmapBase;
begin
  Assert(BitCount mod BitSizeOf(BitmapBase) = 0);

  Result:= -1;
  for i:= 0 to BitCount div BitSizeOf(BitmapBase) - 1 do begin
    cur:= bitmap[i];
    if cur = 0 then
      Continue;
    {$If SizeOf(BitmapBase) = SizeOf(DWord)}
    idx:= BsfDWord(cur);
    {$Else}
    idx:= BsfQWord(cur);
    {$EndIf}
    Exit(i * BitSizeOf(BitmapBase) + idx);
  end;
end;

function BitmapIsFree(const bitmap: PBitmapBase; const Index: PtrUInt): boolean;
var
  subi, bi: Integer;
  mask: BitmapBase;
begin
  subi:= Index div BitSizeOf(BitmapBase);
  bi:= Index mod BitSizeOf(BitmapBase);
  mask:= BitmapBase(1) << bi;
  Result:= bitmap[subi] and mask = 1;
end;

procedure BitmapMarkFree(const bitmap: PBitmapBase; const Index: PtrUInt);
var
  subi, bi: Integer;
  mask: BitmapBase;
begin
  subi:= Index div BitSizeOf(BitmapBase);
  bi:= Index mod BitSizeOf(BitmapBase);
  mask:= BitmapBase(1) << bi;
  Assert(bitmap[subi] and mask = 0);
  bitmap[subi]:= bitmap[subi] or mask;
end;

procedure BitmapMarkUsed(const bitmap: PBitmapBase; const Index: PtrUInt);
var
  subi, bi: Integer;
  mask: BitmapBase;
begin
  subi:= Index div BitSizeOf(BitmapBase);
  bi:= Index mod BitSizeOf(BitmapBase);
  mask:= BitmapBase(1) << bi;
  Assert(bitmap[subi] and mask <> 0);
  bitmap[subi]:= bitmap[subi] and not mask;
end;

procedure EnterCriticalSection(var cs: TRTLCriticalSection);
begin
  if System.TryEnterCriticalSection(cs) <> 0 then
    Exit;
  ThreadSwitch;
  if System.TryEnterCriticalSection(cs) <> 0 then
    Exit;
  System.EnterCriticalSection(cs);
end;

function InterlockedAdd(var Target: PtrUInt; Value: PtrUInt): PtrUInt; inline;
begin
{$IfDef CPU64}
  Result:= InterlockedExchangeAdd64(Target, Value);
{$Else}
  Result:= InterlockedExchangeAdd(Target, Value);
{$EndIf}
end;

function InterlockedSub(var Target: PtrUInt; Value: PtrUInt): PtrUInt; inline;
begin
{$IfDef CPU64}
  Result:= InterlockedExchangeAdd64(Target, PtrUInt(-Value));
{$Else}
  Result:= InterlockedExchangeAdd(Target, PtrUInt(-Value));
{$EndIf}
end;

// Generic MM

procedure MemoryRelocate(const Src: Pointer; const SrcSize: PtrUInt; const Dst: Pointer; const DstSize: PtrUInt); inline;
begin
  Move(Src^, Dst^, Min(SrcSize, DstSize));
end;

procedure MemoryClear(const P: Pointer; const size: PtrUInt); inline;
begin
  FillChar(P^, Size, 0);
end;

procedure MemoryTrash(const P: Pointer; const size: PtrUInt); inline;
var
  aligned, remainder: PtrUInt;
begin
  aligned:= size div sizeof(DWord);
  remainder:= size mod sizeof(DWord);
  FillDWord(P^, aligned, BEtoN($deadbeef));
  FillByte(PByte(PDWord(P) + aligned)^, remainder, $55);
end;

procedure MemoryBeforeRelease(const P: Pointer; const size: PtrUInt); inline;
begin
  if CMM_RELEASE_TRASH then
    MemoryTrash(P, size)
  else if CMM_RELEASE_CLEAR then
    MemoryClear(P, size);
end;

procedure MemoryFreeForPool(const P: Pointer; const size: PtrUInt; const pool: PPoolInstance); inline;
begin
  {$If CMM_ASYNC_PAGE_RELEASE}
    AsyncReleaseAdd(pool, P, size)
  {$Else}
    InterlockedExchangeAdd(pool^.Status.TotalAllocated, PtrUInt(-size));
    SysPageFree(P, size);
  {$IfEnd}
end;

// Pool

procedure PoolInitialize(var pool: PPoolInstance);
var
  sz: PtrUInt;
  i: Integer;
begin
  pool:= CMMSysPageAlloc(sizeof(TPoolInstance));
  Assert(PtrUInt(pool) and PAGE_LOW_MASK = 0);
  FillChar(pool^, SizeOf(pool), 0);
  // Quantum
  InitCriticalSection(pool^.QuantumAlloc.Lock);
  pool^.QuantumAlloc.Free:= CMMSysPageAlloc(sizeof(TQuantumBitmap));
  BitmapInitialize(PPtrUInt(pool^.QuantumAlloc.Free), QUANTUM_COUNT);
  pool^.QuantumAlloc.Space:= CMMSysPageAlloc(sizeof(TQuantumElement) * QUANTUM_COUNT);
  pool^.QuantumAlloc.EndOfLast:= PByte(pool^.QuantumAlloc.Space) + sizeof(TQuantumElement) * QUANTUM_COUNT;
  pool^.Status.TotalAllocated += sizeof(TQuantumElement) * QUANTUM_COUNT;
  // Small
  InitCriticalSection(pool^.SmallAlloc.Lock);
  sz:= SMALL_MIN;
  i:= 0;
  while sz <= SMALL_MAX do begin
    pool^.SmallAlloc.SliceMaxSize[i]:= sz;
    if sz < SMALL_SPLIT then
      inc(sz, SMALL_STEP_1)
    else
      inc(sz, SMALL_STEP_2);
    inc(i);
  end;
  Assert(pool^.SmallAlloc.SliceMaxSize[i-1] = SMALL_MAX);
  Assert(i = SMALL_SLICE_COUNT);
end;

procedure PoolFinalize(var pool: PPoolInstance);
begin
  // Small
  DoneCriticalSection(pool^.SmallAlloc.Lock);
  // Quantum
  CMMSysPageFree(pool^.QuantumAlloc.Free, sizeof(TQuantumBitmap));
  CMMSysPageFree(pool^.QuantumAlloc.Space, sizeof(TQuantumElement) * QUANTUM_COUNT);
  DoneCriticalSection(pool^.QuantumAlloc.Lock);
  CMMSysPageFree(pool, Sizeof(pool));
  pool:= nil;
end;

function PoolGetThreadTag: PtrUInt;
var
  tid: TThreadID;
begin
  tid:= GetCurrentThreadId;
  tid:= tid xor RorDWord(tid, 7);
  tid:= tid * 314159265;
  tid:= tid xor RorDWord(tid, 16);
  Result:= tid and (CONCURRENCY-1);
end;

function PoolGetThread: PPoolInstance;
begin
  if gThreadPagePool <> nil then
    Exit(gThreadPagePool);
  Result:= gPagePools[PoolGetThreadTag];
  gThreadPagePool:= Result;
end;

// Flags

function FlagsMakePtr(const pool: PPoolInstance; const flags: NativeUInt): PoolMaskPtr; inline;
begin
  Result:= PoolMaskPtr(pool) or (flags and PAGE_LOW_MASK);
end;

function FlagsSplitPtr(const PoolAndFlags: PoolMaskPtr; out flags: NativeUInt): PPoolInstance; inline;
begin
  flags:= PoolAndFlags and PAGE_LOW_MASK;
  Result:= PPoolInstance(PoolAndFlags and not PAGE_LOW_MASK);
end;

function FlagsSplitPtr(const PoolAndFlags: PoolMaskPtr): PPoolInstance; inline;
begin
  Result:= PPoolInstance(PoolAndFlags and not PAGE_LOW_MASK);
end;

function FlagsSplitPtrU(const UserPointer: Pointer; out flags: NativeUInt): PPoolInstance; inline;
begin
  Result:= FlagsSplitPtr((PPoolMaskPtr(UserPointer) - 1)^, flags);
end;

function FlagsAll(const flags, mask: NativeUInt): boolean; inline;
begin
  Result:= (flags and mask) = mask;
end;

function FlagsAny(const flags, mask: NativeUInt): boolean; inline;
begin
  Result:= (flags and mask) > 0;
end;

function FlagsUnusedPtr(const PoolAndFlags: PoolMaskPtr): boolean; inline;
begin
  Result:= (PoolAndFlags and HFLAG_USED) = 0;
end;

procedure FlagsSetPtr(var PoolAndFlags: PoolMaskPtr; const mask: NativeUInt); inline;
begin
  PoolAndFlags:= PoolAndFlags or (mask and PAGE_LOW_MASK);
end;

procedure FlagsClearPtr(var PoolAndFlags: PoolMaskPtr; const mask: NativeUInt); inline;
begin
  PoolAndFlags:= PoolAndFlags and not (mask and PAGE_LOW_MASK);
end;


// Quantum Allocation Unit

function QuantumAlloc: Pointer;
var
  pool: PPoolInstance;
  idx: Integer;
  bits: PBitmapBase;
begin
  Result:= nil;
  pool:= PoolGetThread;
  bits:= pool^.QuantumAlloc.Free;
  EnterCriticalSection(pool^.QuantumAlloc.Lock);
  idx:= BitmapFindFree(bits, QUANTUM_COUNT);
  if idx >= 0 then begin
    BitmapMarkUsed(bits, idx);
    Result:= @(pool^.QuantumAlloc.Space[idx]);
  end;
  LeaveCriticalSection(pool^.QuantumAlloc.Lock);
  InterlockedAdd(pool^.Status.TotalUsed, QUANTUM_SIZE);
end;

procedure QuantumFree(const pool: PPoolInstance; const index: Integer);
begin
  MemoryBeforeRelease(@(pool^.QuantumAlloc.Space[index]), QUANTUM_SIZE);
  // TODO: this could be lockfree with InterlockedExchange, waitfree with InterlockedAnd/Or
  EnterCriticalSection(pool^.QuantumAlloc.Lock);
  BitmapMarkFree(pool^.QuantumAlloc.Free, index);
  LeaveCriticalSection(pool^.QuantumAlloc.Lock);
  InterlockedSub(pool^.Status.TotalUsed, QUANTUM_SIZE);
end;

function QuantumIsInSpace(const P: Pointer; out pool: PPoolInstance; out Index: Integer): boolean;
var
  i: Integer;
  offset: PtrUInt;
  start: PQuantumElement;
begin
  Result:= false;
  for i:= 0 to high(gPagePools) do begin
    pool:= gPagePools[i];
    start:= pool^.QuantumAlloc.Space;
    if (P >= start) and (P < pool^.QuantumAlloc.EndOfLast) then begin
      offset:= PtrUInt(P) - PtrUInt(start);
      Index:= offset div QUANTUM_SIZE;
      Assert(Index < QUANTUM_COUNT);
      Assert(P = @pool^.QuantumAlloc.Space[Index]);
      Exit(true);
    end;
  end;
end;

// Small Allocation Unit

function SmallSliceAlloc(const pool: PPoolInstance; const TargetSize: PtrUInt; const allocIndex: NativeUInt): PSmallSlice;
var
  userSize, blockSize, available, alloc: PtrUInt;
  count: NativeUInt;
  headerArray: PSmallHeader;
  i: Integer;
begin
  userSize:= pool^.SmallAlloc.SliceMaxSize[allocIndex];
  blockSize:= Sizeof(TSmallHeader) + userSize;
  available:= TargetSize - SizeOf(TSmallSlice);
  // how many blocks fit in the requested slice size?
  count:= Min(SMALL_MAX_BLOCKS, Max(1, available div blockSize));
  // recalculate from optimized slice size
  alloc:= NextPageSize(SizeOf(TSmallSlice) + count * blockSize);
  available:= alloc - SizeOf(TSmallSlice);
  count:= available div blockSize;
  Assert(count >= 1);
  Assert(count < high(TSmallHeader.IndexInSlice));

  Result:= CMMSysPageAlloc(alloc);
  Result^.NextWithFree:= nil;
  Result^.PrevWithFree:= nil;
  Result^.BlocksInSlice:= count;
  Result^.BlocksFreeInSlice:= count;
  headerArray:= PSmallHeader(Result + 1);
  for i:= 0 to count - 1 do begin
    headerArray^.AllocIndex:= allocIndex;
    headerArray^.IndexInSlice:= i;
    headerArray^.OwnerPoolAndFlags:= FlagsMakePtr(pool, HFLAG_IS_SMALL);
    Inc(PByte(headerArray), blockSize);
  end;
  InterlockedAdd(pool^.Status.TotalAllocated, alloc);
end;

function SmallAlloc(const Size: PtrUInt; const OverAllocHint: PtrUInt = 0): Pointer;
var
  pool: PPoolInstance;
  listHead: PPSmallSlice;
  slice: PSmallSlice;
  headerArray, freeHeader: PSmallHeader;
  i, allocIdx: Integer;
  blockSize, allocSize: PtrUInt;
begin
  pool:= PoolGetThread;
  allocSize:= Max(Size, OverAllocHint);
  listHead:= nil;
  allocIdx:= 0;
  for i:= 0 to SMALL_SLICE_COUNT - 1 do begin
    if allocSize <= pool^.SmallAlloc.SliceMaxSize[i] then begin
      listHead:= @pool^.SmallAlloc.SliceFreeList[i];
      allocIdx:= i;
      break;
    end;
  end;
  Assert(listHead <> nil);

  slice:= nil;
  freeHeader:= nil;
  blockSize:= pool^.SmallAlloc.SliceMaxSize[allocIdx] + SizeOf(TSmallHeader);
  EnterCriticalSection(pool^.SmallAlloc.Lock);
  slice:= listHead^;

  if slice <> nil then begin
    // there should be a free block, find it
    headerArray:= PSmallHeader(slice + 1);
    for i:= 0 to slice^.BlocksInSlice - 1 do begin
      if FlagsUnusedPtr(headerArray^.OwnerPoolAndFlags) then begin
        freeHeader:= headerArray;
        break;
      end;
      Inc(PByte(headerArray), blockSize);
    end;
    Assert(freeHeader <> nil);
  end;

  if freeHeader = nil then begin
    // did not find one and had no currently free slice, need a full new one
    slice:= SmallSliceAlloc(pool, SMALL_ALLOC_SIZE, allocIdx);
    freeHeader:= PSmallHeader(slice + 1);
    // install it as the new first one (can't have PrevWithFree or it would have had free)
    listHead^:= slice;
  end;

  // now we have a header, mark it as used
  Dec(slice^.BlocksFreeInSlice);
  FlagsSetPtr(freeHeader^.OwnerPoolAndFlags, HFLAG_USED);
  freeHeader^.UserSize:= Size;
  Result:= Pointer(freeHeader + 1);

  // did this block just become full?
  if slice^.BlocksFreeInSlice = 0 then begin
    // move the head to the next in the chain, or nil
    listHead^:= slice^.NextWithFree;
    if listHead^ <> nil then
      listHead^^.PrevWithFree:= nil;
  end;
  LeaveCriticalSection(pool^.SmallAlloc.Lock);

  InterlockedAdd(pool^.Status.TotalUsed, Size);
end;

procedure SmallFree(P: Pointer);
var
  header: PSmallHeader;
  pool: PPoolInstance;
  slice: PSmallSlice;
  blockSize, sliceAlloc: PtrUInt;
  listHead: PPSmallSlice;
begin
  header:= PSmallHeader(RightAlign(P, SizeOf(TSmallHeader)));
  pool:= FlagsSplitPtr(header^.OwnerPoolAndFlags);
  blockSize:= pool^.SmallAlloc.SliceMaxSize[header^.AllocIndex] + SizeOf(TSmallHeader);

  slice:= PSmallSlice(PByte(header) - header^.IndexInSlice * blockSize) - 1;

  EnterCriticalSection(pool^.SmallAlloc.Lock);
  listHead:= @pool^.SmallAlloc.SliceFreeList[header^.AllocIndex];

  // unmark
  FlagsClearPtr(header^.OwnerPoolAndFlags, HFLAG_USED);
  InterlockedSub(pool^.Status.TotalUsed, header^.UserSize);
  Inc(slice^.BlocksFreeInSlice);

  if slice^.BlocksFreeInSlice = 1 then begin
    // first free block, prepend to free list
    slice^.NextWithFree:= listHead^;
    if listhead^ <> nil then
      listhead^^.PrevWithFree:= slice;
    listHead^:= slice;
  end else
  if (slice^.BlocksFreeInSlice = slice^.BlocksInSlice) then begin
    // last block freed, deallocate
    if listHead^ = slice then begin
      listHead^:= slice^.NextWithFree;
      if listHead^ <> nil then
        listHead^^.PrevWithFree:= nil;
    end else begin
      if slice^.NextWithFree <> nil then
        slice^.NextWithFree^.PrevWithFree:= slice^.PrevWithFree;
      if slice^.PrevWithFree <> nil then
        slice^.PrevWithFree^.NextWithFree:= slice^.NextWithFree;
    end;

    sliceAlloc:= NextPageSize(SizeOf(TSmallSlice) + slice^.BlocksInSlice * blockSize);

    MemoryFreeForPool(slice, sliceAlloc, pool);
  end;

  LeaveCriticalSection(pool^.SmallAlloc.Lock);
end;

procedure SmallRealloc(var p: Pointer; const newSize: PtrUInt);
var
  pool: PPoolInstance;
  header: PSmallHeader;
  newP: Pointer;
  maxSize, wantExtraAlloc: PtrUInt;
begin
  header:= PSmallHeader(RightAlign(P, SizeOf(TSmallHeader)));
  pool:= FlagsSplitPtr(header^.OwnerPoolAndFlags);

  if newSize = header^.UserSize then
    Exit;

  maxSize:= pool^.SmallAlloc.SliceMaxSize[header^.AllocIndex];

  // Upsize or Downsize?
  if newSize > header^.UserSize then begin
    // Does new size still fit?
    if newSize <= maxSize then begin
      InterlockedAdd(pool^.Status.TotalUsed, PtrUInt(newSize-header^.UserSize));
      header^.UserSize:= newSize;
      Exit;
    end;

    // Growing between Small Sizes is expensive and common (strings), so grow by at least 50% if we do it at all
    wantExtraAlloc:= newSize;
    if newSize <= SMALL_MAX then
      wantExtraAlloc:= header^.UserSize + header^.UserSize div 2;

    // if that isn't small anymore, that's fine
    if wantExtraAlloc > SMALL_MAX then begin
      newP:= HugeAlloc(newSize, wantExtraAlloc);
    end else begin
      newP:= SmallAlloc(newSize, wantExtraAlloc);
    end;
    MemoryRelocate(p, header^.UserSize, newP, newSize);
    SmallFree(P);
    p:= newP;
  end else begin
    // Small blocks never relocate for downsizing
    InterlockedAdd(pool^.Status.TotalUsed, PtrUInt(newSize-header^.UserSize));
    header^.UserSize:= newSize;
  end;
end;

// Huge Allocation Unit

function HugeAlloc(const Size: PtrUInt; const OverAllocHint: PtrUInt = 0): Pointer;
var
  pool: PPoolInstance;
  allocsize: PtrUInt;
  header: PHugeHeader;
begin
  pool:= PoolGetThread;
  allocsize:= NextPageSize(Max(Size, OverAllocHint) + SizeOf(THugeHeader));
  Assert(allocsize > Size);
  header:= CMMSysPageAlloc(allocsize);
  if header = nil then
    Exit(nil);
  header^.AllocSize:= allocsize;
  header^.UserSize:= Size;
  header^.OwnerPoolAndFlags:= FlagsMakePtr(pool, HFLAG_USED or HFLAG_IS_HUGE);
  Result:= Pointer(header + 1);

  InterlockedAdd(pool^.Status.TotalAllocated, allocsize);
  InterlockedAdd(pool^.Status.TotalUsed, Size);
end;

procedure HugeFree(const P: Pointer);
var
  header: PHugeHeader;
  pool: PPoolInstance;
begin
  header:= PHugeHeader(RightAlign(P, SizeOf(THugeHeader)));
  pool:= FlagsSplitPtr(header^.OwnerPoolAndFlags);
  InterlockedSub(pool^.Status.TotalUsed, header^.UserSize);
  MemoryBeforeRelease(header, header^.AllocSize);
  MemoryFreeForPool(header, header^.AllocSize, pool);
end;

procedure HugeRealloc(var p: Pointer; const newSize: PtrUInt);
var
  pool: PPoolInstance;
  header: PHugeHeader;
  newP: Pointer;
begin
  header:= PHugeHeader(RightAlign(P, SizeOf(THugeHeader)));
  pool:= FlagsSplitPtr(header^.OwnerPoolAndFlags);

  if newSize = header^.UserSize then
    Exit;

  // Upsize or Downsize?
  if newSize > header^.UserSize then begin
    // Does new size still fit?
    if newSize <= header^.AllocSize - SizeOf(THugeHeader) then begin
      InterlockedAdd(pool^.Status.TotalUsed, PtrUInt(newSize-header^.UserSize));
      header^.UserSize:= newSize;
      Exit;
    end;

    // Grow by 50% more, expecting a block that grew once will grow again
    newP:= HugeAlloc(newSize, header^.UserSize + header^.UserSize div 2);
    MemoryRelocate(p, header^.UserSize, newP, newSize);
    HugeFree(P);
    p:= newP;
  end else begin
    // How does the fill grade look?
    if newSize >= header^.AllocSize div 2 then begin
      InterlockedAdd(pool^.Status.TotalUsed, PtrUInt(newSize-header^.UserSize));
      header^.UserSize:= newSize;
      Exit;
    end;

    // reallocate new, this may be of any Allocation unit
    newP:= CMMGetMem(newSize);
    MemoryRelocate(p, header^.UserSize, newP, newSize);
    HugeFree(P);
    p:= newP;
  end;
end;

var
  gCMMInstalled: boolean = false;
  gOldMemoryManager: TMemoryManager;

{$If CMM_ASYNC_PAGE_RELEASE}
  var
    gAsyncReleaseThread: TThreadID;
    gAsyncReleaseThreadTerminate: PEventState;

  procedure AsyncReleaseAdd(const pool: PPoolInstance; const StartOfPage: Pointer; const Size: PtrUInt);
  var
    free: PAsyncFreeHeader;
  begin
    free:= PAsyncFreeHeader(StartOfPage);
    free^.AllocSize:= Size;
    free^.Next:= InterlockedExchange(pool^.AsyncFreeList, nil);
    while InterlockedCompareExchange(pool^.AsyncFreeList, free, nil) <> nil do
      ThreadSwitch;
  end;

  procedure AsyncReleaseFreeChain(const pool: PPoolInstance);
  var
    this, next: PAsyncFreeHeader;
    freed: PtrUInt;
  begin
    freed:= 0;
    this:= InterlockedExchange(pool^.AsyncFreeList, nil);
    while this <> nil do begin
      next:= this^.Next;
      inc(freed, this^.AllocSize);
      CMMSysPageFree(this, this^.AllocSize);
      this:= next;
    end;
    InterlockedSub(pool^.Status.TotalAllocated, freed);
  end;

  function AsyncReleaseThreadProc({%H-}parameter: pointer): ptrint;
  var
    i: Integer;
  begin
    Result:= 0;
    while BasicEventWaitFor(CMM_ASYNC_PAGE_RELEASE_INTERVAL, gAsyncReleaseThreadTerminate) = 1 do begin
      for i:= 0 to high(gPagePools) do begin
        AsyncReleaseFreeChain(gPagePools[i]);
      end;
    end;
  end;

  procedure AsyncReleaseInitialize;
  begin
    gAsyncReleaseThreadTerminate:= BasicEventCreate(nil, true, false, '');
    gAsyncReleaseThread:= BeginThread(@AsyncReleaseThreadProc);
    ThreadSetPriority(gAsyncReleaseThread, -10);
  end;

  procedure AsyncReleaseFinalize;
  begin
    if gAsyncReleaseThread <> 0 then begin
      BasicEventSetEvent(gAsyncReleaseThreadTerminate);
      WaitForThreadTerminate(gAsyncReleaseThread, 0);
      CloseThread(gAsyncReleaseThread);
      gAsyncReleaseThread:= 0;
      BasicEventDestroy(gAsyncReleaseThreadTerminate);
      gAsyncReleaseThreadTerminate:= nil;
    end;
  end;
{$IfEnd}


function CMMGetMem(Size: ptruint): Pointer;
begin
  Result:= nil;
  if Size = 0 then
    Exit;

  if Size <= QUANTUM_SIZE then begin
    Result:= QuantumAlloc;
    if Result <> nil then
      Exit;
  end;

  if Size <= SMALL_MAX then
    Exit(SmallAlloc(Size));

  Result:= HugeAlloc(Size);
end;

function CMMFreeMem(p: pointer): ptruint;
var
  pool: PPoolInstance;
  qi: Integer;
  flags: NativeUInt;
begin
  Result:= 0;
  if p = nil then
    Exit;

  if QuantumIsInSpace(P, pool, qi) then begin
    QuantumFree(pool, qi);
    Exit;
  end;

  pool:= FlagsSplitPtrU(p, flags);
  if FlagsAll(flags, HFLAG_IS_SMALL) then
    SmallFree(p)
  else if FlagsAll(flags, HFLAG_IS_HUGE) then
    HugeFree(p);
end;

function CMMFreememSize(p: pointer; Size: ptruint): ptruint;
begin
  Result:= 0;
  if (Size<=0) or (P = nil) then
    Exit;
  //if (size <> CMMMemSize(p)) then
  //  runerror(204);
  Result:= CMMFreeMem(p);
end;

function CMMAllocMem(Size: ptruint): Pointer;
begin
  Result := CMMGetMem(size);
  if Result<>nil then
    FillChar(Result^, Size, 0);
end;

function CMMReAllocMem(var p: pointer; Size: ptruint): Pointer;
var
  oldSize: PtrUInt;
  pool: PPoolInstance;
  qi: Integer;
  flags: NativeUInt;
begin
  if Size = 0 then begin
    CMMFreeMem(p);
    p:= nil;
    Exit(p);
  end;
  if p = nil then begin
    p:= CMMGetMem(Size);
    Exit(p);
  end;

  if QuantumIsInSpace(P, pool, qi) then begin
    oldSize:= QUANTUM_SIZE;

    // Changing from Quantum to Quantum is a free operation
    if Size <= QUANTUM_SIZE then
      Exit(p);
  end else begin
    // Handling non-quantum allocations
    pool:= FlagsSplitPtrU(p, flags);
    Assert(FlagsAll(flags, HFLAG_USED));

    if FlagsAny(flags, HFLAG_IS_HUGE) then begin
      HugeRealloc(p, Size);
      Exit(p);
    end;

    if FlagsAny(flags, HFLAG_IS_SMALL) then begin
      SmallRealloc(p, Size);
      Exit(p);
    end;

    oldSize:= CMMMemSize(p);
  end;

  // No special case, move to new Allocation
  Result:= CMMGetMem(Size);
  if Result<>nil then begin
    MemoryRelocate(p, oldSize, Result, Size);
    CMMFreeMem(p);
    p:= Result;
  end;
end;

function CMMMemSize(p: pointer): ptruint;
var
  flags: NativeUInt;
  pool: PPoolInstance;
  qi: Integer;
begin
  Result:= 0;
  if p = nil then
    Exit;

  if QuantumIsInSpace(P, pool, qi) then
    Exit(QUANTUM_SIZE);

  FlagsSplitPtrU(p, flags);
  if FlagsAll(flags, HFLAG_IS_SMALL or HFLAG_USED) then
    Exit(PSmallHeader(RightAlign(p, SizeOf(TSmallHeader)))^.UserSize);
  if FlagsAll(flags, HFLAG_IS_HUGE or HFLAG_USED) then
    Exit(PHugeHeader(RightAlign(p, SizeOf(THugeHeader)))^.UserSize);
end;

function CMMGetHeapStatus: THeapStatus;
var
  i: Integer;
begin
  FillChar(Result{%H-}, sizeof(Result), 0);
  for i:= 0 to high(gPagePools) do begin
    {$If CMM_ASYNC_PAGE_RELEASE}
      AsyncReleaseFreeChain(gPagePools[i]);
    {$IfEnd}
    inc(Result.TotalAddrSpace, gPagePools[i]^.Status.TotalAllocated);
    inc(Result.TotalAllocated, gPagePools[i]^.Status.TotalUsed);
  end;
  Result.TotalFree:= Result.TotalAddrSpace - Result.TotalAllocated;
end;

function CMMGetFPCHeapStatus: TFPCHeapStatus;
var
  i: Integer;
begin
  FillChar(Result{%H-}, sizeof(Result), 0);
  for i:= 0 to high(gPagePools) do begin
    {$If CMM_ASYNC_PAGE_RELEASE}
      AsyncReleaseFreeChain(gPagePools[i]);
    {$IfEnd}
    inc(Result.CurrHeapSize, gPagePools[i]^.Status.TotalAllocated);
    inc(Result.CurrHeapUsed, gPagePools[i]^.Status.TotalUsed);
  end;
  Result.CurrHeapFree:= Result.CurrHeapSize - Result.CurrHeapUsed;
  Result.MaxHeapSize:= Result.CurrHeapSize;
  Result.MaxHeapUsed:= Result.CurrHeapUsed;
end;

procedure CMMInstall;
var
  newMM: TMemoryManager;
  i: Integer;
begin
  if gCMMInstalled then
    Exit;

  for i:= 0 to high(gPagePools) do begin
    PoolInitialize(gPagePools[i]);
  end;

  {$If CMM_ASYNC_PAGE_RELEASE}
    AsyncReleaseInitialize;
  {$IfEnd}

  GetMemoryManager(gOldMemoryManager);
  FillChar({%H-}newMM, SizeOf(newMM), 0);
  newMM.Getmem:= @CMMGetMem;
  newMM.Freemem:= @CMMFreeMem;
  newMM.FreememSize:= @CMMFreememSize;
  newMM.AllocMem:= @CMMAllocMem;
  newMM.ReAllocMem:= @CMMReAllocMem;
  newMM.MemSize:= @CMMMemSize;
  newMM.GetHeapStatus:= @CMMGetHeapStatus;
  newMM.GetFPCHeapStatus:= @CMMGetFPCHeapStatus;
  SetMemoryManager(newMM);
  gCMMInstalled:= true;
end;

procedure CMMUninstall;
var
  i: Integer;
begin
  if not gCMMInstalled then
    Exit;
  SetMemoryManager(gOldMemoryManager);

  {$If CMM_ASYNC_PAGE_RELEASE}
    AsyncReleaseFinalize;
  {$IfEnd}

  for i:= 0 to high(gPagePools) do begin
    PoolFinalize(gPagePools[i]);
  end;
end;

initialization
  CMMInstall;
finalization
  CMMUninstall;
end.
