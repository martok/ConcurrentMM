unit ConcMM_Sys;

{$mode objfpc}

interface

function CMMSysPageAlloc(size: ptruint): pointer;
procedure CMMSysPageFree(p: pointer; size: ptruint);

implementation

{$IFDEF WINDOWS}

uses
  Windows;

function CMMSysPageAlloc(size: ptruint): pointer;
var
   p : pointer;
begin
  p:= VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE);
  CMMSysPageAlloc := p;
end;

procedure CMMSysPageFree(p: pointer; size: ptruint);
begin
  VirtualFree(p, 0, MEM_RELEASE);
end;

{$ENDIF}

end.

