unit nxData;

interface

uses {$IFDEF UNIX}cthreads,{$ENDIF}
  Forms, Classes, SysUtils, {$IFDEF fpc}zstream{$ELSE}zlib{$ENDIF},
  nxGraph, nxTypes;

type

  TDataBlock = record
    data: DynamicByteArray;
    compressed: TMemoryStream;
    useTime: cardinal;
    threading: boolean;
  end;
  pDataBlock = ^TDataBlock;

  TCustomDataThread = class;
  PCustomDataThread = ^TCustomDataThread;

  { TCustomDataThread }

  TCustomDataThread = class(TThread)
  protected
    pBlock: pDataBlock;
    pt: PCustomDataThread;
  public
    destructor Destroy; override;
  end;

  { TCompressThread }

  TCompressThread = class(TCustomDataThread)
  private
    leaveData: boolean;
    level: TCompressionlevel;
    blockSize: longint;
  public
    constructor Create(_pBlock: pDataBlock; _blockSize: longint;
      _level: TCompressionlevel; _leaveData: boolean);
    procedure Execute; override;
  end;

  { TDeCompressThread }

  TDeCompressThread = class(TCustomDataThread)
  private
    leaveCompressed: boolean;
    blockSize: longint;
  public
    constructor Create(_pBlock: pDataBlock; _blockSize: longint;
      _leaveCompressed: boolean);
    procedure Execute; override;
  end;

  { TDataStore }

  TDataStore = class
  private
    block: array of TDataBlock;
    level: TCompressionlevel;
    FCount, blockSize, FCompressed: longint;
    tick: cardinal;
    FFilename: string;
    threadList: array of TCustomDataThread;
    procedure LoadStore(source: TStream);
    procedure SaveStore(dest: TStream);
    procedure SaveToFile(filename: string);
  public
    MaxTime, lastCheck: cardinal;
    property compressed: longint read FCompressed;
    property count: longint read FCount;
    constructor Create(_blockSize: longint; _level: TCompressionlevel = clDefault);
    destructor Destroy; override;
    function AddThread: integer;
    procedure Clear; overload;
    procedure Compress(n: longint; leaveData, forced: boolean);
    procedure DeCompress(n: longint; leaveCompressed, forced: boolean);
    function GetBlock(n: longint; forced: boolean): DynamicByteArray;
    procedure Initialize(newBlockSize: longint); overload;
    function IsLoaded(n: longint): boolean;
    procedure LoadFromFile(filename: string; CanCreate: boolean = false);
    procedure Save;
    procedure SetCompressionlevel(_level: TCompressionlevel);
    procedure SetFile(filename: string);
    procedure SetSize(n: longint);
    procedure Update;
    procedure WaitAllThreads;
    procedure WaitFor(n: longint);
  end;

var nxDataThreadCount: integer;

implementation

{ TDataStore }

constructor TDataStore.Create(_blockSize: longint; _level: TCompressionlevel);
begin
  blockSize:=_blockSize;
  MaxTime:=1000*15; // 15 seconds default expiration time
  tick:=(nxEngine.GetTick div 1000)*1000;
  level:=_level;
end;

destructor TDataStore.Destroy;
begin
  WaitAllThreads;
  Clear;
  inherited Destroy;
end;

function TDataStore.AddThread: integer;
var i: integer;
begin
  // Check if there exists free slot for new thread in current list
  for i:=0 to high(threadList) do
    if threadList[i]=nil then begin
      result:=i; exit;
    end;
  // Increase list size by 1
  result:=length(threadList);
  setlength(threadList, result+1);
  // Pointers changed after array size changed! Update addresses to threads
  for i:=0 to result-1 do
    if threadList[i]<>nil then threadList[i].pt:=@threadList[i];
end;

procedure TDataStore.Clear;
begin
  SetSize(0); FFilename:='';
end;

procedure TDataStore.Compress(n: longint; leaveData, forced: boolean);
var cs: TCompressionStream; t: integer;
begin
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        WaitFor(n);
      end else exit;
    if forced or (nxDataThreadCount>=100) then begin
      if data<>nil then begin
        // Start compression
        if compressed<>nil then compressed.Clear
        else begin
          compressed:=TMemoryStream.Create;
        end;
        cs:=TCompressionStream.create(level, compressed);
        cs.Write(data[0], blockSize);
        cs.Free;
        if not leaveData then begin
          setlength(data, 0);
        end;
        inc(FCompressed);
      end;
    end else begin
      t:=AddThread;
      threadList[t]:=TCompressThread.Create(@block[n], blocksize, level, leaveData);
      threadList[t].pt:=@threadList[t];
    end;
    inc(FCompressed);
  end;
end;

procedure TDataStore.DeCompress(n: longint; leaveCompressed, forced: boolean);
var ds: TDeCompressionStream; t: integer;
begin
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        WaitFor(n);
      end else exit;
    if forced or (nxDataThreadCount>=100) then begin
      if compressed<>nil then begin
        compressed.Position:=0;
        ds:=TDeCompressionStream.create(compressed);
        setlength(data, blocksize);
        ds.read(data[0], blocksize);
        ds.Free;
        if not leaveCompressed then FreeAndNil(compressed);
        UseTime:=tick;
      end;
    end else begin
      t:=AddThread;
      threadList[t]:=TDeCompressThread.Create(@block[n], blocksize, leaveCompressed);
      threadList[t].pt:=@threadList[t];
    end;
    dec(FCompressed);
  end;
end;

function TDataStore.GetBlock(n: longint; forced: boolean): DynamicByteArray;
begin
  result:=nil;
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        WaitFor(n);
      end else exit;
    if data=nil then
      if compressed<>nil then begin
        DeCompress(n, false, forced);
      end else begin
        setlength(data, blockSize);
      end;
    result:=data; UseTime:=tick;
  end;
end;

procedure TDataStore.Initialize(newBlockSize: longint);
begin
  SetSize(0); blockSize:=newBlockSize; FFilename:='';
end;

function TDataStore.IsLoaded(n: longint): boolean;
begin
  if (n>=0) and (n<count) then
    result:=(block[n].data<>nil) and (not block[n].threading)
  else result:=false;
end;

procedure TDataStore.LoadFromFile(filename: string; CanCreate: boolean = false);
var fs: TFileStream;
begin
  if not fileexists(filename) then begin
    if CanCreate then begin
      FFilename:=filename; Save;
    end;
    exit;
  end;
  fs:=nil;
  try
    fs:=TFileStream.Create(filename, fmOpenRead);
    LoadStore(fs);
    FFilename:=filename;
  finally
    fs.Free;
  end;
end;

procedure TDataStore.Save;
begin
  if FFilename='' then exit;
  SaveToFile(FFilename);
end;

procedure TDataStore.SetCompressionlevel(_level: TCompressionlevel);
begin
  level:=_level;
end;

procedure TDataStore.SetFile(filename: string);
begin
  FFilename:=filename;
end;

procedure TDataStore.LoadStore(source: TStream);
var i: longint; n: cardinal; b: byte;
begin
  Clear;
  blockSize:=0; n:=0; b:=0;
  source.Read(blockSize, sizeof(blockSize));
  source.Read(n, sizeof(n));
  SetSize(n);
  for i:=0 to Count-1 do
    with block[i] do begin
      source.Read(b, sizeof(b));
      if b=1 then begin
        source.Read(n, sizeof(n));
        compressed:=TMemoryStream.Create;
        compressed.CopyFrom(source, n);
      end;
    end;
  FCompressed:=FCount;
end;

procedure TDataStore.SaveStore(dest: TStream);
var i: longint; hasComp: boolean; bVar: byte; intVar: cardinal;
begin
  dest.Write(blockSize, sizeof(blockSize));
  dest.Write(Count, sizeof(Count));
  for i:=0 to Count-1 do
    with block[i] do begin
      if (data<>nil) or (compressed<>nil) then begin
        bVar:=1;
        dest.Write(bVar, sizeof(bVar));
        HasComp:=compressed<>nil;
        if data<>nil then begin
          WaitFor(i);
          Compress(i, true, true);
        end;
        intVar:=compressed.Size;
        dest.Write(intVar, sizeof(intVar));
        compressed.Position:=0;
        dest.CopyFrom(compressed, compressed.Size);
        if not HasComp then FreeAndNil(compressed);
      end else begin
        bVar:=0;
        dest.Write(bVar, sizeof(bVar));
      end;
    end;
end;

procedure TDataStore.SaveToFile(filename: string);
var fs: TFileStream;
begin
  fs:=nil;       
  try
    fs:=TFileStream.Create(filename, fmCreate);
    SaveStore(fs);
    FFilename:=filename;
  finally
    fs.Free;
  end;
end;

procedure TDataStore.SetSize(n: longint);
var i: longint;
begin
  WaitAllThreads;
  // Free left out blocks
  for i:=n to FCount-1 do
    with block[i] do begin
      setlength(data, 0);
      FreeAndNil(compressed);
      threading:=false;
    end;
  setlength(block, n);
  // Initialize new blocks
  for i:=fCount to n-1 do
    with block[i] do begin
      compressed:=nil; data:=nil; threading:=false;
    end;
  FCount:=n; FCompressed:=0;
end;

procedure TDataStore.Update;
var i: longint;
begin
  tick:=(nxEngine.GetTick div 1000)*1000;
  if tick<>lastCheck then begin
    // Compress unused and expired data blocks
    for i:=0 to count-1 do
      with block[i] do
        if (data<>nil) and (not threading) and (tick>block[i].useTime+MaxTime) then
          Compress(i, false, false);
    lastCheck:=tick;
  end;
end;

procedure TDataStore.WaitAllThreads;
var i: integer;
begin
  for i:=0 to high(threadList) do
    if threadList[i]<>nil then threadList[i].WaitFor;
end;

procedure TDataStore.WaitFor(n: longint);
var i: integer;
begin
  for i:=high(threadList) downto 0 do
    if (threadList[i]<>nil) and (threadList[i].pBlock=@block[n]) then
      threadList[i].WaitFor;
end;

{ TCompressThread }

constructor TCompressThread.Create(_pBlock: pDataBlock; _blockSize: longint;
  _level: TCompressionlevel; _leaveData: boolean);
begin
  inc(nxDataThreadCount);
  pBlock:=_pBlock; pBlock^.threading:=true;
  leaveData:=_leaveData; level:=_level; blockSize:=_blockSize;
  inherited Create(false);
  FreeOnTerminate:=true;
end;

procedure TCompressThread.Execute;
var cs: TCompressionStream;
begin
  with pBlock^ do begin
    if data<>nil then begin
      if compressed<>nil then compressed.Clear
      else begin
        compressed:=TMemoryStream.Create;
      end;
      cs:=TCompressionStream.create(level, compressed);
      cs.Write(data[0], blockSize);
      cs.Free;
      if not leaveData then begin
        setlength(data, 0);
      end;
    end;
    pt^:=nil; threading:=false;
  end;
end;

{ TDeCompressThread }

constructor TDeCompressThread.Create(_pBlock: pDataBlock; _blockSize: longint;
  _leaveCompressed: boolean);
begin
  inherited Create(false);
  inc(nxDataThreadCount);
  pBlock:=_pBlock; pBlock^.threading:=true;
  leaveCompressed:=_leaveCompressed; blockSize:=_blockSize;
  FreeOnTerminate:=true;
end;

procedure TDeCompressThread.Execute;
var ds: TDeCompressionStream;
begin
  with pBlock^ do begin
    if compressed<>nil then begin
      compressed.Position:=0;
      ds:=TDeCompressionStream.create(compressed);
      setlength(data, blocksize);
      ds.read(data[0], blocksize);
      ds.Free;
      if not leaveCompressed then FreeAndNil(compressed);
      UseTime:=nxEngine.FrameTick;
    end;
    pt^:=nil; threading:=false;
  end;
end;

{ TCustomDataThread }

destructor TCustomDataThread.Destroy;
begin
  dec(nxDataThreadCount);
  inherited Destroy;
end;

end.

