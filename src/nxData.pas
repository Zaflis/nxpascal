unit nxData;

interface

uses Forms, Classes, SysUtils, {$IFDEF fpc}zstream{$ELSE}zlib{$ENDIF},
  nxGraph, nxTypes;

type

  TDataBlock = record
    data: PointerArrayType;
    compressed: TMemoryStream;
    useTime: cardinal;
    threading: boolean;
  end;
  pDataBlock = ^TDataBlock;

  { TCompressThread }

  TCompressThread = class(TThread)
  private
    pBlock: pDataBlock;
    leaveData: boolean;
    level: TCompressionlevel;
    blockSize: integer;
  public
    constructor Create(_pBlock: pDataBlock; _blockSize: integer;
      _level: TCompressionlevel; _leaveData: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TDeCompressThread }

  TDeCompressThread = class(TThread)
  private
    pBlock: pDataBlock;
    leaveCompressed: boolean;
    blockSize: integer;
  public
    constructor Create(_pBlock: pDataBlock; _blockSize: integer;
      _leaveCompressed: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TDataStore }

  TDataStore = class
  private
    block: array of TDataBlock;
    level: TCompressionlevel;
    FCount, blockSize: integer;
    tick: cardinal;
    FFilename: string;
    procedure LoadStore(source: TStream);
    procedure SaveStore(dest: TStream);
    procedure SaveToFile(filename: string);
  public
    MaxTime, lastCheck: cardinal;
    property count: integer read FCount;
    constructor Create(_blockSize: integer; _level: TCompressionlevel = clDefault);
    destructor Destroy; override;
    procedure Clear; overload;
    procedure Compress(n: integer; leaveData, forced: boolean);
    procedure DeCompress(n: integer; leaveCompressed, forced: boolean);
    function GetBlock(n: integer; forced: boolean): PointerArrayType;
    procedure Initialize(newBlockSize: integer); overload;
    function IsLoaded(n: integer): boolean;
    procedure LoadFromFile(filename: string; CanCreate: boolean = false);
    procedure Save;
    procedure SetCompressionlevel(_level: TCompressionlevel);
    procedure SetFile(filename: string);
    procedure SetSize(n: integer);
    procedure Update;
  end;

var nxDataThreadCount: integer;

implementation

{ TDataStore }

constructor TDataStore.Create(_blockSize: integer; _level: TCompressionlevel);
begin
  blockSize:=_blockSize;
  MaxTime:=1000*15; // 15 seconds default expiration time
  tick:=(nxEngine.GetTick div 1000)*1000;
  level:=_level;
end;

destructor TDataStore.Destroy;
begin
  while (nxDataThreadCount>0) do begin
    application.ProcessMessages; sleep(1);
  end;
  Clear;
  inherited Destroy;
end;

procedure TDataStore.Clear;
begin
  SetSize(0); FFilename:='';
end;

procedure TDataStore.Compress(n: integer; leaveData, forced: boolean);
var cs: TCompressionStream;
begin
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        repeat
          application.ProcessMessages;sleep(1);
        until not threading;
      end else exit;
    if forced or (nxDataThreadCount>=100) then begin
      if data<>nil then begin
        if compressed<>nil then compressed.Clear
        else begin
          compressed:=TMemoryStream.Create;
        end;
        cs:=TCompressionStream.create(level, compressed);
        cs.Write(data[0], blockSize);
        cs.Free;
        if not leaveData then begin
          freemem(data); data:=nil;
        end;
      end;
    end else
      TCompressThread.Create(@block[n], blocksize, level, leaveData);
  end;
end;

procedure TDataStore.DeCompress(n: integer; leaveCompressed, forced: boolean);
var ds: TDeCompressionStream;
begin
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        repeat
          application.ProcessMessages; sleep(1);
        until not threading;
      end else exit;
    if forced or (nxDataThreadCount>=100) then begin
      if compressed<>nil then begin
        compressed.Position:=0;
        ds:=TDeCompressionStream.create(compressed);
        if data=nil then data:=allocmem(blocksize);
        ds.read(data[0], blocksize);
        ds.Free;
        if not leaveCompressed then FreeAndNil(compressed);
        UseTime:=tick;
      end;
    end else
      TDeCompressThread.Create(@block[n], blocksize, leaveCompressed);
  end;
end;

function TDataStore.GetBlock(n: integer; forced: boolean): PointerArrayType;
begin
  result:=nil;
  if (n<0) or (n>=count) then exit;
  with block[n] do begin
    if threading then
      if forced then begin
        repeat
          application.ProcessMessages; sleep(1);
        until not threading;
      end else exit;
    if data=nil then
      if compressed<>nil then begin
        DeCompress(n, false, forced);
      end else begin
        data:=allocmem(blockSize);
      end;
    result:=data; UseTime:=tick;
  end;
end;

procedure TDataStore.Initialize(newBlockSize: integer);
begin
  SetSize(0); blockSize:=newBlockSize; FFilename:='';
end;

function TDataStore.IsLoaded(n: integer): boolean;
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

procedure TDataStore.SetCompressionlevel(_level: Tcompressionlevel);
begin
  level:=_level;
end;

procedure TDataStore.SetFile(filename: string);
begin
  FFilename:=filename;
end;

procedure TDataStore.LoadStore(source: TStream);
var i: integer; n: cardinal; b: byte;
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
end;

procedure TDataStore.SaveStore(dest: TStream);
var i: integer; hasComp: boolean; bVar: byte; intVar: cardinal;
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
          while threading do begin
            application.ProcessMessages; sleep(1);
          end;
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

procedure TDataStore.SetSize(n: integer);
var i: integer;
begin
  while nxDataThreadCount>0 do begin
    application.ProcessMessages; sleep(1);
  end;
  // Free left out blocks
  for i:=n to FCount-1 do
    with block[i] do begin
      if data<>nil then freemem(data);
      if compressed<>nil then compressed.Free;
      threading:=false;
    end;
  setlength(block, n);
  // Initialize new blocks
  for i:=fCount to n-1 do
    with block[i] do begin
      compressed:=nil; data:=nil; threading:=false;
    end;
  FCount:=n;
end;

procedure TDataStore.Update;
var i: integer;
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

{ TCompressThread }

constructor TCompressThread.Create(_pBlock: pDataBlock; _blockSize: integer;
  _level: TCompressionlevel; _leaveData: boolean);
begin
  inherited Create(false);
  inc(nxDataThreadCount);
  pBlock:=_pBlock; pBlock^.threading:=true;
  leaveData:=_leaveData; level:=_level; blockSize:=_blockSize;
  FreeOnTerminate:=true;
end;

destructor TCompressThread.Destroy;
begin
  dec(nxDataThreadCount);
  inherited Destroy;
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
        freemem(data); data:=nil;
      end;
    end;
    threading:=false;
  end;
end;

{ TDeCompressThread }

constructor TDeCompressThread.Create(_pBlock: pDataBlock; _blockSize: integer; _leaveCompressed: boolean);
begin
  inherited Create(false);
  inc(nxDataThreadCount);
  pBlock:=_pBlock; pBlock^.threading:=true;
  leaveCompressed:=_leaveCompressed; blockSize:=_blockSize;
  FreeOnTerminate:=true;
end;

destructor TDeCompressThread.Destroy;
begin
  dec(nxDataThreadCount);
  inherited Destroy;
end;

procedure TDeCompressThread.Execute;
var ds: TDeCompressionStream;
begin
  with pBlock^ do begin
    if compressed<>nil then begin
      compressed.Position:=0;
      ds:=TDeCompressionStream.create(compressed);
      if data=nil then data:=allocmem(blocksize);
      ds.read(data[0], blocksize);
      ds.Free;
      if not leaveCompressed then FreeAndNil(compressed);
      UseTime:=nxEngine.FrameTick;
    end;
    threading:=false;
  end;
end;

end.

