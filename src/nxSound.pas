unit nxSound;

{
  Using OpenAL header from  http://www.noeska.com/doal/
}

{$H+}

interface

uses Openal, nxTypes {$IFDEF fpc},fileutil{$ENDIF};

type

  { TNXSoundBuffer }

  TNXSoundBuffer = class
    name: string;
    index: integer;
    buffer: TALuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadWAV(const filename: string);
  end;

  { TNXSoundSource }

  TNXSoundSource = class
    name: string;
    index: integer;
    source: TALuint;
    buffer: TNXSoundBuffer;
    position, velocity: TVector;
    loaded: boolean;
  public
    constructor Create(const _name: string; buf: TNXSoundBuffer);
    destructor Destroy; override;
    procedure Pause;
    procedure Play;
    procedure SetBuffer(buf: TNXSoundBuffer);
    procedure SetLooping(enable: boolean = true);
    procedure Stop;
    procedure Update;
  end;

  TListenerOrientation = packed record
    front, up: TVector;
  end;

  { TNXSoundListener }

  TNXSoundListener = class
    position, velocity: TVector;
    orientation: TListenerOrientation;
  public
    constructor Create;
    procedure DoMove;
    procedure Update;
  end;

  { TNXSoundEngine }

  TNXSoundEngine = class
    buffer: array of TNXSoundBuffer;
    source: array of TNXSoundSource;
    bufferCount, count: integer;
  private
    function AddBuffer: TNXSoundBuffer;
  public
    listener: TNXSoundListener;
    constructor Create;
    destructor Destroy; override;
    function AddWAV(const filename: string): TNXSoundBuffer;
    function AddSource(const _name: string; buf: TNXSoundBuffer): TNXSoundSource;
    procedure ClearSources;
    procedure Clear;
    procedure DeleteSource(index: integer);
    procedure DeleteBuffer(index: integer);
    function Find(const name: string): TNXSoundSource;
    function IndexOf(const name: string): integer;
  end;

var
  sound: TNXSoundEngine;

implementation

function CheckError: boolean;
var n: TALenum;
begin
  n:=alGetError();
  result:=n<>AL_NO_ERROR;
  if result then
    case n of
      AL_INVALID_NAME: nxSetError('OpenAL error [Invalid name]');
      AL_ILLEGAL_ENUM: nxSetError('OpenAL error [Illegal enum]');
      AL_INVALID_VALUE: nxSetError('OpenAL error [Invalid value]');
      AL_ILLEGAL_COMMAND: nxSetError('OpenAL error [Illegal command]');
      AL_OUT_OF_MEMORY: nxSetError('OpenAL error [Out of memory]');
      else nxSetError('OpenAL error [Unknown error]');
    end;
end;

function ExtractFilename(const fn: string): string;
var p: integer;
begin
  result:=fn;
  repeat
    p:=pos('\',result);
    if p=0 then p:=pos('/',result);
    if p>0 then result:=copy(result,p+1,length(result)-p);
  until p=0;
end;

{ TNXSoundListener }

constructor TNXSoundListener.Create;
begin
  Update;
end;

procedure TNXSoundListener.DoMove;
begin
  position.x:=position.x+velocity.x;
  position.y:=position.y+velocity.y;
  position.z:=position.z+velocity.z;
end;

procedure TNXSoundListener.Update;
begin
  AlListenerfv(AL_POSITION, @position);
  AlListenerfv(AL_VELOCITY, @velocity);
  AlListenerfv(AL_ORIENTATION, @orientation);
end;

{ TNXSoundBuffer }

constructor TNXSoundBuffer.Create;
begin
  AlGenBuffers(1, @buffer);
  checkError;
end;

destructor TNXSoundBuffer.Destroy;
begin
  AlDeleteBuffers(1, @buffer);
  inherited Destroy;
end;

procedure TNXSoundBuffer.LoadWAV(const filename: string);
var format: TALEnum; size: TALSizei; freq: TALSizei;
    loop: TALInt; data: TALVoid;
begin
  if not FileExistsUTF8(filename) then begin
    nxSetError('"'+filename+'" doesn''t exist!'); exit;
  end;
  name:=ExtractFilename(filename);
  AlutLoadWavFile(filename, format{%H-}, data{%H-}, size{%H-}, freq{%H-}, loop{%H-});
  AlBufferData(buffer, format, data, size, freq);
  AlutUnloadWav(format, data, size, freq);
  checkError;
end;

{ TNXSoundSource }

constructor TNXSoundSource.Create(const _name: string; buf: TNXSoundBuffer);
begin
  name:=_name; buffer:=buf;
  AlGenSources(1, @source);
  SetBuffer(buf);
  AlSourcei(source, AL_LOOPING, AL_FALSE);
  AlSourcef(source, AL_PITCH, 1.0);
  AlSourcef(source, AL_GAIN, 1.0);
  Update;
  checkError;
end;

destructor TNXSoundSource.Destroy;
begin
  AlDeleteSources(1, @source);
  inherited Destroy;
end;

procedure TNXSoundSource.Pause;
begin
  if loaded then AlSourcePause(source);
end;

procedure TNXSoundSource.Play;
begin
  if loaded then AlSourcePlay(source);
end;

procedure TNXSoundSource.SetBuffer(buf: TNXSoundBuffer);
begin
  Stop; buffer:=buf;
  if buffer<>nil then begin
    AlSourcei(source, AL_BUFFER, buffer.buffer);
    loaded:=true;
  end else
    loaded:=false;
end;

procedure TNXSoundSource.SetLooping(enable: boolean);
begin
  if enable then AlSourcei(source, AL_LOOPING, AL_TRUE)
  else AlSourcei(source, AL_LOOPING, AL_FALSE);
end;

procedure TNXSoundSource.Stop;
begin
  if loaded then AlSourceStop(source);
end;

procedure TNXSoundSource.Update;
begin
  AlSourcefv(source, AL_POSITION, @position);
  AlSourcefv(source, AL_VELOCITY, @velocity);
end;

{ TNXSoundEngine }

function TNXSoundEngine.AddBuffer: TNXSoundBuffer;
begin
  inc(bufferCount); setlength(buffer, bufferCount);
  buffer[bufferCount-1]:=TNXSoundBuffer.Create;
  buffer[bufferCount-1].index:=bufferCount-1;
  result:=buffer[bufferCount-1];
end;

function TNXSoundEngine.AddSource(const _name: string; buf: TNXSoundBuffer): TNXSoundSource;
begin
  inc(count); setlength(source, count);
  source[count-1]:=TNXSoundSource.Create(_name, buf);
  source[count-1].index:=count-1;
  result:=source[count-1];
end;

constructor TNXSoundEngine.Create;
var argv: array of PalByte;
begin
  InitOpenAL;
  if checkError then exit;
  AlutInit(nil, argv{%H-});
  listener:=TNXSoundListener.Create;
  checkError;
end;

destructor TNXSoundEngine.Destroy;
begin
  listener.Free;
  Clear;
  AlutExit();
  inherited Destroy;
end;

function TNXSoundEngine.AddWAV(const filename: string): TNXSoundBuffer;
begin
  result:=AddBuffer; result.LoadWAV(filename);
end;

procedure TNXSoundEngine.ClearSources;
var i: integer;
begin
  for i:=count-1 downto 0 do source[i].Free;
  count:=0; setlength(source, 0);
end;

procedure TNXSoundEngine.Clear;
var i: integer;
begin
  ClearSources;
  for i:=buffercount-1 downto 0 do buffer[i].Free;
  buffercount:=0; setlength(buffer, 0);
end;

procedure TNXSoundEngine.DeleteSource(index: integer);
var i: integer;
begin
  source[index].Free;
  for i:=index to count-2 do begin
    source[i]:=source[i+1]; source[i].index:=i;
  end;
  dec(count); setlength(source, count);
end;

procedure TNXSoundEngine.DeleteBuffer(index: integer);
var i: integer;
begin
  for i:=count-1 downto 0 do
    if source[i].buffer=buffer[index] then source[i].SetBuffer(nil);

  buffer[index].Free;
  for i:=index to buffercount-2 do begin
    buffer[i]:=buffer[i+1]; buffer[i].index:=i;
  end;
  dec(bufferCount); setlength(buffer, buffercount);
end;

function TNXSoundEngine.Find(const name: string): TNXSoundSource;
var i: integer;
begin
  i:=IndexOf(name);
  if i>=0 then result:=source[i]
  else result:=nil;
end;

function TNXSoundEngine.IndexOf(const name: string): integer;
var i: integer;
begin
  for i:=0 to count-1 do
    if source[i].name=name then begin
      result:=i; exit;
    end;
  result:=-1;
end;

initialization
  sound:=TNXSoundEngine.Create;

finalization
  sound.Free; sound:=nil;

end.
