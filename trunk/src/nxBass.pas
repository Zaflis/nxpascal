unit nxBass;

{
  Uses BASS library ( http://www.un4seen.com/ )

  BASS is free for non-commercial use. If you are a non-commercial
  entity (eg. an individual) and you are not making any money from
  your product (through sales, advertising, etc), then you can use
  BASS in it for free.
}

{$H+}

interface

uses {$IFDEF fpc}LCLType, fileutil{$ELSE}Windows{$ENDIF}, SysUtils,
  Controls, Bass, nxTypes;

type

  TBassSoundType = (stStream, stMusic, stSample);

 { TBassSound }

  TBassSound = class
    name: string;
    loaded, looped, FUse3D: boolean;
    index, ChannelCount: integer;
    Handle: DWORD;
    soundType: TBassSoundType;
  private
    location, velocity, orientation: TVector;
    ChannelIndex: integer;
    FChannel: array of DWORD;
    FPlaying: boolean;
    function GetChannel: QWORD;
    function GetPosition: QWORD;
    procedure SetPosition(p: QWORD);
    function GetTimePos: TTime;
    procedure SetTimePos(p: TTime);
    procedure SetUse3D(enable: boolean);
  public
    property Use3D: boolean read FUse3D write SetUse3D;
    property Channel: QWORD read GetChannel;
    property Position: QWORD read GetPosition write SetPosition;
    property TimePos: TTime read GetTimePos write SetTimePos;
    constructor Create(const filename: string; _3D: boolean; channels: integer);
    destructor Destroy; override;
    procedure FadeFreq(freq: single; ms: cardinal);
    procedure FadePan(pan: single; ms: cardinal);
    procedure FadeVolume(vol: single; ms: cardinal);
    function GetVolume: single;
    function Length: QWORD;
    procedure Load(const filename: string; _3d: boolean = false);
    procedure Pause;
    function Play: boolean;
    function Playing: boolean;
    procedure Set3DAttributes(min, max: FLOAT; iangle, oangle, outvol: LongInt);
    procedure SetLocation(const _loc: TVector);
    procedure SetOrientation(const _front: TVector);
    procedure SetPanning(pan: single);
    procedure SetVelocity(const vel: TVector; fps: single);
    procedure SetVolume(vol: single);
    procedure Stop;
    function TimeLength: TTime;
    procedure Unload;
    procedure Update;
  end;

  TListenerOrientation = packed record
    front, up: TVector;
  end;

  { TNXSoundListener }

  TNXSoundListener = class
    location, velocity: TVector;
    orientation: TListenerOrientation;
  public
    constructor Create;
    procedure DoMove;
    procedure SetLocation(const _loc: TVector);
    procedure SetOrientation(const _front, _up: TVector);
    procedure SetVelocity(const vel: TVector; fps: single);
    procedure Update;
  end;

  { TBassEngine }

  TBassEngine = class
    sound: array of TBassSound;
    count: integer;
  private
  public
    listener: TNXSoundListener;
    Use3D: boolean;
    constructor Create(winHandle: HWND; _3D: boolean; freq: DWORD = 44100);
    destructor Destroy; override;
    function Add(const name, filename: string; _3D: boolean; channels: integer=1): TBassSound;
    function CheckErrors: boolean;
    procedure Clear;
    function CPU: single;
    procedure DeleteSound(index: integer);
    function Find(const name: string): TBassSound;
    function IndexOf(const name: string): integer;
    procedure Resume;
    procedure Set3DFactors(distf, rollf, doppf: single);
    procedure Stop;
    procedure Update;
  end;

implementation

var VerCheck: boolean;

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

{ TBassEngine }

constructor TBassEngine.Create(winHandle: HWND; _3D: boolean; freq: DWORD);
var flag3D: integer;
begin
  if not VerCheck then begin
    nxSetError('Wrong BASS dll version!'); exit;
  end;
  if _3D then flag3D:=BASS_DEVICE_3D
  else flag3D:=0;
  if not BASS_Init(-1, freq, flag3D, winHandle, nil) then
    nxSetError('Error initializing BASS audio!')
  else begin
    listener:=TNXSoundListener.Create;
    Use3D:=_3D; Set3DFactors(1.0, 0.1, 1.0);
  end;
end;

destructor TBassEngine.Destroy;
begin
  if listener<>nil then listener.Free;
  Clear;
  BASS_Free;
  inherited Destroy;
end;

function TBassEngine.Add(const name, filename: string; _3D: boolean; channels: integer): TBassSound;
begin
  result:=nil;
  if not VerCheck then exit;
  inc(Count); setlength(sound, Count);
  sound[Count-1]:=TBassSound.Create(filename, _3D, channels);
  sound[Count-1].index:=Count-1;
  sound[Count-1].name:=name;
  result:=sound[Count-1];
end;

function TBassEngine.CheckErrors: boolean;
begin
  if (not VerCheck) or (listener=nil) then result:=true
  else result:=false
end;

procedure TBassEngine.Clear;
var i: integer;
begin
  for i:=count-1 downto 0 do sound[i].Free;
  count:=0; setlength(sound, 0);
end;

function TBassEngine.CPU: single;
begin
  result:=BASS_GetCPU;
end;

procedure TBassEngine.DeleteSound(index: integer);
var i: integer;
begin
  sound[index].Free;
  for i:=index to count-2 do begin
    sound[i]:=sound[i+1]; sound[i].index:=i;
  end;
  dec(count); setlength(sound, count);
end;

function TBassEngine.Find(const name: string): TBassSound;
var i: integer;
begin
  i:=IndexOf(name);
  if i>=0 then result:=sound[i]
  else result:=nil;
end;

function TBassEngine.IndexOf(const name: string): integer;
var i: integer;
begin
  for i:=0 to count-1 do
    if sound[i].name=name then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TBassEngine.Resume;
begin
  BASS_Start;
end;

procedure TBassEngine.Set3DFactors(distf, rollf, doppf: single);
begin
  if Use3D then BASS_Set3DFactors(distf, rollf, doppf);
end;

procedure TBassEngine.Stop;
begin
  BASS_Pause;
end;

procedure TBassEngine.Update;
var i: integer;
begin
  if listener<>nil then listener.Update;
  for i:=0 to count-1 do
    with sound[i] do begin
      // Looping
      if loaded and FPlaying and (not Playing) then
        if looped then Play
        else FPlaying:=false;
      Update;
    end;
  if Use3D then BASS_Apply3D;
end;

{ TBassSound }

function TBassSound.GetChannel: QWORD;
begin
  result:=FChannel[ChannelIndex]
end;

function TBassSound.GetPosition: QWORD;
begin
  if loaded then begin
    result:=BASS_ChannelGetPosition(channel, BASS_POS_BYTE)
  end else
    result:=0;
end;

procedure TBassSound.SetPosition(p: QWORD);
begin
  if loaded then
    BASS_ChannelSetPosition(channel, p, BASS_POS_BYTE);
end;

function TBassSound.GetTimePos: TTime;
begin
  result:=BASS_ChannelBytes2Seconds(channel, GetPosition)/86400;
end;

procedure TBassSound.SetTimePos(p: TTime);
begin
  SetPosition(BASS_ChannelSeconds2Bytes(channel, p*86400));
end;

procedure TBassSound.SetUse3D(enable: boolean);
begin
  FUse3D:=enable;
  Set3DAttributes(0, 100, 360, 360, 0);
end;

constructor TBassSound.Create(const filename: string; _3D: boolean; channels: integer);
begin
  ChannelCount:=channels; ChannelIndex:=0;
  setlength(FChannel, ChannelCount);
  orientation:=vector(0,1,0);
  if filename<>'' then Load(filename, _3D);
end;

destructor TBassSound.Destroy;
begin
  Unload;
  inherited Destroy;
end;

// Playback frequency from 100 to 100000
procedure TBassSound.FadeFreq(freq: single; ms: cardinal);
var i: integer;
begin
  for i:=0 to ChannelCount-1 do
    BASS_ChannelSlideAttribute(FChannel[i], BASS_ATTRIB_FREQ, freq, ms);
end;

// Panning -1..1 from left to right
procedure TBassSound.FadePan(pan: single; ms: cardinal);
var i: integer;
begin
  for i:=0 to ChannelCount-1 do
    BASS_ChannelSlideAttribute(FChannel[i], BASS_ATTRIB_PAN, pan, ms);
end;

// Volume 0..1 from silence to max
procedure TBassSound.FadeVolume(vol: single; ms: cardinal);
var i: integer;
begin
  for i:=0 to ChannelCount-1 do
    BASS_ChannelSlideAttribute(FChannel[i], BASS_ATTRIB_VOL, vol, ms);
end;

function TBassSound.GetVolume: single;
begin
  result:=0;
  BASS_ChannelGetAttribute(channel, BASS_ATTRIB_VOL, result);
end;

function TBassSound.Length: QWORD;
begin
  result:=BASS_ChannelGetLength(channel, BASS_POS_BYTE);
end;

procedure TBassSound.Load(const filename: string; _3d: boolean);
var ext: string; i, flag3D: integer;
begin
  if _3d then flag3D:=BASS_SAMPLE_3D
  else flag3D:=0;
  if loaded then Unload;
  if (filename<>'') and (not FileExistsUTF8(filename)) then begin
    nxSetError(format('[BassSound] File "%s" not found.',[filename]));
    exit;
  end;
  ext:=lowercase(extractFileExt(filename));
  if ext='.wav' then begin
    Handle:=BASS_SampleLoad(FALSE, PChar(filename), 0, 0, ChannelCount,
      BASS_SAMPLE_OVER_POS {$IFDEF UNICODE}or BASS_UNICODE{$ENDIF} or flag3D);
    for i:=0 to ChannelCount-1 do
      FChannel[i]:=BASS_SampleGetChannel(Handle, true);
    loaded:=true; SoundType:=stSample;
  end else if (ext='.s3m') or (ext='.mod') or (ext='.xm') or
              (ext='.it') or (ext='.mtm') then begin
    ChannelCount:=1; setlength(FChannel, 1);
    Handle:=BASS_MusicLoad(False, PChar(filename), 0, 0,
      BASS_MUSIC_RAMP {$IFDEF UNICODE}or BASS_UNICODE{$ENDIF} or
      BASS_MUSIC_PRESCAN or flag3D, 0);
    FChannel[0]:=Handle; loaded:=true; SoundType:=stMusic;
  end else begin // mp3, ogg
    ChannelCount:=1; setlength(FChannel, 1);
    Handle:=BASS_StreamCreateFile(False, PChar(filename), 0, 0,
      0 {$IFDEF UNICODE}or BASS_UNICODE{$ENDIF} or flag3D);
    FChannel[0]:=Handle; loaded:=true; SoundType:=stStream;
  end;
  Use3D:=_3d;
end;

procedure TBassSound.Pause;
begin
  if not loaded then exit;
  BASS_ChannelPause(channel);
  FPlaying:=Playing;
end;

function TBassSound.Play: boolean;
begin
  if not loaded then begin
    result:=false; exit;
  end;
  if soundType=stSample then begin
    ChannelIndex:=(ChannelIndex+1) mod ChannelCount;
    result:=BASS_ChannelPlay(channel, true);
  end else
    result:=BASS_ChannelPlay(channel, false);
  FPlaying:=true;
end;

function TBassSound.Playing: boolean;
begin
  result:=BASS_ChannelIsActive(channel)=BASS_ACTIVE_PLAYING;
end;

procedure TBassSound.Set3DAttributes(min, max: FLOAT; iangle, oangle, outvol: LongInt);
var mode: integer;
begin
  if FUse3D then begin
    mode:=BASS_3DMODE_NORMAL;
    if not BASS_ChannelSet3DAttributes(channel, mode, min, max, iangle, oangle, outvol) then
      nxSetError(format('Error - BASS_ChannelSet3DAttributes #%d',
        [BASS_ErrorGetCode]));
  end;
end;

procedure TBassSound.SetLocation(const _loc: TVector);
begin
  location.x:=_loc.x;
  location.y:=_loc.y;
  location.z:=-_loc.z;
end;

procedure TBassSound.SetOrientation(const _front: TVector);
begin
  orientation.x:=_front.x;
  orientation.y:=_front.y;
  orientation.z:=-_front.z;
end;

// Panning -1..1 from left to right
procedure TBassSound.SetPanning(pan: single);
var i: integer;
begin
  for i:=0 to ChannelCount-1 do
    BASS_ChannelSetAttribute(FChannel[i], BASS_ATTRIB_PAN, pan);
end;

procedure TBassSound.SetVelocity(const vel: TVector; fps: single);
begin
  velocity.x:=vel.x*fps;
  velocity.y:=vel.y*fps;
  velocity.z:=-vel.z*fps;
end;

// Volume 0..1 from silence to max
procedure TBassSound.SetVolume(vol: single);
var i: integer;
begin
  for i:=0 to ChannelCount-1 do
    BASS_ChannelSetAttribute(FChannel[i], BASS_ATTRIB_VOL, vol);
end;

procedure TBassSound.Stop;
var i: integer;
begin
  FPlaying:=false;
  if not loaded then exit;
  if soundType=stSample then begin
    for i:=0 to channelCount-1 do
      BASS_ChannelPause(FChannel[i]);
  end else BASS_ChannelStop(channel);
end;

function TBassSound.TimeLength: TTime;
begin
  result:=BASS_ChannelBytes2Seconds(channel, Length)*86400;
end;

procedure TBassSound.Unload;
begin
  if loaded then begin
    loaded:=false; FPlaying:=false;
    case SoundType of
      stStream: BASS_StreamFree(handle);
      stMusic: BASS_MusicFree(handle);
      stSample: BASS_SampleFree(handle);
    end;
  end;
end;

procedure TBassSound.Update;
begin
  if Use3D then begin
    if not BASS_ChannelSet3DPosition(channel, BASS_3DVECTOR(location),
      BASS_3DVECTOR(orientation), BASS_3DVECTOR(velocity)) then
      nxSetError('Error - BASS_ChannelSet3DPosition');
  end;
end;

{ TNXSoundListener }

constructor TNXSoundListener.Create;
begin
  orientation.front:=vector(0, 0, 1);
  orientation.up:=vector(0, 1, 0);
  Update;
end;

procedure TNXSoundListener.DoMove;
begin
  location.x:=location.x+velocity.x;
  location.y:=location.y+velocity.y;
  location.z:=location.z+velocity.z;
end;

procedure TNXSoundListener.SetLocation(const _loc: TVector);
begin
  location.x:=_loc.x;
  location.y:=_loc.y;
  location.z:=-_loc.z;
end;

procedure TNXSoundListener.SetOrientation(const _front, _up: TVector);
begin
  orientation.front.x:=_front.x;
  orientation.front.y:=_front.y;
  orientation.front.z:=-_front.z;
  orientation.up.x:=_up.x;
  orientation.up.y:=_up.y;
  orientation.up.z:=-_up.z;
end;

procedure TNXSoundListener.SetVelocity(const vel: TVector; fps: single);
begin
  velocity.x:=vel.x*fps;
  velocity.y:=vel.y*fps;
  velocity.z:=-vel.z*fps;
end;

procedure TNXSoundListener.Update;
begin
  BASS_Set3DPosition(BASS_3DVECTOR(location), BASS_3DVECTOR(velocity),
    BASS_3DVECTOR(orientation.front), BASS_3DVECTOR(orientation.up));
end;

{ TNXSoundEngine }

initialization
{$IFDEF fpc}
  VerCheck:=Hi(BASS_GetVersion) = BASSVERSION;
{$ELSE}
  VerCheck:=HiWord(BASS_GetVersion) = BASSVERSION;
{$ENDIF}

end.
