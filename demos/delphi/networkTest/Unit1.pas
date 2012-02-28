unit Unit1;

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Graphics,
  nxNetwork, Controls;

// All communication can be masked with key.
// If client doesn't have same key as server he will be
// immediately disconnected.
const Key = 'test';

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    serverTest1: TButton;
    ClientTest1: TButton;
    ServerTest2: TButton;
    ClientTest2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;

    Memo1: TMemo;
    Memo2: TMemo;
    Shape1: TShape;
    Shape2: TShape;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ClientTest2Click(Sender: TObject);
    procedure serverTest1Click(Sender: TObject);
    procedure ClientTest1Click(Sender: TObject);
    procedure ServerTest2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

    // Abstract base classes can be created as TCP or UDP
    client: TClient;
    server: TServer;

    procedure ServerEvent(sender: TConnection; event: TConnectionEvent; ID: integer);
    procedure ClientEvent(sender: TConnection; event: TConnectionEvent; ID: integer);
    procedure ServerData(sender: TConnection; data: {$IFDEF fpc}PByte
      {$ELSE}PByteArray{$ENDIF}; size,ID: integer);
    procedure ClientData(sender: TConnection; data: {$IFDEF fpc}PByte
      {$ELSE}PByteArray{$ENDIF}; size,ID: integer);
  public
  end; 

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.ServerData(sender: TConnection; data:
  {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF}; size,ID: integer);
var s: string;
begin
  if server=nil then exit;
  setlength(s,size);
  move(data[0],s[1],size);
  memo1.Lines.Add(format('Client(%d)> [%s]',[ID,s]));
end;

procedure TForm1.ClientData(sender: TConnection; data:
  {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF}; size,ID: integer);
var s: string;
begin
  if client=nil then exit;
  setlength(s,size);
  move(data[0],s[1],size);
  memo2.Lines.Add(format('Server> [%s]',[s]));
end;

procedure TForm1.ServerEvent(sender: TConnection; event: TConnectionEvent; ID: integer);
begin
  if server=nil then exit;
  if event=ceError then
    memo1.Lines.Add(format('<#%d Error(%d): %s>',
      [ID,server.LastError,server.LastErrorMsg]))
  else
    memo1.Lines.Add(format('<#%d: %s>',
      [ID,server.EventToStr(event)]));
  Timer1Timer(nil);
end;

procedure TForm1.ClientEvent(sender: TConnection; event: TConnectionEvent; ID: integer);
begin
  if client=nil then exit;
  if event=ceError then
    memo2.Lines.Add(format('<Error(%d): %s>',[client.LastError,client.LastErrorMsg]))
  else
    memo2.Lines.Add(format('<%s>',[client.EventToStr(event)]));
  Timer1Timer(nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if server<>nil then
    if server.Connected then begin
      if server.count>0 then shape1.Brush.Color:=clLime
      else if server.UDP and TUDPServer(server).opened then
        shape1.Brush.Color:=clLime
      else shape1.Brush.Color:=clYellow;
    end else shape1.Brush.Color:=clGray;
  if client<>nil then
    if client.Connected then shape2.Brush.Color:=clLime
    else shape2.Brush.Color:=clGray;

  label1.Caption:=format('ServerThreads: %d  ClientThreads: %d',
    [ServerThreads,ClientThreads]);

  button5.Enabled:=(serverThreads+ClientThreads)=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  server.Connect;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  server.Disconnect;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  client.Host:=edit1.Text;
  client.Connect;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  client.Disconnect;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  timer1.Enabled:=false; Button5.Enabled:=false;
  if button5.Tag=0 then begin
    button5.Tag:=1;
    TTCPServer(server).Free;
    client.Free;
    server:=TUDPServer.CreateUDPServer('5400');
    client:=TClient.CreateUDPClient(Edit1.Text,'5400');
    label3.Caption:='Now using UDP';
    button5.Caption:='Change to TCP';
  end else begin
    button5.Tag:=1;
    TUDPServer(server).Free;
    client.Free;
    server:=TTCPServer.CreateTCPServer('5400');
    client:=TClient.CreateTCPClient(Edit1.Text,'5400');
    label3.Caption:='Now using TCP';
    button5.Caption:='Change to UDP';
  end;
  client.onEvent:=ClientEvent; client.onData:=ClientData;
  server.onEvent:=ServerEvent; server.onData:=ServerData;
  client.Mask:=Key; server.Mask:=Key;
  timer1.Enabled:=true;
end;

procedure TForm1.ClientTest2Click(Sender: TObject);
var s: string; i,l: integer;
begin
  if not client.Opened then exit;
  l:=10+random(20);
  setlength(s,l);
  for i:=1 to l do s[i]:=chr(random(95)+32);
  memo2.Lines.Add('['+s+']');
  client.Send(@s[1],l);
end;

procedure TForm1.serverTest1Click(Sender: TObject);
var s: string;
begin
  if not server.Opened then exit;
  s:='test';
  memo1.Lines.Add(s);
  if server.UDP then TUDPServer(server).SendString(s)
  else TTCPServer(server).SendString(-1,s);
end;

procedure TForm1.ClientTest1Click(Sender: TObject);
var s: string;
begin
  if not client.Opened then exit;
  s:='test';
  memo2.Lines.Add(s);
  client.SendString(s);
end;

procedure TForm1.ServerTest2Click(Sender: TObject);
var s: string; i,l: integer;
begin
  if not server.Opened then exit;
  l:=10+random(20);
  setlength(s,l);
  for i:=1 to l do s[i]:=chr(random(95)+32);
  memo1.Lines.Add('['+s+']');
  if server.UDP then TUDPServer(server).Send(@s[1],l)
  else TTCPServer(server).Send(-1,@s[1],l);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  client:=TClient.CreateTCPClient(Edit1.Text,'5400');
  server:=TTCPServer.CreateTCPServer('5400');
  edit2.Text:=client.MyIP;

  client.onEvent:=ClientEvent; client.onData:=ClientData;
  server.onEvent:=ServerEvent; server.onData:=ServerData;
  client.Mask:=Key; server.Mask:=Key;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  if client<>nil then client.Free;
  client:=nil;
  if server<>nil then server.Free;
  server:=nil;
end;

end.

