unit nxNetwork;

{
  Uses Synapse library
  http://www.ararat.cz/synapse/doku.php
}

interface

uses Classes, SysUtils, synsock, blcksock;

const
  ExitCode = '#End#';
  UDPInit = '#UDPInit#';

type
  TConnection = class;

  TConnectionEvent = (ceConnected, ceDisconnected, ceError, ceJoined,
    ceLeft, ceListening, ceNewSocket, ceAuthOK, ceAuthFail, ceDebug,
    ceExitCode);

  TConnectionOnEvent = procedure(Sender: TConnection; event: TConnectionEvent;
    ID: integer) of object;
  TConnectionOnData = procedure(Sender: TConnection; Data:
    {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF}; size, ID: integer)
    of object;

  { TClientThread }

  TClientThread = class(TThread)
  private
    Conn: TConnection;
  public
    freeSocket: boolean;
    constructor Create(_conn: TConnection);
    procedure Execute; override;
  end;

  { TServerThread }

  TServerThread = class(TThread)
  private
    Conn: TConnection;
  public
    freeSocket: boolean;
    constructor Create(_conn: TConnection);
    procedure Execute; override;
  end;

  { TServerSubThread }

  TServerSubThread = class(TThread)
  private
    Conn: TConnection;
    sock: TSocket;
    index: integer;
    Opened: boolean;
  public
    tcpSock: TTCPBlockSocket;
    ID: integer;
    constructor Create(_conn: TConnection; _sock: TSocket; _index, _ID: integer);
    procedure Execute; override;
  end;

  TConnectionOnCrypt = procedure(const Data: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
    size: integer; decrypt: boolean) of object;

  { TConnection }

  TConnection = class
    clients: array of TServerSubThread;
  private
    FHost, FPort, FMyIP, FLastErrorMsg, FMask, FKey: string;
    FTCP, FServer, FConnected, FOpened: boolean;
    FClients, FClientsMax, FLastError, FClientID: integer;
    tcpSock: TTCPBlockSocket;
    udpSock: TUDPBlockSocket;
    serverThread: TServerThread;
    clientThread: TClientThread;
    procedure doClose(index: integer);
    procedure doConnect(ID: integer);
    procedure doData(ID: integer; Data: string; size: integer);
    procedure doError(Error: integer; Msg: string; ID: integer);
    procedure SetClients(n: integer);
    function getUDP: boolean;
    function getIsClient: boolean;
    function NewClientID: integer;
    procedure SetMask(s: string);
    procedure SetConnected(enable: boolean);
  public
    onCrypt: TConnectionOnCrypt;
    onData: TConnectionOnData;
    onEvent: TConnectionOnEvent;
    property Mask: string Read FKey Write SetMask;
    property LastError: integer Read FLastError;
    property LastErrorMsg: string Read FLastErrorMsg;
    property Connected: boolean Read FConnected write SetConnected;
    property Count: integer Read FClients;
    property Host: string Read FHost write FHost;
    property MyIP: string Read FMyIP;
    property Port: string Read FPort write FPort;
    property Opened: boolean Read FOpened;
    property Server: boolean Read FServer;
    property Client: boolean Read getIsClient;
    property TCP: boolean Read FTCP;
    property UDP: boolean Read GetUDP;
    destructor Destroy; override;
    function Connect: boolean;
    procedure Disconnect;
    function EventToStr(event: TConnectionEvent): string;
    procedure MaskCrypt(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
      size: integer);
  end;

  TServer = class(TConnection)
  public
    MaxConnections: integer;
    procedure Kick(index: integer);
  end;

  { TTCPServer }

  TTCPServer = class(TServer)
  public
    constructor CreateTCPServer(_port: string);
    procedure Send(toID: integer; p: {$IFDEF fpc}PByte
      {$ELSE}PByteArray{$ENDIF}; size: integer);
    procedure SendString(toID: integer; s: string);
  end;

  { TUDPServer }

  TUDPServer = class(TServer)
  public
    constructor CreateUDPServer(_port: string);
    procedure Send(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
      size: integer);
    procedure SendString(s: string);
  end;

  TClient = class(TConnection)
  public
    constructor CreateTCPClient(_host, _port: string);
    constructor CreateUDPClient(_host, _port: string);
    procedure Send(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
      size: integer);
    procedure SendString(s: string);
  end;

var
  ServerThreads: integer = 0;
  ClientThreads: integer = 0;

implementation

uses Math;

function Pow2fit(n: integer): integer;
var neg: boolean;
begin
  if n<0 then begin
    n :=-n; neg := True;
  end else neg := False;
  if n<3 then Result := n
  else Result := round(intpower(2, ceil(log2(n))));
  if neg then Result :=-Result;
end;

{ TServer }

procedure TServer.Kick(index: integer);
begin
  doClose(index);
end;

function TConnection.Connect: boolean;
begin
  Connect := False;
  if Connected then exit;
  if server and (ServerThreads>0) then exit;
  if client and (ClientThreads>0) then exit;
  FConnected := False; FOpened := False;
  if tcp and Server then tcpSock.Bind(FHost, FPort)
  else if udp and Server then begin
    udpSock.Bind(FHost, FPort);
  end else if tcp and Client then tcpSock.Connect(FHost, FPort)
  else if udp and Client then udpSock.Connect(FHost, FPort);
  if Server then begin
    if tcp and (tcpSock.LastError<>0) then begin
      doError(tcpSock.LastError, tcpSock.LastErrorDesc,-1);
      doClose(-1); exit;
    end else if udp and (udpSock.LastError<>0) then begin
      doError(udpSock.LastError, udpSock.LastErrorDesc,-1);
      doClose(-1); exit;
    end else if tcp then tcpSock.Listen;
  end;
  if tcp and (tcpSock.LastError<>0) then begin
    doError(tcpSock.LastError, tcpSock.LastErrorDesc,-1);
    doClose(-1); exit;
  end else if udp and (udpSock.LastError<>0) then begin
    doError(tcpSock.LastError, tcpSock.LastErrorDesc,-1);
    doClose(-1); exit;
  end;
  FConnected := True;
  if server then begin
    serverThread := TServerThread.Create(self);
    if assigned(onEvent) then onEvent(self, ceListening,-1);
  end else begin
    if assigned(onEvent) then onEvent(self, ceConnected,-1);
    clientThread := TClientThread.Create(self);
  end;
  Connect := True;
end;

destructor TConnection.Destroy;
var i: integer;
begin
  onEvent := nil; onData := nil; onCrypt := nil;
  FConnected := False;
  if server then begin
    for i := Count-1 downto 0 do
      if clients[i]<>nil then begin
        clients[i].Terminate; clients[i].Conn := nil;
        clients[i] := nil;
      end;
    FClients := 0; Setlength(clients, 0);
    if serverThread = nil then begin
      if tcpSock<>nil then tcpSock.Free;
      if udpSock<>nil then udpSock.Free;
    end else begin
      serverThread.freeSocket := True; serverThread.Terminate;
      serverThread.Conn := nil; serverThread := nil;
      if tcpSock<>nil then tcpSock.AbortSocket;
    end;
  end else begin // If client
    if clientThread = nil then begin
      if tcpSock<>nil then tcpSock.Free;
      if udpSock<>nil then udpSock.Free;
    end else begin
      clientThread.freeSocket := True; clientThread.Terminate;
      clientThread.Conn:=nil; clientThread:=nil;
    end;
  end;
  tcpSock := nil; udpSock := nil;
  inherited;
end;

procedure TConnection.Disconnect;
begin
  if connected then doClose(-1);
end;

function TConnection.EventToStr(event: TConnectionEvent): string;
begin
  case event of
    ceAuthFail: EventToStr := 'Authentication Failed';
    ceAuthOK: EventToStr := 'Authentication OK';
    ceConnected: EventToStr := 'Connected';
    ceDebug: EventToStr := 'Debug event';
    ceDisconnected: EventToStr := 'Disconnected';
    ceError: EventToStr := 'Error';
    ceExitCode: EventToStr := 'Exit code';
    ceJoined: EventToStr := 'Client joined';
    ceLeft: EventToStr := 'Socket closed';
    ceListening: EventToStr := 'Listening';
    ceNewSocket: EventToStr := 'Opened new socket';
    else EventToStr := 'Unknown event';
  end;
end;

procedure TConnection.doClose(index: integer);
var i: integer;
begin
  if index<0 then begin // Disconnect
    FConnected := False; FOpened := False;
    if serverThread<>nil then begin
      for i := Count-1 downto 0 do
        if clients[i]<>nil then begin
          clients[i].Terminate; clients[i] := nil;
        end;
      serverThread.Terminate; serverThread := nil;
      FClients := 0; FClientsMax := 0; Setlength(clients, 0);
      if tcpSock<>nil then tcpSock.AbortSocket;
    end;
    if clientThread<>nil then begin
      clientThread.Terminate; clientThread := nil;
    end;
  end else if server and (Count>0) then begin
    // Client left
    if Clients[index]<>nil then begin
      Clients[index].Terminate;
    end;
    Clients[index] := Clients[Count-1];
    Clients[index].index := index; SetClients(Count-1);
    if Count<= 0 then FOpened := False;
  end;
end;

procedure TConnection.doConnect(ID: integer);
begin
  if server then begin
    if ID>=0 then begin
      if assigned(onEvent) then onEvent(self, ceJoined, ID);
    end else if assigned(onEvent) then
      onEvent(self, ceConnected, ID);
  end;
end;

procedure TConnection.doData(ID: integer; Data: string; size: integer);
begin
  MaskCrypt(@Data[1], size);
  if assigned(onCrypt) then onCrypt(@Data[1], size, True);
  if assigned(onData) then onData(self, @Data[1], size, ID);
end;

procedure TConnection.doError(Error: integer; Msg: string; ID: integer);
begin
  FLastError := Error; FLastErrorMsg := Msg;
  if assigned(onEvent) then onEvent(self, ceError, ID);
end;

procedure TConnection.MaskCrypt(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
  size: integer);
var i, l: integer;
begin
  l := length(FMask);
  if l<1 then exit;
  for i := 0 to size-1 do p[i] := p[i] xor byte(FMask[(i mod l)+1]);
end;

function TConnection.NewClientID: integer;
var i: integer; accept: boolean;
begin
  repeat
    accept := True; FClientID := FClientID+1;
    if FClientID<0 then FClientID := 0;
    for i := 0 to Count-1 do
      if FClientID = Clients[i].ID then begin
        accept := False; FClientID := FClientID+1;
        if FClientID<0 then FClientID := 0;
      end;
  until accept;
  NewClientID := FClientID;
end;

procedure TConnection.SetMask(s: string);
var i, l, l2: integer; n: integer;
begin
  FKey := s; l := length(s);
  if l>0 then begin
    l2 := l;
    if l2<64 then l2 := 64;
    setlength(FMask, l2);
    for i := 1 to l2 do begin
      n := byte(s[1+(i-1) mod l]); n := (n+i*59) mod 256;
      FMask[i] := char(byte(n));
    end;
  end else FMask := s;
end;

procedure TConnection.SetConnected(enable: boolean);
begin
  if enable then Connect
  else Disconnect;
end;

procedure TConnection.SetClients(n: integer);
begin
  FClients := n; n := pow2fit(n);
  if n<>FClientsMax then begin
    FClientsMax := n; setlength(clients, FClientsMax);
  end;
end;

function TConnection.getUDP: boolean;
begin
  getUDP := not FTCP;
end;

function TConnection.getIsClient: boolean;
begin
  getIsClient := not FServer;
end;

{ TClientThread }

constructor TClientThread.Create(_conn: TConnection);
begin
  inherited Create(False); Conn := _conn; Inc(ClientThreads);
  FreeOnTerminate := True;
end;

procedure TClientThread.Execute;
var err: integer; errDesc: string; _tcpSock: TTCPBlockSocket;
    _udpSock: TUDPBlockSocket; _TCP: boolean;

  procedure getError;
  begin
    if _TCP then begin
      err := _tcpSock.LastError; errDesc := _tcpSock.LastErrorDesc;
    end else begin
      err := _udpSock.LastError; errDesc := _udpSock.LastErrorDesc;
    end;
  end;

var s: string; l, n: integer;
begin
  _TCP:=conn.TCP; _tcpSock:=conn.tcpSock; _udpSock:=conn.udpSock;
  try
    // Authentication (TCP only)
    if (not Terminated) and _TCP then begin
      //n := length(conn.mask);
      //if n<64 then
      n := 64;
      setlength(s, n); // receive masked string
      _tcpSock.RecvBufferEx(@s[1], n, 2000);
      //s:=_tcpSock.RecvBlock(2000);
      setlength(s, n);
      if conn = nil then Terminate;
      if not Terminated then begin
        getError;
        if err<>0 then begin
          conn.doError(err, errDesc,-1); Terminate;
        end else begin
          conn.MaskCrypt(@s[1], n); // Unmask string
          // Send unmasked string and wait for reply
          _tcpSock.SendBuffer(@s[1], n);

          //s:=_tcpSock.RecvString(2000);
          s := _tcpSock.RecvBlock(2000); // Wait for 'Accepted'

          if conn = nil then Terminate;
          if not Terminated then begin
            getError;
            if err<>0 then begin
              conn.doError(err, errDesc,-1); Terminate;
            end else if s<>'Accepted' then Terminate;
          end;
        end;
      end;
      if (conn<>nil) and assigned(conn.onEvent) then
        if terminated then conn.onEvent(conn, ceAuthFail,-1)
        else conn.onEvent(conn, ceAuthOk,-1);

    // UDP start
    end else if (not Terminated) and (not _TCP) then begin
      // UDP sends init string to let server know about new connection
      // This also checks if connection is possible
      _udpSock.SendString(UDPInit); getError;
      if err<>0 then begin
        if conn<>nil then conn.doError(err, errDesc,-1);
        Terminate;
      end;
    end;

    if (not terminated) and (conn<>nil) then conn.FOpened := True;

    while not Terminated do begin
      if _TCP then s := _tcpSock.RecvBlock(200)
      else s := _udpSock.RecvBlock(200); // Receive data
      if conn = nil then Terminate;
      getError;
      if (err<>0) and (err<>WSAETIMEDOUT) then begin
        if conn<>nil then
          conn.doError(err, errDesc,-1);
        break;
      end;
      if terminated then begin
        if _TCP then begin
          _tcpSock.SendInteger(length(ExitCode));
          _tcpSock.SendString(ExitCode); //_tcpSock.RecvByte(1000);
        end else begin
          _udpSock.SendInteger(length(ExitCode));
          _udpSock.SendString(ExitCode); //_udpSock.RecvByte(1000);
        end;
        break;
      end else begin
        if s = ExitCode then begin
          //if _TCP then _tcpSock.SendByte(1)
          //else _udpSock.SendByte(1);
          Terminate;
          if (conn<>nil) and assigned(conn.onEvent) then
            conn.onEvent(conn, ceExitCode,-1); break;
        end else if conn<>nil then begin
          l := length(s);
          if l>0 then conn.doData(-1, s, l); // Handle data
        end;
      end;
    end; // while
  finally
    if _tcp then _tcpSock.CloseSocket
    else _udpSock.CloseSocket;
    if freeSocket then begin
      if _tcp then _tcpSock.Free
      else _udpSock.Free;
    end else if conn<>nil then begin
      conn.FOpened := False;
      if _tcp then _tcpSock.CloseSocket
      else _udpSock.CloseSocket;
      if conn.Connected then conn.doClose(-1);
    end;
  end;
  if (conn<>nil) and assigned(conn.onEvent) then
    conn.onEvent(conn, ceDisconnected,-1);
  _tcpSock := nil; _udpSock := nil; conn := nil; Dec(ClientThreads);
end;

{ TServerThread }

constructor TServerThread.Create(_conn: TConnection);
begin
  inherited Create(False); Conn := _conn; conn.FClientID := 0;
  FreeOnTerminate := True; Inc(ServerThreads);
end;

procedure TServerThread.Execute;
var ClientSock: TSocket; n, l, newid: integer;
    _tcpSock: TTCPBlockSocket; _udpSock: TUDPBlockSocket;
    _tcp,_opened: boolean; s: string;
begin
  _tcp := conn.TCP; _tcpSock := conn.tcpSock;
  _udpSock := conn.udpSock; _opened:=conn.Opened;
  try
    while not Terminated do begin
      if conn = nil then Terminate;
      if _TCP then begin
        // *** TCP Server ***
        with _tcpSock do begin
          ClientSock := accept; // Wait for new connections
          if (conn = nil) or (not conn.Connected) then Terminate;
          if not Terminated then
            if lastError = 0 then begin
              if conn.count<TServer(conn).MaxConnections then begin
                newID := conn.NewClientID; n := conn.Count;
                conn.SetClients(conn.Count+1);
                conn.Clients[n] := TServerSubThread.Create(conn, ClientSock, n, newID);
                conn.FOpened := True; _opened:=True;
              end else
                sleep(1000);
            end else begin
              if conn<>nil then
                conn.doError(LastError, LastErrorDesc,-1);
              Terminate;
            end;
        end;
      end else // *** UDP Server ***
        with _udpSock do begin
          s := RecvBlock(200); // Receive data
          if conn = nil then Terminate;
          if lastError = WSAETIMEDOUT then begin // Timed out
            //if conn <> nil then conn.doError(LastError, LastErrorDesc, -1);
          end else if lastError<>0 then begin
            if conn<>nil then
              conn.doError(LastError, LastErrorDesc,-1);
            break; // Disconnect
          end;
          if not terminated then begin
            if s = ExitCode then begin
              //SendByte(1);
              if conn<>nil then begin
                conn.FOpened := False; _opened:=false;
                if assigned(conn.onEvent) then
                  conn.onEvent(conn, ceExitCode,-1);
              end;
            end else begin
              l := length(s);
              if (l>0) and (conn<>nil) then begin
                conn.FOpened := True; _opened:=true;
                if s<>UDPInit then
                  conn.doData(-1, s, l) // Handle data
                else if assigned(conn.onEvent) then
                  conn.onEvent(conn, ceAuthOk,-1);
              end;
            end;
          end else begin
            // Terminated
            if _opened then begin
              SendInteger(length(ExitCode));
              SendString(ExitCode); //RecvByte(1000);
            end;
            break;
          end;
        end;
    end;
    if freeSocket then begin
      if _tcp then _tcpSock.Free
      else _udpSock.Free;
    end else begin
      if _tcp then _tcpSock.CloseSocket
      else _udpSock.CloseSocket;
      if (conn<>nil) and conn.connected then
        conn.doClose(-1);
    end;
  finally
    Dec(ServerThreads);
  end;
  if (conn<>nil) and assigned(conn.onEvent) and (ServerThreads=0) then
    conn.onEvent(conn, ceDisconnected,-1); conn := nil;
end;

{ TServerSubThread }

constructor TServerSubThread.Create(_conn: TConnection; _sock: TSocket;
  _index, _ID: integer);
begin
  inherited Create(False); Conn := _conn; sock := _sock;
  index := _index; ID := _ID; Opened := False;
  if Conn.TCP then tcpSock := TTCPBlockSocket.Create;
  FreeOnTerminate := True; Inc(ServerThreads);
end;

procedure TServerSubThread.Execute;
var s, s2: string; i, l, n: integer;
begin
  tcpSock := TTCPBlockSocket.Create;
  try
    if (not Terminated) and (conn<>nil) then begin
      tcpSock.socket := Sock; tcpsock.GetSins;
      if assigned(conn.onEvent) then
        conn.onEvent(conn, ceNewSocket, ID);
      with tcpSock do begin
        // Authenticate
        //n := length(conn.mask);
        //if n<64 then
        n := 64;
        setlength(s, n); setlength(s2, n);
        for i := 1 to n do s2[i] := chr(random(95)+32);
        s := copy(s2,1,n); // Keep original string for comparison later
        conn.MaskCrypt(@s[1], n); // Mask string
        SendBuffer(@s[1], n); // Send masked string
        RecvBufferEx(@s[1], n, 2000); // Receive it unmasked
        setlength(s, n);
        if not Terminated then
          if lastError<>0 then begin
            conn.doError(LastError, LastErrorDesc, ID);
            Terminate;
          end else if s<>s2 then begin
            // Authentication failed
            if assigned(conn.onEvent) then
              conn.onEvent(conn, ceAuthFail, ID);
            Terminate;
          end else begin
            // if sent and received string match
            if conn = nil then Terminate
            else if not Terminated then begin
              SendBlock('Accepted');
              if assigned(conn.onEvent) then
                conn.onEvent(conn, ceAuthOk, ID);
              Opened:=True; conn.doConnect(ID);
            end;

            // Start listening socket
            while not Terminated do begin
              s := RecvBlock(200); // Receive data
              if conn = nil then Terminate;
              if lastError = WSAETIMEDOUT then begin // Timed out
                //if conn <> nil then conn.doError(LastError, LastErrorDesc, ID);
              end else if lastError<>0 then begin
                if conn<>nil then
                  conn.doError(LastError, LastErrorDesc, ID);
                break; // Disconnect
              end;
              if not terminated then begin
                if s = ExitCode then begin
                  //SendByte(1);
                  Terminate;
                  if (conn<>nil) and assigned(conn.onEvent) then
                    conn.onEvent(conn, ceExitCode, ID); break;
                end else begin
                  l := length(s);
                  if l>0 then conn.doData(ID, s, l); // Handle data
                end;
              end else begin // Terminated
                SendInteger(length(ExitCode));
                SendString(ExitCode); //RecvByte(1000);
                break;
              end;
            end;
          end;
      end;
    end;
  finally
    tcpSock.Free; tcpSock := nil; Opened := False;
  end;
  if conn<>nil then begin
    if assigned(conn.onEvent) then conn.onEvent(conn, ceLeft, ID);
    if conn.Connected then conn.doClose(index);
  end;
  Dec(ServerThreads);
  if (conn<>nil) and assigned(conn.onEvent) and (ServerThreads=0) then
    conn.onEvent(conn, ceDisconnected,-1); conn := nil;
end;

{ TUDPServer }

constructor TUDPServer.CreateUDPServer(_port: string);
begin
  FTCP := False; FServer := True; udpSock := TUDPBlockSocket.Create;
  FHost := '0.0.0.0'; FPort := _port; FClientID := 0;
  MaxConnections:=64;
  //FMyIP := udpSock.GetLocalSinIP;
end;

procedure TUDPServer.Send(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
  size: integer);
begin
  if client or (not connected) or (not opened) then exit;
  if assigned(onCrypt) then onCrypt(p, size, False);
  MaskCrypt(p, size);
  if udp then
    with udpSock do begin
      SendInteger(size);
      SendBuffer(p, size);
      if LastError<>0 then doError(LastError, LastErrorDesc,-1);
    end;
  MaskCrypt(p, size);
end;

procedure TUDPServer.SendString(s: string);
var l: integer;
begin
  if client or (not connected) or (not opened) then exit;
  l := length(s);
  if assigned(onCrypt) then onCrypt(@s[1], l, False);
  MaskCrypt(@s[1], l);
  if udp then
    with udpSock do begin
      SendInteger(l);
      SendBuffer(@s[1], l);
      if LastError<>0 then doError(LastError, LastErrorDesc,-1);
    end;
end;

{ TTCPServer }

constructor TTCPServer.CreateTCPServer(_port: string);
begin
  FTCP := True; FServer := True; tcpSock := TTCPBlockSocket.Create;
  FHost := '0.0.0.0'; FPort := _port; FClientID := 0;
  MaxConnections:=64;
  //FMyIP := tcpSock.GetLocalSinIP;
end;

procedure TTCPServer.Send(toID: integer; p: {$IFDEF fpc}PByte
  {$ELSE}PByteArray{$ENDIF}; size: integer);
var i: integer;
begin
  if (not connected) or client then exit;
  if assigned(onCrypt) then onCrypt(p, size, False);
  MaskCrypt(p, size);
  for i := Count-1 downto 0 do
    if (clients[i].ID = toID) or (toID<0) then
      if (clients[i].tcpSock<>nil) and clients[i].Opened then
        with clients[i].tcpSock do begin
          SendInteger(size);
          SendBuffer(p, size);
          if LastError<>0 then begin
            doError(LastError, LastErrorDesc, Clients[i].ID);
            clients[i].Terminate; doClose(i);
          end;
        end;
  MaskCrypt(p, size);
end;

procedure TTCPServer.SendString(toID: integer; s: string);
var i, l: integer;
begin
  if (not connected) or client then exit;
  l := length(s);
  if assigned(onCrypt) then onCrypt(@s[1], l, False);
  MaskCrypt(@s[1], l);
  for i := Count-1 downto 0 do
    if (clients[i].ID=toID) or (toID<0) then
      if clients[i].opened then
        with clients[i].tcpSock do begin
          SendInteger(l);
          SendBuffer(@s[1], l);
          if LastError<>0 then begin
            doError(LastError, LastErrorDesc, Clients[i].ID);
            clients[i].Terminate; doClose(i);
          end;
        end;
end;

{ TClient }

constructor TClient.CreateTCPClient(_host, _port: string);
begin
  FTCP := True; FServer := False; tcpSock := TTCPBlockSocket.Create;
  FHost := _host; FPort := _port;
  FMyIP := tcpSock.GetLocalSinIP;
end;

constructor TClient.CreateUDPClient(_host, _port: string);
begin
  FTCP := False; FServer := False; udpSock := TUDPBlockSocket.Create;
  FHost := _host; FPort := _port;
  //FMyIP := udpSock.GetLocalSinIP;
end;

procedure TClient.Send(p: {$IFDEF fpc}PByte{$ELSE}PByteArray{$ENDIF};
  size: integer);
begin
  if server or (not connected) or (not opened) then exit;
  if assigned(onCrypt) then onCrypt(p, size, False);
  MaskCrypt(p, size);
  if tcp then begin
    tcpsock.SendInteger(size);
    tcpSock.SendBuffer(p, size);
    if tcpSock.LastError<>0 then begin
      doError(tcpSock.LastError, tcpSock.LastErrorDesc,-1);
      doClose(-1);
    end;
  end else begin
    udpsock.SendInteger(size);
    udpsock.SendBuffer(p, size);
    if udpSock.LastError<>0 then begin
      doError(udpSock.LastError, udpSock.LastErrorDesc,-1);
      doClose(-1);
    end;
  end;
  MaskCrypt(p, size);
end;

procedure TClient.SendString(s: string);
var l: integer;
begin
  if server or (not connected) or (not opened) then exit;
  l := length(s);
  if assigned(onCrypt) then onCrypt(@s[1], l, False);
  MaskCrypt(@s[1], l);
  if tcp then begin
    tcpsock.SendInteger(l);
    tcpSock.SendBuffer(@s[1], l);
    if tcpSock.LastError<>0 then begin
      doError(tcpSock.LastError, tcpSock.LastErrorDesc,-1);
      doClose(-1);
    end;
  end else begin
    udpsock.SendInteger(l);
    udpsock.SendBuffer(@s[1], l);
    if udpSock.LastError<>0 then begin
      doError(udpSock.LastError, udpSock.LastErrorDesc,-1);
      doClose(-1);
    end;
  end;
end;

end.

