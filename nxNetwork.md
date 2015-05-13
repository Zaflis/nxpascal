#nxNetwork documentation

# nxNetwork #

---

Uses [Synapse](http://www.ararat.cz/synapse/doku.php)
(Notice: Synapse isn't included in nxPascal. Please download separately, it has its own SVN trunk.)

## Classes ##
  * TConnection
  * TClient (TConnection)
  * TServer (TConnection)
  * TTCPServer (TServer)
  * TUDPServer (TServer)

## TConnection class ##

```
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
procedure MaskCrypt(p: PByte;	size: integer);
```

## TClient class ##

```
constructor CreateTCPClient(_host, _port: string);
constructor CreateUDPClient(_host, _port: string);
procedure Send(p: PByte; size: integer);
procedure SendString(s: string);
```

## TServer class ##

```
procedure Kick(index: integer);
```

## TTCPServer class ##

```
constructor CreateTCPServer(_port: string);
procedure Send(toID: integer; p: PByte; size: integer);
procedure SendString(toID: integer; s: string);
```

## TUDPServer class ##

```
constructor CreateUDPServer(_port: string);
procedure Send(p: PByte; size: integer);
procedure SendString(s: string);
```