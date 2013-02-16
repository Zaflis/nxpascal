program TileGen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, TileUnit, uBaseConfig, uTileStyles
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmBaseConfig, frmBaseConfig);
  Application.CreateForm(TfrmTileStyles, frmTileStyles);
  Application.Run;
end.

