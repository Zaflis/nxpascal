program GLSLTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  dialogUnit in 'dialogUnit.pas' {errorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TerrorForm, errorForm);
  Application.Run;
end.
