program HostSample;

uses
  Vcl.Forms,
  HostMain in 'HostMain.pas' {Form4},
  NetCoreClr in 'NetCoreClr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
