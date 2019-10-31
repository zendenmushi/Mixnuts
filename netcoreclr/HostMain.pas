unit HostMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  NetCoreClr;

type
  TForm4 = class(TForm)
    edtRuntimePath: TEdit;
    Label1: TLabel;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private 宣言 }
    FNetCoreClr : TNetCoreClr;
  public
    { Public 宣言 }
  end;

var
  Form4: TForm4;

implementation


{$R *.dfm}

type
  TCallBack = procedure(msg : PChar); stdcall;
  TClientEntry = procedure (AppPath : PChar; funcptr : TCallBack);

procedure HostShowMessage(msg : PChar); stdcall;
begin
  ShowMessage(string(msg));
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    edtRuntimePath.Text := FileOpenDialog1.FileName;
    if TNetCoreClr.IsValidRuntimePath(edtRuntimePath.Text) then
    begin
      // clr作成
      FNetCoreClr := TNetCoreClr.Create(edtRuntimePath.Text,'HostDemo');

      // dllの検索パスをフルパスで与える。複数のdllがある場合は複数回AddAppPathすることが可能
      // プロジェクトフォルダと同じ位置にexeを出力。そこからの相対パスでクライアント側dllの出力位置までパスを通す
      FNetCoreClr.AddAppPath(ExpandFileName('.\netcore_client\bin\Debug\netstandard2.0'));
      //clr起動
      FNetCoreClr.Start;

      Button1.Enabled := false;
    end;
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  if (FNetCoreClr <> nil) and FNetCoreClr.Started then
  begin
    // 対象DLLのアセンブリ名＋バージョン、呼び出す関数の属するクラス名、呼び出す関数名を渡して、関数ポインタを得る
    var entry := TClientEntry(FNetCoreClr.CreateDelegate('netcore_client, Version=1.0.0', 'netcore_client.Class1', 'Entry'));
    // 呼出し
    entry(PChar(ParamStr(0)), HostShowMessage);
  end;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FNetCoreClr.Free;
end;

end.
