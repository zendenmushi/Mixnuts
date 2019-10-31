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
    { Private �錾 }
    FNetCoreClr : TNetCoreClr;
  public
    { Public �錾 }
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
      // clr�쐬
      FNetCoreClr := TNetCoreClr.Create(edtRuntimePath.Text,'HostDemo');

      // dll�̌����p�X���t���p�X�ŗ^����B������dll������ꍇ�͕�����AddAppPath���邱�Ƃ��\
      // �v���W�F�N�g�t�H���_�Ɠ����ʒu��exe���o�́B��������̑��΃p�X�ŃN���C�A���g��dll�̏o�͈ʒu�܂Ńp�X��ʂ�
      FNetCoreClr.AddAppPath(ExpandFileName('.\netcore_client\bin\Debug\netstandard2.0'));
      //clr�N��
      FNetCoreClr.Start;

      Button1.Enabled := false;
    end;
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  if (FNetCoreClr <> nil) and FNetCoreClr.Started then
  begin
    // �Ώ�DLL�̃A�Z���u�����{�o�[�W�����A�Ăяo���֐��̑�����N���X���A�Ăяo���֐�����n���āA�֐��|�C���^�𓾂�
    var entry := TClientEntry(FNetCoreClr.CreateDelegate('netcore_client, Version=1.0.0', 'netcore_client.Class1', 'Entry'));
    // �ďo��
    entry(PChar(ParamStr(0)), HostShowMessage);
  end;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FNetCoreClr.Free;
end;

end.
