//Created by DCE-Systems [https://github.com/dce-systems]

unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {-}
  Model.User;

type
  TAppForm = class(TForm)
    EditFirstname: TEdit;
    LabelFirstname: TLabel;
    EditLastname: TEdit;
    LabelLastname: TLabel;
    EditEmail: TEdit;
    LabelEmail: TLabel;
    EditPassword: TEdit;
    LabelPwd: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    btnUserValidation: TButton;
    Bevel1: TBevel;
    btnPopulateForm: TButton;
    Label1: TLabel;
    EditRetypePassword: TEdit;
    MemoValidation: TMemo;
    chkHidePassword: TCheckBox;
    procedure btnUserValidationClick(Sender: TObject);
    procedure btnPopulateFormClick(Sender: TObject);
    procedure chkHidePasswordClick(Sender: TObject);
  private
    class function BuildUser(const appForm: TAppForm): TUser; static;
  public
  end;

var
  AppForm: TAppForm;

implementation

uses
  Validators.Attributes,
  Validators.Engine;


{$R *.dfm}

class function TAppForm.BuildUser(const appForm: TAppForm): TUser;
begin
  Result := TUser.Create(
    appForm.EditFirstname.Text,
    appForm.EditLastname.Text,
    appForm.EditEmail.Text,
    appForm.EditPassword.Text);
end;

procedure TAppForm.chkHidePasswordClick(Sender: TObject);
var
  ch: char;
begin
  ch := IFThen(chkHidePassword.Checked,'*',#0)[1];
  EditPassword.PasswordChar := ch;
  EditRetypePassword.PasswordChar := ch;
end;

function AddBullets(const aLines: array of string): string;
var
  sl: TStringList;
  idx: Integer;
begin
  sl := TStringList.Create();
  try
    for idx := 0 to High(aLines) do
      sl.Add(' * '+aLines[idx]);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TAppForm.btnPopulateFormClick(Sender: TObject);
begin
  EditFirstname.Text := 'John';
  EditLastname.Text := 'Kowalski';
  EditEmail.Text := 'john.kowalski@example.com';
  EditPassword.Text := 'john123';
  EditRetypePassword.Text := 'John*123';
end;

procedure TAppForm.btnUserValidationClick(Sender: TObject);
var
  lUser: TUser;
  lValidationResult: IValidationResult;
begin
  lUser := BuildUser(self);
  try
    lValidationResult := TValidationEngine.PropertyValidation(lUser, 'AttributesValidation');
    if not lValidationResult.IsValid then
      MemoValidation.Lines.Text := AddBullets(lValidationResult.BrokenRules)
    else
      MemoValidation.Lines.Text :=  '';
  finally
    lUser.Free;
  end;
end;

end.

