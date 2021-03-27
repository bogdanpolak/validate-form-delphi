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

function GenerateTextWarnings(const aWarnings: IWarnings): string;
var
  sl: TStringList;
  idx: Integer;
  lWarnings: TArray<TWarning>;
  lKind: TWarningKind;
  msg: string;
  lSource: string;
begin
  sl := TStringList.Create();
  try
    lWarnings := aWarnings.ToArray;
    for idx := 0 to High(lWarnings) do
    begin
      lKind := lWarnings[idx].Kind;
      case lKind of
        wkRequired: msg := '%s is requied';
        wkMaxLength: msg := '%s has too many characters';
        wkMinLength: msg := '%s has too less characters';
        wkRegexMatch: msg := '%s has invalid format';
        wkEmail: msg := '%s is not an email';
        else msg := '!!! unknown warning kind';
      end;
      lSource := lWarnings[idx].Source;
      sl.Add(Format(' * '+msg,[lSource]));
    end;
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
  lWarnings: IWarnings;
begin
  lUser := BuildUser(self);
  try
    lWarnings := TValidationEngine.Validate<TUser>(lUser, 'ctx1');
    if lWarnings.HasAny() then
      MemoValidation.Lines.Text := GenerateTextWarnings(lWarnings)
    else
      MemoValidation.Lines.Text :=  '';
  finally
    lUser.Free;
  end;
end;

end.

