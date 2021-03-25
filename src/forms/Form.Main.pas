//Created by DCE-Systems [https://github.com/dce-systems]

unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {-}
  Model.Person;

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
    btnSimpleValidation: TButton;
    btnAnnotationValidation: TButton;
    Bevel1: TBevel;
    btnPopulateForm: TButton;
    Label1: TLabel;
    EditRetypePassword: TEdit;
    MemoValidation: TMemo;
    procedure btnSimpleValidationClick(Sender: TObject);
    procedure btnAnnotationValidationClick(Sender: TObject);
    procedure btnPopulateFormClick(Sender: TObject);
  private
    class function BuildPerson(const appForm: TAppForm): TPerson; static;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AppForm: TAppForm;

implementation

uses
  Validators.Attributes,
  Validators.Engine;


{$R *.dfm}

class function TAppForm.BuildPerson(const appForm: TAppForm): TPerson;
begin
  Result := TPerson.Create(
    appForm.EditFirstname.Text,
    appForm.EditLastname.Text,
    appForm.EditEmail.Text,
    appForm.EditPassword.Text);
end;

function AddBullets(const aLines: string): string;
var
  sl: TStringList;
  idx: Integer;
begin
  sl := TStringList.Create();
  try
    sl.Text := aLines;
    for idx := 0 to sl.Count-1 do
      sl[idx] := ' * '+sl[idx];
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
  EditRetypePassword.Text := 'john123';
end;

procedure TAppForm.btnSimpleValidationClick(Sender: TObject);
var
  Person: TPerson;
  PersonValidator: IValidator<TPerson>;
  ValidationResult: IValidationResult;
  Rule: string;
begin
  Person := BuildPerson(self);
  try
    PersonValidator := TPersonValidator.Create;
    ValidationResult := PersonValidator.Validate(Person);
    ValidationResult.Bind('Firstname', EditFirstname);
    ValidationResult.Bind('Lastname', EditLastname);
    ValidationResult.Bind('Email', EditEmail);
    ValidationResult.Bind('Password', EditPassword);
    ValidationResult.ValidationColor := clYellow;
    if not ValidationResult.IsValid then
      for Rule in ValidationResult.BrokenRules do
        ShowMessage(Rule);
  finally
    Person.Free;
  end;
end;

procedure TAppForm.btnAnnotationValidationClick(Sender: TObject);
var
  Person: TPerson;
  ValidationResult: IValidationResult;
begin
  Person := BuildPerson(self);
  try
    ValidationResult := TValidationEngine.PropertyValidation(Person, 'AttributesValidation');
    ValidationResult.Bind('Firstname', EditFirstname);
    ValidationResult.Bind('Lastname', EditLastname);
    ValidationResult.Bind('Email', EditEmail);
    ValidationResult.Bind('Pwd', EditPassword);
    if not ValidationResult.IsValid then
      MemoValidation.Lines.Text := AddBullets(ValidationResult.BrokenRulesText)
    else
      MemoValidation.Lines.Text :=  '';
  finally
    Person.Free;
  end;
end;

end.

