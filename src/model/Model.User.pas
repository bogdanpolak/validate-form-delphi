unit Model.User;

interface

uses
  System.SysUtils,
  Validators.Attributes,
  Validators.Engine;

type
  TUser = class
  private
    FEmail: string;
    FLastname: string;
    FFirstname: string;
    FAddress: string;
    FPwd: string;
    procedure SetEmail(const Value: string);
    procedure SetFirstname(const Value: string);
    procedure SetLastname(const Value: string);
    procedure SetAddress(const Value: string);
    procedure SetPwd(const Value: string);
  public
    constructor Create(aFirstname, aLastname, aEmail: string; aPwd: string = ''); overload;
    [RequiredValidation('AttributesValidation', 'Firstname is required')]
    [MaxLengthValidation('AttributesValidation', 'Firstname is too long', 8)]
    [MinLengthValidation('AttributesValidation', 'Firstname is too short', 4)]
    property Firstname: string read FFirstname write SetFirstname;
    [RequiredValidation('AttributesValidation', 'Firstname is required')]
    property Lastname: string read FLastname write SetLastname;
    [RequiredValidation('AttributesValidation', 'Firstname is required')]
    [EmailValidation('AttributesValidation', 'Email wrong')]
    property Email: string read FEmail write SetEmail;
    property Address: string read FAddress write SetAddress;
    [RegexValidation('AttributesValidation', 'Password not valid', '^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[$@$!%*?&])[A-Za-z\d$@$!%*?&]{8,}')]
    property Pwd: string read FPwd write SetPwd;
  end;

implementation

constructor TUser.Create(aFirstname, aLastname, aEmail, aPwd: string);
begin
  inherited Create;
  FFirstname := aFirstname;
  FLastname := aLastname;
  FEmail := aEmail;
  FPwd := aPwd;
end;

procedure TUser.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TUser.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TUser.SetFirstname(const Value: string);
begin
  FFirstname := Value;
end;

procedure TUser.SetLastname(const Value: string);
begin
  FLastname := Value;
end;

procedure TUser.SetPwd(const Value: string);
begin
  FPwd := Value;
end;

end.
