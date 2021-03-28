unit Model.User;

interface

uses
  System.SysUtils,
  Validation.Core.Attributes;

type
  TUser = class
  private
    FEmail: string;
    FLastname: string;
    FFirstname: string;
    FAddress: string;
    FPassword: string;
    procedure SetEmail(const Value: string);
    procedure SetFirstname(const Value: string);
    procedure SetLastname(const Value: string);
    procedure SetAddress(const Value: string);
    procedure SetPassword(const Value: string);
  public
    constructor Create(aFirstname, aLastname, aEmail: string; aPwd: string = ''); overload;
    [rule_Required('ctx1')]
    [rule_MinLength('ctx1',4)]
    [rule_MaxLength('ctx1',8)]
    property Firstname: string read FFirstname write SetFirstname;
    [rule_Required('ctx1')]
    property Lastname: string read FLastname write SetLastname;
    [rule_Required('ctx1')]
    [rule_Email('ctx1')]
    property Email: string read FEmail write SetEmail;
    property Address: string read FAddress write SetAddress;
    [rule_MinLength('ctx1',8)]
    [rule_Regex('ctx1','^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[$@$!%*?&])[A-Za-z\d$@$!%*?&]{8,}')]
    property Password: string read FPassword write SetPassword;
  end;

implementation

constructor TUser.Create(aFirstname, aLastname, aEmail, aPwd: string);
begin
  inherited Create;
  FFirstname := aFirstname;
  FLastname := aLastname;
  FEmail := aEmail;
  FPassword := aPwd;
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

procedure TUser.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

end.
