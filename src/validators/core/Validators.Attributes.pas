unit Validators.Attributes;
// created by Daniele Spinetti: [https://github.com/bittimeprofessionals]

interface

uses
  Validators.Engine,
  System.Generics.Collections;

type

  RuleBaseAttribute = class abstract(TCustomAttribute)
  protected
    FContext: string;
    function DoValidate(aValue: string; out lWarningKind: TWarningKind)
      : boolean; virtual; abstract;
  public
    constructor Create(aContext: string);
    property Context: string read FContext;
    function TryValidate(const aValue: string; const aSource: string;
      out aWarning: TWarning): boolean;
  end;

  rule_RequiredAttribute = class(RuleBaseAttribute)
  public
    function DoValidate(aValue: string; out aWarningKind: TWarningKind)
      : boolean; override;
  end;

  LengthValidationAttribute = class abstract(RuleBaseAttribute)
  protected
    FLength: Integer;
  public
    constructor Create(aContext: string; aLength: Integer);
  end;

  rule_MaxLengthAttribute = class(LengthValidationAttribute)
  public
    function DoValidate(aValue: string; out aWarningKind: TWarningKind)
      : boolean; override;
  end;

  rule_MinLengthAttribute = class(LengthValidationAttribute)
  public
    function DoValidate(aValue: string; out aWarningKind: TWarningKind)
      : boolean; override;
  end;

  rule_RegexAttribute = class(RuleBaseAttribute)
  private
    FRegex: string;
  public
    constructor Create(aContext: string; aRegex: string);
    function DoValidate(aValue: string; out aWarningKind: TWarningKind)
      : boolean; override;
  end;

  rule_EmailAttribute = class(RuleBaseAttribute)
  private const
    EmailRegex = '^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$';
    // '/^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/';
  public
    function DoValidate(aValue: string; out aWarningKind: TWarningKind)
      : boolean; override;
  end;

  EntityValidationAttribute = class(RuleBaseAttribute)
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;

{ RequiredValidationAttribute }

function rule_RequiredAttribute.DoValidate(aValue: string;
  out aWarningKind: TWarningKind): boolean;
begin
  aWarningKind := wkRequired;
  Result := not aValue.IsEmpty;
end;

{ LengthValidationAttribute }

constructor LengthValidationAttribute.Create(aContext: string;
  aLength: Integer);
begin
  inherited Create(aContext);
  FLength := aLength;
end;

{ MaxLengthValidationAttribute }

function rule_MaxLengthAttribute.DoValidate(aValue: string;
  out aWarningKind: TWarningKind): boolean;
begin
  aWarningKind := wkMaxLength;
  Result := Length(aValue) <= FLength;
end;

{ MinLengthValidationAttribute }

function rule_MinLengthAttribute.DoValidate(aValue: string;
  out aWarningKind: TWarningKind): boolean;
begin
  aWarningKind := wkMinLength;
  Result := Length(aValue) >= FLength;
end;

{ RegexValidationAttribute }

constructor rule_RegexAttribute.Create(aContext: string; aRegex: string);
begin
  inherited Create(aContext);
  FRegex := aRegex;
end;

function rule_RegexAttribute.DoValidate(aValue: string;
  out aWarningKind: TWarningKind): boolean;
begin
  aWarningKind := wkRegexMatch;
  Result := TRegEx.IsMatch(aValue, FRegex);
end;

{ ValidationAttribute }

constructor RuleBaseAttribute.Create(aContext: string);
begin
  inherited Create;
  FContext := aContext;
end;

function RuleBaseAttribute.TryValidate(const aValue: string; const aSource: string;
  out aWarning: TWarning): boolean;
var
  lIsValid: boolean;
  lWarningKind: TWarningKind;
begin
  Result := DoValidate(aValue, lWarningKind);
  if (not Result) then
    aWarning := TWarning.Create(lWarningKind, aSource);
end;

{ EmailValidationAttribute }

function rule_EmailAttribute.DoValidate(aValue: string;
  out aWarningKind: TWarningKind): boolean;
begin
  aWarningKind := wkEmail;
  Result := TRegEx.IsMatch(aValue, EmailRegex);
end;

end.
