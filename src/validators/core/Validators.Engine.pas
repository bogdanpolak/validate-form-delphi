unit Validators.Engine;

interface

uses
  System.RTTI,
  System.JSON,
  System.StrUtils,
  System.Generics.Collections;

type
  TWarningKind = (wkRequired, wkMaxLength, wkMinLength, wkRegexMatch, wkEmail);

  TWarningKindHelper = record helper for TWarningKind
    function ToString(): string;
  end;

  TWarning = record
  private
    FKind: TWarningKind;
    FSource: string;
  public
    property Kind: TWarningKind read FKind write FKind;
    property Source: string read FSource write FSource;
    constructor Create(aKind: TWarningKind; const aSource: string);
  end;

  IWarnings = interface
    ['{720F1371-1C21-458C-ADDB-68F0DD95CD7C}']
    function ToArray: TArray<TWarning>;
    function ToJSON: string;
    procedure Clear;
    procedure Add(const aWarning: TWarning); overload;
    procedure Add(const aWarnings: TArray<TWarning>); overload;
    function HasAny: boolean;
  end;

  TWarnings = class(TInterfacedObject, IWarnings)
  strict private
    FWarnings: TList<TWarning>;
  private
    function IsAllValid(const aRaiseExceptionIfNot: boolean): boolean;
  public
    constructor Create();
    destructor Destroy; override;
    function ToArray: TArray<TWarning>;
    function ToJSON: string;
    procedure Clear;
    procedure Add(const aWarning: TWarning); overload;
    procedure Add(const aWarnings: TArray<TWarning>); overload;
    function HasAny: boolean;
  end;

  TValidationEngine = class(TObject)
  private const
    ValidateMethodName = 'TryValidate';
  private
    class var FRTTIContext: TRttiContext;
    procedure DoPropertyValidation(aObject: TObject; aProperty: TRttiProperty;
      aWarnings: IWarnings);
    function FindValidateMethod(aCustomAttribute: TCustomAttribute)
      : TRttiMethod;
  public
    class constructor Create;
    class destructor Destroy;
    class function Validate<T: class>(aObject: T): IWarnings;
  end;

implementation

uses
  Validators.Attributes,
  System.SysUtils;

{ TValidationEngine }

class constructor TValidationEngine.Create;
begin
  FRTTIContext := TRttiContext.Create;
end;

class destructor TValidationEngine.Destroy;
begin
  FRTTIContext.Free;
end;

procedure TValidationEngine.DoPropertyValidation(aObject: TObject;
  aProperty: TRttiProperty; aWarnings: IWarnings);
var
  attr: TCustomAttribute;
  lValidateMethod: TRttiMethod;
  lSource: string;
  lPropertyValue: string;
  oWarning: TWarning;
  lWarningValue: TValue;
  lResult: TValue;
  lIsValid: boolean;
begin
  for attr in aProperty.GetAttributes do
  begin
    lValidateMethod := FindValidateMethod(attr);
    if lValidateMethod <> nil then
    begin
      lPropertyValue := aProperty.GetValue(aObject).AsString;
      lSource := Format('%s.%s', [aObject.ClassName, aProperty.Name]);
      lWarningValue := TValue.From(oWarning);
      // Call Rtti Method
      lResult := lValidateMethod.Invoke(attr, [lPropertyValue, lSource,
        lWarningValue]);
      lIsValid := lResult.AsType<boolean>;
      if not lIsValid then
      begin
        oWarning := lWarningValue.AsType<TWarning>;
        aWarnings.Add(oWarning);
      end;
    end;
  end;
end;

function TValidationEngine.FindValidateMethod(aCustomAttribute
  : TCustomAttribute): TRttiMethod;
begin
  if aCustomAttribute is RuleBaseAttribute then
    Result := FRTTIContext.GetType(aCustomAttribute.ClassType)
      .GetMethod(ValidateMethodName)
  else
    Result := nil
end;

class function TValidationEngine.Validate<T>(aObject: T): IWarnings;
var
  lWarnings: IWarnings;
  lValidationEngine: TValidationEngine;
  lRttiType: TRttiType;
  lRttiProperty: TRttiProperty;
begin
  lWarnings := TWarnings.Create;
  lValidationEngine := TValidationEngine.Create;
  try
    lRttiType := FRTTIContext.GetType(aObject.ClassType);
    for lRttiProperty in lRttiType.GetProperties do
      lValidationEngine.DoPropertyValidation(aObject, lRttiProperty, lWarnings);
    Result := lWarnings;
  finally
    lValidationEngine.Free;
  end;
end;

{ TWarning }

constructor TWarning.Create(aKind: TWarningKind; const aSource: string);
begin
  Kind := aKind;
  Source := aSource;
end;

{ TWarnings }

procedure TWarnings.Add(const aWarning: TWarning);
begin
  FWarnings.Add(aWarning);
end;

procedure TWarnings.Add(const aWarnings: TArray<TWarning>);
begin
  FWarnings.AddRange(aWarnings);
end;

function TWarnings.IsAllValid(const aRaiseExceptionIfNot: boolean): boolean;
begin
  Result := FWarnings.Count <= 0;
  if (not Result) and (aRaiseExceptionIfNot) then
    raise Exception.Create(ToJSON);
end;

procedure TWarnings.Clear;
begin
  FWarnings.Clear;
end;

constructor TWarnings.Create;
begin
  FWarnings := TList<TWarning>.Create();
end;

destructor TWarnings.Destroy;
begin
  FWarnings.Free;
  inherited;
end;

function TWarnings.HasAny: boolean;
begin
  Result := FWarnings.Count > 0;
end;

function TWarnings.ToArray: TArray<TWarning>;
begin
  Result := FWarnings.ToArray;
end;

function TWarnings.ToJSON: string;
var
  sb: TStringBuilder;
  lWarning: TWarning;
  idx: Integer;
  sep: string;
begin
  sb := TStringBuilder.Create;
  try
    for idx := 0 to FWarnings.Count - 1 do
    begin
      sep := IFThen(idx = 0, '', ',');
      sb.AppendFormat(sep + '{"kind":"%s", "source":"%s"}',
        [FWarnings[idx].Kind.ToString, FWarnings[idx].Source]);
    end;
  finally
    sb.Free;
  end;
end;

{ TWarningKindHelper }

function TWarningKindHelper.ToString: string;
begin
  Result := TRttiEnumerationType.GetName(Self);
end;

end.
