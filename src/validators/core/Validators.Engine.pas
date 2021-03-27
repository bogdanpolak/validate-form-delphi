unit Validators.Engine;
// created by Daniele Spinetti: [https://github.com/bittimeprofessionals]

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
    class function New: IWarnings; static;
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
  private
    class var FRTTIContext: TRttiContext;
    class procedure DoEntityValidation<T>(aObject: T; aContext: string;
      oWarnings: IWarnings);
    class procedure DoPropertyValidation(aObject: TObject; aContext: string;
      aWarnings: IWarnings);
  public
    class constructor Create;
    class destructor Destroy;
    class function Validate<T: class>(aObject: T; aContext: string): IWarnings;
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

class procedure TValidationEngine.DoEntityValidation<T>(aObject: T;
  aContext: string; oWarnings: IWarnings);
begin
  {
    var
    a: TCustomAttribute;
    lValidator: IValidator<T>;
    Result := TValidationResult.Create;
    lValidator := FValidationContainer.GetValidatorFor<T>(aContext);
    Result.AddWarning(lValidator.Validate(aObject).BrokenRules);
  }
end;

class procedure TValidationEngine.DoPropertyValidation(aObject: TObject;
  aContext: string; aWarnings: IWarnings);
var
  rt: TRttiType;
  a: TCustomAttribute;
  p: TRttiProperty;
  m: TRttiMethod;
  lWarning: TWarning;
  lValue: string;
  lSource: string;
  lResult: TValue;
  isValid: Boolean;
  lKindValue: TValue;
begin
  rt := FRTTIContext.GetType(aObject.ClassType);
  for p in rt.GetProperties do
    for a in p.GetAttributes do
    begin
      if not(a is RuleBaseAttribute) then
        continue;
      if RuleBaseAttribute(a).Context <> aContext then
        continue;
      m := FRTTIContext.GetType(a.ClassType).GetMethod('TryValidate');
      if m = nil then
        continue;
      lValue := p.GetValue(aObject).AsString;
      lSource := Format('%s.%s', [aObject.ClassName, p.Name]);
      lKindValue := TValue.From(lWarning);
      isValid := m.Invoke(a, [lValue, lSource, lKindValue]).AsType<boolean>();
      if not isValid then
      begin
        lWarning := lKindValue.AsType<TWarning>;
        aWarnings.Add(lWarning);
      end;
    end;
end;

class function TValidationEngine.Validate<T>(aObject: T; aContext: string)
  : IWarnings;
begin
  Result := TWarnings.New;
  DoEntityValidation<T>(aObject, aContext, Result);
  DoPropertyValidation(aObject, aContext, Result);
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

class function TWarnings.New: IWarnings;
begin
  Result := TWarnings.Create;
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
    for idx := 0 to FWarnings.Count-1 do
    begin
      sep := IFThen(idx=0,'',',');
      sb.AppendFormat(sep+'{"kind":"%s", "source":"%s"}',
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
