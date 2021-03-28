unit Validation.Core.Engine;

interface

uses
  System.RTTI,
  System.JSON,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  Validation.Core;

type
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
    class function ValidateProperty(aObject: TObject): IWarnings;
  end;

implementation

uses
  Validation.Core.Attributes;

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

class function TValidationEngine.ValidateProperty(aObject: TObject): IWarnings;
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

end.
