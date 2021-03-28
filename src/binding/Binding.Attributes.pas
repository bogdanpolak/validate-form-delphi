unit Binding.Attributes;

interface

type
  BindWithAttribute = class(TCustomAttribute)
  private
    FSource: string;
  public
    constructor Create(const aSource: string);
    property Source: string read FSource;
  end;

implementation

constructor BindWithAttribute.Create(const aSource: string);
begin
  FSource := aSource;
end;

end.
