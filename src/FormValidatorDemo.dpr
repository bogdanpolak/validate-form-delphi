program FormValidatorDemo;

uses
  Vcl.Forms,
  Form.Main in 'forms\Form.Main.pas' {AppForm},
  Model.Person in 'model\Model.Person.pas',
  Validators.Attributes in 'engine\Validators.Attributes.pas',
  Validators.Engine in 'engine\Validators.Engine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.
