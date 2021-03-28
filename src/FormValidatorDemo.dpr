program FormValidatorDemo;

uses
  Vcl.Forms,
  Form.Main in 'forms\Form.Main.pas' {MainForm},
  Model.User in 'model\Model.User.pas',
  Validation.Core.Attributes in 'validation\core\Validation.Core.Attributes.pas',
  Validation.Core in 'validation\core\Validation.Core.pas',
  Validation.Core.Engine in 'validation\core\Validation.Core.Engine.pas',
  Binding.Attributes in 'binding\Binding.Attributes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
