unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, CastleControl, MainGameUnit,
  CastleControls, CastleColors, CastleUIControls, CastleTriangles, CastleShapes,
  CastleVectors, CastleSceneCore, CastleScene, CastleTransform, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse,
  CastleFilesUtils, Types;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PanelViewport: TPanel;
    PanelPicker: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MenuItem2Click(Sender: TObject);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
  end;

var
  CastleForm: TCastleForm;

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  InitializeLog;
  PrepDone := False;
  Profiler.Enabled := true;
  Caption := 'SheetViewer';
  Memo1.Clear;
end;

procedure TCastleForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if(WheelDelta > 1) then
    CastleApp.WheelZoom += 0.25
  else
    CastleApp.WheelZoom -= 0.25;
  if(CastleApp.WheelZoom < 1) then
    CastleApp.WheelZoom := 1;
  if(CastleApp.WheelZoom > 8) then
    CastleApp.WheelZoom := 8;
end;

procedure TCastleForm.MenuItem2Click(Sender: TObject);
begin
  Memo1.Lines.Add('ApplicationConfig = ' + ApplicationConfig(''));
  Memo1.Lines.Add('ApplicationData = ' + ApplicationData(''));

  SelectDirectoryDialog1.InitialDir := ApplicationData('');
  if SelectDirectoryDialog1.Execute then
      Memo1.Lines.Add('executed')
  else
      Memo1.Lines.Add('not executed');

    Memo1.Lines.Add(' Dir = ' + SelectDirectoryDialog1.FileName);
end;

procedure TCastleForm.WindowBeforeRender(Sender: TObject);
begin

end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  WriteLnLog('WindowOpen : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  RenderReady := False;
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usDpiScale;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

end.

