unit MainGameUnit;

{$mode objfpc}{$H+}
// {$define showcam}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleURIUtils,
  X3DNodes, X3DFields, X3DTIme, X3DLoad,
  CastleImages, CastleGLImages, CastleDownload,
  CastleTextureImages, CastleCompositeImage,
  CastleApplicationProperties, CastleLog,
  CastleTimeUtils, CastleKeysMouse,
  MiscFunctions;

type
  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  CheckAssets: String;
  private
    CurrentModel: Integer;
    CurrentAnimation: Integer;
    Viewport: TCastleViewport;
    Backport: TCastleViewport;
    Scene: TCastleScene;
    BackScene: TCastleScene;
    LabelModel: TCastleLabel;
    LabelAnimation: TCastleLabel;
    {$ifdef showcam}
    LabelCamPos: TCastleLabel;
    LabelCamDir: TCastleLabel;
    LabelCamUp: TCastleLabel;
    {$endif}
    BtnNextAnim: TCastleButton;
    BtnPrevAnim: TCastleButton;
    BtnNextModel: TCastleButton;
    BtnPrevModel: TCastleButton;
  public
    LabelFPS: TCastleLabel; // Public for testing
    LabelRender: TCastleLabel;
    WheelZoom: Single;
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const LeftPos: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadScene(filename: String; const ModelIdx: Integer; const AnimationIdx: Integer; SetAnimation: Boolean = True);
    procedure PrevAnimButtonClick(Sender: TObject);
    procedure NextAnimButtonClick(Sender: TObject);
    procedure PrevModelButtonClick(Sender: TObject);
    procedure NextModelButtonClick(Sender: TObject);
    procedure UpdateAnimationLabels;
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  CastleApp: TCastleApp;
  RenderReady: Boolean;

const
  Models: Array[0..5] of String = ('Abberation_SideViewBattler_Large',
               'CarniverousPlant_SideViewBattler_Large',
               'DaemonBone_SideViewBattler_Large',
               'DaemonHowler_SideViewBattler_Large',
               'DaemonInfernalGreater_SideViewBattler_Large',
               'DaemonInfernal_SideViewBattler_Large');

function DetectSheetLayout(const filename: String): TVector2Integer;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

procedure TCastleApp.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const LeftPos: Integer; const ButtonCode: TNotifyEvent = nil);
begin
  objButton := TCastleButton.Create(Application);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpLeft, 10 + (LeftPos * 20));
  objButton.Anchor(vpBottom, 10 + (Line * 35));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TCastleApp.LoadViewport;
begin
  // Set up the background viewport
  Backport := TCastleViewport.Create(Application);
  // Use all the viewport
  Backport.FullSize := true;
  // Position the camera
  Backport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  // Setup 2D
  Backport.Setup2D;
  // Add the viewport to the CGE control
  InsertFront(Backport);

  // Hacky load inline of background
  if not(BackScene = nil) then
    FreeAndNil(BackScene);
  BackScene := TCastleScene.Create(Application);
  BackScene.RenderOptions.MinificationFilter := minNearest;
  BackScene.RenderOptions.MagnificationFilter := magNearest;
  BackScene.Setup2D;
  BackScene.Load('castle-data:/tiles-iso.jpg');
  BackScene.Scale := Vector3(0.5, 0.5, 0.5);
  Backport.Items.Add(BackScene);
  Backport.Items.MainScene := BackScene;

  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Position the camera
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  // Make the viewport Transparent (So we can see the background)
  Viewport.Transparent := True;
  // Setup 2D
  Viewport.Setup2D;
  // Add the viewport to the CGE control
  InsertFront(Viewport);

  // Some Buttons
  CreateButton(BtnPrevAnim, 'Prev  Anim', 6, 0, @PrevAnimButtonClick);
  CreateButton(BtnNextAnim, 'Next  Anim', 6, 8, @NextAnimButtonClick);
  CreateButton(BtnPrevModel, 'Prev Model', 8, 0, @PrevModelButtonClick);
  CreateButton(BtnNextModel, 'Next Model', 8, 8, @NextModelButtonClick);

  // Some Labels
  {$ifdef showcam}
  CreateLabel(LabelCamPos, 0, False);
  CreateLabel(LabelCamDir, 1, False);
  CreateLabel(LabelCamUp, 2, False);
  {$endif}

  CreateLabel(LabelModel, 4);
  CreateLabel(LabelAnimation, 3);
  CreateLabel(LabelRender, 2);
  CreateLabel(LabelFPS, 0);
end;

procedure TCastleApp.NextAnimButtonClick(Sender: TObject);
begin
  if CurrentAnimation < (Scene.AnimationsList.Count - 1) then
    begin
      Inc(CurrentAnimation);
      Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
    end
  else
    begin
      if CurrentModel < (Length(Models) - 1) then
        begin
          Inc(CurrentModel);
          CurrentAnimation := 0;
          LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
        end
      else
        begin
          CurrentModel := 0;
          CurrentAnimation := 0;
          LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
        end;
    end;
    UpdateAnimationLabels;
end;

procedure TCastleApp.PrevAnimButtonClick(Sender: TObject);
begin
  if CurrentAnimation > 0 then
    begin
      Dec(CurrentAnimation);
      Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
    end
  else
    begin
      CurrentModel := Length(Models) - 1;
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, 0, false);
      CurrentAnimation := Scene.AnimationsList.Count - 1;
      Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
    end;
  UpdateAnimationLabels;
end;

procedure TCastleApp.NextModelButtonClick(Sender: TObject);
begin
  if CurrentModel < (Length(Models) - 1) then
    begin
      Inc(CurrentModel);
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
    end
  else
    begin
      CurrentModel := 0;
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
    end;
  UpdateAnimationLabels;
end;

procedure TCastleApp.PrevModelButtonClick(Sender: TObject);
begin
  if CurrentModel > 0 then
    begin
      Dec(CurrentModel);
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
    end
  else
    begin
      CurrentModel := Length(Models) - 1;
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
    end;
end;

procedure TCastleApp.LoadScene(filename: String; const ModelIdx: Integer; const AnimationIdx: Integer; SetAnimation: Boolean = True);
var
  sNode: TX3DRootNode;
  Stream: TStream;
begin
  try
    if not(Scene = nil) then
      FreeAndNil(Scene);
    Scene := TCastleScene.Create(Application);
    Scene.Spatial := [ssDynamicCollisions, ssRendering];
    Scene.RenderOptions.MinificationFilter := minNearest;
    Scene.RenderOptions.MagnificationFilter := magNearest;
    Scene.Setup2D;

    try
      try
        Stream := Download(filename, []);
        sNode := LoadNode(Stream, ExtractURIPath(filename), 'application/x-starling-sprite-sheet');
        Scene.Load(sNode, true);
        Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
            True,
            Viewport.PrepareParams);
        Viewport.Items.Add(Scene);
        Viewport.Items.MainScene := Scene;

        if SetAnimation then
          begin
            Scene.PlayAnimation(Scene.AnimationsList[AnimationIdx], true);
            UpdateAnimationLabels;
          end;
      except
        on E : Exception do
          begin
            WriteLnLog('Error loading TextureAtlas' + LineEnding + E.ClassName + LineEnding + E.Message);
          end;
      end
    finally
      FreeAndNil(Stream);
    end
  except
    on E : Exception do
      begin
        WriteLnLog('Something went wrong' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.UpdateAnimationLabels;
begin
  LabelModel.Caption := Models[CurrentModel] +
                        ' (' + IntToStr(CurrentModel + 1) +
                        ' of ' + IntToStr(Length(Models)) + ')';
  LabelAnimation.Caption := 'Animation : ' + Scene.AnimationsList[CurrentAnimation] +
                        ' (' + IntToStr(CurrentAnimation + 1) +
                        ' of ' + IntToStr(Scene.AnimationsList.Count) + ')';
end;

function DetectSheetLayout(const filename: String): TVector2Integer;
var
  ProcTimer: Int64;
  Layout: TVector2Integer;
  LayoutX, LayoutY: TBooleanArray;
  Texture: TRGBAlphaImage;
  Alpha: TGrayScaleImage;
  AlphaFile: String;
  idx, cnt: Integer;
begin
  try
    Texture := LoadImage(filename, [TRGBAlphaImage]) as TRGBAlphaImage;

    AlphaFile := filename + '.alpha.png';
    if not URIFileExists(AlphaFile) then
      begin
        Alpha := ExtractAlpha(Texture);
        SaveImage(Alpha, AlphaFile);
        FreeAndNil(Alpha);
      end;

    ProcTimer := CastleGetTickCount64;
    LayoutX := ScanForTransparentRows(Texture);
    ProcTimer := CastleGetTickCount64 - ProcTimer;

    if not(LayoutX = nil) then
      begin
        cnt := 0;
        for idx := 0 to Length(LayoutX) - 1 do
          begin
            if not(LayoutX[idx]) then
              begin
                Inc(cnt);
              end;
          end;

        WriteLnLog(filename + LineEnding + ' - ScanForTransparentRows = ' + IntToStr(Length(LayoutX)) +
          ' recs, ' + IntToStr(cnt) + ' in use, time = ' +
          FormatFloat('####0.000000', ProcTimer / 1000) + ' seconds');

      end;


    ProcTimer := CastleGetTickCount64;
    LayoutY := ScanForTransparentColumns(Texture);
    ProcTimer := CastleGetTickCount64 - ProcTimer;

    if not(LayoutY = nil) then
      begin
        cnt := 0;
        for idx := 0 to Length(LayoutY) - 1 do
          begin
            if not(LayoutY[idx]) then
              begin
                Inc(cnt);
              end;
          end;

        WriteLnLog(filename + LineEnding + ' - ScanForTransparentColumns = ' + IntToStr(Length(LayoutY)) +
          ' recs, ' + IntToStr(cnt) + ' in use, time = ' +
          FormatFloat('####0.000000', ProcTimer / 1000) + ' seconds');

      end;
  except
    on E : Exception do
      begin
        ShowMessage('Exception' + LineEnding +
                    'Trying to load : ' + filename + LineEnding +
                     E.ClassName + LineEnding +
                     E.Message);
        Texture := nil;
       end;
  end;
  FreeAndNil(Texture);

  Result := Vector2Integer(1, 1);
end;

function TCastleApp.CheckAssets: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i:= 0 to Length(Models) - 1 do
    begin
      if not URIFileExists('castle-data:/' + Models[i] + '_map_1' + '.starling-xml')then
        Result += 'data' + DirectorySeparator + Models[i] + '_map_1' + '.starling-xml' + LineEnding;
      if not URIFileExists('castle-data:/' + Models[i] + '.png') then
        Result += 'data' + DirectorySeparator + Models[i] + '.png' + LineEnding
      else
        DetectSheetLayout('castle-data:/' + Models[i] + '.png');
    end;
end;

procedure TCastleApp.Start;
var
  assets: String;
begin
  inherited;
  assets := CheckAssets;
  if not(assets = EmptyStr) then
    begin
      ShowMessage('The following assets are missing ...' + LineEnding +
        LineEnding + assets + LineEnding +
        'Please READ the file readme.txt');
      Application.Terminate;
    end;
  CurrentModel := 0;
  CurrentAnimation := 0;
  WheelZoom := 1;
  Scene := nil;
  LoadViewport;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  inherited;
end;

procedure TCastleApp.BeforeRender;
{$ifdef showcam}
var
  Pos, Dir, Up: TVector3;
{$endif}
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Zoom = ' + FormatFloat('####0.00', WheelZoom);
  if not(Scene = nil) then
    Scene.Scale := Vector3(WheelZoom, WheelZoom, WheelZoom);
{$ifdef showcam}
  if not(Viewport = nil) then
    begin
    Viewport.Camera.GetView(Pos, Dir, Up);

    LabelCamPos.Caption := 'Cam Pos : ' +
      FormatFloat('####0.00', Pos.X) + ', ' +
      FormatFloat('####0.00', Pos.Y) + ', ' +
      FormatFloat('####0.00', Pos.Z);

      LabelCamDir.Caption := 'Cam Dir : ' +
        FormatFloat('####0.00', Dir.X) + ', ' +
        FormatFloat('####0.00', Dir.Y) + ', ' +
        FormatFloat('####0.00', Dir.Z);

      LabelCamUp.Caption := 'Cam Up : ' +
        FormatFloat('####0.00', Up.X) + ', ' +
        FormatFloat('####0.00', Up.Y) + ', ' +
        FormatFloat('####0.00', Up.Z);
    end;
{$endif}
end;

procedure TCastleApp.Render;
begin
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
      LoadScene('castle-data:/' + Models[CurrentModel] + '_map_1' + '.starling-xml', CurrentModel, CurrentAnimation);
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

end.

