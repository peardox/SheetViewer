unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors, CastleImages, CastleUtils;

type
  TBooleanArray = Array of Boolean;
  PBooleanArray = ^TBooleanArray;

function ScanForTransparentRows(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TBooleanArray;
function ScanForTransparentColumns(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TBooleanArray;
function ExtractAlpha(const ImageIn: TCastleImage; const InvertAlpha: Boolean = False): TGrayscaleImage;

implementation

function ExtractAlpha(const ImageIn: TCastleImage; const InvertAlpha: Boolean = False): TGrayscaleImage;
var
  ImageOut: TGrayscaleImage;
  PSource: PVector4Byte;
  PDest: PByte;
  x, y: Integer;
begin
  ImageOut := nil;

  if not(ImageIn.HasAlpha) then
    Exit(nil);

  if not(ImageIn = nil) then
    begin
      if not(ImageIn.Dimensions.IsZero) then
        begin
          ImageOut := TGrayscaleImage.Create(ImageIn.Dimensions.X, ImageIn.Dimensions.Y);
          PSource := ImageIn.PixelPtr(0, 0);
          PDest := ImageOut.PixelPtr(0, 0);
          for y := 0 to ImageIn.Dimensions.Y -1 do
            begin
              for x := 0 to ImageIn.Dimensions.X -1 do
                begin
                  if InvertAlpha then
                    PDest^ := not(PSource^.W)
                  else
                    PDest^ := PSource^.W;
                  Inc(PSource);
                  Inc(PDest);
                end;
            end;
        end;
    end;
  Result := ImageOut;
end;

function ScanForTransparentRows(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TBooleanArray;
var
  PSource: PVector4Byte;
  x, y: Integer;
  scanWidth, scanHeight: Integer;
  scanrow: TBooleanArray;
begin
  scanrow := nil;

  if not(ImageIn.HasAlpha) then
    Exit(nil);

  if not(ImageIn = nil) then
    begin
      if not(ImageIn.Dimensions.IsZero) then
        begin
          if Width = 0 then
            scanWidth := ImageIn.Dimensions.X - 1
          else
            begin
              scanWidth := Width;
                if (scanWidth < 1) or (scanWidth >= ImageIn.Dimensions.X) then
                  Exit(nil);
            end;

          if Height = 0 then
            scanHeight := ImageIn.Dimensions.Y - 1
          else
            begin
              scanHeight := Height;
                if (scanHeight < 1) or (scanHeight >= ImageIn.Dimensions.Y) then
                  Exit(nil);
            end;
          SetLength(scanrow, scanHeight + 1);
          for y := Top to scanHeight do
            begin
              PSource := ImageIn.PixelPtr(Left, y);
              scanrow[y] := True;
              for x := Left to scanWidth do
                begin
                  if PSource^.W <> 0 then
                    begin
                      scanrow[y] := False;
                      break;
                    end;
                  Inc(PSource);
                end;
            end;
        end;
    end;
  Result := scanrow;
end;

function ScanForTransparentColumns(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TBooleanArray;
var
  PSource: PVector4Byte;
  x, y: Integer;
  scanWidth, scanHeight: Integer;
  scancolumn: TBooleanArray;
begin
  scancolumn := nil;

  if not(ImageIn.HasAlpha) then
    Exit(nil);

  if not(ImageIn = nil) then
    begin
      if not(ImageIn.Dimensions.IsZero) then
        begin
          if Width = 0 then
            scanWidth := ImageIn.Dimensions.X - 1
          else
            begin
              scanWidth := Width;
                if (scanWidth < 1) or (scanWidth >= ImageIn.Dimensions.X) then
                  Exit(nil);
            end;

          if Height = 0 then
            scanHeight := ImageIn.Dimensions.Y - 1
          else
            begin
              scanHeight := Height;
                if (scanHeight < 1) or (scanHeight >= ImageIn.Dimensions.Y) then
                  Exit(nil);
            end;

          SetLength(scancolumn, scanWidth + 1);
          for x := Left to scanWidth do
            begin
              scancolumn[x] := True;
              PSource := ImageIn.PixelPtr(x, Top);
              for y := Top to scanHeight do
                begin
                  if PSource^.W <> 0 then
                    begin
                      scancolumn[x] := False;
                      break;
                    end;
                  PSource := PSource + (scanWidth + 1);
                end;
            end;
        end;
    end;
  Result := scancolumn;
end;

end.

