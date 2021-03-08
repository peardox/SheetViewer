unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPripemd128,CastleVectors, CastleImages,
  CastleURIUtils, CastleUtils;

type
  TBooleanArray = Array of Boolean;
  PBooleanArray = ^TBooleanArray;

  TLineArray = Array of TVector2;
  PLineArray = ^TLineArray;

const
  MinimumSize: Integer = 4;

function ScanForTransparentRows(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TLineArray;
function ScanForTransparentColumns(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TLineArray;
function ExtractAlpha(const ImageIn: TCastleImage; const InvertAlpha: Boolean = False): TGrayscaleImage;
function ScanRowsToLines(const ScanRow: TBooleanArray): TLineArray;
function HashFile(const filename: String): String;

implementation

function HashFile(const filename: String): String;
var
  Hash: TDCP_ripemd128;
  Digest: array[0..15] of byte;  // RIPE-MD-128 produces a 128bit digest (16bytes)
  Source: TFileStream;
  i: integer;
  s: string;
begin
  Source:= nil;
  Result := EmptyStr;
  try
    Source:= TFileStream.Create(URIToFilenameSafe(filename),fmOpenRead);  // open the file specified by filename
  except
    Raise Exception.Create('Unable to open file for hashing');
  end;
  if Source <> nil then
  begin
    Hash:= TDCP_ripemd128.Create(nil);          // create the hash
    Hash.Init;                                   // initialize it
    Hash.UpdateStream(Source,Source.Size);       // hash the stream contents
    Hash.Final(Digest);                          // produce the digest
    Source.Free;
    s:= '';
    for i:= 0 to 15 do
      s:= s + IntToHex(Digest[i],2);
    FreeAndNil(Hash);
    Result := s;                              // display the digest
  end;
end;

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

function ScanRowsToLines(const ScanRow: TBooleanArray): TLineArray;
var
  x, y: Integer;
  lines: TLineArray;
  DefiningLine: Boolean;
begin
  y := 0;
  DefiningLine := False;
  for x := 0 to Length(ScanRow) - 1 do
    begin
      if not(ScanRow[x]) and not(DefiningLine) then
        begin
          SetLength(lines, y + 1);
          lines[y].Items[0] := x;
          DefiningLine := True;
        end;

      if ScanRow[x] and DefiningLine then
        begin
          lines[y].Items[1] := x - 1;
          DefiningLine := False;
          Inc(y);
        end;
    end;

  { We could have ended on an edge }
  if DefiningLine then
    begin
      lines[y].Items[1] := x;
    end;

  { Remove any line segments found shorter than MinimumSize }
  for x := Length(lines) - 1 downto 0 do
    begin
      if (lines[x].Items[1] - lines[x].Items[0] + 1) < MinimumSize then
        begin
          delete(lines, x, 1);
        end;
    end;

  Result := lines;
end;

function ScanForTransparentRows(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TLineArray;
var
  PSource: PVector4Byte;
  x, y: Integer;
  scanWidth, scanHeight: Integer;
  ScanRow: TBooleanArray;
begin
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
          SetLength(ScanRow, scanHeight + 1);
          for y := Top to scanHeight do
            begin
              PSource := ImageIn.PixelPtr(Left, y);
              ScanRow[y] := True;
              for x := Left to scanWidth do
                begin
                  if PSource^.W <> 0 then
                    begin
                      ScanRow[y] := False;
                      break;
                    end;
                  Inc(PSource);
                end;
            end;
        end;
    end;

  Result := ScanRowsToLines(ScanRow);
end;

function ScanForTransparentColumns(const ImageIn: TRGBAlphaImage; const Left: Integer = 0;
    const Top: Integer = 0; const Width: Integer = 0; const Height: Integer = 0): TLineArray;
var
  PSource: PVector4Byte;
  x, y: Integer;
  scanWidth, scanHeight: Integer;
  ScanRow: TBooleanArray;
begin
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

          SetLength(ScanRow, scanWidth + 1);
          for x := Left to scanWidth do
            begin
              ScanRow[x] := True;
              PSource := ImageIn.PixelPtr(x, Top);
              for y := Top to scanHeight do
                begin
                  if PSource^.W <> 0 then
                    begin
                      ScanRow[x] := False;
                      break;
                    end;
                  PSource := PSource + (scanWidth + 1);
                end;
            end;
        end;
    end;
    
  Result := ScanRowsToLines(ScanRow);
end;

end.

