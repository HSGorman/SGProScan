unit SGProScan;
{
The MIT License (MIT)

Copyright (c) 2004 Sam Gorman

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
}

interface

uses
  SysUtils, Windows, Classes, Messages, Twain, Forms,
  Graphics, Dialogs;

resourcestring
  SErrDSMEntryNotFound = 'DSMEntry not found in the TWAIN DLL';
  SErrDSMCallEntryFail = 'DSM call failed';
  SErrDSMUnknownError = 'DSM call failed in %s: Code %.04x';
  SErrDSUnknownError = 'DSM call failed in %s: Code %.04x';
  SErrDSActive = 'Cannot close source manager: A source is currently Open';
  SErrTwainNotLoaded = 'TWAIN DLL couldn''t be loaded';
  SErrCantGetStatus = 'Can not get Status';
  STWErrGeneralDSM = 'DSM Error at %s:'#13'%s';
  STWErrGeneralDS = 'DS Error at %s:'#13'%s';
  SErrNeedList = 'Please pass a TList to the MultiScan procedure';
  SErrNeedBitmap = 'Please pass a Bitmap to the SingleScan procedure';
//  SErrPixelTypeNegotiationFailed = 'Pixel Type Negotiation Failed';

type

  TSGProScan = class;
  TDSMObject = class;
  TDSObject = class;

  // Exceptions:
  ETWAINError = class(Exception);
  TTWAINTransfer = (twNative, twMemory, twFile);

  TTWAINPixelType = (ptBlackAndwhite, ptGray, ptRGB,
                    ptPalette, ptCMY, ptCMYK, ptYUV, ptYUVK, ptCIEXYZ);
  TTWAINPixelTypeSet = Set of TTWAINPixelType;

  TSGProScanError = (sgpsSetResolutionFail, sgpsPixelTypeFail, sgpsSetBitDepthFail);

  TNotifyErrorEvent = procedure (Sender: TObject; aError : TSGProScanError; var aAbort : Boolean) of object;
  TPixelNegotiationEvent = procedure(Sender : TObject;
                                    FailedType : TTWAINPixelType;
                                    aSupportedPixelTypes : TTWAINPixelTypeSet;
                                    var TryAgain : Boolean;
                                    var TryType : TTWAINPixelType) of object;
  TImageNotifyEvent = procedure (Sender: TObject; aImageNumber : Integer; aBitmap : TBitmap) of object;

  TDSMObject = class(TObject)
  private
    FDSObject : TDSObject;
    FDSMDLLHandle: HModule;
    FDSMActive : Boolean;
    FAppId : TW_IDENTITY;
    FApplicationHandle : HWND;
    function GetDSActive: Boolean;
  protected
  public
    constructor Create(aDSObject : TDSObject);
    destructor Destroy; override;

    function LoadTWAIN : Boolean;
    procedure UnloadTWAIN;
    procedure Open(WindowHandle : HWND);
    procedure Close;
    procedure RaiseLastCondition(at: string);
    procedure Check(res: TW_UINT16; at: string);
    function CallEntry(pDest: pTW_IDENTITY; DG: TW_UINT32; DAT: TW_UINT16;
                            MSG: TW_UINT16; pData: TW_MEMREF): TW_UINT16;


    property DSMDLLHandle : HModule read FDSMDLLHandle;
    property DSActive : Boolean read GetDSActive;
    property Active : Boolean read FDSMActive;
    property AppId : TW_IDENTITY read FAppId write FAppId;
    property ApplicationHandle : HWND read FApplicationHandle write FApplicationHandle;
  end;

  TDSObject = class(TObject)
  private
    FDSId: TW_IDENTITY;
    FDSMObject : TDSMObject;
    FDSOpen : Boolean;
    FDSEnabled : Boolean;
    FApplicationHandle : HWND;
    procedure SetApplicationHandle(const Value: HWND);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Check(res: TW_UINT16; at: string);
    procedure RaiseLastCondition(at: string);
    function Call(DG: TW_UINT32; DAT: TW_UINT16; MSG: TW_UINT16;
                            pData: TW_MEMREF): TW_UINT16;
    function SetCapOne(aCapability : TW_UINT16; aItemType : TW_UINT16; aItemValue : TW_UINT32): TW_UINT16; overload;
    function SetCapOne(aCapability : TW_UINT16; aItemType : TW_UINT16; aItemValue : TW_UINT16): TW_UINT16; overload;
//DEBUG    function GetCapOne

    procedure Open;
    procedure Close;
    procedure Enable(Show: Boolean);
    procedure EnableUIOnly;
    procedure Disable;
    procedure SelectDS;

    function SetResolution(aValue : Double) : Boolean;
    function SetBitDepth(aValue : Integer) : Boolean;
    function SetPixelType(aValue : TTWAINPixelType) : Boolean;
    function SupportedPixelTypes : TTWAINPixelTypeSet;


    property Active : Boolean read FDSOpen;
    property Enabled : Boolean read FDSEnabled;
    property DSMObject : TDSMObject read FDSMObject;
    property DSId : TW_IDENTITY read FDSId;
    property ApplicationHandle : HWND read FApplicationHandle write SetApplicationHandle;
  end;

  TSGProScan = class(TComponent)
  private
    // internal
    FDSObject : TDSObject;
    FMessagesComplete : Boolean;
    FSingleScan : Boolean;
    FNumberOfDibs : Integer;
    FBitmapList : TList;
    FWindowHandle : HWND;

    FNeedToFreeApplication : Boolean;
    // The Image returned from a SingleScan
    FBitmap : TBitmap;

    //properties
    FResolution : Double;
    FRequestPixelType : Boolean;
    FPixelType : TTWAINPixelType;

    FBitDepth : Integer;
    FEnableUI : Boolean;
    FADF: Boolean;

    FScanApplication : TApplication;
    // Events
    FErrorEvent : TNotifyErrorEvent;
    FPixelNegotiationEvent : TPixelNegotiationEvent;
    FOnImageNotify : TImageNotifyEvent;
    function GetScanApplication: TApplication;
    procedure SetScanApplication(const Value: TApplication);
  protected
    procedure TWXferMech(transfer: TTWAINTransfer);
    procedure ProcessSourceMessage(var Msg: TMsg; var Handled: Boolean);
    function MultiTransfer : Boolean;
    procedure ModalEventLoop;
    function CreateProxyWindow : HWND;
    procedure DestroyProxyWindow(aWindow : Hwnd);

    procedure Scan;
  public
    constructor Create(AnOwner : TComponent); override;
    destructor Destroy; override;

    procedure SelectSource;
    procedure SingleScan(aBitmap : TBitmap);
    procedure MultiScan(aList : TList);

    property ExplicitAutoDocumentFeed : Boolean read FADF write FADF;
    property ScanApplication : TApplication read GetScanApplication write SetScanApplication;
    property DSObject : TDSObject read FDSObject;
  published
    property Resolution : Double read FResolution write FResolution;
    property PixelType : TTWAINPixelType read FPixelType write FPixelType;
    property RequestPixelType : Boolean read FRequestPixelType write FRequestPixelType;
    property EnableUI : Boolean read FEnableUI write FEnableUI default True;
    property BitDepth : Integer read FBitDepth write FBitDepth;

    // Events
    property OnErrorEvent : TNotifyErrorEvent read FErrorEvent write FErrorEvent;
    property OnPixelNegotiation : TPixelNegotiationEvent read FPixelNegotiationEvent write FPixelNegotiationEvent;
    property OnImageNotify : TImageNotifyEvent read FOnImageNotify write FOnImageNotify;
  end;

procedure Register;

function Condition2String(ConditionCode: TW_UINT16): string;
function DibNumColors(dib: Pointer): Integer;
function ToFix32(r: Double): Cardinal;

implementation

const
  cTWAIN_DLL_NAME = 'TWAIN_32.DLL';
  cDSM_ENTRY_NAME = 'DSM_Entry';
  cTRYCOUNT = 10;

procedure Register;
begin
  RegisterComponents('SG', [TSGProScan]);
end;

(******************************************************************
 Some helping functions for error handling

ToDo:
 check texts, change to resourcestrings and move to
 something like a ScannersConst.pas unit
*******************************************************************)
function Condition2String(ConditionCode: TW_UINT16): string;
begin
  // Texts copied from PDF Documentation: Rework needed
  case ConditionCode of
    TWCC_BADCAP:
      Result :=
        'Capability not supported by Source or operation (get,'#13+
        'set) is not supported on capability, or capability had'#13+
        'dependencies on other capabilities and cannot be'#13+
        'operated upon at this time';
    TWCC_BADDEST:
      Result := 'Unknown destination in DSM_Entry.';
    TWCC_BADPROTOCOL:
      Result := 'Unrecognized operation triplet.';
    TWCC_BADVALUE:
      Result := 'Data parameter out of supported range.';
    TWCC_BUMMER:
      Result := 'General failure. Unload Source immediately.';
    TWCC_CAPUNSUPPORTED:
      Result := 'Capability not supported by Source.';
    TWCC_CAPBADOPERATION:
      Result := 'Operation not supported on capability.';
    TWCC_CAPSEQERROR:
      Result :=
        'Capability has dependencies on other capabilities and '#13+
        'cannot be operated upon at this time.';
    TWCC_DENIED:
      Result := 'File System operation is denied (file is protected).';
    TWCC_PAPERDOUBLEFEED:
      Result := 'Transfer failed because of a feeder error';
    TWCC_FILEEXISTS:
      Result := 'Operation failed because file already exists.';
    TWCC_FILENOTFOUND:
      Result := 'File not found.';
    TWCC_LOWMEMORY:
      Result := 'Not enough memory to complete operation.';
    TWCC_MAXCONNECTIONS:
      Result :=
      'Source is connected to maximum supported number of applications.';
    TWCC_NODS:
      Result := 'Source Manager unable to find the specified Source.';
    TWCC_NOTEMPTY:
      Result := 'Operation failed because directory is not empty.';
    TWCC_OPERATIONERROR:
      Result :=
      'Source or Source Manager reported an error to the'#13+
      'user and handled the error; no application action required.';
    TWCC_PAPERJAM:
      Result := 'Transfer failed because of a feeder error';
    TWCC_SEQERROR:
      Result := 'Illegal operation for current Source Manager'#13+
      'Source state.';
    TWCC_SUCCESS:
      Result := 'Operation worked.';
  else
    Result := Format('Unknown Condition %.04x', [ConditionCode]);
  end;
end;

function DibNumColors(dib: Pointer): Integer;
var
  lpbi: PBITMAPINFOHEADER;
  lpbc: PBITMAPCOREHEADER;
  bits: Integer;
begin
  lpbi := dib;
  lpbc := dib;

  if lpbi.biSize <> SizeOf(BITMAPCOREHEADER) then
  begin
    if lpbi.biClrUsed <> 0 then
    begin
      Result := lpbi.biClrUsed;
      Exit;
    end;
    bits := lpbi.biBitCount;
  end
  else
    bits := lpbc.bcBitCount;

  case bits of
    1:
      Result := 2;
    4:
      Result := 4;
    8:
      Result := 8;
  else
    Result := 0;
  end;
end; // DibNumColors

function ToFix32(r: Double): Cardinal;
var
   fix: TW_FIX32;
   v: Integer;
begin
   v := Round(r * 65536.0 + 0.5);
   fix.Whole := ShortInt(V shr 16);
   fix.Frac := Word (v and $ffff);
   ToFix32 := Cardinal(fix);
end;

{TDSMObject}
constructor TDSMObject.Create(aDSObject : TDSObject);
begin
  inherited Create;
  FDSObject := aDSObject;
end;

destructor TDSMObject.Destroy;
begin
  if FDSMActive then
    Self.Close;
  inherited;
end;

function TDSMObject.CallEntry(pDest: pTW_IDENTITY; DG: TW_UINT32; DAT,
  MSG: TW_UINT16; pData: TW_MEMREF): TW_UINT16;
begin
(*******************************************************************
 CallEntry:
  Short form for DSM Calls: appId is not neaded as parameter
*******************************************************************)
  Assert(@DSM_Entry <> nil);
  Result := DSM_Entry(@appID, pDest, DG, DAT, MSG, pData);
  if (Result <> TWRC_SUCCESS) and (DAT <> DAT_EVENT) then
  begin
    raise ETwainError.Create(SErrDSMCallEntryFail);
  end;
end;


procedure TDSMObject.Check(res: TW_UINT16; at: string);
begin
(*******************************************************************
TwainCheckDSM (idea: like Win32Check or GDICheck in Graphics.pas)
*******************************************************************)
  if res <> TWRC_SUCCESS then
  begin
    if res = TWRC_FAILURE then
      RaiseLastCondition(at)
    else
      raise ETwainError.CreateFmt(SErrDSMUnknownError, [at, res]);
  end;
end;


procedure TDSMObject.Close;
begin
  if DSActive then
    raise ETwainError.Create(SErrDSActive);

  if FDSMActive then
  begin
    // This call performs one important function:
    // - tells the SM which application, appID.id, is requesting SM to close
    // - be sure to test return code, failure indicates SM did not close !!
    Check(CallEntry(nil, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @(FApplicationHandle)), 'CloseDSM');
    FDSMActive := False;
    UnloadTwain;
  end;
end;


function TDSMObject.LoadTWAIN : Boolean;
begin
  if FDSMDLLHandle = 0 then
  begin
    // Attempt Load
    FDSMDLLHandle := LoadLibrary(cTWAIN_DLL_NAME);
    Result := (FDSMDLLHandle <> 0);
    if not Result then
      EXIT;
    // DSM_Entry is from the Twain.pas file
    DSM_Entry := GetProcAddress(FDSMDLLHandle, cDSM_ENTRY_NAME);

    if @DSM_Entry = nil then
      raise ETWAINError.Create(SErrDSMEntryNotFound);
  end;
end; // TDSMObject.LoadTWAIN

procedure TDSMObject.Open(WindowHandle: HWND);
begin
  if not FDSMActive then
  begin
    Assert(WindowHandle <> 0);

    if not LoadTwain then
      raise ETwainError.Create(SErrTwainNotLoaded);

    with FAppId do
    begin
      Id := 0;  // init to 0, but Source Manager will assign real value

      Version.MajorNum := 1;
      Version.MinorNum := 0;
      Version.Language := TWLG_USA;
      Version.Country := TWCY_USA;
      Version.Info := 'SGProScan Twain Test';

      ProtocolMajor := 1; // TWON_PROTOCOLMAJOR;
      ProtocolMinor := 7; //TWON_PROTOCOLMINOR;
      SupportedGroups := DG_IMAGE or DG_CONTROL;

      ProductName := 'SGProScan';
      ProductFamily := 'Delphi Twain';
      Manufacturer := 'GoodNewsSoftware';

      Check(CallEntry(nil, DG_CONTROL, DAT_PARENT, MSG_OPENDSM, @(FApplicationHandle),), 'OpenDSM');
    end;
    FDSMActive := True;
  end;
end; // TDSMObject.Open

procedure TDSMObject.RaiseLastCondition(at: string);
var
  status: TW_STATUS;
begin
  Assert(@DSM_Entry <> nil);
  if DSM_Entry(@appId, nil, DG_CONTROL, DAT_STATUS, MSG_GET,
    @status) <> TWRC_SUCCESS then
    raise ETwainError.Create(SErrCantGetStatus)
  else
    raise ETwainError.CreateFmt(STWErrGeneralDSM, [at,
      Condition2String(status.ConditionCode)]);
end;

procedure TDSMObject.UnloadTWAIN;
begin
begin
  if FDSMDLLHandle <> 0 then
  begin
    DSM_Entry := nil;
    FreeLibrary(FDSMDLLHandle);
    FDSMDLLHandle := 0;
  end;
end; // TDSMObject.UnloadTWAIN

end;

function TDSMObject.GetDSActive: Boolean;
begin
  if Assigned(FDSObject) then
    Result := FDSObject.Active
  else
    Result := False;
end;

{DSObject}
function TDSObject.Call(DG: TW_UINT32; DAT, MSG: TW_UINT16;
  pData: TW_MEMREF): TW_UINT16;
begin
(*******************************************************************
  Short form for (actual) DS Calls. appId and dsID is not needed
(this should be a DS class method)
*******************************************************************)
  Assert(@DSM_Entry <> nil);

  Result := DSM_Entry(@(FDSMObject.AppId), @dsID, DG, DAT, MSG, pData);
end;

procedure TDSObject.Check(res: TW_UINT16; at: string);
(*******************************************************************
TwainCheckDS
  same again, but for the actual DS
(should be a method of DS)
*******************************************************************)
begin
  if res <> TWRC_SUCCESS then
  begin
    if res = TWRC_FAILURE then
      RaiseLastCondition(at)
    else
      raise ETWAINError.CreateFmt(SErrDSUnknownError, [at, res]);
  end;
end;

procedure TDSObject.Close;
begin
  Assert(FDSMObject.Active, 'DSM must be open');
  if FDSOpen then
  begin
    FDSMObject.Check(FDSMObject.CallEntry(nil, DG_CONTROL, DAT_IDENTITY, MSG_CLOSEDS, @FDSId), 'Close');
   FDSOpen := False;
  end;
end;

constructor TDSObject.Create;
begin
  inherited Create;
  FDSMObject := TDSMObject.Create(Self);
end;

destructor TDSObject.Destroy;
begin
  if FDSEnabled then
    Disable;
  if Active then
    Close;
  FDSMObject.Free;
  inherited;
end;

procedure TDSObject.Disable;
var
  twUI: TW_USERINTERFACE;
begin
  Assert(FDSOpen, 'DS must be open');

  if FDSEnabled then
  begin
    twUI.hParent := FApplicationHandle;
    twUI.ShowUI := TW_BOOL(TWON_DONTCARE8); (*!!!!*)
    FDSMObject.Check(FDSMObject.CallEntry(@FDSId, DG_CONTROL, DAT_USERINTERFACE,
      MSG_DISABLEDS, @twUI), 'TWDisableDS');
    FDSEnabled := False;
  end;
end;


procedure TDSObject.Enable(Show: Boolean);
var
  twUI: TW_USERINTERFACE;
begin
  Assert(FDSOpen, 'DS must be open');

  if not FDSEnabled then
  begin
    FillChar(twUI, SizeOf(twUI), #0);

    twUI.hParent := FApplicationHandle;
    twUI.ShowUI := Show;

    FDSMObject.Check(FDSMObject.CallEntry(@FDSId, DG_CONTROL, DAT_USERINTERFACE,
                                          MSG_ENABLEDS, @twUI), 'TWEnableDS');

    FDSEnabled := True;
  end;
end;

procedure TDSObject.EnableUIOnly;
var
  twUI: TW_USERINTERFACE;
begin
  Assert(FDSOpen, 'DS must be open');

  if not FDSEnabled then
  begin
    FillChar(twUI, SizeOf(twUI), #0);

    twUI.hParent := FApplicationHandle;
    twUI.ShowUI := True;

    FDSMObject.Check(FDSMObject.CallEntry(@FDSId, DG_CONTROL, DAT_USERINTERFACE,MSG_ENABLEDSUIONLY, @twUI), 'TWEnableDSUIOnly');
    FDSEnabled := True;
  end;
end;

procedure TDSObject.Open;
begin
  Assert(FDSMObject.Active, 'DSM must be open');

  if not FDSOpen then
  begin
    FDSMObject.Check(FDSMObject.CallEntry(nil, DG_CONTROL, DAT_IDENTITY, MSG_OPENDS, @FDSId), 'TWOpenDS');
    FDSOpen := True;
  end;
end;

procedure TDSObject.RaiseLastCondition(at: string);
var
  status: TW_STATUS;
begin
  Assert(@DSM_Entry <> nil);
  if DSM_Entry(@(FDSMObject.AppId), @dsID, DG_CONTROL, DAT_STATUS, MSG_GET, @status) <>
    TWRC_SUCCESS then
   raise ETwainError.Create(SErrCantGetStatus)
  else
    raise ETwainError.CreateFmt(STWErrGeneralDS, [at,
      Condition2String(status.ConditionCode)]);
end;

procedure TDSObject.SelectDS;
var
  NewDSIdentity: TW_IDENTITY;
  twRC: TW_UINT16;
begin
  Assert(FDSMObject.Active, 'DSM Must Be Open');
  Assert(not FDSOpen, 'Source must not be open');

  FDSMObject.Check(FDSMObject.CallEntry(nil, DG_CONTROL, DAT_IDENTITY, MSG_GETDEFAULT,
    @NewDSIdentity), 'TWSelectDS:Select Default');

  twRC := FDSMObject.CallEntry(nil, DG_CONTROL, DAT_IDENTITY, MSG_USERSELECT,
    @NewDSIdentity);

  case twRC of
    TWRC_SUCCESS:
      FdsID := NewDSIdentity; // log in new Source
    TWRC_CANCEL:
      ;                      // keep the current Source
  else
    FDSMObject.Check(twRC, 'TWSelectDS:User Select');
  end;
end;

function TDSObject.SetResolution(aValue : Double) : Boolean;
var
  lSetResult : TW_UINT16;
begin
  lSetResult := SetCapOne(ICAP_YRESOLUTION, TWTY_FIX32, ToFix32(aValue));
  lSetResult := lSetResult + SetCapOne(ICAP_XRESOLUTION, TWTY_FIX32, ToFix32(aValue));
  Result := (lSetResult = 0);
end;

function TDSObject.SetBitDepth(aValue : Integer) : Boolean;
var
  lSetResult : TW_UINT16;
begin
  lSetResult := SetCapOne(ICAP_BITDEPTH, TWTY_UINT16, TW_UINT16(aValue));
  Result := (lSetResult = 0);
end; // SetBitDepth

function TDSObject.SetPixelType(aValue: TTWAINPixelType): Boolean;
var
  lSetResult : TW_UINT16;
begin
  if not Self.Active then
  begin
    raise ETWAINError.Create('Not Open');
    Exit;
  end;

  /////////////////////
  // Check capability
  /////////////////////
  if not(aValue in SupportedPixelTypes) then
  begin
    Result := False;
    Exit;
  end;
  case aValue of
    ptBlackAndwhite : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_BW));
    ptGray          : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_GRAY));
    ptRGB           : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_RGB));
    ptPalette       : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_PALETTE));
    ptCMY           : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_CMY));
    ptCMYK          : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_CMYK));
    ptYUV           : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_YUV));
    ptYUVK          : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_YUVK));
    ptCIEXYZ        : lSetResult := SetCapOne(ICAP_PIXELTYPE, TWTY_INT16, TW_UINT16(TWPT_CIEXYZ));
  end;
  Result := (lSetResult = 0);
end; // SetPixelType


function TDSObject.SupportedPixelTypes : TTWAINPixelTypeSet;
var
	lCapability : TW_CAPABILITY;
	lCapabilityEnum : TW_CAPABILITY;
	lpOneValue : pTW_ONEVALUE;
  lSuccess : Boolean;
  lTWRes : TW_UINT16;
  lpEnumeration : pTW_ENUMERATION;
  lcount : Integer;
begin
  Result := [];
  
  if not Self.Active then
  begin
    raise ETWAINError.Create('Not Open');
    Exit;
  end;

  /////////////////////
  // Check capability
  /////////////////////
  // Fill in capability structure
  lCapability.Cap := ICAP_PIXELTYPE;			// capability id
  lCapability.ConType := TWON_DONTCARE16;
  lCapability.hContainer := 0;

  // Get the Capability data
  lTWRes := Call(DG_CONTROL, DAT_CAPABILITY, MSG_GET, @lCapability);

  // Check the Result
  if lTWRes <> TWRC_SUCCESS then
    Exit;

  // Check the type of result
  case lCapability.ConType of
    TWON_ENUMERATION :
      begin
        lpEnumeration := pTW_ENUMERATION(GlobalLock(lCapability.hContainer));
        try
          for lcount := 0 to (lpEnumeration^.NumItems - 1) do
          begin
            lTWRes := TW_UINT16(lpEnumeration^.ItemList[lcount * 2]);
            case  lTWRes of
              TWPT_BW :       Result := Result + [ptBlackAndwhite];
              TWPT_GRAY :     Result := Result + [ptGray];
              TWPT_RGB :      Result := Result + [ptRGB];
              TWPT_PALETTE :  Result := Result + [ptPalette];
              TWPT_CMY :      Result := Result + [ptCMY];
              TWPT_CMYK  :    Result := Result + [ptCMYK];
              TWPT_YUV :      Result := Result + [ptYUV];
              TWPT_YUVK  :    Result := Result + [ptYUVK];
              TWPT_CIEXYZ :   Result := Result + [ptCIEXYZ];
            end;
          end;
        finally
          GlobalUnlock(lCapability.hContainer);
        end;
      end;
   		TWON_ARRAY: ;
  		TWON_ONEVALUE: ;
  end;
end; // SupportedPixelTypes


function TDSObject.SetCapOne(aCapability : TW_UINT16; aItemType : TW_UINT16; aItemValue : TW_UINT32): TW_UINT16;
var
	lCapability : TW_CAPABILITY;
	lpOneValue : pTW_ONEVALUE;
  lSuccess : Boolean;
begin
  if not Self.Active then
  begin
    raise ETWAINError.Create('Not Open');
    Exit;
  end;

  lCapability.Cap := aCapability;
  lCapability.ConType := TWON_ONEVALUE; // container type
	lCapability.hContainer := GlobalAlloc(GHND, sizeof (TW_ONEVALUE));
  Assert(lCapability.hContainer <> 0);

	lpOneValue := pTW_ONEVALUE(GlobalLock(lCapability.hContainer));
  Assert(lpOneValue <> nil);
  lpOneValue.ItemType := aItemType;
  lpOneValue.Item := aItemValue;
	GlobalUnlock(lCapability.hContainer);
  
	Result := Call(DG_CONTROL, DAT_CAPABILITY, MSG_SET, @lCapability);



	GlobalFree(lCapability.hContainer);
end;

function TDSObject.SetCapOne(aCapability : TW_UINT16; aItemType : TW_UINT16; aItemValue : TW_UINT16): TW_UINT16;
var
	lCapability : TW_CAPABILITY;
	lpOneValue : pTW_ONEVALUE;
  lSuccess : Boolean;
begin
  if not Self.Active then
  begin
    raise ETWAINError.Create('Not Open');
    Exit;
  end;

  lCapability.Cap := aCapability;
  lCapability.ConType := TWON_ONEVALUE; // container type
	lCapability.hContainer := GlobalAlloc(GHND, sizeof (TW_ONEVALUE));
  Assert(lCapability.hContainer <> 0);

	lpOneValue := pTW_ONEVALUE(GlobalLock(lCapability.hContainer));
  Assert(lpOneValue <> nil);
  lpOneValue.ItemType := aItemType;
  lpOneValue.Item := aItemValue;

	GlobalUnlock(lCapability.hContainer);

	Result := Call(DG_CONTROL, DAT_CAPABILITY, MSG_SET, @lCapability);

	GlobalFree(lCapability.hContainer);
end;

procedure TDSObject.SetApplicationHandle(const Value: HWND);
begin
  FApplicationHandle := Value;
  if Assigned(FDSMObject) then
    FDSMObject.ApplicationHandle := FApplicationHandle;
end;

{ TSGProScan }

constructor TSGProScan.Create(AnOwner: TComponent);
begin
  inherited;
  FDSObject := TDSObject.Create;
  FEnableUI := True;
  FWindowHandle := 0;
end;


destructor TSGProScan.Destroy;
begin
  if FWindowHandle <> 0 then
  begin
    DestroyProxyWindow(FWindowHandle);
    FWindowHandle := 0;
  end;

  if FNeedToFreeApplication then
    FScanApplication.Free;
  FDSObject.Free;
  inherited;
end;

procedure TSGProScan.ModalEventLoop;
var
  lMessageEvent : TMessageEvent;
begin
  lMessageEvent := ScanApplication.OnMessage;
  ScanApplication.OnMessage := ProcessSourceMessage;
  try
    ScanApplication.ModalStarted;
    try
      FMessagesComplete := False;
      repeat
        ScanApplication.HandleMessage;
      until FMessagesComplete;
    finally
      ScanApplication.ModalFinished;
    end;
  finally
    ScanApplication.OnMessage := lMessageEvent;
  end;
end;

function TSGProScan.CreateProxyWindow: HWND;
begin
	Result := CreateWindow('STATIC',						// class
						'Acquire Proxy',				// title
						WS_POPUPWINDOW or WS_VISIBLE ,	// style
						CW_USEDEFAULT, CW_USEDEFAULT,	// x, y
						CW_USEDEFAULT, CW_USEDEFAULT,	// width, height
						HWND_DESKTOP,					// parent window
						0,							// hmenu
						FDSObject.DSMObject.FDSMDLLHandle,						// hinst
						0);							// lpvparam

end;

procedure TSGProScan.DestroyProxyWindow(aWindow: Hwnd);
begin
  DestroyWindow(aWindow);
  aWindow := 0;
end;

procedure TSGProScan.MultiScan(aList: TList);
begin
  FSingleScan := False;
  FNumberOfDibs := 0;
  FBitmapList := aList;
  if not Assigned(FBitmapList) then
  begin
    raise ETwainError.Create(SErrNeedList);
    Exit;
  end;
  Scan;
end;

function TSGProScan.MultiTransfer: Boolean;
var
  twRC: TW_UINT16;
  hDIB: TW_UINT32;
  hBmp: HBITMAP;
  lpDib: ^TBITMAPINFO;
  lpBits: PChar;
  ColorTableSize: Integer;
  dc: HDC;
  lPendingXfers : TW_PENDINGXFERS;
  pending: TW_PENDINGXFERS;
begin
  Result := False;

  repeat
    twRC := FDSObject.Call(DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @hDIB);

    case twRC of
      TWRC_XFERDONE:
        begin
          lpDib := GlobalLock(hDIB);
          try
            ColorTableSize := (DibNumColors(lpDib) * SizeOf(RGBQUAD));

            lpBits := PChar(lpDib);
            Inc(lpBits, lpDib.bmiHeader.biSize);
            Inc(lpBits, ColorTableSize);

            dc := GetDC(0);
            try
              hBMP := CreateDIBitmap(dc, lpdib.bmiHeader, CBM_INIT,
                lpBits, lpDib^, DIB_RGB_COLORS);
              if FSingleScan then
              begin
                FDSObject.Check(FDSObject.Call(DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @pending),
                                'Check for Pending Transfers');

                if FSingleScan and (pending.Count > 0) then
                  FDSObject.Check(FDSObject.Call(DG_CONTROL, DAT_PENDINGXFERS, MSG_RESET,
                                                    @pending), 'Abort Pending Transfers');
                end; // SingleScan

              if not FSingleScan then
              begin
                // Multiple Images so need to create the extra bitmap
                FBitmap := TBitmap.Create;
                FBitmapList.Add(FBitmap);
              end;
              FBitmap.Handle := hBMP;
              if Assigned(FOnImageNotify) then
              begin
                if FSingleScan then
                begin
                  FOnImageNotify(Self, 0, FBitmap);
                end
                else
                begin
                  FOnImageNotify(Self, (FBitmapList.Count - 1), FBitmap);
                end;
              end;
            finally
              ReleaseDC(0, dc);
            end;
          finally
            GlobalUnlock(hDIB);
            GlobalFree(hDIB);
          end;
          Inc(FNumberOfDibs);
          Result := True;
        end;

      TWRC_CANCEL:
        Result := True;


      TWRC_FAILURE:
        begin
          FDSObject.DSMObject.RaiseLastCondition('Native Transfer');
          Result := True;
        end;
    end;
    if not FSingleScan then
    begin
      FDSObject.Call(DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @lPendingXfers);
      Result := (lPendingXfers.Count = 0);
    end else
    begin
      Result := True;
    end;
  until Result;
end; // MultiTransfer

procedure TSGProScan.ProcessSourceMessage(var Msg: TMsg; var Handled: Boolean);
var
  twRC: TW_UINT16;
  event: TW_EVENT;
  pending: TW_PENDINGXFERS;
begin
  Handled := False;

  if FDSObject.DSMObject.Active and FDSObject.Active then
  begin
    event.pEvent := @Msg;
    event.TWMessage := 0;

    twRC := FDSObject.Call(DG_CONTROL, DAT_EVENT, MSG_PROCESSEVENT, @event);

    case event.TWMessage of
      MSG_XFERREADY:
      begin
        //////////////
        // State 6
        //////////////
        FMessagesComplete := MultiTransfer;
      end;
      MSG_CLOSEDSOK, MSG_CLOSEDSREQ:
      begin
        FMessagesComplete := True;
      end;
    end;

    Handled := not (twRC = TWRC_NOTDSEVENT);
  end;
end;

procedure TSGProScan.SelectSource;
var
  lOldHandle : HWND;
begin
  if FWindowHandle = 0 then
    FWindowHandle := CreateProxyWindow;
  lOldHandle := ScanApplication.Handle;
  try
    ScanApplication.Handle := FWindowHandle;
    FDSObject.ApplicationHandle := ScanApplication.Handle;
    FDSObject.DSMObject.Open(FWindowHandle);
    FDSObject.SelectDS;
  finally
    ScanApplication.Handle := lOldHandle;
  end;
end;


procedure TSGProScan.Scan;
var
  lAbort : Boolean;
  lSetPixelType : Boolean;
  lNextPixelType : TTWAINPixelType;
  lInfiniteLoopCounter : Integer;
  lTryToSetPixelType :  Boolean;
  lOldHandle : HWND;
begin
  FNumberOfDibs := 0;
  lOldHandle := ScanApplication.Handle;
  if FWindowHandle = 0 then
    FWindowHandle := CreateProxyWindow;
  try
    ScanApplication.Handle := FWindowHandle;
    // Ensure the DS and DSM Objects have a window to work with
    FDSObject.ApplicationHandle := ScanApplication.Handle;

    // Reopen
    if FDSObject.DSMObject.LoadTWAIN then
    begin
      FDSObject.DSMObject.Open(FWindowHandle);
      FDSObject.Open;
      ////////////////////
      // State 4
      ////////////////////
      if FADF then
      begin
        FDSObject.SetCapOne(CAP_XFERCOUNT, TWTY_INT16, TW_UINT16(-1));
      end;

      if FRequestPixelType then
      begin
        lTryToSetPixelType := True;
        lNextPixelType := FPixelType;
        lInfiniteLoopCounter := 0;
        while lTryToSetPixelType and
              (not FDSObject.SetPixelType(lNextPixelType)) and
              (lInfiniteLoopCounter < cTRYCOUNT) do
        begin
          if Assigned(FPixelNegotiationEvent) then begin
            FPixelNegotiationEvent(Self, lNextPixelType,
                                  FDSObject.SupportedPixelTypes,
                                  lTryToSetPixelType,
                                  lNextPixelType);
          end else
          begin
            lTryToSetPixelType := False;
            lAbort := False;
            FErrorEvent(Self, sgpsPixelTypeFail, lAbort);
            if lAbort then
              Exit;
          end;
          Inc(lInfiniteLoopCounter);
        end; // SetPixelType
        if lInfiniteLoopCounter = cTRYCOUNT then
        begin
          lAbort := False;
          FErrorEvent(Self, sgpsPixelTypeFail, lAbort);
          if lAbort then
            Exit;
        end;
      end;

      if FResolution > 0 then
      begin
        if not FDSObject.SetResolution(FResolution) then
        begin
          lAbort := False;
          if Assigned(FErrorEvent) then
            FErrorEvent(Self, sgpsSetResolutionFail, lAbort);
          if lAbort then
            Exit;
        end;
      end;

      if FBitDepth > 0 then
      begin
        if not FDSObject.SetBitDepth(FBitDepth) then
        begin
          lAbort := False;
          if Assigned(FErrorEvent) then
            FErrorEvent(Self, sgpsSetBitDepthFail, lAbort);
          if lAbort then
            Exit;
        end;
      end;

      TWXferMech(twNative);
      EnableWindow(FWindowHandle, TRUE);
      FDSObject.Enable(FEnableUI);
      ////////////////////
      //  State 5
      ////////////////////
      ModalEventLoop;
      EnableWindow(FWindowHandle, FALSE);
      FDSObject.Disable;
      FDSObject.Close;
    end;
//TODO          FResolution := TWAIN_GetCurrentResolution;
//TODO          FBitDepth := TWAIN_GetBitDepth;
  finally
    ScanApplication.Handle := lOldHandle;
  end;
end;

procedure TSGProScan.SingleScan(aBitmap: TBitmap);
begin
  FSingleScan := True;
  FNumberOfDibs := 0;
  FBitmap := aBitmap;
  if not Assigned(FBitmap) then
    raise ETWAINError.Create(SErrNeedBitmap);
  Scan;
end;


procedure TSGProScan.TWXferMech(transfer: TTWAINTransfer);
var
  cap: TW_CAPABILITY;
  pVal: pTW_ONEVALUE;
begin
  cap.Cap := ICAP_XFERMECH;
  cap.ConType := TWON_ONEVALUE;

  cap.hContainer := GlobalAlloc(GHND, SizeOf(TW_ONEVALUE));
  Assert(cap.hContainer <> 0);
  try

    pval := pTW_ONEVALUE(GlobalLock(cap.hContainer));
    Assert(pval <> nil);
    try
      pval.ItemType := TWTY_UINT16;
      case transfer of
        twMemory:
          pval.Item := TWSX_MEMORY;
        twFile:
          pval.Item := TWSX_FILE;
      else
        pval.Item := TWSX_NATIVE
      end;
    finally
      GlobalUnlock(cap.hContainer);
    end;

    FDSObject.Check(FDSObject.Call(DG_CONTROL, DAT_CAPABILITY, MSG_SET, @cap),
      'Set Xfer Mech');

  finally
    GlobalFree(cap.hContainer);
  end;
end;


function TSGProScan.GetScanApplication: TApplication;
begin
  if Assigned(FScanApplication) then
  begin
    Result := FScanApplication;
    Exit;
  end else
  begin
    if Assigned(Application) then
    begin
      Result := Application;
      Exit;
    end;
  end;

  if (not Assigned(Application)) then
  begin
    FNeedToFreeApplication := True;
    FScanApplication := TApplication.Create(nil);
    Exit;
  end;
end;

procedure TSGProScan.SetScanApplication(const Value: TApplication);
begin
  FScanApplication := Value;
end;


end.

