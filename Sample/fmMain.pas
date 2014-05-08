unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SGProScan, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    SGProScan1: TSGProScan;
    btnSelectSource: TButton;
    btnScan: TButton;
    Image1: TImage;
    procedure btnSelectSourceClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure SGProScan1ErrorEvent(Sender: TObject;
      aError: TSGProScanError; var aAbort: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnSelectSourceClick(Sender: TObject);
begin
  SGProScan1.SelectSource;
end;

procedure TForm1.btnScanClick(Sender: TObject);
var
  lBitmap : TBitmap;
begin
  lBitmap := TBitmap.Create;
  try
    try
      SGProScan1.SingleScan(lBitmap);
      Image1.Picture.Bitmap := lBitmap;
//      Form1.Canvas.Draw(0, 0, lBitmap);
    except
      ShowMessage('Scan failed');
    end;
  finally
    lBitmap.Free;
  end; 
end;

procedure TForm1.SGProScan1ErrorEvent(Sender: TObject;
  aError: TSGProScanError; var aAbort: Boolean);
begin
  ShowMessage('Failed');
end;

end.
