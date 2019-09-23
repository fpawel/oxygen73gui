unit vclutils;

interface

uses vcl.stdctrls, vcl.controls, vcl.ComCtrls, vcl.graphics, system.Types;

type
    TControlProc = reference to procedure(const AControl: TControl);

procedure SetButtonMultiline(b: TButton);

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);

procedure ConvertImagesToHighColor(ImageList: TImageList);

function GetVCLControlAtPos(c: TWinControl; mousePos: TPoint): TWinControl;

implementation

uses Winapi.commctrl, Winapi.Windows, SysUtils;

function GetVCLControlAtPos(c: TWinControl; mousePos: TPoint): TWinControl;
var
    p: TPoint;
begin

    p := c.ScreenToClient(mousePos);
    c := TWinControl(c.ControlAtPos(p, false, true));
    while Assigned(c) do
    begin
        Result := c;
        p := c.ScreenToClient(mousePos);
        c := TWinControl(c.ControlAtPos(p, false, true));
    end;
end;

procedure ConvertImagesToHighColor(ImageList: TImageList);

// To show smooth images we have to convert the image list from 16 colors to high color.

var
    IL: TImageList;

begin
    // Have to create a temporary copy of the given list, because the list is cleared on handle creation.
    IL := TImageList.Create(nil);
    IL.Assign(ImageList);

    with ImageList do
        Handle := ImageList_Create(Width, Height, ILC_COLOR16 or ILC_MASK,
          Count, AllocBy);
    ImageList.Assign(IL);
    IL.Free;
end;

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);
var
    i: integer;
begin
    if AControl = nil then
        Exit;
    if AControl is TWinControl then
    begin
        for i := 0 to TWinControl(AControl).ControlCount - 1 do
            ModifyControl(TWinControl(AControl).controls[i], ARef);
    end;
    ARef(AControl);
end;

procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);
var
    i: integer;
    PageControl: TPageControl;
    word, word2: string;
    words : TArray<string>;
    x, y: integer;
    txt_height: double;
begin
    PageControl := Control as TPageControl;
    Active := PageControl.ActivePageIndex = TabIndex;
    if PageControl.ActivePageIndex = TabIndex then
    begin
        PageControl.Canvas.Brush.Color := clGradientInactiveCaption;
        PageControl.Canvas.Font.Color := clNavy;
    end
    else
    begin
        PageControl.Canvas.Brush.Color := clWindow;
        PageControl.Canvas.Font.Color := clBlack;
    end;

    word := PageControl.Pages[TabIndex].Caption;
    words := word.Split([' '], TStringSplitOptions.ExcludeEmpty);

    x := Rect.Left + 7;
    txt_height := PageControl.Canvas.TextHeight(word);
    if Length(words) = 1 then
    begin
        y := Rect.Top + round((Rect.Height - txt_height) / 2.0);
        PageControl.Canvas.TextRect(Rect, x, y, word);
    end else
    begin
        y := Rect.Top + 5;
        PageControl.Canvas.FillRect(Rect);
        PageControl.Canvas.TextOut(x, y, words[0]);
        y := y + Round(txt_height) + 3;
        PageControl.Canvas.TextOut(x, y, words[1]);
    end;
end;

end.
