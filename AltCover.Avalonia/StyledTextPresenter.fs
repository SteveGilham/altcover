namespace AltCover

open System
open System.Collections.Generic
open System.Linq
open System.Reflection

open Avalonia
open Avalonia.Controls.Presenters
open Avalonia.Media
open Avalonia.Media.TextFormatting
open Avalonia.Controls.Presenters
open Avalonia
open System.Collections.Generic

type StyledTextRun = { Text: string; Foreground: IBrush }

module internal PropertyHelper =
    let x<'T when 'T :> TextPresenter>() =
      let temp = AvaloniaProperty.Register<'T, IEnumerable<StyledTextRun>>("StyledText")
      temp.Changed.AddClassHandler<'T>(fun x (e: AvaloniaPropertyChangedEventArgs) -> x.UpdateTextLayout()) |> ignore
      temp

type StyledTextPresenter() as this =
    inherit TextPresenter()

    static member StyledTextProperty = PropertyHelper.x<StyledTextPresenter>()

    member this.StyledText
        with get () = this.GetValue(StyledTextPresenter.StyledTextProperty)
        and set (value) = this.SetValue(StyledTextPresenter.StyledTextProperty, value) |> ignore

    member private this.UpdateTextLayout() =
        // Invalidate the text layout when the styled text changes
        typeof<TextPresenter>.GetMethod("InvalidateTextLayout", BindingFlags.NonPublic ||| BindingFlags.Instance)
        |> Option.ofObj
        |> Option.iter (fun m -> m.Invoke(this, [||]) |> ignore)

    override this.CreateTextLayout() =
        if this.StyledText = null then
            base.CreateTextLayout()
        else
            let fullText =
                this.StyledText
                |> Seq.map (fun r -> r.Text)
                |> String.concat ""

            let textStyleOverrides = List<Span<TextRunProperties>>()
            let mutable currentIndex = 0

            for run in this.StyledText do
                if not (String.IsNullOrEmpty(run.Text)) then
                    let properties =
                        GenericTextRunProperties(
                            Typeface(this.FontFamily, this.FontStyle, this.FontWeight, this.FontStretch),
                            this.FontSize,
                            foregroundBrush = run.Foreground
                        )
                    textStyleOverrides.Add(Avalonia.Media.TextFormatting.ValueSpan<TextRunProperties>(currentIndex, run.Text.Length, properties))
                    currentIndex <- currentIndex + run.Text.Length

            TextLayout(
                fullText,
                Typeface(this.FontFamily, this.FontStyle, this.FontWeight, this.FontStretch),
                this.FontSize,
                this.Foreground,
                this.TextAlignment,
                this.TextWrapping,
                (this :> TextPresenter).TextTrimming,
                null,
                this.FlowDirection,
                this.Bounds.Width,
                this.Bounds.Height,
                this.LineHeight,
                this.LetterSpacing,
                0,
                textStyleOverrides
            )