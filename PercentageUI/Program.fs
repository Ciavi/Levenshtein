namespace PercentageUI

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Simple
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open System

type Levenshtein(s0, s1) = 
    let mutable first : string = s0
    let mutable second : string = s1
    let mutable longest : string = if s0.Length > s1.Length then s0 elif s0.Length < s1.Length then s1 else s1

    member this.First
        with get() = first
        and set(value) = first <- value

    member this.Second
        with get() = second
        and set(value) = second <- value

    member this.Longest
        with get() = longest
        
    member this.Run() = 
        let firstLength : int = first.Length
        let secondLength : int = second.Length
        let mutable distance : int[,] = Array2D.zeroCreate (firstLength + 1) (secondLength + 1)

        if firstLength = 0 then secondLength
        elif secondLength = 0 then firstLength
        else
            for i = 0 to firstLength do
                distance.[i, 0] <- i

            for j = 0 to secondLength do
                distance.[0, j] <- j

            for i = 1 to firstLength do
                for j = 1 to secondLength do
                    let cost : int = if (second.[j - 1] = first.[i - 1]) then 0 else 1
                    distance.[i, j] <- Math.Min(Math.Min((distance.[i - 1, j] + 1), (distance.[i, j - 1] + 1)), (distance.[i - 1, j - 1] + cost))

            let result = distance.[firstLength, secondLength]

            result

type State = {
    first: string
    second: string
}

type Msg =
    | UpdateString of string

module Main =    
    let updateFirst msg state =
        match msg with
        | UpdateString newString ->
            { state with first = newString }
    
    let updateSecond msg state =
        match msg with
        | UpdateString newString ->
            { state with second = newString }

    let view state dispatch () =
        Component(fun ctx ->
            StackPanel.create [
                StackPanel.children [
                    StackPanel.create [
                        StackPanel.children [
                            TextBox.create [
                                
                            ]
                            TextBox.create [
                                
                            ]
                        ]
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 24.0
                    ]
                ]
                StackPanel.orientation Orientation.Vertical
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Counter Example"
        base.Content <- Main.view

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (SimpleTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
