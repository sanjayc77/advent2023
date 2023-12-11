app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

extract = \total, calibration ->
    # Separate the calibration string into individual letters.
    letters = (Str.graphemes calibration)
    
    # Find the first numeric letter encountered.
    first = List.walkUntil  letters "" \empty, char ->
        when (Str.toU32 char) is
            (Ok _) -> Break char
            _ -> Continue empty
    
    # Find the last numeric letter encountered.
    last = List.walkBackwardsUntil  letters "" \empty, char ->
        when (Str.toU32 char) is
            (Ok _) -> Break char
            _ -> Continue empty

    when Str.toU32(Str.concat first last) is
        (Ok num) -> total + num
        _ -> total

main =
    # Break input into lines.
    lines = Str.split input "\n"
    # Extract calibrarion from each line.
    total = lines
            |> List.walk 0 extract
    # Print the result!
    Stdout.line (Num.toStr total)