app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

tokens =
    Dict.empty {}
    |> Dict.insert "1" "1"
    |> Dict.insert "2" "2"
    |> Dict.insert "3" "3"
    |> Dict.insert "4" "4"
    |> Dict.insert "5" "5"
    |> Dict.insert "6" "6"
    |> Dict.insert "7" "7"
    |> Dict.insert "8" "8"
    |> Dict.insert "9" "9"
    |> Dict.insert "one" "1"
    |> Dict.insert "two" "2"
    |> Dict.insert "three" "3"
    |> Dict.insert "four" "4"
    |> Dict.insert "five" "5"
    |> Dict.insert "six" "6"
    |> Dict.insert "seven" "7"
    |> Dict.insert "eight" "8"
    |> Dict.insert "nine" "9"

extractCalibration = \total, calibration ->
    # Find the first and last index of token in calibration.
    # Returns a Dict with token -> {digit, firstIndex, lastIndex}
    a = Dict.map tokens \token, digit ->
        if Str.contains calibration token then
            firstIndex =
                Str.splitFirst calibration token
                |> Result.map \splits -> Str.countGraphemes splits.before
            lastIndex =
                Str.splitLast calibration token
                |> Result.map \splits -> Str.countGraphemes splits.before
            when (firstIndex, lastIndex) is
                (Ok num1, Ok num2) -> { digit, firstIndex: Num.toI32 num1, lastIndex: Num.toI32 num2 }
                _ -> { digit, firstIndex: -1, lastIndex: -1 }
        else
            { digit, firstIndex: -1, lastIndex: -1 }

    # Discard all tokens not found (firstIndex == -1)
    b =
        (Dict.keepIf a \(_, v) -> v.firstIndex != -1)
        |> Dict.toList

    # Sort by firstIndex ascending and lastIndex descending.
    sortByFirstIndex = List.sortWith b \x, y -> Num.compare x.1.firstIndex y.1.firstIndex
    sortByLastIndex = List.sortWith b \x, y -> Num.compare y.1.lastIndex x.1.lastIndex

    # Concat digits from firstIndex and lastIndex.
    word =
        when (List.first sortByFirstIndex, List.first sortByLastIndex) is
            (Ok (_, { digit: firstDigit }), Ok (_, { digit: lastDigit })) -> "\(firstDigit)\(lastDigit)"
            _ -> ""
    
    # Convert to number.
    num =
        when Str.toU32 word is
            Ok n -> n
            _ -> 0

    # Return total
    total + num

main =
    expect extractCalibration 0 "two1nine" == 29
    expect extractCalibration 0 "eightwothree" == 83
    expect extractCalibration 0 "abcone2threexyz" == 13
    expect extractCalibration 0 "xtwone3four" == 24
    expect extractCalibration 0 "4nineeightseven2" == 42
    expect extractCalibration 0 "zoneight234" == 14
    expect extractCalibration 0 "7pqrstsixteen" == 76
    expect extractCalibration 2 "nine4nine8" == 100
    expect extractCalibration 0 "oneight" == 18
    expect extractCalibration 0 "one" == 11
    expect extractCalibration 0 "27two583" == 23
    expect extractCalibration 0 "2threesixqczmrxhdrsevenfouroneight" == 28

    expect extractCalibration 0 "1two" == 12
    expect extractCalibration 0 "twone3" == 23
    expect extractCalibration 0 "xtwone3" == 23
    expect extractCalibration 0 "xtwelvene3" == 33
    expect extractCalibration 0 "7pqrstsixteen" == 76
    expect extractCalibration 0 "1one2" == 12
    expect extractCalibration 0 "one" == 11
    expect extractCalibration 0 "oneight" == 18
    expect extractCalibration 0 "twenty" == 0
    expect extractCalibration 0 "1two3" == 13
    
    # Break input into lines.
    lines = Str.split input "\n"
    
    # Extract calibrarion from each line.
    total =
        lines
        |> List.walk 0 extractCalibration

    # Print the result!
    Stdout.line (Num.toStr total)
