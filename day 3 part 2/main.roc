app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

# Convert input to a 2D array of individual character strings.
inputData =
    Str.split input "\n"
    |> Str.joinWith ""
    |> Str.graphemes

testData =
    [
        "12.......*..",
        "+.........34",
        ".......-12..",
        "..78........",
        "..*....60...",
        "78.........9",
        ".5.....23..$",
        "8...90*12...",
        "............",
        "2.2......12.",
        ".*.........*",
        "1.1..503+.56",
    ]
    |> Str.joinWith ""
    |> Str.graphemes

expect gearRatios testData 12 12 == 6756

main =
    result = gearRatios inputData 140 140
    Stdout.line (Num.toStr result)

gearRatios = \data, xDim, yDim ->
    b = buffer data xDim yDim
    b.scanIndexes "*"
    |> List.map b.indexesToCheck
    |> List.map \indexes ->
        List.map indexes \i -> b.numberAtIndex i
        |> List.keepIf (\n -> n != "")
    |> List.keepIf (\l -> List.len l == 2)
    |> List.map \e ->
        List.map e \s -> Result.withDefault (Str.toU32 s) 0
        |> List.product
    |> List.sum

buffer = \data, xDim, yDim -> 
    {
        scanIndexes: \char ->
            List.walkWithIndex data [] \indexes, c, index ->
                if c == char then
                    List.append indexes index
                else
                    indexes,
        numberAtIndex: \index ->
            state = {index, num: []}
            pre = List.walkUntil data state \s, _ ->
                char = Result.withDefault (List.get data s.index) ""
                num = Str.toU8 char
                if Result.isOk num && s.index >= 0 then
                    Continue {index: s.index - 1, num: List.concat [char] s.num}
                else
                    Break s
            if pre.num == [] then
                ""
            else 
                post = List.walkUntil data {index: pre.index + 1, num: []} \s, _ ->
                    char = Result.withDefault (List.get data s.index) ""
                    num = Str.toU8 char
                    if Result.isOk num && s.index < (List.len data) then
                        Continue {index: s.index + 1, num: List.concat s.num [char]}
                    else
                        Break s
                Str.joinWith post.num "",
        indexesToCheck: \index ->
            []
            |> List.appendIfOk (leftOfIndex index xDim yDim)
            |> List.appendIfOk (rightOfIndex index xDim yDim)
            |> List.concat (
                when topRightOfIndex index xDim yDim is
                    Ok topRight if !(isNumericAtIndex data (topRight - 1)) ->
                        [topRight - 2, topRight]
                    Ok topRight if (isNumericAtIndex data (topRight - 2)) ->
                        [topRight - 2]
                    Ok topRight ->
                        [topRight - 1]
                    _  -> when topOfIndex index xDim yDim is
                            Ok top if !(isNumericAtIndex data (top - 1)) ->
                                [top]
                            Ok top ->
                                [top - 1]
                            _ -> []
            )
            |> List.concat (
                when bottomRightOfIndex index xDim yDim is
                    Ok bottomRight if !(isNumericAtIndex data (bottomRight - 1)) ->
                        [bottomRight - 2, bottomRight]
                    Ok bottomRight if (isNumericAtIndex data (bottomRight - 2)) ->
                        [bottomRight - 2]
                    Ok bottomRight ->
                        [bottomRight - 1]
                    _  -> when bottomOfIndex index xDim yDim is
                            Ok bottom if !(isNumericAtIndex data (bottom - 1)) ->
                                [bottom]
                            Ok bottom ->
                                [bottom - 1]
                            _ -> []
            )
    }
expect
    b = buffer testData 12 12
    a = b.scanIndexes "*"
    a == [9, 50, 90, 121, 131]
expect
    b = buffer testData 12 12
    a = b.numberAtIndex 23
    a == "34"
expect
    b = buffer testData 12 12
    a = b.numberAtIndex 21
    a == ""

isNumericAtIndex = \data, index ->
    when List.get data index is
        Ok char -> Result.isOk (Str.toU8 char)
        _ -> Bool.false

leftOfIndex = \index, xDim, yDim ->
    if
        (isFirstCol index xDim yDim) == Bool.false
    then
        Ok (index - 1)
    else
        Err (index)
expect leftOfIndex 4 3 3 == Ok 3
expect leftOfIndex 3 3 3 == Err 3

rightOfIndex = \index, xDim, yDim ->
    if
        (isLastCol index xDim yDim) == Bool.false
    then
        Ok (index + 1)
    else
        Err (index)
expect rightOfIndex 4 3 3 == Ok 5
expect rightOfIndex 2 3 3 == Err 2

topOfIndex = \index, xDim, yDim ->
    if
        (isFirstRow index xDim yDim) == Bool.false
    then
        Ok (index - yDim)
    else
        Err (index)
expect topOfIndex 4 3 3 == Ok 1
expect topOfIndex 2 3 3 == Err 2

bottomOfIndex = \index, xDim, yDim ->
    if
        (isLastRow index xDim yDim) == Bool.false
    then
        Ok (index + yDim)
    else
        Err (index)
expect bottomOfIndex 4 3 3 == Ok 7
expect bottomOfIndex 7 3 3 == Err 7

topLeftOfIndex = \index, xDim, yDim ->
    if
        (isFirstRow index xDim yDim) == Bool.false &&
        (isFirstCol index xDim yDim) == Bool.false 
    then
        Ok (index - yDim - 1)
    else
        Err (index)
expect topLeftOfIndex 4 3 3 == Ok 0
expect topLeftOfIndex 3 3 3 == Err 3

topRightOfIndex = \index, xDim, yDim ->
    if
        (isFirstRow index xDim yDim) == Bool.false &&
        (isLastCol index xDim yDim) == Bool.false 
    then
        Ok (index - yDim + 1)
    else
        Err (index)
expect topRightOfIndex 4 3 3 == Ok 2
expect topRightOfIndex 5 3 3 == Err 5

bottomLeftOfIndex = \index, xDim, yDim ->
    if
        (isLastRow index xDim yDim) == Bool.false &&
        (isFirstCol index xDim yDim) == Bool.false 
    then
        Ok (index + yDim - 1)
    else
        Err (index)
expect bottomLeftOfIndex 4 3 3 == Ok 6
expect bottomLeftOfIndex 3 3 3 == Err 3

bottomRightOfIndex = \index, xDim, yDim ->
    if
        (isLastRow index xDim yDim) == Bool.false &&
        (isLastCol index xDim yDim) == Bool.false 
    then
        Ok (index + yDim + 1)
    else
        Err (index)
expect bottomRightOfIndex 4 3 3 == Ok 8
expect bottomRightOfIndex 5 3 3 == Err 5

isFirstRow = \index, _, yDim -> (Num.toI32 index) - (Num.toI32 yDim) < 0
expect isFirstRow 2 3 3
expect isFirstRow 3 3 3 == Bool.false

isLastRow = \index, xDim, yDim -> index + yDim >= xDim * yDim
expect isLastRow 6 3 3
expect isLastRow 9 3 3
expect isLastRow 4 3 3 == Bool.false

isFirstCol = \index, _, yDim -> index % yDim == 0
expect isFirstCol 0 3 3
expect isFirstCol 6 3 3
expect isFirstCol 4 3 3 == Bool.false

isLastCol = \index, xDim, yDim -> isFirstCol (index + 1) xDim yDim
expect isLastCol 2 3 3
expect isLastCol 1 3 3 == Bool.false
expect isLastCol 6 3 3 == Bool.false
