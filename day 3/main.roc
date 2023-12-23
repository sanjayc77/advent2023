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
data =
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

expect scanForNumbers testData 12 == 925

main =
    total = scanForNumbers data 140
    Stdout.line (Num.toStr total)

scanForNumbers = \buffer, yDim ->
    # Scan string data for numeric characters and store indexes for when then occur.
    r = List.walkWithIndex data { indexes: [], found: Bool.false } \state, char, index ->
        # If char is numeric, check if it is the first of a word of numeric digits.
        if Result.isOk (Str.toU8 char) then
            if !state.found then
                # Append to list of indexes.
                { indexes: List.append state.indexes index, found: Bool.true }
            else
                # We are currentl traversing numeric digits, nothing to do.
                state
        else
            # Non-numeric char, reset found to false.
            { indexes: state.indexes, found: Bool.false }
    allNumbers = List.map r.indexes \i -> getNumberAt i buffer

    expect
        List.len r.indexes == List.len allNumbers

    zipped = List.map2 r.indexes allNumbers \a, b -> { index: a, number: b }

    List.keepIf zipped (\e -> hasSymbolAround buffer yDim e.index e.number)
        |> List.map \rec -> Result.withDefault (Str.toNat rec.number) 0
        |> List.sum

hasSymbolAround = \buffer, yDim, index, number ->
    state = {set: (Set.empty {}), index}
    indexesToCheck = Str.walkUtf8 number state \s, _ ->
        pos = positionFromIndex s.index yDim
        newState = List.walk (directions pos) s \sTmp, p -> 
            {set: Set.insert sTmp.set p, index: sTmp.index}
        {set: newState.set, index: newState.index + 1}
    Set.map indexesToCheck.set \e -> indexFromPosition e.x e.y yDim yDim
    |> Set.toList
    |> List.any \i -> isSymbol (Result.withDefault (List.get buffer i) ".")

getNumberAt = \index, buffer ->
    walkWithIndexForwardUntil buffer index [] \state, char, _ ->
        when Str.toU8 char is
            Ok _ -> Continue (List.append state char)
            _ -> Break state
    |> Str.joinWith ""

walkWithIndexForwardUntil : List elem, Nat, state, (state, elem, Nat -> [Continue state, Break state]) -> state
walkWithIndexForwardUntil = \list, index, state, fn ->
    List.walkWithIndexUntil list state \s, e, currIndex ->
        if currIndex < index then
            Continue s
        else
            fn s e currIndex

indexFromPosition = \x, y, _, dimY -> y * dimY + x
positionFromIndex = \i, dimY -> { x: i % dimY, y: i // dimY }
expect indexFromPosition 5 1 140 140 == 145
expect positionFromIndex 145 140 == { x: 5, y: 1 }

isSymbol = \c -> c != "." && Str.toU8 c == Err InvalidNumStr

directions = \{ x, y } ->
    # dbg {x, y}
    l =
        if x > 0 && y > 0 then
            [
                { x: x - 1, y: y - 1 },
                { x: x, y: y - 1 },
                { x: x + 1, y: y - 1 },
                { x: x - 1, y: y },
                { x: x - 1, y: y + 1 },
            ]
        else if y > 0 then
            [
                { x: x, y: y - 1 },
                { x: x + 1, y: y - 1 },
            ]
        else if x > 0 then
            [
                { x: x - 1, y: y },
                { x: x - 1, y: y + 1 },
            ]
        else
            []
    List.concat l [
        { x: x + 1, y: y },
        { x: x, y: y + 1 },
        { x: x + 1, y: y + 1 },
    ]