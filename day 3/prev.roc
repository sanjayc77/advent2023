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

# Sanity check it.
expect List.len data == 140 * 140
expect charAt data 0 10 == "."
expect charAt data 10 0 == "4"
expect charAt data 7 1 == "@"
expect charAt data -1 5 == ""

# testData =
#     [
#         "...567...",
#         "...&.78..",
#         ".12..78..",
#     ]
#     |> Str.joinWith ""
#     |> Str.graphemes
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

main =
    dbg "Starting"

    # numbers = walkData data 140 140
    #     # |> Set.toList
    #     |> List.sortAsc
    #     # |> List.sum
    # dbg numbers
    dbg testData

    dbg List.len testData

    numbers = walkData testData 12 12
    dbg numbers

    sum = List.sum numbers
    dbg sum

    Stdout.line "done"

walkData = \buf, xDim, yDim ->
    List.walkWithIndexUntil buf [] \list, char, index ->
        if isSymbol char then
            # Get xy pos from index.
            pos = positionFromIndex index yDim
            dbg pos

            # Iterate over eight cardinal directions around x,y position.
            d = directions pos

            newSet = List.walkUntil d (Set.empty {}) \s, p ->
                i = indexFromPosition p.x p.y xDim yDim
                w = wordAroundIndex buf i
                # _ = if w == "983" then
                #         dbg p
                #         1
                #     else
                #         2
                when Str.toU32 w is
                    Ok n ->
                        # dbg Set.contains s n
                        Continue (Set.insert s n)

                    _ -> Continue s
            Continue (List.concat list (Set.toList newSet))
        else
            Continue list

isSymbol = \c -> c != "." && Str.toU8 c == Err InvalidNumStr

directions = \{ x, y } ->
    l =
        if x > 0 && y > 0 then
            [
                { x: x - 1, y: y - 1 },
                { x: x, y: y - 1 },
                { x: x + 1, y: y - 1 },
                { x: x - 1, y: y },
                { x: x - 1, y: y + 1 },
            ]
        else if x == 0 then
            [
                { x: x, y: y - 1 },
                { x: x + 1, y: y - 1 },
            ]
        else
            [
                { x: x - 1, y: y },
                { x: x - 1, y: y + 1 },
            ]
    List.concat l [
        { x: x + 1, y: y },
        { x: x, y: y + 1 },
        { x: x + 1, y: y + 1 },
    ]
# l

# directions = \{ x, y } -> [
#         { x: x - 1, y: y - 1 },
#         { x: x, y: y - 1 },
#         { x: x + 1, y: y - 1 },
#         { x: x - 1, y: y },
#         { x: x + 1, y: y },
#         { x: x - 1, y: y + 1 },
#         { x: x, y: y + 1 },
#         { x: x + 1, y: y + 1 },
#     ]
#     |> List.dropIf \e -> e.x < 0 || e.y < 0

d1 = directions { x: 2, y: 0 }
expect List.len d1 == 8

wordAroundIndex = \list, index ->
    when List.get list index is
        Ok c if isNumeric c ->
            pre = walkWithIndexBackwardUntil list index "" \word, char, _ ->
                if isNumeric char then
                    Continue (Str.concat char word)
                else
                    Break word
            post = walkWithIndexForwardUntil list (index + 1) "" \word, char, _ ->
                if isNumeric char then
                    Continue (Str.concat word char)
                else
                    Break word
            Str.concat pre post

        _ -> ""

w1 = wordAroundIndex (Str.graphemes "...845..") 4
expect w1 == "845"
w2 = wordAroundIndex (Str.graphemes "...845..") 5
expect w2 == "845"
w3 = wordAroundIndex (Str.graphemes "...845.7") 6
expect w3 == ""
w4 = wordAroundIndex (Str.graphemes ".9.845.7") 2
expect w4 == ""

indexFromPosition = \x, y, _, dimY -> y * dimY + x
positionFromIndex = \i, dimY -> { x: i % dimY, y: i // dimY }
expect indexFromPosition 5 1 140 140 == 145
expect positionFromIndex 145 140 == { x: 5, y: 1 }

charAt : List Str, I32, I32 -> Str
charAt = \a, x, y ->
    if x < 0 || y < 0 then
        ""
    else
        List.get a (Num.toNat (y * 140 + x))
        |> Result.withDefault ""

walkWithIndexForwardUntil : List elem, Nat, state, (state, elem, Nat -> [Continue state, Break state]) -> state
walkWithIndexForwardUntil = \list, index, state, fn ->
    List.walkWithIndexUntil list state \s, e, currIndex ->
        if currIndex < index then
            Continue s
        else
            fn s e currIndex

walkWithIndexBackwardUntil : List elem, Nat, state, (state, elem, Nat -> [Continue state, Break state]) -> state
walkWithIndexBackwardUntil = \list, index, startState, fn ->
    lastIndex = (List.len list) - 1
    final = List.walkBackwardsUntil list { state: startState, index: lastIndex } \{ state, index: currIndex }, e ->
        if currIndex > index then
            Continue { state, index: currIndex - 1 }
        else
            result = fn state e currIndex
            nextIndex = if currIndex == 0 then 0 else currIndex - 1
            when result is
                Continue s -> Continue { state: s, index: nextIndex }
                Break s -> Break { state: s, index: nextIndex }
    final.state

x1 = walkWithIndexBackwardUntil [1, 2, 3] 2 0 \n, e, _i -> Continue (n + e)
expect x1 == 6
y1 = walkWithIndexBackwardUntil [1, 2, 3] 1 0 \n, e, _i -> Continue (n + e)
expect y1 == 3

isNumeric = \c ->
    when Str.toU8 c is
        Ok _ -> Bool.true
        _ -> Bool.false
