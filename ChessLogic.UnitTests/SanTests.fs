module SanTests

open FsUnit.Xunit
open Xunit
open ChessKit.ChessLogic
open ChessKit.ChessLogic.Text
open ChessKit.ChessLogic.BoardTextExtensions

// ----- ToSanString --------
let check move expectedSan position = 
    let p = position |> Fen.Parse
    printfn "%s" (Dump p)
    p
    |> MoveLegality.ParseLegal move
    |> San.ToString
    |> should equal expectedSan

[<Fact>]
let ``O-O``() = 
    "r3kb1r/pppb1pp1/8/7p/3PPBn1/2N5/PPP3PP/R2QK2R w KQkq - 0 14" 
    |> check "e1-g1" "O-O"

[<Fact>]
let ``O-O-O``() = 
    "r3kn1r/pp4pp/1q2b3/n2pPpP1/2pP4/2P1BB2/PP4QP/RN3RK1 b kq - 0 16" 
    |> check "e8-c8" "O-O-O"

[<Fact>]
let ``Pawn capture``() = 
    "r1b1kn1r/pp4pp/1q2p3/n2pPpP1/2pP2P1/2P1B3/PP1QB2P/RN3RK1 w kq - 0 14" 
    |> check "g4-f5" "gxf5"

[<Fact>]
let ``Pawn push``() = 
    "r1b1kb1r/pp1n1ppp/1q2p3/n2pP3/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 b kq - 0 10" 
    |> check "f7-f5" "f5"

[<Fact>]
let ``Knight f3-g5``() = 
    "r1b1kb1r/pp1n2pp/1q2p3/n2pPp2/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 w kq f6 0 11" 
    |> check "f3-g5" "Ng5"

[<Fact>]
let ``Gives check``() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "b1-d3" "Bd3+"

[<Fact>]
let Promotion() = 
    "5r2/ppkP2p1/4P3/n2n1q2/1Np2p2/2P2Q2/P4B1r/R3K3 w - - 0 36" 
    |> check "d7-d8=N" "d8=N"

[<Fact>]
let Nec6() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "e7-c6" "Nec6"

[<Fact>]
let N7c6() = 
    "2kr4/pp2n1p1/6qr/4n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e7-c6" "N7c6"

[<Fact>]
let Ne5c6() = 
    "2kr4/pp2n1p1/6qr/n3n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e5-c6" "Ne5c6"

// ----- ParseSanString --------
let parse str expected = 
    San.ParseSanString str
    |> Operators.getSuccess
    |> sprintf "%+A"
    |> should equal expected

[<Fact>]
let ``parse O-O-O``() = parse "O-O-O" "(LongCastling, null)"

[<Fact>]
let ``parse O-O``() = parse "O-O" "(ShortCastling, null)"

[<Fact>]
let ``parse Bg5``() = 
    parse "Bg5" "(Usual (Bishop, (NoHint, (null, (6, 3)))), null)"

[<Fact>]
let ``parse Nec6``() = 
    parse "Nec6" "(Usual (Knight, (FileHint 4, (null, (2, 2)))), null)"

[<Fact>]
let ``parse K5c6``() = 
    parse "N5c6" "(Usual (Knight, (RankHint 3, (null, (2, 2)))), null)"

[<Fact>]
let ``parse Qe1f8``() = 
    parse "Qe1f8" "(Usual (Queen, (SquareHint (4, 7), (null, (5, 0)))), null)"

[<Fact>]
let ``parse Ne:c6``() = 
    parse "Ne:c6" 
        "(Usual (Knight, (FileHint 4, (Some SanCapture, (2, 2)))), null)"

[<Fact>]
let ``parse K5:c6``() = 
    parse "N5:c6" 
        "(Usual (Knight, (RankHint 3, (Some SanCapture, (2, 2)))), null)"

[<Fact>]
let ``parse Qe1xf8``() = 
    parse "Qe1xf8" 
        "(Usual (Queen, (SquareHint (4, 7), (Some SanCapture, (5, 0)))), null)"

[<Fact>]
let ``parse e4``() = parse "e4" "(PawnPush ((4, 4),null), null)"

[<Fact>]
let ``parse f8=Q``() = parse "f8=Q" "(PawnPush ((5, 0),Some Queen), null)"

[<Fact>]
let ``parse c1=N+``() = 
    parse "c1=N+" "(PawnPush ((2, 7),Some Knight), Some SanCheck)"

[<Fact>]
let ``parse a2#``() = parse "a2#" "(PawnPush ((0, 6),null), Some SanMate)"

[<Fact>]
let ``parse gxe4``() = parse "gxe4" "(PawnCapture (6,((4, 4), null)), null)"

// ----- Scanners --------
let findPushingPawns square (expected : string list) board = 
    let scan, _, _ = San.sanScanners (Fen.ParseCore board)
    scan (X88.parse square)
    |> List.map Coordinate.ToString
    |> should equal expected

[<Fact>]
let ``push black pawn: b6-b5``() = 
    "8/1p6/1p6/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b5" [ "b6" ]

[<Fact>]
let ``push black pawn: b7-b1 (as if pawns slid)``() = 
    "8/1p6/8/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b1" [ "b7" ]

[<Fact>]
let ``black pawns don't push back: b7-b8``() = 
    "8/1p6/8/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b8" []

[<Fact>]
let ``push white pawn: e2-e4``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findPushingPawns "e4" [ "e2" ]

let findCapturingPawns square (expected : string list) board = 
    let _, scan, _ = San.sanScanners (Fen.Parse board).Core
    scan (X88.parse square)
    |> List.map Coordinate.ToString
    |> should equal expected

[<Fact>]
let ``white pawn captures: e2-d3``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "d3" [ "e2" ]

[<Fact>]
let ``white pawn does not capture: e2-e3``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "e3" []

[<Fact>]
let ``white pawn does not capture backwards: e2-f1``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "f1" []

[<Fact>]
let ``white pawn captures, while black does not: e2-f3``() = 
    "8/8/8/8/8/5Q2/4P1p1/8 w - - 0 1" |> findCapturingPawns "f3" [ "e2" ]

[<Fact>]
let ``2 black pawns can capture``() = 
    "8/8/8/8/8/8/2p1p3/8 b - - 0 1" |> findCapturingPawns "d1" [ "e2"; "c2" ]

let findNonPawnPieces pieceType square (expected : string list) board = 
    let _, _, scan = San.sanScanners (Fen.ParseCore board)
    scan pieceType (X88.parse square)
    |> List.map Coordinate.ToString
    |> should equal expected

[<Fact>]
let ``2 black knights can capture e5``() = 
    "2k1r3/pp3ppp/2n3n1/1p1rP3/6b1/1NP2NB1/PPK3PP/R3R3 b - - 0 19" 
    |> findNonPawnPieces Knight "e5" [ "c6"; "g6" ]

[<Fact>]
let ``2 black rooks can capture e5``() = 
    "2k1r3/pp3ppp/2n3n1/1p1rP3/6b1/1NP2NB1/PPK3PP/R3R3 b - - 0 19" 
    |> findNonPawnPieces Rook "e5" [ "e8"; "d5" ]

[<Fact>]
let ``white rook can capture e5``() = 
    "2k1r3/pp3ppp/2n3n1/1p1rP3/6b1/1NP2NB1/PPK3PP/R3R3 w - - 0 19" 
    |> findNonPawnPieces Rook "e5" [ "e1" ]

[<Fact>]
let ``white bishop can capture e5``() = 
    "2k1r3/pp3ppp/2n3n1/1p1rP3/6b1/1NP2NB1/PPK3PP/R3R3 w - - 0 19" 
    |> findNonPawnPieces Bishop "e5" [ "g3" ]

[<Fact>]
let ``white knight can capture e5``() = 
    "2k1r3/pp3ppp/2n3n1/1p1rP3/6b1/1NP2NB1/PPK3PP/R3R3 w - - 0 19" 
    |> findNonPawnPieces Knight "e5" [ "f3" ]

[<Fact>]
let ``black queen can capture d1``() = 
    "rn1qk1nr/pp2Pppp/8/1p2P3/6b1/2P2N2/PP4PP/RNBQK2R b KQkq - 0 12" 
    |> findNonPawnPieces Queen "d1" [ "d8" ]

[<Fact>]
let ``white king can capture d1``() = 
    "rn1qk1nr/pp2Pppp/8/1p2P3/6b1/2P2N2/PP4PP/RNBQK2R w KQkq - 0 12" 
    |> findNonPawnPieces King "d1" [ "e1" ]

[<Fact>]
let ``findNonPawnPieces throws when given pawn``() = 
    let board = "8/8/8/8/8/8/8/8 w KQkq - 0 12"
    (fun () -> board |> findNonPawnPieces Pawn "d1" []) 
    |> should throw typeof<System.Exception>

// ----- toSanMove --------
let san move (expected : string) board = 
    let fromLegalSanString str board = 
        match San.FromSanString str board with
        | San.LegalSan(move, warns) -> 
            if not warns.IsEmpty then 
                failwithf "%s" (concatFieldNames ", " warns)
            move
        | x -> failwithf "%A" x
    
    Fen.Parse board
    |> fromLegalSanString move
    |> sprintf "%A"
    |> should equal expected

let warn move (expected : string) warnings board = 
    let fromLegalSanString str board = 
        match San.FromSanString str board with
        | San.LegalSan(move, warns) -> 
            concatFieldNames ", " warns |> should equal warnings
            move
        | x -> failwithf "%A" x
    
    Fen.Parse board
    |> fromLegalSanString move
    |> sprintf "%A"
    |> should equal expected

let illegal move expected errors board = 
    let MoveToString m = 
        let getStrings piece castling observations warnings errors 
            resultObservations = 
            seq { 
                if piece <> None then yield fieldName piece.Value
                if castling <> None then yield fieldName castling.Value
                for x in observations -> fieldName x
                for x in warnings -> fieldName x
                for x in errors -> fieldName x
                for x in resultObservations -> fieldName x
            }
        
        getStrings m.Piece m.Castling m.Observations m.Warnings m.Errors [] 
        |> String.concat " | "
    Fen.Parse board
    |> San.FromSanString move
    |> function 
    | San.IllegalSan il -> 
        il.Move.AsString |> should equal expected
        il
        |> MoveToString
        |> should equal errors
    | x -> failwithf "Unexpected: %A" x

let nonsense move errors board = 
    Fen.Parse board
    |> San.FromSanString move
    |> function 
    | San.Nonsense x -> (sprintf "%A" x) |> should equal errors
    | x -> failwithf "Unexpected: %A" x

[<Fact>]
let ``San: white O-O``() = 
    "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 12" |> san "O-O" "e1-g1"

[<Fact>]
let ``San: black O-O``() = 
    "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 12" |> san "O-O" "e8-g8"

[<Fact>]
let ``San: white O-O-O``() = 
    "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 12" |> san "O-O-O" "e1-c1"

[<Fact>]
let ``San: black O-O-O``() = 
    "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 12" |> san "O-O-O" "e8-c8"

[<Fact>]
let ``San: pawn push``() = "8/8/8/8/8/8/P7/8 w - - 0 12" |> san "a3" "a2-a3"

[<Fact>]
let ``San: pawn push and promote``() = 
    "8/P7/8/8/8/8/8/8 w - - 0 12" |> san "a8=R" "a7-a8=R"

[<Fact>]
let ``San: pawn capture and promote``() = 
    "1n6/P7/8/8/8/8/8/8 w - - 0 12" 
    |> warn "axb8=Q" "a7-b8=Q" "DisambiguationIsExcessive"

[<Fact>]
let ``San: pawn double push``() = 
    "8/8/8/8/8/8/P7/8 w - - 0 12" |> san "a4" "a2-a4"

[<Fact>]
let ``San: pawn 3 squares push``() = 
    "8/8/8/8/8/8/P7/8 w - - 0 12" 
    |> illegal "a5" "a2-a5" "Pawn | DoesNotMoveThisWay"

[<Fact>]
let ``San: pawn captures``() = 
    "8/8/8/8/8/1p6/P7/8 w - - 0 12" 
    |> warn "axb3" "a2-b3" "DisambiguationIsExcessive"

[<Fact>]
let ``San: 2 pawns can capture``() = 
    "8/8/8/8/8/1p6/P1P5/8 w - - 0 12" |> san "cxb3" "c2-b3"

[<Fact>]
let ``San: pawn move does not make sense``() = 
    "8/8/8/8/8/8/P7/8 w - - 0 12" 
    |> nonsense "axb4" "PieceNotFound (White, Pawn)"

[<Fact>]
let ``San: can't push pawn cause it's pinned``() = 
    "8/8/8/3rP1K1/8/8/8/8 w - f6 0 1" |> illegal "e6" "MoveToCheck"

[<Fact>]
let ``San: pawn can't capture cause it's pinned``() = 
    "8/6K1/3r4/4P3/8/2b5/8/2k5 w - f6 0 1" |> illegal "exd6" "MoveToCheck"

[<Fact>]
let ``San: Nf3``() = "8/8/8/8/8/8/8/6N1 w - - 0 12" |> san "Nf3" "g1-f3"

[<Fact>]
let ``San: over-disambiguate N1f3``() = 
    "8/8/8/8/8/8/8/6N1 w - - 0 12" 
    |> warn "N1f3" "g1-f3" "DisambiguationIsExcessive"

[<Fact>]
let ``San: under-disambiguate Nf3``() = 
    "8/8/8/6N1/8/8/8/6N1 w - - 0 12" 
    |> nonsense "Nf3" "AmbiguousChoice [g1-f3; g5-f3]"

[<Fact>]
let ``San: disambiguate N1f3``() = 
    "8/8/8/6N1/8/8/8/6N1 w - - 0 12" |> san "N1f3" "g1-f3"

[<Fact>]
let ``San: no candidates found Nf3``() = 
    "8/8/8/8/8/8/8/8 w - - 0 12" 
    |> nonsense "Nf3" "PieceNotFound (White, Knight)"

[<Fact>]
let ``San: wrong disambiguation N1f3``() = 
    "8/8/8/6N1/8/8/8/8 w - - 0 12" 
    |> nonsense "N1f3" "PieceNotFound (White, Knight)"

[<Fact>]
let ``San: one of the knights is pinned``() = 
    "8/6K1/8/4N3/8/2b5/8/2k1N3 w - f6 0 1" |> san "Nd3+" "e1-d3"

[<Fact>]
let ``San: file and rank disambiguation``() = 
    "8/5N2/8/6q1/8/5N1N/8/2k1K3 w - - 0 2" |> san "Nf3xg5" "f3-g5"

[<Fact>]
let ``San: no check when there should be``() = 
    "Q7/8/8/8/8/8/8/8 w - - 0 2" |> warn "Qh1+" "a8-h1" "IsNotCheck"

[<Fact>]
let ``San: check is not marked``() = 
    "Q7/8/8/8/8/8/8/k7 w - - 0 2" |> warn "Qh1" "a8-h1" "IsCheck"

[<Fact>]
let ``San: it is not capture when it should be``() = 
    "Q7/8/8/8/8/8/8/7n w - - 0 2" |> warn "Qh1" "a8-h1" "IsCapture"

[<Fact>]
let ``San: it is not mate when it should be``() = 
    "8/Q7/8/8/8/8/8/5K1k w - - 0 1" |> warn "Qh7" "a7-h7" "IsMate"

[<Fact>]
let ``San: it is mate when it should be``() = 
    "8/Q7/8/8/8/8/8/5K1k w - - 0 1" |> san "Qh7#" "a7-h7"

[<Fact>]
let ``San: it is check, not mate``() = 
    "8/Q7/8/8/8/8/8/5K1k w - - 0 1" |> warn "Qa8#" "a7-a8" "IsNotMate, IsCheck"

[<Fact>]
let ``San: it not marked capture when it should be``() = 
    "Q7/8/8/8/8/8/8/8 w - - 0 2" |> warn "Qxh1" "a8-h1" "IsNotCapture"

[<Fact>]
let ``San: 2 candidates, 0 valid moves``() = 
    "8/8/2B3B1/3n1n2/4k3/8/8/8 b - - 0 2" 
    |> nonsense "Ne7" 
           "ChoiceOfIllegalMoves [d5-e7 (MoveToCheck); f5-e7 (MoveToCheck)]"

[<Fact>]
let ``San: disambiguation is excessive only after validation``() = 
    "4b3/5N2/8/6nK/8/5N1N/8/2k4r w - - 0 2" 
    |> warn "Nf3xg5" "f3-g5" "DisambiguationIsExcessive"

[<Fact>]
let ``San: invalid castling``() = 
    "rn2k2r/ppp2ppp/3B1n2/8/3P2b1/6P1/PPP1N2P/RN1QKB1q b Qkq - 0 9" 
    |> illegal "O-O" "e8-g8" "King | BK | CastleThroughCheck"
