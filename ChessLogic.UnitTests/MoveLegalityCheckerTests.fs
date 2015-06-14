module MoveLegalityCheckerTests

open FsUnit.Xunit
open FenParser
open Xunit
open System
open MoveLegalityChecker
open CoordinateNotation
open Parsing
open Definitions

let MoveToString (move : MoveInfo) = 
    let getStrings piece castling observations warnings errors = 
        seq { 
            if piece <> None then yield toString piece.Value
            if castling <> None then yield toString castling.Value
            for x in observations -> toString x
            for x in warnings -> toString x
            for x in errors -> toString x
        }
    
    let strings = 
        match move with
        | LegalMove m -> 
            let m = m.Data
            getStrings (Some(m.Piece)) m.Castling m.Observations m.Warnings [] 
        | IllegalMove m -> 
            let m = m.Data
            getStrings m.Piece m.Castling m.Observations m.Warnings m.Errors
    
    String.Join(" | ", strings)

let check position move expectedHint = 
    position
    |> ParseFen
    |> unwrap
    |> ValidateMove(_cn move)
    |> MoveToString
    |> should equal expectedHint

module ``Generic errors`` = 
    [<Fact>]
    let ``Cannot move from empty cell2``() = 
        check "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1" 
            "f6-g8" "EmptyCell | ToOccupiedCell"
    
    [<Fact>]
    let ``Move to check that gives check``() = 
        check "K1k5/8/8/8/8/8/8/8 b - - 1 1" 
            "c8-b8" "King | MoveToCheck"
    
    [<Fact>]
    let ``Cannot move from empty cell``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR w KQkq - 1 3" 
            "e2-e3" "EmptyCell"
    
    [<Fact>]
    let ``Move to occupied cell``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR w KQkq - 1 3" 
            "a1-a2" "Rook | ToOccupiedCell"
    
    [<Fact>]
    let ``Move where from and to is the same square means ToOccupiedCell``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR w KQkq - 1 3" 
            "a1-a1" "Rook | ToOccupiedCell"
    
    [<Fact>]
    let ``Wrong side to move``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR b KQkq - 1 3" 
            "e4-e5" "Pawn | WrongSideToMove"
    
    [<Fact>]
    let ``Promotion hint is not needed``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR w KQkq - 1 3" 
            "e4-e5=Q" "Pawn | PromotionHintIsNotNeeded"
            
module ``Moves along the pin line`` = 
    [<Fact>]
    let ``white pawn double push``() = 
        check "r3k1n1/B5b1/n3q1pr/2p4p/PpN4P/1P1P2P1/R1P1P1b1/1N1QK3 w - - 4 33" 
            "e2-e4" "Pawn | DoublePush"
    
    [<Fact>]
    let ``white pawn push``() = 
        check "r1q4b/6kr/8/pP1p1pP1/R1P2pnB/NPK1p2P/2b1B2R/6N1 w - - 2 52" 
            "c4-c5" "Pawn"
    
    [<Fact>]
    let ``white pawn takes the pinning piece``() = 
        check "r4bnr/2p1p1pp/ppn2p2/2k5/P7/1PN1P1Rb/3PBPPP/2B1QKNR w - - 1 19" 
            "g2-h3" "Pawn | Capture"
    
    [<Fact>]
    let ``Valid pawn move towards pinning piece``() = 
        check "rnb1kbnr/1p2pp2/p1p4p/3p3q/4R2N/5P2/1PPPP1PR/1NBQKB2 b q - 1 11" 
            "e7-e6" "Pawn"
    
    [<Fact>]
    let ``Valid bishop move towards pinning piece``() = 
        check "1r1qk3/2ppnp2/bp2p1Pb/p2P4/3nPB2/PPPQ3R/3K1PP1/RN3BN1 w - - 5 20" 
            "f4-h6" "Bishop | Capture"
    
    [<Fact>]
    let ``Knight takes piece that gives check``() = 
        check "4kb1r/r1p3q1/4b2p/1Bnpp3/1P2P1pP/P1P4n/3P1PP1/RNBR3K b k - 0 20" 
            "c5-d7" "Knight"
    
    [<Fact>]
    let ``King moves along attack line``() = 
        check "rn5r/1b4q1/3p4/1k3p1p/p1NK1PPb/3R4/1B1P4/R3N3 w - - 3 44" "d4-c3" 
            "King | MoveToCheck"
    
    [<Fact>]
    let ``Knight takes attacking piece``() = 
        check "1nb2b2/3p2pQ/P1pk2n1/1B2Pp2/p6R/3P4/PPN1P1q1/R1B1K1N1 b - - 0 25" 
            "g6-e5" "Knight | Capture"

module ``Moves from under check`` = 
    [<Fact>]
    let ``White pawn takes queen that gives check``() = 
        check "rn3k2/p7/1p5n/1p1prb1p/1N2p2R/P1P2q2/4K1P1/R4B2 w - - 8 43" 
            "g2-f3" "Pawn | Capture"
    
    [<Fact>]
    let ``White pawn takes pawn that gives check``() = 
        check "3k3r/r1p5/4n3/pp3pp1/P1K5/N4P1B/1R3B1p/5R2 w - - 0 42" "a4-b5" 
            "Pawn | Capture"
    
    [<Fact>]
    let ``White pawn takes knight that gives check``() = 
        check "8/8/r7/p2np1N1/2B1P1P1/4K3/PPPPQP1P/RNB2R2 w - - 1 11" "e4-d5" 
            "Pawn | Capture"
    
    [<Fact>]
    let ``Pawn takes piece that gives check en-passant2``() = 
        check "8/8/6b1/2k2NP1/p1pP4/P7/6K1/8 b - d3 0 96" "c4-d3" 
            "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``Pawn takes piece that gives check en-passant4``() = 
        check "r4bn1/5p2/pp1Nq2Q/n1kppb2/PPp5/2P1P3/3P2P1/R1B1K1NR b - b3 0 30" 
            "c4-b3" "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``Pawn takes piece that gives check en-passant1``() = 
        check 
            "q3kbr1/1rpb2n1/2P4p/p3ppP1/2p1KB1P/1PP4R/R3P1P1/3Q1BN1 w - f6 0 26" 
            "g5-f6" "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``Pawn takes piece that gives check en-passant3``() = 
        check "k7/8/1P4B1/1P2Pp1P/2pPK1QN/7Q/8/8 w - f6 0 93" "e5-f6" 
            "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``White pawn takes en-passant and block check``() = 
        check "1r6/2pk1p2/p2b4/PprN1b2/4n2p/4RP1p/3p4/1K6 w - b6 0 48" "a5-b6" 
            "Pawn | Capture | EnPassant"

module ``Moves to check`` = 
    [<Fact>]
    let ``White pawn move would discover a check``() = 
        check 
            "2r1kb1r/2p1ppp1/pn3n2/7p/PpPp1P1N/BP1P2PN/3KP1qR/1R1Q1B2 w k - 1 23" 
            "e2-e3" "Pawn | MoveToCheck"
    
    [<Fact>]
    let ``Black pawn move to check``() = 
        check 
            "rnbqkbnr/p1pp2p1/4ppQ1/1p6/2P5/N4Pp1/PP1PP2P/R1B1KBNR b KQkq - 1 7" 
            "g3-g2" "Pawn | MoveToCheck"
    
    [<Fact>]
    let ``There is check from bishop``() = 
        check 
            "rnb1kb1r/1pp1pppp/p4n2/2qp4/5P2/3P2PP/PPPNP1B1/R1BQK1NR w KQkq - 3 8" 
            "e1-f2" "King | MoveToCheck"
    
    [<Fact>]
    let ``Black pawn move to check2``() = 
        check 
            "rnbqkbnr/p1pp2p1/4ppQ1/1p6/2P5/N4Pp1/PP1PP2P/R1B1KBNR b KQkq - 1 7" 
            "g3-h2" "Pawn | Capture | MoveToCheck"
    
    [<Fact>]
    let ``Move to check``() = 
        check "8/8/8/8/6n1/8/8/4K3 w - - 1 1" "e1-f2" "King | MoveToCheck"
    
    [<Fact>]
    let ``Move to check from pawn``() = 
        check 
            "r2q2n1/p1p1kp1r/b5p1/1p1Pn2p/8/bP1P1P1P/PR1BB1P1/3QK1NR b K - 0 1" 
            "e7-e6" "King | MoveToCheck"
    
    [<Fact>]
    let ``Move to discovered check``() = 
        check "2r4r/p1Bk1p2/b1n1qnp1/1p5p/5P1P/1P1P2PQ/Pb2BK2/6NR b - - 10 1" 
            "e6-b3" "Queen | Capture | MoveToCheck"

module ``White pawn`` = 
    [<Fact>]
    let ``double push from the 4th rank``() = 
        check "rnbqkbnr/1p1ppppp/B1p5/p7/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 3" 
            "e4-e6" "Pawn | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``double push to capture``() = 
        check "r1bk4/2q3n1/p1p1rp2/2p2p1p/p3QPnP/2K3P1/P1P3R1/B3N1R1 w - - 1 38" 
            "a2-a4" "Pawn | Capture | DoesNotCaptureThisWay"
    
    [<Fact>]
    let ``double push over an occupied square``() = 
        check 
            "rnb1k3/2q1r1np/p1pp1p2/p1b1Pp2/1P2P3/4QNP1/P1PK1PRP/R1B5 w q - 3 23" 
            "f2-f4" "Pawn | DoesNotJump"
    
    [<Fact>]
    let ``double push``() = 
        check "8/8/8/8/8/8/P7/8 w - - 0 1" "a2-a4" "Pawn | DoublePush"
    
    [<Fact>]
    let ``push to capture``() = 
        check 
            "rnb1k2r/3p1pnp/pqp3p1/p1b1p3/3PP2N/6P1/PPP2P1P/RNBQK1R1 w kq - 1 13" 
            "e4-e5" "Pawn | Capture | DoesNotCaptureThisWay"
    
    [<Fact>]
    let ``push to promote``() = 
        check "8/P7/8/8/8/8/8/8 w - - 0 1" "a7-a8" 
            "Pawn | Promotion | MissingPromotionHint"
    
    [<Fact>]
    let push() = 
        check "1rn2k2/3p2qQ/1p1p3b/4p3/P3P3/P1P1R3/1R2KPPB/1N4N1 w - - 9 37" 
            "f2-f3" "Pawn"
    
    [<Fact>]
    let ``catures en-passant``() = 
        check "rnbqkbnr/ppp2p1p/3p4/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq g6 0 4" 
            "f5-g6" "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``attempts to capture en-passant on the wrong file``() = 
        check "rnbqkbnr/ppp2p1p/8/3ppPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq g6 0 4" 
            "f5-e6" "Pawn | Capture | EnPassant | HasNoEnPassant"
    
    [<Fact>]
    let ``attempts to capture en-passant style at the correct rank when situation does not look like en-passant``() = 
        check 
            "rnbqkb1r/1pp2ppp/p7/3pp1NP/1P3Pn1/2P5/P2PP1P1/RNBQKBR1 w Qkq - 0 1" 
            "h5-g6" "Pawn | OnlyCapturesThisWay"
    
    [<Fact>]
    let ``attempts to capture empty square``() = 
        check "rnbqkbnr/ppppppp1/8/7p/8/N7/PPPPPPPP/R1BQKBNR w KQkq h6 0 2" 
            "g2-h3" "Pawn | OnlyCapturesThisWay"
    
    [<Fact>]
    let ``captures with promotion``() = 
        check "1q6/P7/8/8/8/8/8/8 w - - 0 1" "a7-b8=N" 
            "Pawn | Promotion | Capture"
    
    [<Fact>]
    let captures() = 
        check "8/1q6/P7/8/8/8/8/8 w - - 0 1" "a6-b7" "Pawn | Capture"
    
    [<Fact>]
    let ``captures across the board edge``() = 
        check "r1bk4/2q3n1/p1p1rp2/2p2p1p/p3QPnP/2K3P1/P1P3R1/B3N1R1 w - - 1 38" 
            "h4-a6" "Pawn | Capture | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``does not move this way``() = 
        check "rnbqk1nr/pppp1ppp/3bp3/8/4P3/7P/PPPP1PP1/RNBQKBNR w KQkq - 1 3" 
            "a2-e2" "Pawn | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``does not move backwards``() = 
        check "8/8/8/8/8/8/4P3/8 w - - 0 1" "e2-e1" "Pawn | DoesNotMoveThisWay"

module ``Black pawn`` = 
    [<Fact>]
    let ``double push from the 5th rank``() = 
        check "8/p3ppp1/2p5/1p1p3p/3P4/2P5/PPQ1PPPP/8 b KQkq - 1 5" "b5-b3" 
            "Pawn | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``double push to capture``() = 
        check 
            "rn1qkb2/p5pr/2p2p1n/1p1pp1Bp/3P2b1/1PP2P2/P3P1PP/RN2KBNR b KQq - 1 10" 
            "g7-g5" "Pawn | Capture | DoesNotCaptureThisWay"
    
    [<Fact>]
    let ``double push over an occupied square``() = 
        check "r1bqkb1r/pppp1ppp/2n5/4p3/PPP1n3/N7/R2PPPPP/2BQKBNR b Kkq c3 0 5" 
            "c7-c5" "Pawn | DoesNotJump"
    
    [<Fact>]
    let ``double push``() = 
        check "rnbqkbnr/pppppppp/8/8/8/N7/PPPPPPPP/R1BQKBNR b KQkq - 1 1" 
            "a7-a5" "Pawn | DoublePush"
    
    [<Fact>]
    let ``push to capture``() = 
        check "r1bq1b1r/1pppnkpp/8/p3ppP1/PPP1n3/N4P2/R2PP2P/2BQKBNR b K - 3 10" 
            "a5-a4" "Pawn | Capture | DoesNotCaptureThisWay"
    
    [<Fact>]
    let ``push to promote``() = 
        check "8/8/8/8/8/8/p7/8 b - - 0 1" "a2-a1=N" "Pawn | Promotion"
    
    [<Fact>]
    let push() = 
        check "1rn2k2/3p2qQ/1p1p3b/4p3/P3P3/P1P1R3/1R2KPPB/1N4N1 b - - 9 37" 
            "d6-d5" "Pawn"
    
    [<Fact>]
    let ``catures en-passant``() = 
        check 
            "1nb2b2/3p2pQ/P1p3nB/1B1k1p2/pP2R3/3P4/P1N1P1q1/R3K1N1 b - b3 0 28" 
            "a4-b3" "Pawn | Capture | EnPassant"
    
    [<Fact>]
    let ``attempts to capture en-passant on the wrong file``() = 
        check "7r/B2b2pp/3qpbk1/pN2n3/1PpPNP1P/7K/P5P1/R1Q2BR1 b - d3 0 32" 
            "c4-b3" "Pawn | Capture | EnPassant | HasNoEnPassant"
    
    [<Fact>]
    let ``attempts to capture en-passant style at the correct rank when situation does not look like en-passant``() = 
        check "7r/B2b2pp/3qpbk1/pN2n3/1Pp1NP1P/7K/P5P1/R1Q2BR1 b - - 0 32" 
            "c4-d3" "Pawn | OnlyCapturesThisWay"
    
    [<Fact>]
    let ``attempts to capture empty square``() = 
        check "rnbqkbnr/ppppp1p1/5p2/7p/2P5/N4P2/PP1PP1PP/8 b KQkq c3 0 3" 
            "b7-c6" "Pawn | OnlyCapturesThisWay"
    
    [<Fact>]
    let ``captures with promotion``() = 
        check "8/8/8/8/8/8/p7/1Q6 b - - 0 1" "a2-b1" 
            "Pawn | Promotion | Capture | MissingPromotionHint"
    
    [<Fact>]
    let captures() = 
        check "r1bq1b1r/1pppnkpp/8/p3ppP1/PPP1n3/N4P2/R2PP2P/2BQKBNR b K - 3 10" 
            "a5-b4" "Pawn | Capture"
    
    [<Fact>]
    let ``does not move this way``() = 
        check "r1bqkb1r/pppp1ppp/2n5/4p3/PPP1n3/N7/R2PPPPP/2BQKBNR b Kkq c3 0 5" 
            "e5-a1" "Pawn | DoesNotMoveThisWay"

module Knight = 
    [<Fact>]
    let ``moves NNE``() = 
        check "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "b1-c3" 
            "Knight"
    
    [<Fact>]
    let ``moves NNW``() = 
        check 
            "r1bq1bnr/p2pppkp/2n3p1/1pp5/4N2P/3P4/PPP1PPP1/R1BQKBNR w KQ - 0 2" 
            "e4-d6" "Knight"
    
    [<Fact>]
    let ``moves SSE``() = 
        check "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1" 
            "b8-c6" "Knight"
    
    [<Fact>]
    let ``moves NWW``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 1" 
            "c3-a4" "Knight"
    
    [<Fact>]
    let ``moves NEE``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 1" 
            "c3-e4" "Knight"
    
    [<Fact>]
    let ``moves SWW``() = 
        check "rnbqkbnr/p2ppp1p/6p1/1pp5/4N2P/8/PPPPPPP1/R1BQKBNR w KQkq b6 0 1" 
            "e4-c3" "Knight"
    
    [<Fact>]
    let ``moves SEE``() = 
        check "rnbqkbnr/p2ppp1p/6p1/1pp5/4N2P/8/PPPPPPP1/R1BQKBNR w KQkq b6 0 1" 
            "e4-g3" "Knight"
    
    [<Fact>]
    let ``moves SSW``() = 
        check 
            "r1bqkbnr/p2ppp1p/2n3p1/1pp5/4N2P/3P4/PPP1PPP1/R1BQKBNR w KQkq - 0 2" 
            "e4-d2" "Knight"
    
    [<Fact>]
    let ``Does not move h3-a6``() = 
        check "rnbqkb1r/p2ppp1p/6pn/1pp5/4N2P/7N/PPPPPPP1/R1BQKB1R w KQkq - 2 1" 
            "h3-a6" "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move h5-a4``() = 
        check 
            "r1b2k1r/2n1qpbp/2p3p1/1p1Pp2n/1PBP3P/PQ6/2P2PP1/RNBK2N1 b - - 6 20" 
            "h5-a4" "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move a3-g1``() = 
        check "r1bqkb2/2ppp2r/p4Qpn/5p1p/1p6/N2PBN2/PPP1PPPP/R3KB1R w Qq - 2 13" 
            "a3-g1" "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move a5-h2``() = 
        check "8/2r2r2/2bb2k1/N2Pp3/1p2PBnp/p1PK1B2/PP6/R6R w - - 4 48" "a5-h2" 
            "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move h6-b8``() = 
        check "r1bqkb2/1ppppr2/p5pn/5p1p/3Q4/N2PBN2/PPP1PPPP/R3KBR1 b Qq - 3 10" 
            "h6-b8" "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move a3-h4``() = 
        check "r1bqkb2/1ppppp1r/p5pn/7p/3Q4/N2P1N2/PPP1PPPP/R1B1KBR1 w Qq - 0 8" 
            "a3-h4" "Knight | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``Does not move b8-b6``() = 
        check "rnbqkbnr/ppp2p1p/3p2P1/4p3/4P3/8/PPPP2PP/RNBQKBNR b KQkq - 0 4" 
            "b8-b6" "Knight | DoesNotMoveThisWay"

module King = 
    [<Fact>]
    let ``moves N``() = check "8/5k2/R6K/8/8/8/8/8 w - - 7 143" "h6-h7" "King"
    
    [<Fact>]
    let ``moves NW``() = 
        check "1r1nkb1N/p3n3/q2p3p/7P/Pp2p1p1/RPPp1Pp1/1B2PK2/5R2 w - - 0 33" 
            "f2-e3" "King"
    
    [<Fact>]
    let ``captures NS``() = 
        check "1r1nkb1N/p3n3/q2p3p/7P/Pp2p1p1/RPPp1Pp1/1B2PK2/5R2 w - - 0 33" 
            "f2-g3" "King | Capture"
    
    [<Fact>]
    let ``moves S``() = 
        check "3N4/2n2r2/k7/1q3Pb1/2Q4p/4RK1P/4p3/8 b - - 7 85" "a6-a5" "King"
    
    [<Fact>]
    let ``moves SW``() = 
        check "7b/P6r/1R6/7k/8/8/6KB/8 w - - 0 108" "g2-f1" "King"
    
    [<Fact>]
    let ``moves SE``() = 
        check "7b/P4r2/1R6/7k/8/8/6KB/8 w - - 5 108" "g2-h1" "King"
    
    [<Fact>]
    let ``moves W``() = check "1k1K4/8/8/8/8/8/8/B7 b - - 3 100" "b8-a8" "King"
    
    [<Fact>]
    let ``moves E``() = check "1K1k4/8/8/8/8/8/8/B7 b - - 3 100" "d8-e8" "King"
    
    [<Fact>]
    let ``captures the piece that gives it check``() = 
        check "8/1k6/4R3/1NP4p/p2Q3P/4K1n1/3q4/8 w - - 5 73" "e3-d2" 
            "King | Capture"
    
    [<Fact>]
    let ``doesn't move a6-h5``() = 
        check "8/5K2/k7/8/3N4/1P4Q1/8/8 b - - 6 110" "a6-h5" 
            "King | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``doesn't move h1-a2``() = 
        check "1rk3r1/1b5p/1pp5/p3R1PP/1PP2p2/1N3B2/1P5R/2B4K w - - 0 33" 
            "h1-a2" "King | DoesNotMoveThisWay"

module Castling = 
    // ================ WQ ================
    [<Fact>]
    let ``WQ works``() = 
        check "r3k1r1/3bbpp1/2q5/1npN3p/pQ4n1/1P6/3P1PBP/R3K1NR w Qq - 0 30" 
            "e1-c1" "King | WQ"
    
    [<Fact>]
    let ``WQ does not jump above b1``() = 
        check 
            "rn1qkbn1/p3p1pr/2p2p2/1p1p3p/3P2b1/2P5/PP1BPPPP/RN2KBNR w KQq - 2 8" 
            "e1-c1" "King | WQ | DoesNotJump"
    
    [<Fact>]
    let ``WQ does not jump above d1``() = 
        check "8/4k3/8/6b1/8/8/8/R2bK3 w Q - 0 1" "e1-c1" 
            "King | WQ | DoesNotJump"
    
    [<Fact>]
    let ``WQ castling is impossible when d1 is attacked``() = 
        check 
            "1nb1kb2/3p1Np1/rpp4r/p1P1pp2/1Q2P1p1/PP2n3/4PP2/R3KBR1 w Q - 1 28" 
            "e1-c1" "King | WQ | CastleThroughCheck"
    
    // ================ BQ ================
    [<Fact>]
    let ``BQ castling doesn't care if B8 square is under attack``() = 
        check "r3k1r1/3bbpp1/2q5/2pN3p/pQ4n1/1P1K4/n2P1PBP/R5NR b q - 0 30" 
            "e8-c8" "King | BQ"
    
    [<Fact>]
    let ``BQ is impossible if king is presently under check``() = 
        check 
            "r3k2r/ppp2ppp/n2q1n2/1Bbppb2/4P3/P1N2N2/1PPP1PPP/R1B1QRK1 b kq - 0 8" 
            "e8-c8" "King | BQ | CastleFromCheck"
    
    [<Fact>]
    let ``BQ cannot Capture a piece``() = 
        check 
            "r1B1k1r1/p2p1p2/1p1b3p/2pqp1p1/PPP2n2/B1N2NP1/3P3P/1R1K3n b q - 0 35" 
            "e8-c8" "King | BQ | Capture | DoesNotCaptureThisWay"
    
    [<Fact>]
    let ``BQ is impossible because there is no rook in the corner``() = 
        check "4kb1N/p2nn3/1rq4p/3p3P/PP2p1p1/2ppKPp1/1B2P3/2R4R b - - 0 40" 
            "e8-c8" "King | BQ | HasNoCastling"
    
    [<Fact>]
    let ``BQ is blocked by black's own pieces on b8 and d8``() = 
        check 
            "rn1qkb2/p5pr/2p2p1n/1p1pp1Bp/3P2b1/1PP2P2/P3P1PP/RN2KBNR b KQq - 1 10" 
            "e8-c8" "King | BQ | DoesNotJump"
    
    // ================ WK ================
    [<Fact>]
    let ``WK works``() = 
        check 
            "1rbqkb1r/p1pp1ppp/1p6/2n1p3/4nP1P/3PPN2/PPPNB1P1/R1BQK2R w KQk - 1 8" 
            "e1-g1" "King | WK"
    
    [<Fact>]
    let ``WK is blocked by white's own piece on f1``() = 
        check "rnbqkbn1/pppppppr/8/6Np/8/8/PPPPPPPP/RNBQKB1R w KQq - 2 3" 
            "e1-g1" "King | WK | DoesNotJump"
    
    [<Fact>]
    let ``WK is blocked by black's piece on f1``() = 
        check "8/4k3/8/8/8/8/8/4Kb1R w K - 0 1" "e1-g1" 
            "King | WK | DoesNotJump"
    
    [<Fact>]
    let ``WK is impossible because f1 is under attack``() = 
        check 
            "rn2kbn1/p4p1r/2p4q/1p1Bp1pp/PP1pPP2/2N4b/2PP3P/R1BQK2R w KQq - 0 13" 
            "e1-g1" "King | WK | CastleThroughCheck"
    
    // ================ BK ================
    [<Fact>]
    let ``BK works``() = 
        check 
            "1rbqk2r/p1ppbppp/1p6/2n1p3/4nP1P/P2PPN1R/1PPNB1P1/R1BQK3 b Qk - 0 9" 
            "e8-g8" "King | BK"
    
    [<Fact>]
    let ``BK is blocked and also king is under check``() = 
        check 
            "rn1qk2r/1b2p1b1/2ppPp1n/p5pQ/p1PP1N2/6P1/PP3P1P/1RB1KB1R b kq - 0 15" 
            "e8-g8" "King | BK | CastleFromCheck"
    
    [<Fact>]
    let ``BK is impossible if king is presently under check``() = 
        check 
            "rnbqk2r/ppp2ppp/5n2/1Bbpp3/4P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 0 5" 
            "e8-g8" "King | BK | CastleFromCheck"
    
    [<Fact>]
    let ``BK is impossible because there is no rook in the corner``() = 
        check "4k3/8/1P4qP/2P2N2/8/P5r1/4n3/1K6 b - - 2 90" "e8-g8" 
            "King | BK | HasNoCastling"
    
    [<Fact>]
    let ``BK is impossible because f8 is under attack``() = 
        check "r1bqk2r/pppp4/1P4pp/4pP2/n4PN1/B1n5/P2PP1BP/RN1QK2R b KQk - 1 15" 
            "e8-g8" "King | BK | CastleThroughCheck"

module Bishop = 
    [<Fact>]
    let ``moves one square SW``() = 
        check "8/4ppbp/bp4pn/p1pp4/P2P1P2/2P1PN2/1P2B1PP/RNB1QRK1 w - c6 0 10" 
            "e2-d1" "Bishop"
    
    [<Fact>]
    let ``moves SW``() = 
        check "rnbqkb1r/p2ppp1p/6pn/1pp5/4N2P/7N/PPPPPPP1/R1BQKBR1 b Qkq - 3 5" 
            "c8-a6" "Bishop"
    
    [<Fact>]
    let ``captures adjacent SW``() = 
        check "8/8/8/4B3/3r4/8/8/8 w - - 0 1" "e5-d4" "Bishop | Capture"
    
    [<Fact>]
    let ``captures SW``() = 
        check "8/8/8/4B3/8/8/1q6/8 w - - 0 1" "e5-b2" "Bishop | Capture"
    
    [<Fact>]
    let ``doesn't move across border SW``() = 
        check "b7/8/8/8/8/8/1q6/8 b - - 0 1" "a8-h8" 
            "Bishop | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``doesn't jump SW``() = 
        check "8/4ppbp/bp4pn/p1pp4/P2P1P2/2P1PN2/4B1PP/RNB1QRK1 b - c6 0 10" 
            "g7-b2" "Bishop | DoesNotJump"
    
    [<Fact>]
    let ``moves NE``() = 
        check "rnbqkbnr/1pp1pppp/8/p2p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3" 
            "c1-g5" "Bishop"
    
    [<Fact>]
    let ``doesn't move d8-d1``() = 
        check "1N1b4/8/2P5/1k3K2/8/p7/P7/8 b - - 16 140" "d8-d1" 
            "Bishop | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``doesn't move c1-b1``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 3" 
            "c1-b1" "Bishop | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``moves SE``() = 
        check "8/8/8/4B3/8/8/7r/8 w - - 0 1" "e5-h2" "Bishop | Capture"
    
    [<Fact>]
    let ``moves NW``() = 
        check "8/8/8/8/8/8/1Kp5/3Bk3 w - - 0 1" "d1-c2" "Bishop | Capture"

module Rook = 
    [<Fact>]
    let ``moves one square N``() = 
        check 
            "1n1qkbnr/rpp1pppp/p2pb3/8/8/NP2P1PN/P1PP1P1P/R1BQKB1R b KQk - 0 5" 
            "a7-a8" "Rook"
    
    [<Fact>]
    let ``moves one square E``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 3" 
            "a1-b1" "Rook"
    
    [<Fact>]
    let ``moves E``() = 
        check "r2qkbnr/1pp2ppp/p2pb3/8/6Q1/PP2P1PN/2PP1PBP/R1B1K2R b KQk - 0 10" 
            "a8-c8" "Rook"
    
    [<Fact>]
    let ``captures W``() = 
        check 
            "2r1kn1r/5p1p/1pq4b/p2p2p1/1P2PPP1/P1P1K2P/1B4N1/2R2bR1 w - - 7 33" 
            "g1-f1" "Rook | Capture"
    
    [<Fact>]
    let ``does not move a1-h2``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 3" 
            "a1-h2" "Rook | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``does not jump N``() = 
        check "r2qkbnr/1ppn1ppp/p2pb3/8/8/1P2P1PN/P1PP1P1P/R1BQKB1R w KQk - 0 8" 
            "a1-a3" "Rook | DoesNotJump"
    
    [<Fact>]
    let ``does not jump N over opponent's pieces``() = 
        check "2r1kn1r/5p1p/1pq4b/3p2p1/1P2PPP1/P1p1K2P/1B4N1/2R2bR1 w - - 0 33" 
            "c1-c4" "Rook | DoesNotJump"
    
    [<Fact>]
    let ``does not move a7-f6``() = 
        check 
            "1n1qkbnr/rpp1pppp/p2pb3/8/8/NP2P1PN/P1PP1P1P/R1BQKB1R b KQk - 0 5" 
            "a7-f6" "Rook | DoesNotMoveThisWay"
    
    [<Fact>]
    let ``does not jump N over immediate pices``() = 
        check 
            "1n1qkbnr/rpp1pppp/p2pb3/8/8/NP2P1PN/P1PP1P1P/R1BQKB1R b KQk - 0 5" 
            "a7-d7" "Rook | DoesNotJump"

module Queen = 
    [<Fact>]
    let ``moves W``() = 
        check "8/2rnk2p/1p5b/pP1p3P/P5p1/2PK4/7q/5R2 b - - 9 65" "h2-a2" "Queen"
    
    [<Fact>]
    let ``moves one square S``() = 
        check 
            "r1b1kb1r/p2pp2p/n6n/qpp2p1P/4N1p1/PQP4N/1P1PPPP1/R1B1KB1R b Qkq - 0 10" 
            "a5-a4" "Queen"
    
    [<Fact>]
    let ``moves SW``() = 
        check "rnbqkb1r/p2ppp1p/6pn/1pp5/4N2P/7N/PPPPPPP1/R1BQKBR1 b Qkq - 3 5" 
            "d8-c7" "Queen"
    
    [<Fact>]
    let ``captures one square E``() = 
        check 
            "4k2r/2r1np1p/1p5b/p2p2p1/qPb2PP1/P1P1P2P/1B1P1KN1/R5R1 b k - 2 25" 
            "a4-b4" "Queen | Capture"
    
    [<Fact>]
    let ``does not jump over adjacent pieces``() = 
        check "rnbqkbnr/pp1ppp1p/6p1/2p5/7P/2N5/PPPPPPP1/R1BQKBNR w KQkq c6 0 3" 
            "d1-b1" "Queen | DoesNotJump"
    
    [<Fact>]
    let ``does not jump over remote opponent's piece``() = 
        check "rnbqkbnr/1pp1ppp1/p7/7p/8/3p1N2/PPP1PPPP/RNBQKB1R w KQkq - 0 3" 
            "d1-d5" "Queen | DoesNotJump"
    
    let extendedSuite = 
        [| (101, 
            "rnbqkb1r/p2ppp1p/6pn/1pp5/4N2P/7N/PPPPPPP1/R1BQKBR1 b Qkq - 3 5", 
            "c8-b7", "Bishop")
           
           (102, 
            "rnbqkbnr/1pppppp1/p7/7p/8/3P1N2/PPP1PPPP/RNBQKB1R w KQkq - 0 3", 
            "d1-g4", "Queen | DoesNotJump")
           
           (103, 
            "rnbqkbnr/1pp1pppp/8/p2p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3", 
            "c1-a3", "Bishop | DoesNotJump")
           
           (104, 
            "rnb1k3/2q1r1np/p1pp1p2/p1b1Pp2/1P2P3/4QNP1/P1PK1PRP/R1B5 w q - 3 23", 
            "d2-b2", "King | DoesNotMoveThisWay")
           
           (105, "2b1k2r/r2p1ppp/2p1p3/p7/4P1nq/1P3PP1/P3K2P/3RbBNR w k - 7 23", 
            "e2-g2", "King | DoesNotMoveThisWay")
           
           (106, 
            "r1bqkbnr/pppp1ppp/n7/4p3/5P2/3P4/PPP1P1PP/RNBQKBNR w KQkq - 1 3", 
            "e1-a3", "King | DoesNotMoveThisWay")
           
           (107, 
            "rnbqkbnr/p3ppp1/2p5/1p1p3p/3P4/2P5/PPQ1PPPP/RNB1KBNR b KQkq - 1 5", 
            "b5-b3", "Pawn | DoesNotMoveThisWay")
           
           (108, 
            "rnbqkb1r/ppp2ppp/8/3pp1N1/1P3PnP/2P5/P2PP1P1/RNBQKBR1 w Qkq - 1 1", 
            "e1-f2", "King | MoveToCheck")
           
           (109, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "b1-a3", "Knight")
           
           (110, 
            "1n2kb1r/q3p1p1/3p1Bpp/p1pN4/1p2P3/2PP1N1b/PP3P1P/R3K2R w Kk - 2 23", 
            "e1-g1", "King | WK | CastleThroughCheck")
           
           (111, "1n3kn1/rpPb1p2/1P6/p1p3p1/P1B1PP2/2P4r/R5p1/1N2K2R w K - 1 28", 
            "e1-g1", "King | WK | CastleThroughCheck")
           
           (112, "r3kbn1/p2nBNp1/r7/1b1P4/RP1P1P1P/3q2p1/3N2BR/4K3 b q - 1 30", 
            "e8-c8", "King | BQ | CastleThroughCheck")
           
           (113, 
            "rnbqkbnr/ppp2p1p/3p4/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq g3 0 4", 
            "f5-g6", "Pawn | Capture | EnPassant")
           
           (114, 
            "2r1kn1r/5p1p/1pq4b/p2p2p1/1P2PPP1/P1P1K2P/1B4N1/2R2bR1 w - - 7 33", 
            "c1-c4", "Rook | DoesNotJump")
           
           (115, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "a2-a4", "Pawn | DoublePush")
           
           (116, 
            "rnbqkbnr/ppppp1p1/5p2/7p/2P5/N4P2/PP1PP1PP/R1BQKBNR b KQkq c3 0 3", 
            "b7-c6", "Pawn | OnlyCapturesThisWay")
           
           (117, 
            "2b1qb1r/1pppkppp/r7/p2np1N1/2B1P1P1/4K3/PPPPQP1P/RNB2R2 w - - 1 11", 
            "e4-d5", "Pawn | Capture")
           
           (118, "7r/B2b2pp/3qpbk1/pN2n3/1Pp1NP1P/7K/P5P1/R1Q2BR1 b - b3 0 32", 
            "c4-b3", "Pawn | Capture | EnPassant")
           
           (119, "r2k4/p1p1np2/1p2b1p1/n3p2r/2P2P1P/b6N/RB2P1KR/1NQ5 w - - 2 21", 
            "b2-a1", "Bishop")
           
           (120, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "c1-e3", "Bishop | DoesNotJump")
           
           (121, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "d1-a3", "Queen | DoesNotMoveThisWay")
           
           (122, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "d1-b3", "Queen | DoesNotJump")
           
           (123, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "d1-b3", "Queen | DoesNotJump")
           
           (124, "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1", 
            "b8-a6", "Knight")
           
           (125, "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1", 
            "c8-h3", "Bishop | DoesNotJump")
           
           (126, "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1", 
            "c8-a6", "Bishop | DoesNotJump")
           
           (127, 
            "rnbqkbnr/pppppp2/8/6pp/2P1P2P/8/PP1P1PP1/RNBQKBNR b KQkq c3 0 3", 
            "f8-h6", "Bishop")
           
           (128, 
            "rnbqkbnr/pp1ppp2/2p5/6pp/2P1P2P/8/PP1P1PP1/RNBQKBNR w KQkq - 0 4", 
            "d1-c2", "Queen")
           
           (129, 
            "rnbqkbnr/pp1p1p2/2p5/4p1pp/2P1P2P/3B4/PP1P1PP1/RNBQK1NR w KQkq e6 0 5", 
            "e1-f1", "King")
           
           (130, 
            "rnbqkbn1/pp1p1p1r/2p5/4p1pp/2P1P2P/3B4/PPQP1PP1/RNB1K1NR w KQq - 2 6", 
            "e1-d1", "King")
           
           (131, 
            "rnbqkbn1/pp1p1p1r/2p5/4p1pp/2P1P2P/3B4/PPQP1PP1/RNB1K1NR w KQq - 2 6", 
            "c2-d1", "Queen")
           
           (132, 
            "rnbqkbn1/pp1p1p1r/2p5/4p1pp/2P1P2P/3B4/PPQP1PP1/RNB1K1NR w KQq - 2 6", 
            "c2-c3", "Queen")
           
           (133, 
            "rnbqkbn1/pp1p1p1r/2p5/4p1pp/2P1P2P/3B4/PPQP1PP1/RNB1K1NR w KQq - 2 6", 
            "c2-c5", "Queen | DoesNotJump")
           
           (134, 
            "rnbqkbn1/pp1p1p1r/2p5/4p1pp/2P1P2P/3B4/PPQP1PP1/RNB1K1NR w KQq - 2 6", 
            "c2-c5", "Queen | DoesNotJump")
           
           (135, 
            "rnbqkbn1/pp3p1r/2p5/3pp1pp/Q1P1P2P/3B4/PP1P1PP1/RNB1K1NR w KQq d6 0 7", 
            "a4-a3", "Queen")
           
           (136, 
            "rnbqkbn1/pp3p1r/2p5/3pp1pp/Q1P1P2P/3B4/PP1P1PP1/RNB1K1NR w KQq d6 0 7", 
            "a4-a3", "Queen")
           
           (137, 
            "rnbqkbn1/pp3p1r/2p5/3pp1pp/Q1P1P2P/3B4/PP1P1PP1/RNB1K1NR w KQq d6 0 7", 
            "a4-d4", "Queen | DoesNotJump")
           
           (138, 
            "rnbqkbn1/pp3p1r/2p5/3pp1pp/Q1P1P2P/8/PP1PBPP1/RNB1K1NR b KQq - 1 7", 
            "b8-d7", "Knight")
           
           (139, 
            "rn2kbn1/pp3p1r/2p1b3/3Pp1Pp/1QP5/8/PP1PBPPR/RNB2KN1 w q - 1 13", 
            "b4-e1", "Queen | DoesNotJump")
           
           (140, 
            "rn2kbn1/pp3p1r/2p1b3/3Pp1Pp/1QP5/8/PP1PBPPR/RNB2KN1 w q - 1 13", 
            "b4-a3", "Queen")
           
           (141, 
            "rn2kbn1/pp3p1r/2p1b3/3Pp1Pp/Q1P5/8/PP1PBPPR/RNB2KN1 b q - 2 13", 
            "e6-d7", "Bishop")
           
           (142, 
            "rn2kb2/pp3p1r/2p1bn2/3Pp1PR/Q1P5/8/PP1PBPP1/RNB2KN1 b q - 0 14", 
            "e6-g8", "Bishop | DoesNotJump")
           
           (143, 
            "rn2kb2/pp3p1r/2p1bn2/3Pp1PR/Q1P5/8/PP1PBPP1/RNB2KN1 b q - 0 14", 
            "f6-d7", "Knight")
           
           (144, 
            "rn2k3/pp3p1r/2pbbn2/3Pp1PR/Q1P5/8/PP1P1PP1/RNBB1KN1 b q - 2 15", 
            "d6-e7", "Bishop")
           
           (145, 
            "rn1k1bn1/pp1b4/2p2p2/3PpBr1/QPPB3R/3P1N2/P4PP1/RN3K2 w - - 0 26", 
            "d4-g1", "Bishop | DoesNotJump")
           
           (146, 
            "r2k1bn1/pp1n3B/2p2p2/3Ppb2/QPP4R/3P1N2/P2N1Pr1/R3BK2 b - - 5 29", 
            "f5-c8", "Bishop | DoesNotJump")
           
           (147, 
            "r2k1bn1/pp1n3B/2p2pb1/3Pp3/QPP4R/3P1N2/P2N1Pr1/R3BK2 w - - 6 30", 
            "a1-c1", "Rook")
           
           (148, 
            "r4bn1/ppkn3B/2p2pb1/3Pp3/QPP4R/3P1N2/P2N1Pr1/1R2BK2 w - - 8 31", 
            "a4-a1", "Queen | DoesNotJump")
           
           (149, 
            "r4bn1/p1kn3B/2p2pb1/1p1P4/QPP1p3/3P1N2/P2N1PKR/1R2B3 w - - 0 33", 
            "g2-g1", "King")
           
           (150, "r4bn1/p1k5/2p2pB1/1p1Pn3/QPP1p3/3P1N2/P2N1PKR/3RB3 b - - 0 34", 
            "e5-g6", "Knight | Capture")
           
           (151, "r4bn1/p1k5/2p2pB1/1p1Pn3/QPP1p3/3P1N2/P2N1PKR/3RB3 b - - 0 34", 
            "e5-d7", "Knight")
           
           (152, "7Q/8/1k2B3/p7/3R2p1/8/4b3/6K1 w - - 11 88", "h8-a1", 
            "Queen | DoesNotJump")
           
           (153, 
            "rnbqkbr1/2pppppp/pp6/4B3/PPP3n1/1Q6/3PPPPP/RN2KBNR w KQq - 4 8", 
            "b3-c3", "Queen")
           
           (154, 
            "r1bq4/n1Bkb1r1/1p6/pP1PP2p/P4pn1/5N1P/3PP1PR/RN1K1B2 w - - 0 21", 
            "h2-h1", "Rook")
           
           (155, "r2qk3/n7/3P2r1/pPB4p/N1R2pn1/5P1P/2bP3R/4K2B w - - 6 34", 
            "c4-c1", "Rook | DoesNotJump")
           
           (156, 
            "r1bqkbr1/2B1ppp1/ppn5/3p3p/PPP2Pn1/8/Q2PP1PP/RN2KBNR w KQq d6 0 11", 
            "c7-a5", "Bishop | DoesNotJump")
           
           (157, 
            "r1bq1b2/n1Bk2r1/pp3p2/1P1pP2p/P1P3n1/5N2/3PP1PP/RN2KB1R w KQ - 2 17", 
            "f3-g1", "Knight")
           
           (158, 
            "r1bq4/n1Bkb1r1/1p6/pP1PPp1p/P5n1/5N1P/3PP1P1/RN1K1B1R w - - 2 20", 
            "f3-e1", "Knight")
           
           (159, 
            "rnbqkb1r/1ppppppp/p7/5n2/1PP5/B7/P2PPPPP/RN1QKBNR w KQkq - 3 4", 
            "h1-c1", "Rook | DoesNotJump")
           
           (160, "r2q4/nb1k4/1B1P4/pP1P2rp/N4pn1/5P1P/3P2BR/R3K3 b - - 2 29", 
            "g5-g7", "Rook")
           
           (161, "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1", 
            "a8-a1", "Rook | Capture | DoesNotJump")
           
           (162, "rnbqkbnr/1ppppppp/p7/8/1PP5/8/P2PPPPP/RNBQKBNR b KQkq b3 0 2", 
            "a8-a7", "Rook")
           
           (163, "rnbqkb1r/1ppppppp/p6n/8/1PP5/B7/P2PPPPP/RN1QKBNR b KQkq - 2 3", 
            "h8-g8", "Rook")
           
           (164, 
            "r1bqkbr1/2pppppp/ppn5/4B3/PPP2Pn1/8/Q2PP1PP/RN2KBNR b KQq f3 0 9", 
            "g8-b8", "Rook | DoesNotJump")
           
           (165, "1r3k2/n3q3/1P1r4/p3B2p/NR3pP1/5P2/2bP3R/4K2B b - - 5 38", 
            "e7-e8", "Queen")
           
           (166, "8/r7/1P1k2b1/pB3R2/3P1qpR/8/2n5/6K1 b - - 7 56", "f4-f6", 
            "Queen | DoesNotJump")
           
           (167, "rnbqkb1r/1ppppppp/p6n/8/1PP5/B7/P2PPPPP/RN1QKBNR b KQkq - 2 3", 
            "d8-g8", "Queen | DoesNotJump")
           
           (168, "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1", 
            "d8-d1", "Queen | Capture | DoesNotJump")
           
           (169, "1r3k2/3r4/1P6/p1q1B2p/1R1P1pPR/n4P2/2b3B1/4K3 b - - 1 42", 
            "c5-d6", "Queen")
           
           (170, "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1", 
            "d8-h4", "Queen | DoesNotJump")
           
           (171, 
            "r1bqkbr1/2B2pp1/ppn5/3pP2p/PPP3n1/8/2QPP1PP/RN2KBNR b KQq - 0 12", 
            "d8-h4", "Queen")
           
           (172, "1r3k2/n3q3/1P1r4/p3B2p/NR3pP1/5P2/2bP3R/4K2B b - - 5 38", 
            "e7-d8", "Queen")
           
           (173, "1r3k2/n3q3/1P1r4/p3B2p/NR3pP1/5P2/2bP3R/4K2B b - - 5 38", 
            "e7-d8", "Queen")
           
           (174, "1r3k2/3r4/1P6/p1q1B2p/1R1P1pPR/n4P2/2b3B1/4K3 b - - 1 42", 
            "c5-a7", "Queen | DoesNotJump")
           
           (175, 
            "r1bqkbr1/2B1ppp1/ppn5/3p3p/PPP2Pn1/8/2QPP1PP/RN2KBNR b KQq - 1 11", 
            "e8-d7", "King")
           
           (176, 
            "r1bq1br1/2Bk1pp1/ppn5/1P1pP2p/P1P3n1/8/2QPP1PP/RN2KBNR b KQ - 0 13", 
            "d7-e6", "King")
           
           (177, 
            "r1bq1br1/2Bk1pp1/ppn5/1P1pP2p/P1P3n1/8/2QPP1PP/RN2KBNR b KQ - 0 13", 
            "d7-e7", "King")
           
           (178, 
            "r1bq1br1/2Bk1pp1/ppn5/1P1pP2p/P1P3n1/8/2QPP1PP/RN2KBNR b KQ - 0 13", 
            "d7-e8", "King")
           
           (179, "r2q4/nb1kb1r1/1B6/pP1PP2p/P4pn1/5P1P/3PP2R/RN1K1B2 b - - 0 22", 
            "d7-c8", "King")
           
           (180, "rq4r1/nb1k4/1B1P4/pP1P3p/P4pn1/2N2P1P/3PP1BR/R2K4 b - - 4 25", 
            "d7-d8", "King | MoveToCheck")
           
           (181, 
            "rnbqkb1r/1ppppppp/p7/5n2/1PP5/B7/P2PPPPP/RN1QKBNR w KQkq - 3 4", 
            "a1-c1", "Rook | DoesNotJump")
           
           (182, "4k3/p1rn2br/1qpBpnpp/1P6/P7/3p1P2/1RP2P1P/4K2R b K - 0 21", 
            "b6-d8", "Queen | DoesNotJump")
           
           (183, "2b1r3/4ppb1/6r1/1pkpn3/1p5Q/1n1p3B/2N2K2/6R1 b - - 1 48", 
            "g6-g8", "Rook | DoesNotJump")
           
           (184, "2k5/8/4Q3/K7/4r3/8/8/7q b - - 1 122", "e4-e7", 
            "Rook | DoesNotJump")
           
           (185, 
            "1r4nr/p1p3kp/2bq2pb/2nNp3/pP1p1P1P/2PPBPN1/QR2K3/7R w - - 5 26", 
            "a2-c1", "Queen | DoesNotMoveThisWay")
           
           (186, 
            "1n3bn1/N1p1q3/1p3p2/3Bp1pr/r2NPP1p/pQ1PBK1k/P1R4P/6R1 b - - 5 34", 
            "e7-a8", "Queen | DoesNotMoveThisWay")
           
           (187, 
            "r3kbnr/p1pqpp2/n6p/Pp6/1Q6/2p2b1P/1P1PP1P1/RNB1KBNR w KQq - 2 13", 
            "b4-d5", "Queen | DoesNotMoveThisWay")
           
           (188, 
            "rn3r2/pbpn1p2/6q1/P2Pk3/1p1b4/3BPpP1/1B1P4/1N1Q1K1R b - - 5 34", 
            "g6-a1", "Queen | DoesNotMoveThisWay")
           
           (189, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "c1-b3", "Bishop | DoesNotMoveThisWay")
           
           (190, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "h1-a3", "Rook | DoesNotMoveThisWay")
           
           (191, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "h1-a3", "Rook | DoesNotMoveThisWay")
           
           (192, "rnbqkbnr/1ppppppp/8/p7/6P1/8/PPPPPP1P/RNBQKBNR w KQkq a6 0 2", 
            "h2-g2", "Pawn | DoesNotMoveThisWay")
           
           (193, "rnbqkb1r/pppppppp/5n2/8/4PP2/8/PPPP2PP/RNBQKBNR b KQkq e3 0 2", 
            "a7-g8", "Pawn | DoesNotMoveThisWay")
           
           (194, "rnb1kbnr/ppqppppp/8/2p5/5P2/5K2/PPPPP1PP/RNBQ1BNR b kq - 3 3", 
            "c8-d8", "Bishop | DoesNotMoveThisWay")
           
           (195, 
            "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/7N/PPPP1PPP/RNBQKB1R w KQkq e6 0 3", 
            "d1-e2", "Queen")
           
           (196, "rnbqkbnr/1ppppppp/p7/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 0 2", 
            "e1-b1", "King | DoesNotMoveThisWay")
           
           (197, 
            "rnbqkb1r/p1ppp1pp/1p3p1n/8/1P6/N4N2/P1PPPPPP/R1BQKB1R w KQkq - 0 4", 
            "c1-b2", "Bishop")
           
           (198, 
            "rnb1kb2/pp1ppppr/2p4n/3P3p/2P1P2R/P4q1N/1P2QPP1/RN2KB2 b Qq - 6 9", 
            "h7-d8", "Rook | DoesNotMoveThisWay")
           
           (199, 
            "rn1k1b1r/pb1pp1p1/2p2p2/7p/Npq1P3/P2PP3/1PQB2PP/R2K1BNR b - - 3 13", 
            "d8-g8", "King | DoesNotMoveThisWay")
           
           (200, 
            "rnb1k2r/pppp1ppp/3bp2n/8/3P3q/PP6/1BP1PPPP/RN1QKBNR w KQkq - 3 5", 
            "b2-c1", "Bishop")
           
           (201, "2b3k1/6p1/p1r3p1/P2p2B1/n1P1p3/2R5/3p1K2/8 w - - 3 69", 
            "c3-a1", "Rook | DoesNotMoveThisWay")
           
           (202, "rnbqkbnr/pppppppp/8/8/5P2/8/PPPPP1PP/RNBQKBNR b KQkq f3 0 1", 
            "d8-a1", "Queen | Capture | DoesNotMoveThisWay")
           
           (203, "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1", 
            "d8-a5", "Queen | DoesNotJump")
           
           (204, 
            "r2qkbnr/2p1pppp/p1n5/1B1p4/3PPB2/N6b/PPP2PPP/R2QK1R1 w Qkq - 0 9", 
            "d1-b1", "Queen")
           
           (205, "2r1k1nr/Qp1b4/B1p4p/2P2p2/3p1P2/7N/PBK3pP/2R2R2 w k - 2 33", 
            "f1-f2", "Rook")
           
           (206, "6R1/2Pr4/b2k1n2/3B2N1/p2n4/6pp/3R3P/4K3 w - - 1 68", "e1-c1", 
            "King | WQ | HasNoCastling")
           
           (207, "rnbqk1nr/ppppppbp/6p1/8/8/5P1P/P1PPBPPR/RNBQK3 w Qkq - 4 6", 
            "e1-g1", "King | WK | HasNoCastling")
           
           (208, "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1", 
            "e8-a1", "King | Capture | DoesNotMoveThisWay")
           
           (209, 
            "r1b1kbnr/ppqn1p1p/2p1p1p1/2Np4/4PB2/P2P4/1PPQ1PPP/R3KBNR w KQkq - 2 10", 
            "e1-c1", "King | WQ")
           
           (210, "8/8/8/8/8/8/8/4K1k1 w - - 18487 9430", "e1-g1", 
            "King | WK | Capture | DoesNotCaptureThisWay")
           
           (211, "8/8/8/8/8/8/8/2k1K3 w - - 12136 6266", "e1-c1", 
            "King | WQ | Capture | DoesNotCaptureThisWay")
           
           (212, 
            "rnb4r/p2p1k1p/1ppbp1p1/6Q1/1P2P1pn/P1NP1P1N/R3q1BP/2B1K2R w K - 4 16", 
            "e1-g1", "King | WK | CastleFromCheck")
           
           (213, "8/8/k7/5p1P/pP3P2/4q3/8/4K3 w - - 5 75", "e1-c1", 
            "King | WQ | HasNoCastling")
           
           (214, 
            "r1bqkb1r/pppp2pp/n3pn2/5p2/4P1Q1/3B4/PPPP1PPP/RNB1K1NR b KQkq - 3 5", 
            "e8-g8", "King | BK | DoesNotJump")
           
           (215, 
            "rnb1k2r/1p1p2p1/2pbp3/p2n1p2/3P2pP/P1P2N2/1PK1P1B1/RNB1Q2R b kq - 3 16", 
            "e8-g8", "King | BK")
           
           (216, "4k1B1/2r5/1B3pn1/3p1P2/p4b1p/7P/8/4K3 b - - 7 72", "e8-g8", 
            "King | BK | Capture | DoesNotCaptureThisWay")
           
           (217, "r1bqkbnr/pppppppp/2n5/8/8/5P2/PPPPP1PP/RNBQKBNR w KQkq - 1 2", 
            "e2-f3", "Pawn | ToOccupiedCell")
           
           (218, "r1bqkbnr/pppppppp/2n5/8/8/5P2/PPPPP1PP/RNBQKBNR w KQkq - 1 2", 
            "g2-f3", "Pawn | ToOccupiedCell")
           
           (219, "8/8/p2P4/B7/P1B2p2/4k1P1/K4p2/4R3 b - - 1 61", "f2-e1=Q", 
            "Pawn | Promotion | Capture")
           
           (220, 
            "rnbqk1nr/2pp4/4pp2/ppP3p1/7p/5PPN/PPPBP2P/RN1QKB1R w KQkq b6 0 10", 
            "b7-b5", "Capture | EmptyCell")
           
           (221, 
            "3b1rrk/pp3N2/P1pN2p1/P7/4p1b1/B2PPp1P/RQP2P1q/3B1K2 b - - 5 37", 
            "f8-f7", "Rook | Capture | MoveToCheck")
           
           (222, "8/3Prp2/p2N4/1nP2k1R/8/1p5P/7R/2K4B b - - 6 62", "b5-d6", 
            "Knight | Capture | MoveToCheck")
           
           (223, "8/8/5k1P/8/4BK2/8/8/8 b - - 4 123", "f6-g5", 
            "King | MoveToCheck")
           
           (224, "8/8/4k2P/5B2/5K2/8/8/8 b - - 6 124", "e6-f5", 
            "King | Capture | MoveToCheck")
           
           (225, "6n1/8/2k3PR/K4P2/4p3/2p5/4b3/8 b - - 9 101", "c6-b5", 
            "King | MoveToCheck")
           
           (226, "nb6/1b6/2p5/p1PP4/3K1p1Q/1P6/5k2/8 b - - 2 84", "f2-e3", 
            "King | MoveToCheck")
           
           (227, "nb6/1b4Q1/2p5/p1PP4/3K1p2/1P6/3k4/8 b - - 6 86", "d2-d3", 
            "King | MoveToCheck")
           
           (228, "8/8/2P4K/p2p4/5n2/1P3p2/8/3k1bb1 w - - 10 101", "h6-g6", 
            "King | MoveToCheck")
           
           (229, "8/6K1/b1P3n1/p7/3p4/1P3p2/8/3k2b1 w - - 4 104", "g7-f8", 
            "King | MoveToCheck")
           
           (230, 
            "8/rb1k1r1p/1p1q3n/p1pp1p2/P3pQ1P/1P1P1BPN/R1n1P3/2B2K1R w - - 0 37", 
            "f1-e1", "King | MoveToCheck")
           
           (231, "nb6/1b6/2p5/p1PP4/3K1p2/1P4Q1/3k4/8 w - - 5 86", "d4-d3", 
            "King | MoveToCheck")
           
           (232, "8/8/4k1BP/8/5K2/8/8/8 w - - 1 122", "f4-e5", 
            "King | MoveToCheck")
           
           (233, "8/8/5k1P/8/5K2/8/2B5/8 w - - 3 123", "f4-e5", 
            "King | MoveToCheck")
           
           (234, "3R4/2rn2r1/3k1Np1/p1pp2p1/p2P4/2b4p/7P/B2K4 b - - 0 0", 
            "d8-e8", "Rook | WrongSideToMove")
           
           (235, "nb6/1b3k2/2p1p2P/p1KP4/2P5/1PR5/8/8 w - - 1 77", "h6-h7", 
            "Pawn")
           (236, "1K6/1P6/8/8/k7/7b/pq6/8 w - - 14 150", "b8-a7", "King")
           
           (237, "r1b1kbnr/ppp2p2/6p1/n3p2p/8/P4PP1/1PPqPK1P/RNBQ2NR w kq - 2 8", 
            "d2-d3", "Queen | WrongSideToMove")
           
           (238, "7R/4n3/2k3P1/5P2/1K2p3/2p2b2/8/8 w - - 0 97", "f3-g4", 
            "Bishop | WrongSideToMove")
           
           (239, 
            "1r1q1rnk/pb1pp3/npp4b/B2P1p2/P1PKN1P1/4PNPB/1P3P2/R3R3 w - - 7 24", 
            "f5-f4", "Pawn | WrongSideToMove")
           
           (240, "3B4/8/1K6/2R1r3/p5k1/8/5q2/8 w - - 8 139", "g2-g3", 
            "EmptyCell")
           
           (241, "3k4/4n3/1p4pr/5B2/7n/3qR1P1/2P1P2P/R4K2 w - - 15 51", "d2-d3", 
            "Capture | EmptyCell")
           
           (242, "4k3/8/8/2p5/2P4P/p5K1/2N2B2/4q3 w - - 16 134", "h1-h2", 
            "EmptyCell")
           
           (243, "Rnb2kr1/3pnq2/1p3pp1/1N4Np/4P3/8/1bPKPPPP/2BQ1BR1 b - - 0 19", 
            "a1-a2", "EmptyCell")
           
           (244, "8/2n5/2P5/p1bp4/6K1/1P3p2/4b3/3k4 b - - 1 96", "b3-b4", 
            "Pawn | WrongSideToMove")
           
           (245, 
            "rn2k1nQ/pb1p1p2/5q2/1p2p3/1b1pP3/1P3P2/P1PNB1PP/R1BK2NR b q - 0 13", 
            "a1-b1", "Rook | WrongSideToMove")
           
           (246, 
            "3r1B1r/1bppn2p/1b1k2p1/pP2p3/4P2P/2RP1P1P/2P3B1/4K1nR b - - 3 27", 
            "e1-f1", "King | WrongSideToMove")
           
           (247, "1B3q2/5p2/3n4/p3k2P/P1P4P/PK3P2/6R1/8 b - - 6 68", "g2-g3", 
            "Rook | WrongSideToMove")
           
           (248, "2Q5/5k2/8/1P3P2/3B3P/8/4R1K1/6q1 w - - 2 88", "d4-g1", 
            "Bishop | Capture")
           
           (249, "r1b1k1n1/ppp1bp2/6pr/n3p2P/1qP4P/P4P2/4P3/RNBQK1NR w q - 1 13", 
            "b1-c3", "Knight")
           
           (250, "Rnb2kr1/3pn1q1/1p1N1pp1/6Np/4P3/b7/2PKPPPP/2BQ1BR1 w - - 3 21", 
            "d6-c8", "Knight | Capture")
           
           (251, "3r4/2P5/3r3k/3n1PN1/Pp1b1n2/4p1P1/R7/1N4K1 w - - 2 65", 
            "g5-h3", "Knight")
           
           (252, 
            "r1b1kbnr/ppp2p2/2n3p1/4p2p/8/P4PP1/1PPqP2P/RNBQK1NR w KQkq - 0 7", 
            "b1-d2", "Knight | Capture")
           
           (253, "4qk2/4n1r1/1p2b1p1/3p4/7N/5nPB/1RP1P2P/4K1R1 w - - 0 38", 
            "h4-f3", "Knight | Capture")
           (254, "8/3k4/1q4BP/8/8/8/5K2/4R3 w - - 19 117", "e1-e3", "Rook")
           
           (255, "3k4/8/q3R2n/1p4pB/4P2P/2P5/7P/1rR4K w - - 1 61", "c1-d1", 
            "Rook")
           
           (256, "3kr3/npp2p2/1P6/3rpbpp/p1PRP2P/2N2N2/5PQ1/3KBBR1 w - - 5 38", 
            "d4-d3", "Rook")
           
           (257, "8/3N2r1/4k3/p1p3R1/p2r4/6pp/1b1B3P/3K4 w - - 0 68", "d7-c5", 
            "Knight | Capture")
           
           (258, "1n3R2/2N3k1/p7/r2p3q/5p2/4K3/8/4N3 w - - 0 85", "f8-f4", 
            "Rook | Capture")
           
           (259, "1n6/1pk2r2/3R1pP1/1pP2b1p/3NP3/1Q5B/3K1P2/6R1 w - - 1 62", 
            "d6-c6", "Rook")
           
           (260, 
            "r2q3r/1bpkp2p/np1p3n/p4p2/P4p2/N1Q3PP/1P1PP1K1/R1B2BNR w - - 1 18", 
            "c3-f3", "Queen")
           
           (261, 
            "rn3b1r/p2pk2p/b3p1pn/1pp5/1P2Pp1P/2PP3R/PB2K1P1/RN1QqBN1 w - - 2 16", 
            "d1-e1", "Queen | Capture")
           
           (262, "3K4/1P6/8/8/3Q4/k4b2/p1q5/8 w - - 5 141", "d4-d3", 
            "Queen")
           (263, "3K4/1P6/8/8/3Q4/k4b2/p1q5/8 w - - 5 141", "d4-c4", "Queen")
           
           (264, 
            "2k5/rbp2r1p/np1p1q1n/p4p2/P3pQ1P/N5P1/1P1PP1B1/R1B1K1NR w - - 1 27", 
            "f4-e4", "Queen | Capture")
           
           (265, "k7/3n3Q/6p1/1P2P3/2pRN1Bp/p7/P1K2r2/R6N w - - 0 57", "e4-f2", 
            "Knight | Capture")
           
           (266, 
            "rn1q3r/2p1pk1p/3B1npb/1p1pQ3/1p1PP2P/2P3pP/P4P2/RN2KBR1 w Q - 6 15", 
            "e5-f6", "Queen | Capture")
           
           (267, 
            "rn1q3r/2p1pk1p/3B1npb/1p1pQ3/1p1PP2P/2P3pP/P4P2/RN2KBR1 w Q - 6 15", 
            "e5-g3", "Queen | Capture")
           
           (268, 
            "2k5/rb3r1p/1p1Q3n/p1pp1p2/P3p2P/1P1P1BPN/R1n1P3/2B2K1R w - - 1 38", 
            "d6-c5", "Queen | Capture")
           
           (269, "B4Q2/3r4/4kP2/ppp5/2P2K2/8/8/3q4 w - - 1 95", "f8-c5", 
            "Queen | Capture")
           
           (270, 
            "rnb2kn1/2pp4/pp2pp2/3q4/P2BPb2/2NP2Pp/1PPRNP1P/2K2B1R b - - 2 21", 
            "f4-g5", "Bishop")
           
           (271, 
            "rnb1kQn1/2pp4/pp1bpp2/q7/4P3/P1NPB1Pp/1PP1NP1P/2KR1B1R b q - 1 18", 
            "d6-f8", "Bishop | Capture")
           
           (272, "3Q1R2/8/k7/p7/P2P1p2/8/1N2B2K/5b2 b - - 2 88", "f1-e2", 
            "Bishop | Capture")
           
           (273, "8/5Rb1/p6n/P4k2/R6p/2p5/3p1K2/8 b - - 1 99", "h6-f7", 
            "Knight | Capture")
           
           (274, "2k2r2/7P/5Q2/1PQ5/p7/3n4/6K1/6N1 b - - 2 78", "d3-c5", 
            "Knight | Capture")
           
           (275, "4r3/5P2/6p1/4kR2/2pn3p/7K/8/6N1 b - - 3 86", "d4-f5", 
            "Knight | Capture")
           
           (276, "8/1P6/8/1N4P1/b4P2/P6N/1KR1n1k1/R5B1 b - - 2 56", "e2-f4", 
            "Knight | Capture | MoveToCheck")
           
           (277, "3k4/np2r3/1pNr1pP1/2P4p/3RP3/6Qb/p1K1NPB1/4B1R1 b - - 1 52", 
            "a7-c6", "Knight | Capture")
           
           (278, "1B6/3k1p2/pp4p1/nPpr2P1/4p2P/b2R3b/4P2R/3N2K1 b - - 3 41", 
            "d5-d6", "Rook")
           
           (279, "3k2n1/8/qp2R3/6p1/3r2BP/2P5/4P2P/2R3K1 b - - 6 57", "d4-e4", 
            "Rook")
           
           (280, 
            "2k5/rbQ1r2p/1p5n/p1pp1p2/P3p2P/1P1P1BPN/R1n1P3/2B2KR1 b - - 4 39", 
            "e7-c7", "Rook | Capture")
           
           (281, "2k5/6R1/8/7P/2R5/8/5NK1/2q5 b - - 5 123", "c1-c4", 
            "Queen | Capture")
           
           (282, 
            "3q1r2/rbpk3p/np1p3n/p3pp2/P4p1P/N2Q2P1/1P1PPKB1/R1B3NR b - - 3 23", 
            "d8-e8", "Queen")
           
           (283, "1r6/r1k2ppp/R1PqN2b/7n/3P1p1P/2R3P1/3N2K1/8 b - - 0 44", 
            "d6-e6", "Queen | Capture")
           
           (284, 
            "r1bqkbnr/pppB1p2/2n3p1/4p2p/8/5PP1/PPPPP2P/RNBQK1NR b KQkq - 0 5", 
            "d8-d7", "Queen | Capture")
           
           (285, "nr2r1n1/pb1p3k/1pp5/8/PbP1PpP1/1P1K2Pq/3R1P2/R3B3 b - - 1 36", 
            "h3-g3", "Queen | Capture")
           (286, "8/7n/5k1B/3P4/1K6/p7/5q2/8 b - - 8 121", "f2-g3", "Queen")
           
           (288, 
            "1nb5/r2p1p2/3k1q2/1p2Q2B/p2pPP2/1Pb4P/P1P3P1/RNBK2NR b - - 0 20", 
            "f6-e5", "Queen | Capture")
           
           (289, "8/8/k2B3n/p2Pp1p1/2b3PP/1R2p3/RK5b/n7 b - - 3 70", "a1-b3", 
            "Knight | Capture")
           
           (290, "4R3/4k2B/8/8/7P/2p5/7K/8 b - - 26 93", "e7-e8", 
            "King | Capture")
           
           (291, "8/3R4/4kn2/1p6/4P1pP/2PB4/4R2P/7K b - - 5 68", "e6-d7", 
            "King | Capture") |]
    
    [<Fact>]
    let checkExtended() = 
        let mutable errCounter = 0
        for case in extendedSuite do
            match case with
            | (n, fen, move, expected) -> 
                let actual = 
                    fen
                    |> ParseFen
                    |> unwrap
                    |> ValidateMove(_cn move)
                    |> MoveToString
                if expected <> actual then 
                    errCounter <- errCounter + 1
                    printfn "%A: %A -------> %A" n expected actual
        if errCounter > 0 then 
            failwithf "there were %A failing tests" errCounter
