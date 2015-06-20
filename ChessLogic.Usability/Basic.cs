using System;
using ChessKit.ChessLogic;
using Xunit;

namespace ChessLogic.Usability
{
    public class Basic
    {
        [Fact]
        public void MakeMoveThenDump()
        {
            // Fool's Mate
            var position = Board.StartingPosition
                .MakeMove("f3")
                .MakeMove("e5")
                .MakeMove("g4")
                .MakeMove("Qh4#");
            Console.WriteLine(position.Dump());
            Console.WriteLine(position.Properties);
        }

        [Fact]
        public void FenAndValidateLegalMove()
        {
            var move = Move.Parse("b7-b8=Q");
            var legalMove = Fen
                .Parse("8/1P6/8/8/8/8/8/8 w - - 0 1")
                .ValidateLegalMove(move);
            var nextPosition = legalMove.ToPosition();
            Console.WriteLine(Fen.Print(nextPosition));
        }

    }
}
