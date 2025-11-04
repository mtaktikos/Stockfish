# Recycle Variant Implementation

## Overview
The **recycle** variant has been successfully implemented in Multi-Variant Stockfish. This variant combines two powerful mechanics:
1. **Crazyhouse drops**: Captured pieces can be dropped back onto the board
2. **Self-capture**: Pieces can capture their own pieces (except kings cannot be captured)

## Implementation Details

### Files Modified

1. **src/types.h**
   - Added `RECYCLE_VARIANT` enum value
   - Added `"recycle"` string to variants array

2. **src/Makefile**
   - Added `-DRECYCLE` to CXXFLAGS for compilation

3. **src/movegen.cpp**
   - Added self-capture logic in `generate_pawn_moves()` to include friendly pieces (except kings) as capture targets
   - Added self-capture logic in `generate_all()` to add friendly pieces to the capture target bitboard
   - Added special handling for king moves in EVASIONS to allow self-captures
   - Added crazyhouse-style drop move generation for pieces in hand
   - Added RECYCLE_VARIANT case in the variant switch statement

4. **src/position.cpp**
   - Modified `pseudo_legal()` to allow self-captures for RECYCLE_VARIANT
   - Added check to prevent capturing friendly kings
   - Updated pawn capture validation to include friendly pieces
   - Modified `see_ge()` to evaluate self-captures with negative value

5. **src/position.h**
   - Updated `is_house()` to return true for RECYCLE_VARIANT, enabling all crazyhouse drop mechanics

6. **src/uci.cpp**
   - Added starting FEN for recycle: `"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w - - 0 1"`
   - Includes hand notation `[]` and no castling rights (consistent with captureanything)

## Features

### Self-Capture Mechanics
- Pieces can capture friendly pieces as if they were opponent pieces
- **Kings are immune**: No piece can capture a friendly king
- **Negative evaluation**: Self-captures are evaluated as negative in SEE (Static Exchange Evaluation)

### Crazyhouse Drop Mechanics
- Captured pieces go into the capturing player's hand
- Pieces in hand can be dropped on any empty square
- Pawns cannot be dropped on the first or eighth rank
- Drops are notated with `@` symbol (e.g., `N@e4` means drop knight on e4)

### Starting Position
- No castling rights (consistent with captureanything variant)
- Empty hand at start `[]`
- Standard piece placement

## Test Results

### Test 1: Variant Recognition
✓ PASS - The variant is correctly registered in UCI options as "recycle"

### Test 2: Starting Position
✓ PASS - Starting position uses correct FEN with hand notation
- FEN: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w - - 0 1`

### Test 3: Self-Capture Functionality
✓ PASS - Pieces can capture friendly pieces (except kings)
- Example: In position `4k3/8/8/8/8/1N1N4/2P5/4K3[] w - - 0 1`
- Pawn on c2 can capture friendly knights on b3 (c2b3) and d3 (c2d3)

### Test 4: Drop Functionality
✓ PASS - Pieces in hand can be dropped on empty squares
- Example: In position `4k3/8/8/8/8/8/8/4K3[N] w - - 0 1`
- Knight can be dropped on any empty square (N@a1, N@b1, etc.)

### Test 5: Combined Mechanics
✓ PASS - Both self-capture and drops work together
- From starting position: 39 moves (20 normal moves + 19 self-captures)
- Engine can analyze positions with pieces in hand and self-capture opportunities

## Usage Examples

### Basic Setup
```
uci
setoption name UCI_Variant value recycle
position startpos
go depth 10
```

### Custom Position with Pieces in Hand
```
uci
setoption name UCI_Variant value recycle
position fen 4k3/8/8/8/8/8/8/4K3[NQR] w - - 0 1
go depth 10
```

### Playing Self-Capture Moves
```
uci
setoption name UCI_Variant value recycle
position fen 4k3/8/8/8/8/1N1N4/2P5/4K3[] w - - 0 1
go movetime 1000
```

## Perft Results

### Starting Position
```
Position: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w - - 0 1
Perft 1: 39 nodes (20 normal + 19 self-captures)
```

### Position with Self-Capture Opportunities
```
Position: 4k3/8/8/8/8/1N1N4/2P5/4K3[] w - - 0 1
Perft 1: 22 nodes
Notable moves:
- c2c3, c2c4 (pawn advances)
- c2b3, c2d3 (pawn captures friendly knights!)
- b3a5, b3c5, etc. (knight moves)
```

### Position with Drops
```
Position: 4k3/8/8/8/8/8/8/4K3[N] w - - 0 1
Perft 1: 67 nodes
Notable moves:
- e1d1, e1f1, e1d2, e1f2 (king moves)
- N@a1, N@b1, ..., N@h8 (knight drops on all empty squares except e1, e8)
```

## Compatibility

The variant integrates seamlessly with the existing Multi-Variant Stockfish architecture:
- Uses the same template-based move generation system
- Follows the same variant pattern as other variants (ANTI, ATOMIC, CRAZYHOUSE, CAPTUREANYTHING, etc.)
- Fully compatible with UCI protocol
- Works with the NNUE evaluation
- Inherits all crazyhouse infrastructure through `is_house()` function

## Technical Notes

### Why No Castling Rights?
The recycle variant disables castling rights (like captureanything) because:
1. Self-capture mechanics would complicate castling rules
2. Consistency with the base captureanything implementation
3. Simplified game rules for this experimental variant

### SEE Evaluation
Self-captures are evaluated with negative values in Static Exchange Evaluation (SEE) to properly reflect that capturing your own piece loses material rather than gains it.

### Drop Restrictions
Following standard crazyhouse rules:
- Pawns cannot be dropped on ranks 1 or 8
- Drops can only occur on empty squares
- Drops are legal in all phases of the game

## Future Enhancements

Potential improvements that could be considered:
- Custom evaluation weights for positions with many pieces in hand
- Opening book for recycle variant
- Endgame tablebases for recycle positions
- Analysis of optimal self-capture/drop strategies

## Credits

Based on:
- Captureanything variant implementation (self-capture mechanics)
- Crazyhouse variant implementation (drop mechanics)
- Multi-Variant Stockfish architecture by D. Dugovic, F. Fichter et al.
