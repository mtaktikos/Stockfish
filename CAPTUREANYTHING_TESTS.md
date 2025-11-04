# CaptureAnything Variant Test Results

## Overview
The captureanything variant has been successfully implemented in Multi-Variant Stockfish. This variant allows pieces to capture their own pieces (with the exception that kings cannot be captured, even by friendly pieces).

## Implementation Details

### Key Changes Made:
1. **types.h**: Added CAPTUREANYTHING_VARIANT enum and "captureanything" string
2. **movegen.cpp**: Modified move generation to include friendly pieces as capture targets (except kings)
3. **position.cpp**: Updated pseudo_legal() to allow self-captures and modified SEE to handle negative values for self-captures
4. **uci.cpp**: Added starting FEN for captureanything variant (no castling rights)
5. **Makefile**: Added -DCAPTUREANYTHING compilation flag

### Features:
- Pieces can capture friendly pieces diagonally/adjacently as normal captures
- Kings are immune from capture (even from friendly pieces)
- Self-captures are evaluated negatively in SEE (Static Exchange Evaluation)
- Starting position has no castling rights per the variant rules

## Test Results

### Test 1: Variant Recognition
✓ PASS - The variant is correctly registered in UCI options

### Test 2: Move Generation from Starting Position
✓ PASS - Starting position generates 39 moves (vs 20 in standard chess)
- Extra 19 moves are self-captures of pawns by pieces

### Test 3: Pawn Self-Capture
✓ PASS - Pawns can capture friendly pieces diagonally
- Example: In position `4k3/8/8/8/8/1N1N4/2P5/4K3 w - - 0 1`
- Pawn on c2 can capture friendly knights on b3 (c2xb3) and d3 (c2xd3)

### Test 4: King Protection
✓ PASS - Kings cannot be captured even by friendly pieces
- In position `4k3/8/8/8/8/8/8/3KK3 w - - 0 1`
- King on d1 cannot move to e1 (friendly king on e1)

### Test 5: Engine Functionality
✓ PASS - Engine can analyze positions and return best moves
- Engine successfully evaluates positions and plays moves

## Sample Perft Results

### Starting Position
```
Position: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1
Perft 1: 39 nodes
Perft 2: 1519 nodes
```

### Position with Self-Capture Possibilities
```
Position: 4k3/8/8/8/8/1N1N4/2P5/4K3 w - - 0 1
Perft 1: 22 nodes
Notable moves:
- c2c3, c2c4 (pawn advances)
- c2b3, c2d3 (pawn captures friendly knights!)
- b3a5, b3c5, etc. (knight moves)
```

## Compatibility

The variant integrates seamlessly with the Multi-Variant Stockfish architecture and follows the same patterns as other variants like atomic, antichess, etc.

## Usage

To play with the captureanything variant:
```
uci
setoption name UCI_Variant value captureanything
position startpos
go depth 10
```

Or with a custom position:
```
uci
setoption name UCI_Variant value captureanything
position fen 4k3/8/8/8/8/1N1N4/2P5/4K3 w - - 0 1
go depth 10
```
