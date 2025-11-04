# CaptureAnything Variant Implementation Summary

## Overview
This implementation adds the **captureanything** chess variant to Multi-Variant Stockfish. In this variant, pieces can capture their own pieces (except kings cannot be captured).

## Reference Implementation
Based on the captureanything branch from fairy-stockfish repository by mtaktikos:
https://github.com/mtaktikos/Fairy-Stockfish/tree/captureanything

## Files Modified

### 1. src/types.h
- Added `CAPTUREANYTHING_VARIANT` enum value
- Added `"captureanything"` string to variants array

### 2. src/movegen.cpp
- Added self-capture logic in `generate_pawn_moves()` to include friendly pieces (except kings) as capture targets
- Modified `generate_all()` to add friendly pieces to the capture target bitboard
- Added special handling for king moves in EVASIONS to allow self-captures
- Added CAPTUREANYTHING_VARIANT case in the variant switch statement

### 3. src/position.cpp  
- Modified `pseudo_legal()` to allow self-captures for CAPTUREANYTHING variant
- Added check to prevent capturing friendly kings
- Updated pawn capture validation to include friendly pieces
- Modified `see_ge()` to evaluate self-captures with negative value

### 4. src/uci.cpp
- Added starting FEN for captureanything: `"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"`
  (no castling rights, following the reference implementation)

### 5. src/Makefile
- Added `-DCAPTUREANYTHING` to CXXFLAGS

## Implementation Details

### Self-Capture Logic
Pieces can capture friendly pieces as if they were opponent pieces, with two key restrictions:

1. **Kings are immune**: No piece can capture a friendly king
2. **Negative evaluation**: Self-captures are evaluated as negative in SEE (Static Exchange Evaluation)

### Move Generation
The implementation modifies the target bitboard during move generation:
- For CAPTURES and NON_EVASIONS: `target |= pos.pieces(Us) & ~pos.pieces(Us, KING)`
- For EVASIONS: Special handling to also allow friendly captures when escaping check

### Starting Position
- No castling rights (consistent with fairy-stockfish implementation)
- Standard piece placement

## Testing

All tests pass successfully:
- ✓ Variant recognition in UCI
- ✓ Correct move generation (39 moves from starting position vs 20 in regular chess)
- ✓ Self-capture functionality (pawns, knights, bishops, rooks, queens can capture friendly pieces)
- ✓ King protection (kings cannot be captured)
- ✓ Engine gameplay (can analyze and return best moves)

See `CAPTUREANYTHING_TESTS.md` for detailed test results.

## Usage Example

```bash
./stockfish
uci
setoption name UCI_Variant value captureanything
position startpos
go depth 10
```

## Compatibility

The variant integrates seamlessly with the existing Multi-Variant Stockfish architecture:
- Uses the same template-based move generation system
- Follows the same variant pattern as other variants (ANTI, ATOMIC, etc.)
- Fully compatible with UCI protocol
- Works with the NNUE evaluation

## Performance

From starting position:
- Perft 1: 39 nodes (vs 20 in standard chess)
- Perft 2: 1,519 nodes (vs 400 in standard chess)

The increase in nodes is due to additional self-capture moves being legal.

## Notes

- The implementation is minimal and surgical, changing only what's necessary
- No existing functionality is broken
- The variant can be played alongside all other supported variants
- SEE properly handles the negative value of self-captures for move ordering
