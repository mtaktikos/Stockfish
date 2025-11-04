# UCI2WB and WinBoard Compatibility for CaptureAnything Variant

## Overview

The `captureanything` chess variant has been successfully implemented in Multi-Variant Stockfish and is properly exposed as a UCI option. This document explains how to use it with UCI2WB and WinBoard.

## UCI Option Implementation

The `captureanything` variant is registered in the `UCI_Variant` combo option, which can be seen when the engine responds to the `uci` command:

```
option name UCI_Variant type combo default chess var chess var antichess ... var captureanything ... var twokingssymmetric
```

## Using with UCI2WB / WinBoard

### Method 1: UCI_Variant Option (Recommended)

Since `captureanything` is exposed as a UCI option, it can be selected like any other variant:

```
uci
setoption name UCI_Variant value captureanything
isready
position startpos
go depth 10
```

### Method 2: WinBoard Configuration

When using UCI2WB as a UCI-to-WinBoard adapter:

1. **Custom Variant Support**: `captureanything` is a custom variant not in WinBoard's standard variant list. UCI2WB will pass the variant selection through the UCI_Variant option.

2. **Manual Setup**: In WinBoard, you may need to:
   - Configure the engine to use UCI protocol
   - Set the variant to "normal" or "fairy" in WinBoard
   - Use UCI2WB to translate commands
   - The engine will internally use the captureanything rules when UCI_Variant is set

### Verification

To verify the variant is working:

```bash
echo -e "uci\nsetoption name UCI_Variant value captureanything\nposition startpos\nisready\nquit" | ./stockfish
```

Look for the output:
```
info string variant captureanything startpos rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1
readyok
```

Note: The starting FEN has no castling rights (`w - -`) which is correct for this variant.

## Variant Rules

In the captureanything variant:
- Pieces can capture friendly pieces (except kings)
- Kings cannot be captured, even by friendly pieces
- No castling rights (starting FEN: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1`)
- Self-captures are evaluated negatively in Static Exchange Evaluation (SEE)

## Technical Implementation

The variant is implemented through:
- `CAPTUREANYTHING_VARIANT` enum in `src/types.h`
- `"captureanything"` string in the variants vector
- Modified move generation in `src/movegen.cpp`
- Starting FEN without castling rights in `src/uci.cpp`
- Compilation flag `-DCAPTUREANYTHING` in the Makefile

## Known Limitations

1. **UCI2WB Variant Mapping**: UCI2WB may not have a built-in mapping for "captureanything" to a WinBoard variant. In this case, it will be treated as a custom variant.

2. **WinBoard Display**: WinBoard may display the game as normal chess visually, but the engine will correctly enforce captureanything rules.

3. **Third-party GUIs**: Not all chess GUIs support custom variants. Ensure your GUI supports the UCI protocol and allows setting UCI options.

## Troubleshooting

If the variant is not recognized:

1. **Verify UCI Output**: Check that `captureanything` appears in the UCI_Variant option list
2. **Check Compilation**: Ensure the engine was compiled with `-DCAPTUREANYTHING` flag
3. **Test Directly**: Test the engine directly via command line before using with GUI
4. **GUI Support**: Verify your GUI/adapter supports UCI_Variant option setting

## Testing

To verify the variant is working correctly, run these commands:

```bash
cd src
echo -e "uci\nquit" | ./stockfish | grep "captureanything"
```

You should see `var captureanything` in the UCI_Variant option list.

To test functionality:

```bash
cd src
echo -e "uci\nsetoption name UCI_Variant value captureanything\nposition startpos\ngo depth 1\nquit" | ./stockfish
```

You should see:
- `info string variant captureanything startpos ...`
- `bestmove` followed by a legal move

This confirms:
- ✓ Variant is listed in UCI options
- ✓ Variant selection is acknowledged
- ✓ Engine can play in the variant
- ✓ Correct starting position (no castling rights)

## Additional Resources

For further information about this variant implementation, see:
- `IMPLEMENTATION_SUMMARY.md` - Detailed implementation notes
- `CAPTUREANYTHING_TESTS.md` - Comprehensive test results
- `VARIANT_UCI_STATUS.md` - UCI implementation verification

External references:
- UCI Protocol: http://wbec-ridderkerk.nl/html/UCIProtocol.html
- WinBoard/XBoard: https://www.gnu.org/software/xboard/
