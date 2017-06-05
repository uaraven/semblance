package net.ninjacat.semblance.reader.macros;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.reader.ReaderMacro;
import net.ninjacat.semblance.utils.Values;

/**
 * Reader macro for @ (unwrap list)
 */
public class AtMacro implements ReaderMacro {
    @Override
    public String getMacroCharacter() {
        return "@";
    }

    @Override
    public LispValue replaceReaderMacro(final LispValue value) {

        return Values.list(Values.symbol("#--unwrap-me--#"), value);
    }
}
