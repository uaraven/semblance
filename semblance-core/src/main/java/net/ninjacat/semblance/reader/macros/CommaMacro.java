package net.ninjacat.semblance.reader.macros;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.ReaderMacro;
import net.ninjacat.semblance.utils.Values;

/**
 * Reader macro for , (comma)
 */
public class CommaMacro implements ReaderMacro {
    @Override
    public String getMacroCharacter() {
        return ",";
    }

    @Override
    public LispValue replaceReaderMacro(final LispValue value) {

        return Values.list(Values.symbol("#--eval-me--#"), value);
    }
}
