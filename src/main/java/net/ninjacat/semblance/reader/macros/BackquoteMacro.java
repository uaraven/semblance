package net.ninjacat.semblance.reader.macros;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.ReaderMacro;
import net.ninjacat.semblance.utils.Values;

/**
 * Back quote reader macro
 */
public class BackquoteMacro implements ReaderMacro {
    @Override
    public String getMacroCharacter() {
        return "`";
    }

    @Override
    public LispValue replaceReaderMacro(final LispValue value) {

        return Values.list(Values.symbol("backquote"), value);
    }

}
