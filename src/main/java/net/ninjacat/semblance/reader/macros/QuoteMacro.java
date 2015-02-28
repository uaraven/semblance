package net.ninjacat.semblance.reader.macros;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.reader.ReaderMacro;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class QuoteMacro implements ReaderMacro {
    @Override
    public String getMacroCharacter() {
        return "'";
    }

    @Override
    public LispValue replaceReaderMacro(LispValue value) {

        return Values.list(Values.symbol("quote"), value);
    }
}
