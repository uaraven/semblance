package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.LispValue;

/**
 * <p>
 * Handles special cases during stream read.
 * </p>
 * <p>
 * Special cases are triggered by certain characters in the input stream. Most recognizable example would be
 * replacement of '(1 2 3) with (quote (1 2 3))
 * </p>
 * Created on 25/02/15.
 */
public interface ReaderMacro {

    String getMacroCharacter();

    LispValue replaceReaderMacro(LispValue value);
}
