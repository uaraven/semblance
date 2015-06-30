package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.collections.LispValue;

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

    /**
     * @return Macro character handled by this ReaderMacro instance
     */
    String getMacroCharacter();

    /**
     * Returns a replacement value for a reader macro
     *
     * @param value reader macro
     * @return replacement
     */
    LispValue replaceReaderMacro(LispValue value);
}
