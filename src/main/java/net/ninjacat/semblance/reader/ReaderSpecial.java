package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.errors.TermExpectedException;

import java.util.Collection;

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
public interface ReaderSpecial {

    char getSpecialTrigger();

    Collection<Token> replaceTokenStream(String trigger, ReaderStream reader) throws TermExpectedException;
}
