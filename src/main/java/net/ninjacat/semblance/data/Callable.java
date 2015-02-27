package net.ninjacat.semblance.data;

import net.ninjacat.semblance.interpreter.Context;

/**
 * Base interface for all callables: functions, special forms, macros
 *
 * Created on 24/02/15.
 */
public interface Callable extends LispValue {
    SymbolAtom name();

    LispValue apply(Context context, LispCollection parameters);
}
