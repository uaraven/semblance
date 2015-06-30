package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Base interface for all kinds of callable: functions, special forms, macros
 */
public interface Callable extends LispValue {
    /**
     * Callable name.
     *
     * @return name symbol.
     */
    SymbolAtom name();

    /**
     * Applies callable.
     *
     * @param context    Context in which this callable is applied.
     * @param parameters Parameter list.
     * @return Evaluated value.
     */
    LispValue apply(Context context, LispCollection parameters);
}
