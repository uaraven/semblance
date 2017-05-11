package net.ninjacat.semblance.builtin.spforms.logic;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

/**
 * Logical and special form
 */
@SuppressWarnings("ClassNamingConvention")
public class And extends SpecialForm {
    /**
     * Creates a new instance of and special form
     */
    public And() {
        super("and", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return SymbolAtom.fromBoolean(parameters.stream().anyMatch(Values::isFalse));
    }
}
