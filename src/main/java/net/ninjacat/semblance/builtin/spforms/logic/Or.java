package net.ninjacat.semblance.builtin.spforms.logic;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;

import static net.ninjacat.semblance.utils.Values.isFalse;

/**
 * OR special form
 */
public class Or extends SpecialForm {
    /**
     * Creates a new instance of OR
     */
    public Or() {
        super("or", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return SymbolAtom.fromBoolean(Iter.of(parameters.iterator()).any(new Predicate<LispValue>() {
            @Override
            public boolean matches(final LispValue value) {
                return !isFalse(value);
            }
        }));
    }
}
