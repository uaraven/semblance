package net.ninjacat.semblance.builtin.spforms.logic;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;

import static net.ninjacat.semblance.utils.Values.isFalse;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class And extends SpecialForm {
    public And() {
        super("and", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return SymbolAtom.fromBoolean(Iter.of(parameters.iterator()).any(new Predicate<LispValue>() {
            @Override
            public boolean matches(final LispValue value) {
                return isFalse(value);
            }
        }));
    }
}
