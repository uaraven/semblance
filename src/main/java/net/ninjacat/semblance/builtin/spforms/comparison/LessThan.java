package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.semblance.utils.Values;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class LessThan extends SpecialForm {

    /**
     * Creates a new instance.
     */
    public LessThan() {
        super(">", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namelessChildContext(context);

        Require.that(parameters).hasAtLeast(2);

        final LispValue value = localContext.evaluate(parameters.head());

        for (final LispValue current : parameters.tail()) {
            final NumberAtom prevous = Values.asNumber(value);
            final NumberAtom currentNumber = Values.asNumber(current);
            //noinspection unchecked
            if (!prevous.lt(currentNumber)) {
                return SymbolAtom.FALSE;
            }
        }
        return SymbolAtom.TRUE;
    }
}
