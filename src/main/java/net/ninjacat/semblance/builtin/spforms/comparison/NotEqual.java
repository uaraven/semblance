package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.functions.Predicates;
import net.ninjacat.smooth.iterators.Iter;

/**
 * != evaluator.
 */
public class NotEqual extends BaseEqual {

    /**
     * Creates a new instance.
     */
    public NotEqual() {
        super("!=", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispValue first = context.evaluate(parameters.head());

        final Predicate<LispValue> comparator = Predicates.not(createComparatorFor(first));

        return SymbolAtom.fromBoolean(
                Iter.of(parameters.tail().iterator()).all(comparator)
        );
    }
}
