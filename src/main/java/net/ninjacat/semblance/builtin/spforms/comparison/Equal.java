package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;

/**
 * Equals evaluator
 */
public class Equal extends BaseEqual {

    /**
     * Creates a new instance.
     */
    public Equal() {
        super("=", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispValue first = context.evaluate(parameters.head());

        final Predicate<LispValue> comparator = createComparatorFor(first);

        return SymbolAtom.fromBoolean(
                Iter.of(parameters.tail().iterator()).all(comparator)
        );
    }

}
