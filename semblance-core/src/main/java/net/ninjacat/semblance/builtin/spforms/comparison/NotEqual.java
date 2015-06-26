package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
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
    public LispValue doApply(final LispCollection parameters) {
        final LispValue first = parameters.head();

        final Predicate<LispValue> comparator = Predicates.not(createComparatorFor(first));

        return SymbolAtom.fromBoolean(
                Iter.of(parameters.tail().iterator()).all(comparator)
        );
    }
}
