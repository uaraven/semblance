package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

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
        return SymbolAtom.fromBoolean(
                parameters.tail().stream().noneMatch(createComparatorFor(first))
        );
    }
}
