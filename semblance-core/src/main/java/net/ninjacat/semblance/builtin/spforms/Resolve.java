package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.evaluator.Context;

import java.util.Optional;

import static net.ninjacat.semblance.utils.Values.asSymbol;
import static net.ninjacat.semblance.utils.Values.isSymbol;

/**
 * Resolves a symbol to its value or NIL if symbol is not bound.
 *
 * All parameters but first one are ignored
 *
 * Any self-evaluating values will be resolved to themselves
 */
public class Resolve extends SpecialForm {
    public Resolve() {
        super("resolve", "s");
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        final LispValue value = parameters.head();
        if (isSymbol(value)) {
            final Optional<LispValue> resolved = context.findSymbol(asSymbol(value));
            return resolved.orElse(NilCollection.INSTANCE);
        } else {
            return context.evaluate(value);
        }
    }
}
