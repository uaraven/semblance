package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.asCallable;
import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * Call function
 */
@CreatesContext
public class Funcall extends SpecialForm {

    private static final long serialVersionUID = 5690616970134102911L;

    /**
     * Create new instance.
     */
    public Funcall() {
        super("funcall", "function", "paramerters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCallable callable = asCallable(context.evaluate(parameters.head()));
        final LispCollection params = context.evaluateList(asCollection(parameters.tail().head()));

        return callable.apply(LocalContext.namedChildContext(callable.name(), context), params);
    }
}
