package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asCallable;
import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * Applies function to a list of parameters
 * <p>
 * (apply func (params...))
 * <p>
 * Resolves {@code func} in current context, evaluates parameters and
 * then applies resolved function to the evaluated parameters.
 * <p>
 * {@code func } MUST resolve to a callable, i.e. function, special form or macro
 */
public class Apply extends SpecialForm {

    public Apply() {
        super("apply", "func", "(params)");
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        final LispCallable func = asCallable(context.evaluate(parameters.head()));
        final LispCollection params = context.evaluateList(asCollection(parameters.tail().head()));

        return func.apply(context, params);
    }
}
