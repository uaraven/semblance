package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.utils.Values;

/**
 * Loop special form
 */
@SuppressWarnings("ClassNamingConvention")
public class Loop extends SpecialForm {

    /**
     * Creates a new instance.
     */
    public Loop() {
        super("loop", "condition", "&rest", "expressions");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namedChildContext(Constants.LOOP, context);
        final LispValue condition = parameters.head();
        final LispCollection expressions = parameters.tail();
        LispValue result = NilCollection.INSTANCE;
        while (Values.isTrue(localContext.evaluate(condition))) {
            result = localContext.evaluateBlock(expressions);
            if (result.getType() == SemblanceType.BREAK) {
                return ((BreakValue) result).getValue();
            }
        }
        return result;
    }
}
