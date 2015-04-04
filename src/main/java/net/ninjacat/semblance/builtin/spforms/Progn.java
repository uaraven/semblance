package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

/**
 * @author oleksiivoronin, date: 15-03-06.
 */
@CreatesContext
public class Progn extends SpecialForm {

    /**
     * Creates instance of Progn
     */
    public Progn() {
        super("progn", "&rest", "expressions");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namelessChildContext(context);
        return localContext.evaluateBlock(parameters);
    }
}
