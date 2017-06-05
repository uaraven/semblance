package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

/**
 * @author oleksiivoronin, date: 15-03-06.
 */
@CreatesContext
public class Progn extends SpecialForm {

    private static final long serialVersionUID = -1560542200137925785L;

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
