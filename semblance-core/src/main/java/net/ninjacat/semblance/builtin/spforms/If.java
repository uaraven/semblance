package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.semblance.utils.Values;

/**
 * If special form.
 */
@SuppressWarnings("ClassNamingConvention")
public class If extends SpecialForm {

    private static final long serialVersionUID = 2060300854196650298L;

    /**
     * Creates new instance of If special form
     */
    public If() {
        super("if", "cond", "then", "&optional", "else");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispValue condition = parameters.head();

        final LispValue thenValue = parameters.tail().head();
        final LispValue elseValue = parameters.tail().tail().isNil()
                ? NilCollection.INSTANCE
                : parameters.tail().tail().head();

        final LispValue evaluated = context.evaluate(condition);
        if (Values.isTrue(evaluated)) {
            return context.evaluate(thenValue);
        } else if (!elseValue.equals(NilCollection.INSTANCE)) {
            return context.evaluate(elseValue);
        } else {
            return NilCollection.INSTANCE;
        }
    }
}
