package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.asSList;
import static net.ninjacat.semblance.utils.Values.isTrue;

/**
 * {@code cond} special form
 * <p>
 * <pre>
 *     (cond
 *          (cond1 expr1-1 expr1-2 expr1-3)
 *          (cond2 expr2-1 expr2-2)
 *          (cond3 expr3-1)
 *     )
 * </pre>
 */
@SuppressWarnings("ClassNamingConvention")
@CreatesContext
public class Cond extends SpecialForm {
    private static final long serialVersionUID = 2745050756133964790L;

    /**
     * Creates a new instance of COND
     */
    public Cond() {
        super("cond", "&rest", "expr-block");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        for (final LispValue el : parameters) {
            final SList element = asSList(el);
            final LispValue condition = context.evaluate(element.head());
            if (isTrue(condition)) {
                final Context local = LocalContext.namelessChildContext(context);
                return local.evaluateBlock(element.tail());
            }
        }
        return NilCollection.INSTANCE;
    }
}
