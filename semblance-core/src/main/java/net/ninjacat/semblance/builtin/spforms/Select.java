package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.asSList;

/**
 * {@code select} special form
 * <p>
 * <pre>
 * (select
 *      (block-1)
 *      (block-2)
 *      ...
 *      (block-N)
 * )
 * </pre>
 */
@CreatesContext
public class Select extends SpecialForm {
    private static final long serialVersionUID = -3001316130532064386L;

    /**
     * Creates a new instance of SELECT
     */
    public Select() {
        super("select", "&rest", "blocks");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        for (final LispValue item : parameters) {
            final SList element = asSList(item);
            final Context local = LocalContext.namelessChildContext(context);
            final LispValue result = local.evaluateBlock(element);
            if (!NilCollection.INSTANCE.equals(result)) {
                return result;
            }
        }
        return NilCollection.INSTANCE;
    }

}