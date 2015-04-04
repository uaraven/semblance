package net.ninjacat.semblance.builtin.spforms.logic;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.ValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.isFalse;
import static net.ninjacat.semblance.utils.Values.isNilCollection;

/**
 * NOT boolean operator
 */
@SuppressWarnings("ClassNamingConvention")
public class Not extends SpecialForm {

    /**
     * Creates new instance of NOT
     */
    public Not() {
        super("not", "expression");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (parameters.isNil()) {
            throw new ValueExpectedException(parameters.getSourceInfo());
        }
        final LispValue head = parameters.head();
        if (isNilCollection(head) || isFalse(head)) {
            return Constants.TRUE;
        } else {
            return Constants.FALSE;
        }
    }
}
