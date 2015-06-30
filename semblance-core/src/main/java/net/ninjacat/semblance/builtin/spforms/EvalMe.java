package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSList;
import static net.ninjacat.semblance.utils.Values.isList;

/**
 * This is implementation of comma operator which will just evaluate its only parameter.
 */
public class EvalMe extends SpecialForm {

    /**
     * Creates new instance of comma operator special form.
     */
    public EvalMe() {
        super("#--eval-me--#", "expression");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispValue head = parameters.head();
        if (isList(head) && asSList(head).head().equals(Constants.HiddenFunctions.UNWRAP)) {
            return head;
        } else {
            return context.evaluate(head);
        }
    }
}
