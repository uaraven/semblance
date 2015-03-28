package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.ReturnValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * (return [value] [scope])
 */
public class Return extends SpecialForm {

    /**
     * Creates new instance of return.
     */
    public Return() {
        super("return", "&optional", "value", "&optional", "block-name");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispValue returnValue = parameters.isNil() ? NilCollection.INSTANCE : context.evaluate(parameters.head());
        if (parameters.length() > 1) {
            final LispValue scope = parameters.tail().head();
            return new ReturnValue(returnValue, asSymbol(scope));
        } else {
            return new ReturnValue(returnValue);
        }
    }
}
