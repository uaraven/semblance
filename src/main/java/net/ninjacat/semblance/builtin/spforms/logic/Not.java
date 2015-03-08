package net.ninjacat.semblance.builtin.spforms.logic;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.errors.runtime.ValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class Not extends SpecialForm {

    public Not() {
        super(list(symbol("not"), symbol("expression")));
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (parameters.isNil()) {
            throw new ValueExpectedException(parameters.getSourceInfo());
        }
        final LispValue head = parameters.head();
        if (isNilCollection(head) || isFalse(head)) {
            return SymbolAtom.TRUE;
        } else {
            return SymbolAtom.FALSE;
        }
    }
}
