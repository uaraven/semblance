package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * (var name value [name value]...)
 */
public class Var extends SpecialForm {

    public static final SymbolAtom BINDING = symbol("name");
    public static final SymbolAtom VALUE = symbol("value");

    public Var() {
        super(list(symbol("var"), BINDING, VALUE));
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        return bindVariable(context, parameters);
    }

    private LispValue bindVariable(Context context, LispCollection parameters) {
        LispValue name = parameters.head();
        LispValue value = context.evaluate(parameters.tail().head());
        context.bind(asSymbol(name), value);
        if (parameters.tail().tail().isNil()) {
            return value;
        } else {
            return bindVariable(context, parameters.tail().tail());
        }
    }
}
