package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.DefaultContext;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Created on 01/03/15.
 */
public class Var extends SpecialForm {

    public static final SymbolAtom BINDING = symbol("name");
    public static final SymbolAtom VALUE = symbol("value");

    public Var() {
        super(list(symbol("var"), BINDING, VALUE));
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        Context localContext = DefaultContext.namelessChildContext(context);
        getParameters().apply(localContext, asSList(parameters));

        SymbolAtom binding = asSymbol(localContext.findSymbol(BINDING).get());
        LispValue value = localContext.findSymbol(VALUE).get();

        context.bind(binding, value);

        return value;
    }
}
