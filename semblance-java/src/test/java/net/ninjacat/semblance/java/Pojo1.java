package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.BaseCallable;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

class Pojo1 extends BaseCallable {

    boolean concatCalled = false;
    boolean concat2Called = false;

    @Override
    public SymbolAtom name() {
        return symbol("pojo");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return CallableDispatcher.dispatch(this, context, parameters);
    }

    public LispValue concat(final Context context, final LispCollection params) {
        concatCalled = true;
        final LispCollection evaled = context.evaluateList(params);
        return string(asString(evaled.head()).getValue() + asString(evaled.tail().head()).getValue());
    }

    public LispValue concat2(final LispCollection params) {
        concat2Called = true;
        return string(asString(params.head()).getValue() + asString(params.tail().head()).getValue());
    }

    @Override
    public String printIt() {
        return repr();
    }
}
