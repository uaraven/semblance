package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Created on 28/02/15.
 */
public class Function implements Callable {

    private final SymbolAtom name;

    public Function(SList definition) {
        this.name = asSymbol(definition.head());
//        this.formalParameters = parseFormalParameters(definition.tail());
    }

    @Override
    public SymbolAtom name() {
        return name;
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        return null;
    }

    @Override
    public SemblanceType getType() {
        return null;
    }

    @Override
    public String repr() {
        return null;
    }

    private void parseFormalParameters(SList definition) {

    }
}
