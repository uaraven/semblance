package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Created on 28/02/15.
 */
public class Function extends AbstractCallable {

    private final SymbolAtom name;

    public Function(final SList definition) {
        name = asSymbol(definition.head());
    }

    @Override
    public SymbolAtom name() {
        return name;
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
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

    private void parseFormalParameters(final SList definition) {

    }
}
