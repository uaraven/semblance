package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSList;

public class Macro implements Callable {

    private final SymbolAtom name;
    private final LispCollection params;
    private final LispCollection body;
    private final Parameters formalParameters;

    public Macro(final SymbolAtom name, final LispCollection params,
                 final LispCollection body) {
        this.name = name;
        this.params = params;
        formalParameters = new Parameters(asSList(params));
        this.body = body;
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
        return SemblanceType.MACRO;
    }

    @Override
    public String repr() {
        return String.format("(defmacro %s %s %s)", name.repr(), params.repr(), body.repr());
    }
}
