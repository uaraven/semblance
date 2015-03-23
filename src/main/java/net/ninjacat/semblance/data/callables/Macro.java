package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.asSList;

/**
 * Implements macro callable. When applied performs macro expansion and execution.
 */
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
        final Context macroContext = LocalContext.namelessChildContext(context);
        final Context executionContext = LocalContext.namedChildContext(name.repr(), context);

        formalParameters.bindExpressions(macroContext, parameters);
        final LispValue expandedMacro = macroContext.evaluateBlock(body);

        return executionContext.evaluateBlock(asCollection(expandedMacro));
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

