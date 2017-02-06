package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.asSList;

/**
 * Implements macro callable. When applied performs macro expansion and execution.
 */
public class Macro extends AbstractCallable {

    private static final long serialVersionUID = 3295367220730180060L;

    private final SymbolAtom name;
    private final LispCollection params;
    private final LispCollection body;
    private final Parameters formalParameters;

    /**
     * Creates a new instance of macro callable
     *
     * @param name   name of the macro
     * @param params list of formal parameters
     * @param body   body of the macro
     */
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

        formalParameters.bindExpressions(macroContext, parameters);
        final LispValue expandedMacro = macroContext.evaluateBlock(body);

        return context.evaluateBlock(asCollection(expandedMacro));
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.MACRO;
    }

    @Override
    public String repr() {
        return String.format("(defmacro %s %s %s)", name.repr(), params.repr(), body.repr());
    }

    @Override
    public String printIt() {
        return repr();
    }

}

