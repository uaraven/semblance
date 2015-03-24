package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.toSList;

/**
 * Function as defined by {@link net.ninjacat.semblance.builtin.spforms.Fn}
 */
public class InterpretedFunction implements Callable {

    private final SList body;
    private final Parameters formalParameters;
    private final SymbolAtom name;
    private final LispCollection paramDeclaration;

    /**
     * Creates new instance of function.
     *
     * @param name             Function name.
     * @param formalParameters Formal parameter declaration.
     * @param body             List of s-expressions.
     */
    public InterpretedFunction(final SymbolAtom name, final LispCollection formalParameters, final LispCollection body) {
        this.name = name;
        paramDeclaration = formalParameters;
        this.formalParameters = new Parameters(toSList(formalParameters));
        this.body = toSList(body);
    }

    @Override
    public SymbolAtom name() {
        return name;
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namedChildContext(name.repr(), context);

        formalParameters.apply(localContext, parameters);

        return localContext.evaluateBlock(body);
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.FUNCTION;
    }

    @Override
    public String repr() {
        return String.format("(fn %s %s %s)", name.repr(), paramDeclaration.repr(), body.repr());
    }
}
