package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.special.RecursiveCallValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.toSList;

/**
 * Function as defined by {@link net.ninjacat.semblance.builtin.spforms.Fn}
 */
public class InterpretedFunction extends AbstractCallable {

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
        final Context localContext = LocalContext.namedChildContext(name, context);

        formalParameters.apply(localContext, parameters);
        do {
            final LispValue result = localContext.evaluateBlock(body);
            if (result.getType() == SemblanceType.RECURSIVE) {
                formalParameters.apply(localContext, ((RecursiveCallValue) result).getValue());
            } else {
                return result;
            }
        } while (true);
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
