package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.InvalidFunctionDeclarationException;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Created on 28/02/15.
 */
public abstract class ParametrizedCallable implements LispCallable {

    private static final long serialVersionUID = 2569182753561285703L;

    private final SList definition;
    private final SymbolAtom name;
    private final Parameters parameters;

    @SuppressWarnings("OverloadedVarargsMethod")
    ParametrizedCallable(final String... definition) {
        this(list(Iter.of(definition).map(new Func<SymbolAtom, String>() {
            @Override
            public SymbolAtom apply(final String s) {
                return new SymbolAtom(s);
            }
        }).toArray(new SymbolAtom[]{})));
    }

    protected ParametrizedCallable(final SList definition) {
        this.definition = definition;
        if (definition.isNil()) {
            throw new InvalidFunctionDeclarationException(definition);
        }
        name = asSymbol(definition.head());
        parameters = new Parameters(asSList(definition.tail()));
    }

    @Override
    public SymbolAtom name() {
        return name;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.FUNCTION;
    }

    @Override
    public String repr() {
        return "(fun " + definition + ")";
    }

    @Override
    public String printIt() {
        return repr();
    }

    protected Parameters getParameters() {
        return parameters;
    }

}
