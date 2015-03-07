package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.runtime.InvalidFunctionDeclarationException;

import static net.ninjacat.semblance.utils.Values.asSList;
import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Created on 28/02/15.
 */
public abstract class ParametrizableCallable implements Callable {

    private final SList definition;
    private final SymbolAtom name;
    private final Parameters parameters;

    protected ParametrizableCallable(final SList definition) {
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

    protected Parameters getParameters() {
        return parameters;
    }
}
