package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.UnexpectedValueException;
import net.ninjacat.smooth.utils.Option;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Created on 28/02/15.
 */
public class Parameters implements Iterable<Parameter> {
    private static final SymbolAtom OPTIONAL = symbol("&optional");
    private static final SymbolAtom REST = symbol("&rest");

    private final List<Parameter> formalParameters;

    public Parameters(SList definitions) {
        formalParameters = new ArrayList<>();
        Sweeper sweeper = Sweeper.Normal;
        for (LispValue value : definitions) {
            if (OPTIONAL.equals(value)) {
                sweeper = Sweeper.Optional;
            } else if (REST.equals(value)) {
                sweeper = Sweeper.Rest;
            } else {
                Parameter parameter = createParameter(value, sweeper);
                formalParameters.add(parameter);
            }
        }
    }

    @Override
    public Iterator<Parameter> iterator() {
        return formalParameters.iterator();
    }

    public List<Parameter> getFormalParameters() {
        return formalParameters;
    }

    private Parameter createParameter(LispValue value, Sweeper sweeper) {
        switch (sweeper) {
            case Normal:
                return createNormalParameter(value);
            case Optional:
                return createOptionalParamter(value);
            case Rest:
                return createRestParameter();
        }
        throw new UnexpectedValueException(value);
    }

    private Parameter createRestParameter() {
        return new RestParameter();
    }

    private Parameter createNormalParameter(LispValue value) {
        return new StandardParameter(asSymbol(value));
    }

    private Parameter createOptionalParamter(LispValue value) {
        if (isSymbol(value)) {
            return new OptionalParameter(asSymbol(value),
                    Option.<LispValue>absent(), Option.<SymbolAtom>absent());
        } else if (isList(value)) {
            SList list = asList(value);
            SymbolAtom name = asSymbol(list.head());
            LispValue defaultValue = list.tail().head();
            Option<SymbolAtom> flagName = list.length() > 2
                    ? Option.of(asSymbol(list.tail().tail().head()))
                    : Option.<SymbolAtom>absent();
            return new OptionalParameter(name, Option.of(defaultValue), flagName);
        } else {
            throw new UnexpectedValueException(value);
        }
    }

    private enum Sweeper {
        Normal,
        Optional,
        Rest
    }
}
